/*
 * macOS async I/O backend using Grand Central Dispatch + pipe.
 *
 * Strategy: dispatch_async submits pread/pwrite work items to GCD's thread
 * pool; each completed item writes a fixed-size result message to a pipe.
 * The completion thread uses kqueue EVFILT_READ on the pipe read-end to
 * block or non-block-poll for results, then reads one message per call.
 *
 * This avoids EVFILT_AIO (EPERM on modern macOS) and POSIX AIO entirely.
 * Pipe writes of sizeof(pipe_msg_t) bytes are guaranteed atomic on macOS
 * (well within PIPE_BUF ≥ 512 B), so messages are never interleaved.
 */

#include "darwin_io.h"

#include <dispatch/dispatch.h>
#include <sys/event.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define OP_NOP   0
#define OP_READ  1
#define OP_WRITE 2

/* One prepared-but-not-yet-submitted I/O operation */
struct darwin_sqe {
    int      op;
    int      fd;
    void    *buf;
    uint32_t len;
    uint64_t offset;
    uint64_t user_data;
};

/* Message written atomically to the result pipe by each GCD work item */
typedef struct {
    uint64_t user_data;
    int32_t  res;
    int32_t  _pad;
} pipe_msg_t;

struct darwin_ring {
    dispatch_queue_t queue;  /* GCD concurrent queue for I/O work items */
    int kq;                  /* kqueue watching rfd via EVFILT_READ      */
    int rfd;                 /* pipe read  end – completion thread        */
    int wfd;                 /* pipe write end – GCD worker threads       */

    /* Pending SQEs: prepared between darwin_prep_* and darwin_submit.
       Submission is single-threaded (serialised by the Haskell MVar),
       so no locking is needed for this array. */
    struct darwin_sqe *pending;
    unsigned           pending_count;
    unsigned           pending_cap;   /* = sq_size */

    /* Scratch space for the one CQE returned between peek/wait and cqe_seen */
    darwin_cqe current_cqe;
};

/* ---------- lifecycle ---------------------------------------------------- */

darwin_ring *darwin_ring_create(unsigned sq_size) {
    darwin_ring *ring = calloc(1, sizeof(*ring));
    if (!ring) return NULL;
    ring->kq  = -1;
    ring->rfd = -1;
    ring->wfd = -1;

    ring->queue = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0);
    if (!ring->queue) goto fail;

    int pfd[2];
    if (pipe(pfd) < 0) goto fail;
    ring->rfd = pfd[0];
    ring->wfd = pfd[1];

    ring->kq = kqueue();
    if (ring->kq < 0) goto fail;

    /* Watch the pipe read-end: fires when result bytes are available */
    struct kevent kev;
    EV_SET(&kev, ring->rfd, EVFILT_READ, EV_ADD, 0, 0, NULL);
    if (kevent(ring->kq, &kev, 1, NULL, 0, NULL) < 0) goto fail;

    ring->pending = malloc(sq_size * sizeof(struct darwin_sqe));
    if (!ring->pending) goto fail;
    ring->pending_count = 0;
    ring->pending_cap   = sq_size;

    return ring;

fail:
    darwin_ring_destroy(ring);
    return NULL;
}

void darwin_ring_destroy(darwin_ring *ring) {
    if (!ring) return;
    if (ring->kq  >= 0) close(ring->kq);
    if (ring->rfd >= 0) close(ring->rfd);
    if (ring->wfd >= 0) close(ring->wfd);
    free(ring->pending);
    free(ring);
}

/* ---------- submission --------------------------------------------------- */

static int push_sqe(darwin_ring *ring, struct darwin_sqe sqe) {
    if (ring->pending_count >= ring->pending_cap) return -EAGAIN;
    ring->pending[ring->pending_count++] = sqe;
    return 0;
}

int darwin_prep_read(darwin_ring *ring, int fd, void *buf,
                     uint32_t len, uint64_t offset, uint64_t user_data) {
    struct darwin_sqe sqe = { OP_READ, fd, buf, len, offset, user_data };
    return push_sqe(ring, sqe);
}

int darwin_prep_write(darwin_ring *ring, int fd, const void *buf,
                      uint32_t len, uint64_t offset, uint64_t user_data) {
    struct darwin_sqe sqe = { OP_WRITE, fd, (void *)buf, len, offset, user_data };
    return push_sqe(ring, sqe);
}

int darwin_prep_nop(darwin_ring *ring, uint64_t user_data) {
    struct darwin_sqe sqe = { OP_NOP, 0, NULL, 0, 0, user_data };
    return push_sqe(ring, sqe);
}

int darwin_submit(darwin_ring *ring) {
    int wfd = ring->wfd;

    for (unsigned i = 0; i < ring->pending_count; i++) {
        struct darwin_sqe s = ring->pending[i];  /* copy by value into block */

        if (s.op == OP_NOP) {
            pipe_msg_t msg = { s.user_data, 0, 0 };
            ssize_t n;
            do { n = write(wfd, &msg, sizeof(msg)); } while (n < 0 && errno == EINTR);
            if (n < 0) { ring->pending_count = 0; return -errno; }
        } else {
            dispatch_async(ring->queue, ^{
                ssize_t n = (s.op == OP_READ)
                    ? pread (s.fd, s.buf, s.len, (off_t)s.offset)
                    : pwrite(s.fd, s.buf, s.len, (off_t)s.offset);
                int saved_errno = errno;
                pipe_msg_t msg = { s.user_data,
                                   (n >= 0) ? (int32_t)n : -saved_errno,
                                   0 };
                ssize_t w;
                do { w = write(wfd, &msg, sizeof(msg)); } while (w < 0 && errno == EINTR);
            });
        }
    }

    ring->pending_count = 0;
    return 0;
}

/* ---------- completion --------------------------------------------------- */

static int read_one_msg(darwin_ring *ring, darwin_cqe **cqe_out) {
    pipe_msg_t msg;
    ssize_t n;
    do { n = read(ring->rfd, &msg, sizeof(msg)); } while (n < 0 && errno == EINTR);
    if (n < 0) return -errno;
    if (n == 0) return -EAGAIN;
    ring->current_cqe.user_data = msg.user_data;
    ring->current_cqe.res       = msg.res;
    *cqe_out = &ring->current_cqe;
    return 0;
}

/* Blocking – safe/interruptible FFI; EINTR propagated to Haskell for retry */
int darwin_wait_cqe(darwin_ring *ring, darwin_cqe **cqe_out) {
    struct kevent event;
    int n = kevent(ring->kq, NULL, 0, &event, 1, NULL);
    if (n < 0) return -errno;   /* includes -EINTR */
    if (n == 0) return -EAGAIN;
    return read_one_msg(ring, cqe_out);
}

/* Non-blocking – unsafe FFI; must return immediately */
int darwin_peek_cqe(darwin_ring *ring, darwin_cqe **cqe_out) {
    static const struct timespec zero = {0, 0};
    struct kevent event;
    int n = kevent(ring->kq, NULL, 0, &event, 1, (struct timespec *)&zero);
    if (n <= 0) return -EAGAIN;
    return read_one_msg(ring, cqe_out);
}

void darwin_cqe_seen(darwin_ring *ring, darwin_cqe *cqe) {
    (void)ring; (void)cqe;
}

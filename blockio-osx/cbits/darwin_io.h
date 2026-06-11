#pragma once
#include <stdint.h>

/* Opaque handle type */
typedef struct darwin_ring darwin_ring;

/* Completion queue entry */
typedef struct darwin_cqe {
    uint64_t user_data;
    int32_t  res;
    int32_t  _pad;
} darwin_cqe;

/* Ring lifecycle */
darwin_ring *darwin_ring_create (unsigned sq_size);
void         darwin_ring_destroy(darwin_ring *ring);

/* Submission: each call appends one operation to the pending queue.
   Returns 0 on success, -EAGAIN if the queue is full. */
int darwin_prep_read (darwin_ring *ring, int fd, void       *buf,
                      uint32_t len, uint64_t offset, uint64_t user_data);
int darwin_prep_write(darwin_ring *ring, int fd, const void *buf,
                      uint32_t len, uint64_t offset, uint64_t user_data);
int darwin_prep_nop  (darwin_ring *ring, uint64_t user_data);
int darwin_submit    (darwin_ring *ring);

/* Completion – mirrors io_uring peek/wait/seen semantics */
int  darwin_wait_cqe(darwin_ring *ring, darwin_cqe **cqe_out); /* blocking     */
int  darwin_peek_cqe(darwin_ring *ring, darwin_cqe **cqe_out); /* non-blocking */
void darwin_cqe_seen(darwin_ring *ring, darwin_cqe *cqe);

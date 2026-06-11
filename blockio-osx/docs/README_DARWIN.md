# macOS Darwin port

This document describes the `blockio-osx` package and gives an honest
assessment of what it can and cannot offer compared to the Linux `io_uring`
backend in `blockio-uring`.

## Implementation

The macOS kernel has no equivalent to `io_uring`.  `EVFILT_AIO` is present in
the SDK headers but returns `EPERM` at runtime on modern macOS, so POSIX AIO
with kqueue notification is not a viable path.

The Darwin backend instead uses:

- **Grand Central Dispatch** (`dispatch_async` on a concurrent queue) to
  dispatch each `pread`/`pwrite` as an independent work item, giving the
  operating system control over parallelism.
- A **pipe** to carry fixed-size completion messages (user-data + result) from
  GCD worker threads back to the library's completion thread.
- **kqueue `EVFILT_READ`** on the pipe read-end for blocking (`wait_cqe`) and
  non-blocking (`peek_cqe`) completion polling.  `EVFILT_READ` on a pipe is
  fully supported on macOS.

The public API (`initIOCtx`, `submitIO`, `closeIOCtx`) is identical on both
platforms, exposed from `System.IO.BlockIO.OSX` here and `System.IO.BlockIO`
on Linux.

## Throughput assessment

### What the Linux backend uniquely provides

The performance advantage of `io_uring` comes from three mechanisms that have
no macOS equivalent:

1. **Batched submission via shared-memory rings** — N operations are handed to
   the kernel in a single syscall, eliminating per-operation context-switch
   overhead.
2. **True async kernel I/O** — submitted operations do not block OS threads;
   the kernel drives them independently.
3. **Registered buffers** — zero-copy I/O paths in recent kernel versions.

None of these are available on macOS.

### What the Darwin backend actually does

Each submitted I/O operation becomes one `dispatch_async` call whose body
executes a blocking `pread` or `pwrite` inside a GCD worker thread.  This is
structurally equivalent to spawning a pool of threads each calling
`runInBoundThread . p(read|write)`.  GCD replaces the thread pool; the pipe replaces
an MVar for result delivery.

### Genuine wins over naive Haskell code

The Darwin backend will outperform any GHC code that does file I/O
sequentially, because it keeps many operations in flight concurrently.  For
random 4 KB reads on a modern NVMe SSD (Apple Silicon in particular), the
hardware can service 32–64 concurrent operations simultaneously.  Sequential
code saturates none of that; this library saturates all of it.

### Where it does not improve over well-written Haskell

GHC code that manually spawns threads and calls `pread` via safe FFI with
sufficient concurrency will reach approximately the same throughput, because
both approaches ultimately submit the same blocking syscalls from OS threads.
GHC's own I/O manager already uses kqueue internally; there is no structural
latency advantage from going through this library on macOS.

### Bottom line

The Darwin port is a **compatibility and ergonomics layer**, not a performance
primitive.  

It allows code written against this library's API to build and run
correctly on macOS — useful for development, CI, and cross-platform testing —
and it will outperform naive sequential I/O.  For macOS production workloads
where maximum I/O throughput is the goal, the structural advantage that
`io_uring` provides on Linux (eliminating per-operation syscall overhead) does
not exist here, and you should expect throughput comparable to a well-tuned
concurrent `pread` program rather than a step-change improvement.

# blockio-osx

macOS (Darwin) backend for batched asynchronous disk I/O. Provides the same
public API as [`blockio-uring`](../blockio-uring) on Linux.

## Design

The macOS kernel has no equivalent to `io_uring`. The Darwin backend uses:

- **Grand Central Dispatch** — each `pread`/`pwrite` is dispatched as a
  `dispatch_async` work item on a GCD concurrent queue.
- **pipe** — GCD worker threads write fixed-size completion messages atomically.
- **kqueue `EVFILT_READ`** on the pipe read-end — for both blocking and
  non-blocking completion polling.

See [docs/README_DARWIN.md](docs/README_DARWIN.md) for the full design
rationale and an honest performance assessment.

## Benchmarks

See [docs/BENCH_DARWIN.md](docs/BENCH_DARWIN.md) for benchmark results on
Apple M4 Pro (GHC 9.12.2).

## Usage

The public API (`initIOCtx`, `submitIO`, `closeIOCtx`) is identical to
`blockio-uring`, so code written against that library can switch to this
package on macOS without any source changes.

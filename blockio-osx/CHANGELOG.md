# Revision history for blockio-osx

## 0.1.0.0 -- unreleased

### New features

* Initial release. macOS (Darwin) support via a GCD-based I/O backend.
  Dispatches `pread`/`pwrite` through Grand Central Dispatch and delivers
  completions via a pipe/kqueue. See `docs/README_DARWIN.md` for the design
  rationale and `docs/BENCH_DARWIN.md` for benchmark results.

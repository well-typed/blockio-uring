# Revision history for blockio-uring

## 0.2.0.0 -- 2026-04-30

### Breaking changes

* `IOCtxParams` has a new record field for configuring the use of IOWAIT metrics
  (see below).

### New features

* Support enabling/disabling IOWAIT metrics. See [issue
  #55](https://github.com/well-typed/blockio-uring/issues/55) and [PR
  #57](https://github.com/well-typed/blockio-uring/pull/57).

### Minor changes

None

### Bug fixes

* On systems with limited CPU and memory resources, `submitIO` could reliably
  fail with `EFAULT` error numbers because of frequent reaping and rescheduling
  of Haskell's lightweight threads. The bug is fixed by always running
  `submitIO` in a fresh bound thread. See [issue
  #58](https://github.com/well-typed/blockio-uring/issues/58) and [PR
  #60](https://github.com/well-typed/blockio-uring/pull/60).

## 0.1.0.3 -- 2026-03-12

* PATCH: support `ghc-9.14`. See [PR
  #53](https://github.com/well-typed/blockio-uring/pull/53).

## 0.1.0.2 -- 2025-09-22

* PATCH: support all `2.*` versions of `liburing`

## 0.1.0.1 -- 2025-07-31

* PATCH: support `liburing` up to version `2.11`

## 0.1.0.0 -- 2025-06-09

* First release

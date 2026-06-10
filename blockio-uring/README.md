# blockio-uring

This library supports disk I/O operations using the Linux `io_uring` API. The
library supports submitting large batches of I/O operations in one go. It also
supports submitting batches from multiple Haskell threads concurrently. The I/O
only blocks the calling thread, not all other Haskell threads. In this style,
using a combination of batching and concurrency, it is possible to saturate
modern SSDs, thus achieving maximum I/O throughput. This is particularly helpful
for performing lots of random reads.

The library only supports recent versions of Linux, because it uses the `io_uring`
kernel API. It only supports disk operations, not socket operations. The library
is tested only with Ubuntu (versions `22.04` and `24.04`), but other Linux
distributions should probably also work out of the box. Let us know if you run
into any problems!

## Installation

1. Install `liburing` version 2.1 or higher. Use your package manager of choice,
   or clone a recent version of https://github.com/axboe/liburing and install
   using `make install`.
2. Invoke `cabal build` or `cabal run`.

## Benchmarks

We can compare the I/O performance that can be achieved using library against
the best case of what the system can do. The most interesting comparison is
for performing random 4k reads, and measuring the IOPS -- the I/O operations
per second.

### Baseline using `fio`

We can use the `fio` (flexible I/O tester) tool to give us a baseline for the
best that the system can manage. While manufacturers often claim that SSDs can
hit certain speeds or IOPs for specific workloads (like random reads at queue
depth 32), `fio` gives a more realistic number that includes all the overheads
that are present in practice, such as overheads induced by file systems,
encrypted block devices, etc. This makes `fio` a sensible baseline.

The repo contains an `fio` configuration file for a random read benchmark using
`io_uring`, which we can use like so:

```bash
❯ fio ./benchmark/randread.fio
```

This will produce a page full of output, like so:

```
  read: IOPS=234k, BW=915MiB/s (960MB/s)(1024MiB/1119msec)
    slat (usec): min=41, max=398, avg=70.98, stdev=15.76
    clat (usec): min=59, max=4611, avg=342.75, stdev=199.02
     lat (usec): min=121, max=4863, avg=413.75, stdev=200.25
    clat percentiles (usec):
     |  1.00th=[   86],  5.00th=[  102], 10.00th=[  120], 20.00th=[  174],
     | 30.00th=[  262], 40.00th=[  289], 50.00th=[  318], 60.00th=[  343],
     | 70.00th=[  371], 80.00th=[  490], 90.00th=[  586], 95.00th=[  652],
     | 99.00th=[  922], 99.50th=[ 1090], 99.90th=[ 1598], 99.95th=[ 2180],
     | 99.99th=[ 4015]
   bw (  KiB/s): min=938280, max=946296, per=100.00%, avg=942288.00, stdev=5668.17, samples=2
   iops        : min=234570, max=236574, avg=235572.00, stdev=1417.04, samples=2
  lat (usec)   : 100=4.32%, 250=21.96%, 500=54.23%, 750=16.10%, 1000=2.74%
  lat (msec)   : 2=0.60%, 4=0.05%, 10=0.01%
  cpu          : usr=10.20%, sys=64.67%, ctx=66125, majf=0, minf=138
  IO depths    : 1=0.0%, 2=0.0%, 4=0.0%, 8=0.0%, 16=0.0%, 32=0.1%, >=64=100.0%
     submit    : 0=0.0%, 4=0.0%, 8=0.0%, 16=0.0%, 32=100.0%, 64=0.0%, >=64=0.0%
     complete  : 0=0.0%, 4=100.0%, 8=0.0%, 16=0.0%, 32=0.0%, 64=0.1%, >=64=0.0%
     issued rwts: total=262144,0,0,0 short=0,0,0,0 dropped=0,0,0,0
     latency   : target=0, window=0, percentile=100.00%, depth=128
```

The headline number to focus on is this bit:

```
  read: IOPS=234k
```

### Caching

By default, the `fio` configuration file declares that the random read benchmark
should use direct I/O, which bypasses any caching of file contents by the
operating system. This means that `fio` measures the raw performance of the disk
hardware. If you change the configuration file to use direct I/O, then the
reported statistics will be different because of caching. It is possible to
manually drop caches before running the benchmark to reduce the effects of
caching by invoking:

```bash
❯ sudo sysctl -w vm.drop_caches=1
```

### Haskell benchmarks

The `bench` executable expects three arguments: (1) a benchmark name, either
`low` or `high`, (2) a caching mode, either `Cache` or `NoCache`, and (3) a
filepath to a data file.

* The `low` benchmark targets low-level, internal code of the library. The
`high` benchmark uses the public API that is exposed from `blockio-uring`.

* `NoCache` enables direct I/O, while `Cache` disables it.

* The filepath to a data file will be used to read bytes from during the
  benchmark.

For a fair comparison with `fio` we use the same file that `fio` generated for
its own benchmark, `./benchmark/benchfile.0.0`. For example, we can invoke the
high-level benchmark with direct I/O as follows:

```bash
❯ cabal run bench -- high NoCache ./benchmark/benchfile.0.0
```

This will report some statistics:

```
High-level API benchmark
File caching:    False
Capabilities:    1
Threads     :    4
Total I/O ops:   262016
Elapsed time:    1.216050853s
IOPS:            215465
Allocated total: 24883952
Allocated per:   95
```

Though the output prints a number of useful metrics, the headline number to
focus on here is this bit:

```
IOPS:            215465
```

### Comparing results

For comparisons, we are primarily interested in IOPS.

On a maintainer's laptop (@jdral) with direct I/O enabled and using a single OS
thread the numbers in question are:

* `fio`: 234k IOPS
* High-level Haskell benchmark: 215k IOPS
* Low-level Haskell benchmark: 208k IOPS

So as a rough conclusion, we can get about 92% of the maximum IOPS using the
high-level Haskell API, or about 88% when using the low-level internals. On some
other machines, it was observed that the high-level benchmark could even match
the IOPs reported by `fio`. It might be unintuitive that the high-level
benchmark gets higher numbers than the low-level benchmark. However, the
high-level benchmarks uses multiple lightweight Haskell threads, which allows
the benchmark to have multiple I/O batches in flight at once even when using one
OS thread, while the low-level benchmark only submits I/O batches serially.

### Multi-threading

Note that much IOPs can be achieved by running the benchmarks with more than one
OS thread. This allows (more) I/O operations to be in flight at once, which
better utilises the bandwidth of modern SSDs. For example, if we want to use 4
OS threads for our benchmarks, then:

* In the `fio` configuration file we can add `numjobs=4` to fork 4 identical
  jobs that perform I/O concurrently.

* In the Haskell benchmarks we can configure the GHC run-time system to use 4 OS
  threads (capabilities) by passing `+RTS -N4 -RTS` to the benchmark executable.

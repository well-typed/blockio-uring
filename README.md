# blockio-uring

This library supports disk I/O operations using the Linux `io_uring` API. The
library supports submitting large batches of I/O operations in one go. It also
supports submitting batches from multiple Haskell threads concurrently. The
I/O only blocks the calling thread, not all other Haskell threads. In this
style, using a combination of batching and concurrency, it is possible to
saturate modern SSDs, thus achieving maximum I/O throughput. This is
particularly helpful for performing lots of random reads.

The library only supports recent versions of Linux, because it uses the
`io_uring` kernel API. The library only supports disk operations, not socket
operations.

## Installation

1. Install `liburing` version 2.2 or higher. Use your package manager of choice, or clone a recent version of https://github.com/axboe/liburing and install using `make install`.
2. Invoke `cabal build` or `cabal run`.

## Benchmarks

We can compare the I/O performance that can be achieved using library against
the best case of what the system can do. The most interesting comparison is
for performing random 4k reads, and measuring the IOPS -- the I/O operations
per second.

### Baseline using `fio`

We can use the `fio` (flexible I/O tester) tool to give us a baseline for the
best that the system can manage. The repo contains an fio configuration file
for a random read benchmark using io_uring, which we can use like so

```bash
user@machine:~/blockio-uring$ fio ./benchmark/randread.fio
```

This will produce a page full of output, like so
```
  read: IOPS=129k, BW=505MiB/s (529MB/s)(1024MiB/2028msec)
    slat (usec): min=10, max=735, avg=26.51, stdev=12.57
    clat (usec): min=54, max=4455, avg=695.23, stdev=204.80
     lat (usec): min=148, max=4492, avg=721.75, stdev=202.13
    clat percentiles (usec):
     |  1.00th=[  330],  5.00th=[  400], 10.00th=[  461], 20.00th=[  537],
     | 30.00th=[  578], 40.00th=[  627], 50.00th=[  668], 60.00th=[  725],
     | 70.00th=[  775], 80.00th=[  840], 90.00th=[  963], 95.00th=[ 1057],
     | 99.00th=[ 1287], 99.50th=[ 1401], 99.90th=[ 1778], 99.95th=[ 1942],
     | 99.99th=[ 2802]
   bw (  KiB/s): min=515760, max=518656, per=100.00%, avg=517584.00, stdev=1344.43, samples=4
   iops        : min=128940, max=129664, avg=129396.00, stdev=336.11, samples=4
  lat (usec)   : 100=0.01%, 250=0.17%, 500=13.42%, 750=51.94%, 1000=26.72%
  lat (msec)   : 2=7.70%, 4=0.04%, 10=0.01%
  cpu          : usr=18.55%, sys=30.04%, ctx=140673, majf=0, minf=10
  IO depths    : 1=0.0%, 2=0.0%, 4=0.0%, 8=0.0%, 16=0.0%, 32=0.1%, >=64=100.0%
     submit    : 0=0.0%, 4=0.0%, 8=0.0%, 16=0.0%, 32=100.0%, 64=0.0%, >=64=0.0%
     complete  : 0=0.0%, 4=100.0%, 8=0.0%, 16=0.0%, 32=0.0%, 64=0.1%, >=64=0.0%
     issued rwts: total=262144,0,0,0 short=0,0,0,0 dropped=0,0,0,0
     latency   : target=0, window=0, percentile=100.00%, depth=128
```
The headline number to focus on is the this bit
```
  read: IOPS=129k
```

Note that if you run this benchmark again, you'll get different numbers due
to caching. See below for the command to dropping caches.

### Haskell benchmarks

The `bench` executable expects two arguments: (1) a test name, either "low" or
"high", and (2) a filepath to a data file. For a fair comparison we use the
same file generated by fio, `./benchmark/benchfile.0.0`. So we can invoke the
test as follows:

```
user@machine:~/blockio-uring$ cabal run bench -- high ./benchmark/benchfile.0.0
```

This will report something like
```
High-level API benchmark
Total I/O ops: 262144
Elapsed time:  2.423867756s
IOPS:          108151
```

### Comparing results

We are primarily interested in IOPS.

On the author's laptop the numbers in question are:
* fio: 129k IOPS
* High-level Haskell API: 108k IOPS
* Low-level Haskell API: 117k IOPS

This is with dropping caches before each run.

So as a rough conclusion, we can get about 90% of the maximum IOPS using the
low level Haskell API, or about 80% when using the high level API.

For reference, the SSD in the laptop is a "Samsung SSD 970 EVO Plus 250GB"
which the manufacturer claims can do 250K IOPS for 4k random reads, at QD32,
or only 17k IOPS at QD1. Of course fio gives a more realistic number, including
all the overheads that are present in practice (file systems, encrypted block
devices etc).

We can get slightly higher performance with `fio` using `direct=1`, 140k IOPS,
on the same system. This uses Linux `O_DIRECT`. The same optimisation is
available to the Haskell API, but requires opening the files in `O_DIRECT` mode.


### Dropping caches

Remember to drop page caches in between test runs, or reported statistics will
be heavily distorted by page cache hits.

One way to drop page caches is to invoke the following:

```bash
user@machine:~/blockio-uring$ sudo sysctl -w vm.drop_caches=1
```

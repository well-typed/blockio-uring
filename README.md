# blockio-uring

## Installation

1. Install `liburing` version 2.2 or higher. Use your package manager of choice, or clone a recent version of https://github.com/axboe/liburing and install using `make install`.
2. Invoke `cabal build` or `cabal run`.

## Running tests

The `test` executable expects two arguments: (1) a test number 1..4, and (2) a filepath to a data file. Say `./fio-datadir/benchfile.0.0` is the path to the data file, then we invoke the tests as follows:

```
user@machine:~/blockio-uring$ cabal run test -- 3 ./fio-datadir/benchfile.0.0
```

### Generating a data file

The easiest way to generate a data file is by using `fio` on the
`bench_randread.fio` FIO job file included in this respository. This will create a
directory `fio-datadir` with a data file inside.

```bash
user@machine:~/blockio-uring$ fio ./bench_randread.fio
```

In addition to creating a data file, FIO will report a number of statistics like `iops` that we can compare to the statistics as reported by the `blockio-uring` tests.

### Dropping caches

Remember to drop page caches in between test runs, or reported staticistics will be heavily influenced by cache hits. For example, when running test number 3 with cleared caches, the IOPS is reported to be `220977`. When running the same test again without clearing the page caches, the IOPS is reported to be `478941`.

```bash
user@machine:~/blockio-uring$ cabal run test -- 3 ./fio-datadir/benchfile.0.0
Up to date
(262144,1.186297308s,220977)
user@machine:~/blockio-uring$ cabal run test -- 3 ./fio-datadir/benchfile.0.0
Up to date
(262144,0.547340811s,478941)
```

One way to drop page caches is to invoke the following:

```bash
user@machine:~/blockio-uring$ sudo sysctl -w vm.drop_caches=1
```
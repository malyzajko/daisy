<img src="daisy_logo.jpg" width="150">

# Project Daisy

## News

* The newest features (modular analysis and analysis of array-like data structures) are now merged in master!


## Getting started

First make sure that you have the following:

* Java 22 to 25

* a C compiler (`cc`)

* (for most features) [MPFR](http://www.mpfr.org/): \[`apt-get install libmpfr4`\] or \[`brew install mpfr`\]. If you get a linking error mentioning MPFR at runtime, you may need to recompile the [Java bindings](https://github.com/kframework/mpfr-java) and place them in lib/.

* (optionally) [Z3](https://github.com/Z3Prover/z3) and/or [dReal](https://github.com/dreal/dreal3)


### Compile

Daisy is set up to work with the [simple build tool (sbt)](http://www.scala-sbt.org/).

To compile Daisy type in Daisy's home directory:
```
$ sbt compile
```
(This may take a while the first time around.)

### Run

You can either create and run a standalone Daisy script:
```
$ sbt script
$ ./daisy [command-line options] path/to/input/file
```

or you can start an interactive sbt session:
```
$ sbt
[...]
> run [command-line options] path/to/input/file
```

**Note:** Daisy currently supports only one input file at a time.

For example:
```
> run testcases/rosa/Doppler.scala
```
should produce an output such as (your own timing information will naturally vary):
```
[  Info  ] ************ Starting Daisy ************
[  Info  ] Starting Scala extraction phase
[  Info  ] Starting Specs processing phase
[  Info  ] Starting functions phase
[  Info  ] Starting Dataflow error phase
[  Info  ] using interval for ranges, affine for errors
[  Info  ] analyzing fnc: doppler
[  Info  ] error analysis for uniform Double precision
[  Info  ] Starting Info phase
[  Info  ] doppler
[  Info  ]   Absolute error: 4.1911988101104756e-13
[  Info  ]   Real range:     [-158.7191444098274, -0.02944244059231351]
[  Info  ]   Relative error: 1.4235228893370562e-11
[  Info  ] time:
[  Info  ] Info: 5 ms, Dataflow error: 36 ms, functions: 1 ms, Specs processing: 3 ms, Scala extraction: 706 ms, total: 752 ms
```

### Input languages

By default Daisy reads Scala programs DSL, using the
Scala compiler as a frontend. Passing `--treesitter` selects the Tree-sitter frontend instead, which reads
Scala, C and FPCore. The language is chosen from the file extension:

| Extension | Language |
| --------- | -------- |
| `.scala`  | Scala|
| `.c`      | C |
| `.fpcore` | [FPCore 2.0](https://fpbench.org/spec/fpcore-2.0.html) |

```
$ ./daisy --treesitter testcases/fpbench-c-individual/doppler1.c
$ ./daisy --treesitter testcases/fpbench-fpcore/daisy.fpcore
```

### Test
```
$ sbt test
```
runs the regression suites, which check computed error bounds against reference
results, and the frontend suites, which check that every benchmark under
`testcases/` still parses.

```
./regression/scripts/run_all.sh
```
runs Daisy over a set of test cases and compares computed error bounds to
reference results. Passes tests, if it prints `All results consistent` for all tests.
Warning printed for Z3 analysis (Unexpected error from z3 solver) can be ignored.


## Daisy Features

Daisy is a framework that includes several different analyses of rounding errors
and finite-precision optimizations; they are enabled using command-line options.

The `scripts` folder has bash scripts that demonstrate the core features and the
corresponding command-line options. Many of these will run Daisy on the standard
[FPBench](https://fpbench.org/benchmarks.html) benchmarks.

More details can be found in the (always work-in-progress) [documentation](doc/documentation.md)!


## Publications

Daisy's features have been described in a number of papers:

  * [Modular Optimization-Based Roundoff Error Analysis of Floating-Point Programs](https://malyzajko.github.io/papers/sas2023a.pdf), SAS'23

  * [Scaling up Roundoff Analysis of Functional Data Structure Programs](https://malyzajko.github.io/papers/sas2023b.pdf), SAS'23

  * [Regime Inference for Sound Floating-Point Optimizations](https://malyzajko.github.io/papers/emsoft2021.pdf), EMSOFT'21 - see the 'regimes' branch

  * [Sound Probabilistic Numerical Error Analysis](https://malyzajko.github.io/papers/iFM2019.pdf), iFM'19 - see the 'probabilistic' branch

  * [Synthesizing Efficient Low-Precision Kernels](https://malyzajko.github.io/papers/atva2019.pdf), ATVA'19 - see the 'approx' branch

  * [Sound Approximation of Programs with Elementary Functions](https://malyzajko.github.io/papers/cav2019b.pdf), CAV'19

  * [Discrete Choice in the Presence of Numerical Uncertainties](https://malyzajko.github.io/papers/emsoft2018.pdf), EMSOFT'18 - see the 'probabilistic' branch

  * [Sound Mixed-Precision Optimization with Rewriting](https://malyzajko.github.io/papers/iccps18_mixedtuning_rewriting.pdf), ICCPS'18

  * [Daisy tool paper](https://malyzajko.github.io/papers/tacas18_daisy_toolpaper.pdf), TACAS'18

  * [On Sound Relative Error Bounds for Floating-Point Arithmetic](https://malyzajko.github.io/papers/fmcad17_relative.pdf), FMCAD'17


## Contributors

In alphabetic order: Anastasia Isychev (Anastasiia Izycheva), Anastasia Volkova, Andrea Gilot, Arpit Gupta, Debasmita Lohar, Einar Horn, Ezequiel Postan, Fabian Ritter, Fariha Nasir, Heiko Becker, Joachim Bard, Jonas Kraemer, Ramya Bankanal, Raphael Monat, Robert Bastian, Robert Rabe, Rosa Abbasi, Saksham Sharma.

## Acknowledgements

A big portion of the infrastructure has been inspired by and sometimes
directly taken from the Leon project (see the LEON_LICENSE).

Especially the files in frontend, lang, solvers and utils bear more than
a passing resemblance to Leon's code.
Leon version used: 978a08cab28d3aa6414a47997dde5d64b942cd3e

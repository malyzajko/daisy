# Project Daisy

<img src="https://github.com/malyzajko/daisy/daisy_logo.jpg" width="150">


## News

* The newest features (modular analysis and analysis of array-like data structures) are now merged in master!


## Getting started

First make sure that you have the following:

* Java 8 or Java 11

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
Extracting program
[  Info  ]
[  Info  ] Starting specs preprocessing phase
[  Info  ] Finished specs preprocessing phase
[  Info  ]
[  Info  ]
[  Info  ] Starting range-error phase
[  Info  ] Finished range-error phase
[  Info  ]
[  Info  ] Starting info phase
[  Info  ] doppler
[  Info  ] error: 4.1911988101104756e-13, range: [-158.7191444098274, -0.02944244059231351]
[  Info  ] Finished info phase
[  Info  ] time:
[  Info  ] info:      6 ms, rangeError:    360 ms, analysis:      6 ms, frontend:   2902 ms,
```

### Test
```
./regression/scripts/run_all.sh
```
will run Daisy on a set of test cases and compare computed error bounds to
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

In alphabetic order: Anastasia Isychev (Anastasiia Izycheva), Anastasia Volkova, Arpit Gupta, Debasmita Lohar, Einar Horn, Ezequiel Postan, Fabian Ritter, Fariha Nasir, Heiko Becker, Joachim Bard, Jonas Kraemer, Ramya Bankanal, Raphael Monat, Robert Bastian, Robert Rabe, Rosa Abbasi, Saksham Sharma.

## Acknowledgements

A big portion of the infrastructure has been inspired by and sometimes
directly taken from the Leon project (see the LEON_LICENSE).

Especially the files in frontend, lang, solvers and utils bear more than
a passing resemblance to Leon's code.
Leon version used: 978a08cab28d3aa6414a47997dde5d64b942cd3e

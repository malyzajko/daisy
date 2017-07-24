# Project Daisy

Daisy is a framework for accuracy analysis of numerical programs whose aim
is to be modular and extensible. It is currently very much work in progress,
so stay tuned for updates and new features.

The current functionality includes:
- static analysis of roundoff errors in arithmetic expressions for various
  floating-point and fixed-point precisions
- code generation of fixed-point arithmetic code from mathematical expressions
- dynamic analysis using rationals or arbitrary precision
- formal certificate generation for analyses with interval arithmetic (in branch certification)

## Contributors

Many people have contributed to this code (in alphabetical order): 
Heiko Becker, Einar Horn, Anastasiia Izycheva, Debasmita Lohar, Raphaël Monat, Ezequiel Postan, Fabian Ritter, Saksham Sharma

## Documentation

  To come soon.

## First steps

Daisy is set up to work with the [simple build tool (sbt)](http://www.scala-sbt.org/).
Once you have sbt, type in daisy's home directory:
```
$ sbt
```
This will run sbt in interactive mode. Note, if you run sbt for the first time,
*this operation may take a substantial amount of time* (heaven knows why). SBT helpfully
prints what it is doing in the process. If you feel like nothing has happened for an unreasonable
amount of time, abort and retry. This usually fixes the problem, otherwise try the old trick: restart.

To compile daisy:
```bash
> compile
```

To Daisy run an example:
```
> run testcases/rosa/Doppler.scala
```
Note that Daisy currently supports only one input file at a time.
The above command should produce an output such as (your own timing information will naturally vary):
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


To see all command-line options:
```
> run --help
```

If you don't want to run in interactive mode, you can also call all of the above
commands simply with 'sbt' prefixed, e.g. $ sbt compile.

You can also run Daisy outside of sbt. For this use '$ sbt script' which will
generate a script called 'daisy' which includes all the necessary files (and then some).
You can then run Daisy on an input file as follows:
```bash
$ ./daisy testcases/rosa/Doppler.scala
```

## Additional Software

Some features of Daisy require additional software to be installed.
Currently, this is

* Z3: if you want to compute ranges with the help of the SMT solver Z3, you need to
[install it first on your machine](https://github.com/Z3Prover/z3).

* MPFR: Daisy uses a [Java binding](https://github.com/kframework/mpfr-java).


Acknowledgements
----

A big portion of the infrastructure has been inspired by and sometimes
directly taken from the Leon project (see the LEON_LICENSE).

Especially the files in frontend, lang, solvers and utils bear more than
a passing resemblance to Leon's code.
Leon version used: 978a08cab28d3aa6414a47997dde5d64b942cd3e



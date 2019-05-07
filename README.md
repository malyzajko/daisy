# Project Daisy

<img src="https://people.mpi-sws.org/~eva/daisy_logo.jpg" width="150">

## News

  * Daisy now has [documentation](doc/documentation.md)!

  * Mixed-precision tuning is now available through the `--mixed-tuning` flag!

  * Polynomial approximation of floating-point elementary functions is available through the `--metalibm --codegen --lang=C` flags!

  * Daisy now also has an online interface: http://daisy.mpi-sws.org/! 

## First steps

Note you need to have ***at most*** Java 8 and SBT version 0.13.

Daisy is set up to work with the [simple build tool (sbt)](http://www.scala-sbt.org/).
Once you have sbt (version 0.13*!), type in daisy's home directory:
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
commands simply with 'sbt' prefixed, e.g.
```
$ sbt compile
```

You can also run Daisy outside of sbt. For this use 
```
$ sbt script
```
which will generate a script called 'daisy' which includes all the necessary files.
You can then run Daisy on an input file as follows:
```bash
$ ./daisy testcases/rosa/Doppler.scala
```

## Additional Software

Some features of Daisy require additional software to be installed.
Currently, this is

* An SMT solver which can be used to improve ranges: [Z3](https://github.com/Z3Prover/z3) \[[Linux](https://github.com/Z3Prover/z3/releases)\] \[[Mac](https://github.com/Z3Prover/z3/releases)\] and/or [dReal](https://github.com/dreal/dreal3) \[[Linux](https://github.com/dreal/dreal3/releases)\]

* [MPFR](http://www.mpfr.org/) for error under-approximation and transcendental calculations: \[`apt-get install libmpfr4`\]  \[`brew install mpfr`\]. 
    (On macOS, if the library installed is not libmpfr.4.dylib, you may have to recompile the [Java bindings](https://github.com/kframework/mpfr-java) and place them in lib/.)

* Metalibm and all additional dependencies:

  1. Install/update dependencies (all but fplll on Mac worked with Homebrew): 
    mpfr, mpfi, fplll (https://github.com/fplll/fplll.git, you may have to add the installation path to LD_LIBRARY_PATH), automake, libtool, flex, bison, boost

  2. Install Sollya
    - check out repository: git clone https://scm.gforge.inria.fr/anonscm/git/sollya/sollya.git 
    - ./autogen.sh
    - ./configure
    - make
    - make install

  3. Install Gappa
    - get code: http://gappa.gforge.inria.fr/
    - ./configure
    - ./remake
    - ./remake install

  4. Setup Metalibm 
    - extract lib/metalibm.zip to metalibm/ (i.e. in the daisy home directory)
    - add paths to Sollya and Gappa to your PATH
    - make gcc and g++ and not clang your default GCC compiler
    - make  (in the metalibm/ directory)

  5. Run Daisy+Metalibm on an example:
    - sbt  (in the home directory)
    - run testcases/transcendentals/TransBenchsErrorBoundsLarge.scala --functions=sinxx10 --metalibm --codegen --lang=C

## Contributors

In no particular order: Saksham Sharma, Einar Horn, Debasmita Lohar, Heiko Becker, Ezequiel Postan, 
Fabian Ritter, Anastasiia Izycheva, Raphael Monat, Fariha Nasir, Robert Bastian, Anastasia Volkova.

## Intellij Idea Setup
To run Daisy in Intellij Idea you first have to install the Scala Plugin: Settings (Ctrl + Alt + S) -> Plugins. 
Choose Scala in the list and select "Install JetBrains Plugin ...". 
Then let Idea know where is your Scala (or make sure Scala SDK is already there): Project Structure -> Global Libraries -> New Global Library -> Scala SDK -> select the source folder for the SDK on your machine.
Also make sure the Java SDK is set up for Idea (Project Structure -> SDKs -> check that your JDK is here or add it here).

Choose File -> New -> Project from Existing Source -> path-to-the-build.sbt-file
or
File -> New -> Project from Version Control -> Git -> and put git-rts@gitlab.mpi-sws.org:AVA/daisy.git into the URL field and 
select the destination folder for source files to be copied.

After the setup run Daisy in the Terminal of Intellij Idea using sbt as described above.

Acknowledgements
----

A big portion of the infrastructure has been inspired by and sometimes
directly taken from the Leon project (see the LEON_LICENSE).

Especially the files in frontend, lang, solvers and utils bear more than
a passing resemblance to Leon's code.
Leon version used: 978a08cab28d3aa6414a47997dde5d64b942cd3e



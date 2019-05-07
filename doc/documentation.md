# Daisy's Documentation


Daisy is a framework for verifying and optimizing numerical programs,
especially focused on the accuracy of computations.

Its goal is to provide a modular framework that allows quick and easy
experimenting with new techniques and ideas. To this end, it provides basic
infrastructure such as parsing and type checking of inputs as well as
basic utilities such as interval arithmetic, interfaces to solvers and
well-established techniques for accuracy analysis of roundoff errors.

While performance is, of course, a consideration, the first priority of Daisy's
code design is ease of understanding and extensibility of the code.

## Publications

  * Sound Approximation of Programs with Elementary Functions, CAV'19

  * [Sound Mixed-Precision Optimization with Rewriting](https://people.mpi-sws.org/~eva/papers/iccps18_mixedtuning_rewriting.pdf), ICCPS'18

  * [Daisy tool paper](https://people.mpi-sws.org/~eva/papers/tacas18_daisy_toolpaper.pdf), TACAS'18

  * [On Sound Relative Error Bounds for Floating-Point Arithmetic](https://people.mpi-sws.org/~eva/papers/fmcad17_relative.pdf), FMCAD'17

## User's Guide

### Input Syntax

Daisy accepts one input file which must follow the following basic structure::
```Scala
  import daisy.lang._
  import Real._

  object TestObject {

    def function1(x: Real): Real = {
      require(0 <= x && x <= 1)

      val z = x + x
      z * x

    } ensuring(res => res +/- 1e-5)

    def function2 ...
  }
```
Input programs are written over a real-valued non-executable data type `Real`
and consist of a top-level object and a number of function definitions,
where currently each function is analyzed in isolation.
The Real data type is implemented as a DSL in Scala and its definition and
operations can be found in library/Real.scala.

To note about the input language:

  * Function bodies may consist of arithmetic expressions, including square root and the most common transcendental functions (sin, cos, tan, exp, log) and immutable variable definitions ('val z = ').

  * The require clause must specify the ranges (i.e. lower AND upper bounds) for all input variables.

  * The ensuring clause is optional in general, but may be required for certain functionalities like for instance mixed-precision tuning.

  * The notation `x +/- e` defines an absolute roundoff error of magnitude 'e' on the variable 'x'.


### Command-line Options

These are the main command-line options. For a full list, use `--help`.

`--analysis=[dataflow:opt:relative] {--subdiv}`
  
Daisy supports forward dataflow analysis (as implemented in Rosa, Fluctuat and Gappa) and an optimization-based analysis (as implemented in FPTaylor and Real2Float). These methods compute absolute error bounds, and whenever
a relative error can be computed, it is also reported. Daisy also supports a dedicated relative error computation which is often more accurate, but also more expensive. All methods can be combined with interval subdivision, which
can provide tighter error bounds at the expense of larger running times. Accuracy and correspondingly cost of both dataflow and optimization-based analysis can be adjusted by choosing the method which is used to bound ranges:

`--rangeMethod=[interval:affine:smt] {--solver=[z3, dReal]}`

With the smt option, the user can select between currently two SMT solvers, which have to be installed separately. For dataflow analysis, one can also select
the method for bounding errors:

`--errorMethod=[interval, affine]`

Daisy performs roundoff error analysis by default w.r.t. to uniform double floating-point precision, but it also supports various other floating-point and
fixed-point precisions:

`--precision=[Fixed8:Fixed16:Fixed32:Float16:Float32:Float64:Quad:QuadDouble]`

Mixed-precision, i.e. choosing different precisions for different variables, is supported by providing a mapping from variables to precisions in a separate file:

`--mixed-precision=file`

Finite-precision arithmetic is not associative, i.e. different rewritings, even though they are equivalent under a real-valued semantics, will exhibit different
roundoff errors. The `--rewrite` optimization uses genetic search to find a rewriting for which it can show the smallest roundoff error.

Daisy prints the analysis result to the terminal. If a postcondition is specified, but the computed error does not satisfy it, Daisy also prints a warning.
Optionally, the user can also choose to generate executable code `--codegen` in Scala or C.

Static analysis computes a sound over-approximation of roundoff errors, but an under-approximation can also be useful, e.g. to estimate how big the over-
approximation of static analysis is. This is provided by the `--dynamic` analysis in Daisy which runs a program in the finite precision of interest and a higher-
precision version side-by-side. For this, the MPFR library is required.


### Running Generated Code

  Daisy can generate code in Scala and in C which then needs to be compiled
  with a standard C or Scala compiler to be run. However, for the reported analysis
  results to be correct, care must be taken when choosing compiler options.
  In Scala, this is not an issue as the compiler does not reorder floating-point
  computations or performs other aggressive optimizations.

  For gcc, here are the recommended compiler options:

`gcc -O3 -ffast-math -fno-unsafe-math-optimizations -fno-signed-zeros -ffp-contract=off`

---
 - `-ffp-contract=fast`\
 Discouraged. Might introduce FMAs that Daisy doesn't know about, so it will calculate suboptimal accuracy guarantees. Instead, either use a)&nbsp;explicit FMAs in the input code, or b)&nbsp;`daisy --comp-opts=fma` to introduce FMAs (greedily by default, or using `--rewrite`).\
 Daisy-generated code containing FMAs will disable this option.

    - `-ffast-math`

      - `-fno-math-errno`\
     Encouraged. Verified programs will not throw arithmetic exceptions.

      - `-funsafe-math-optimizations`\
      Reordering will change rounding behavior. Use Daisy's reordering (`--rewrite`) to get correct accuracy guarantees.

        - `-fassociative-math`\
        Disallowed. Associative reordering will change rounding behavior.
        - `-freciprocal-math`\
        Disallowed. Reciprocal rewriting will change rounding behavior.

      - `-ffinite-math-only`\
      Encouraged. Verified code will not produce NaNs or Infs.

      - `-fno-trapping-math`\
      Encouraged. Daisy-generated code does not use traps. Verification ensures there will be no fatal traps (_division by zero_, _overflow_, _underflow_, _invalid operation_), and _inexact_ is taken into consideration.

      - `-fno-signaling-nans`\
      Encouraged. Verified code will not produce NaNs.

      - `-fno-rounding-math`\
      Encouraged. Daisy assumes (and Daisy-generated code only uses) default rounding behavior.


    - `-fno-signed-zeros`\
    Encouraged

    - `-fno-fp-int-builtin-inexact`\
    Unnecessary. Daisy does not generate ceil, floor, round or trunc.

    - `-ffloat-store`\
    Unnecessary (performance hit). Verification assumes this, but not setting this flag will improve accuracy and not introduce errors.

    - `-fexcess-precision=standard` \
    Unnecessary (performance hit). Same as `-ffloat-store`.



## Developer's Guide

### Daisy's Structure

Daisy is build up in phases (or stages) to provide a modular architecture.
In addition, commonly used functionality is provided in a set of tools and
utilities. Each of Daisy's phases has a 'run' function which takes as input
a 'Context' and a 'Program' object. The context carries around information
produced by previous phases and the program captures the current state of
the program. E.g. a phase performing an analysis will modify the context
(only) and a phase performing an optimization will modify, i.e. return a
modified program.


### Repository Navigation

  Here are the most notable components of the Daisy repository:

  * lib/: jar files that Daisy requires

  * library/: definition of the 'Real' data type used in the specification language

  * project/: simple build tool (sbt) configuration

  * scripts/: various bash scripts to run sets of experiments

  * src/main/scala/daisy: Daisy's source code

    * Main.scala: the main entry point

    * analysis/: phases performing various accuracy analyses

    * backend/CodeGenerationPhase: generates output code

    * backend/InfoPhase: prints accumulated information in a nice format

    * experiments/: phases performing experiments which were needed for some subproject

    * frontend/: parsing of the input program is performed by calling the normal Scala compiler and extracting the Scala AST's to Daisy AST's

    * lang/: definition of Daisy's internal representation of programs, e.g. Trees.scala defines the AST

    * opt/: phases performing optimization of programs

    * search/: general search algorithms

    * solvers/: interface to backend solvers

    * tools/: useful tools such as interval arithmetic, affine arithmetic, rational, interface to arbitrary precision, etc.

    * transform/: phases performing a transformation of the program

    * utils/: utilities used by Daisy, e.g. code printers for various languages, internal data structures

  * src/test: unit and regression tests

  * testcases/: collected testcases from various projects, the name of the subdirectory mostly reflects the source



### Make

  Daisy is written in Scala and set up with the simple build tool which handles
  most of the dependencies.


### Daisy's Phases

  * frontend.ExtractionPhase: parsing of input program through Scala compiler

  * analysis.SpecsProcessingPhase: parsing of require and ensuring clauses, as well as mixed-precision input files

  * analysis.RangePhase: computes real-valued ranges of all intermediate AST nodes

  * analysis.AbsErrorPhase: computes absolute roundoff errors (expects ranges to have been computed)

  * analysis.DataflowPhase: computes absolute roundoff errors (computes both ranges and errors)

  * analysis.RelativeErrorPhase: computes relative errors directly

  * analysis.TaylorErrorPhase: computes absolute errors using the approach from FPTaylor

  * analysis.DataflowSubdivisionPhase: computes absolute errors using subdivision of the input domains

  * analysis.DynamicPhase: performs dynamic analysis of errors

  * transform.TACTransformerPhase: transforms the program into three-address code

  * transform.ConstantTransformerPhase: pulls out constants and assigns them to local variables

  * opt.RewritingOptimizationPhase: rewrites the program to optimize for better accuracy

  * opt.MixedPrecisionOptimizationPhase: performs mixed-precision tuning (see the ICCPS'18 paper)

  * backend.InfoPhase: prints information stored in the 'Context' in a nice common format

  * backend.CodeGenerationPhase: generates code of the final program


### Tests

  Daisy has a number of unit and regression tests. These can be run in sbt's interactive
  mode via

  > test

  If you want to run only a specific test:

  > testOnly regression.AbsErrorRegressionTest


### Benchmarking

  Daisy is set up to use Scalameter for benchmarking on the JVM. Scalameter takes care of warming up the JVM and other mundane tasks. You can run Scalameter benchmarks from src/test, however, then the benchmarking will also be done each time someone calls > test. This is suboptimal, because these benchmarking runs can potentially take a very long time.

  To avoid this, the project has a configuration called 'bench' which allow you to run Scalameter tests in the src/bench folder. (Note, by default, the sbt 'test' command will only search the src/test folder.)

  To run all benchmarking in src/bench:

  > bench:test

  To run only one benchmark:

  > bench:testOnly RangeBenchmark

### Contribution Guidelines

  We are happy about any contributions to Daisy. Please note the following though:

  * Most of standard Scala's data structures are immutable, except for Seq. Thus, when using Seq, include 'import scala.collection.immutable.Seq' to make sure you are using the correct one.

  * All tests should pass, or need to be adapted (with an explanation/justification).


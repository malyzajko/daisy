# Omelette

Term rewriting and interval evaluation engine powered by [egg's](https://egraphs-good.github.io/)
implementation of equality saturation. Rewritten expressions are extracted only if a cost function is given. 


## Usage

TLDR: install Rust and run `./install.sh`.

---

First, make sure Rust is installed. Then, Omelette can be compiled and ran using:
```
$ cargo build -r
$ ./target/release/omelette <ARGUMENTS>...
```
Or, equivalently: 
```
$ cargo run -r -- <ARGUMENTS>...
```

The `-r` flag (`--release` alias) is not mandatory but strongly recommended for performance (it can reduce the time to run tests by a factor of 250). 

The arguments should be given as follows: 
```
Usage: omelette [OPTIONS] <EXPRESSIONS>... [INTERVALS]...

Arguments:
  <EXPRESSIONS>...
          Expression(s) to process. Given in prefix notation. Terminate list with `:`

  [INTERVALS]...
          Intervals of all variables referenced in the expression(s). Given with the syntax `ID:[MIN,MAX]`

Options:
  -i, --iter-limit <ITER_LIMIT>
          Maximum number of iterations to run equality saturation for

          [default: 30]

  -c, --cost <COST>
          Cost function used to compare two rewritings during extraction. If no value is given, extraction is
          disabled

          Possible values:
          - ast-size:        Number of nodes
          - width:           Interval width; `upper - lower`
          - magnitude:       Interval magnitude; `max(|lower|, |upper|)`
          - width-first:     Interval width first, then AST size
          - magnitude-first: Interval magnitude first, then AST size

  -v, --verbose
          Set to write debug information to STDERR

  -h, --help
          Print help (see a summary with '-h')
```

The output is a line for each evaluated expression; `REWRITTEN_EXPR : [MIN,MAX]` if extraction is enabled; 
`[MIN,MAX]` otherwise. All expressions are given in prefix notation, and all interval bounds are given as
rationals or integers. 

For usage in Daisy, the binary must be compiled and copied as `../lib/omelette`. This is handled by the 
provided `install.sh` script. 


## Code maintenance

Tests are ran with: 
```
$ cargo test -r
```
Code documentation is built and opened with: 
```
$ cargo doc --open
```

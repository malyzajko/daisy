use std::ops::Deref;
use egg::*;
use lazy_static::lazy_static;
use crate::{EGraph, Rewrite, interval::Interval};

/// Gets the set of [rewrite rules](Rewrite). 
pub fn get<'a>() -> &'a [Rewrite] {
    RULES.deref()
}

/// Helper function to build a predicate over a set of pattern variables with a predicate over a set of
/// [`Interval`]s. 
/// 
/// Parses the set of pattern variables, and constructs a function that first extracts their respective
/// [`Interval`]s from the e-graph analysis and then calls the given predicate with the intervals as
/// argument. 
/// 
/// # Parameters
/// - `vars`: pattern variable identifiers. 
/// - `predicate`: predicate over a set of [`Interval`]s. 
fn predicate<P, const N: usize>(vars: [&str; N], predicate: P) -> impl Fn(&mut EGraph, Id, &Subst) -> bool
where
    P: Fn([&Interval; N]) -> bool
{
    let vars = vars
        .map(str::parse)
        .map(|v| v.expect("String should be valid pattern variable"));
    move |egraph, _, subst| {
        let intervals = vars.map(|v| &egraph[subst[v]].data);
        predicate(intervals)
    }
}

/// Returns predicate determining whether e-class [`Interval`] contains zero. 
fn non_zero(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    predicate([var], |[x]| !x.contains_zero())
}

/// Returns predicate determining whether e-class [`Interval`] is greater than zero. 
fn positive(var: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    predicate([var], |[x]| x.0.is_sign_positive() && !x.0.is_zero())
}

/// Returns predicate determining whether the first e-class interval is greater (has only larger values) than
/// the second.  
fn geq(a: &str, b: &str) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    predicate([a, b], |[x, y]| x.0 >= y.1)
}

/// Wraps [egg's rewrite macro](egg::rewrite) to create a list of rewrite rules. 
/// 
/// This solves the problem that when egg's macro is provided a bidirectional rule, it returns a vector with
/// two rewrite rules. In order to include these in a list together with unidirectional rules, the latter
/// would need to be placed in a vector of length 1 before all nested vectors are flattened to a single
/// vector, which becomes the rule-set.
/// 
/// This implementation instead utilizes a
/// [TT-muncher](https://danielkeep.github.io/tlborm/book/pat-incremental-tt-munchers.html) and
/// [push-down accumulator](https://danielkeep.github.io/tlborm/book/pat-push-down-accumulation.html) to
/// place all rules in a single flat array. This approach needs no hacks to place uni- and bidirectional
/// rules in the same array, and requires no dynamic memory allocation (on top of whatever egg allocates for
/// each rule.)
/// 
/// A note on how the implementation works: Rust's macros are essentially pure functions over tokens. This
/// macro parses one rule at a time from the token stream and appends it to an accumulator argument, which is
/// then simply returned once the token stream is emptied. 
macro_rules! rewrite_rules {
    // base case; all rules have been gathered in the array $acc, so simply return it
    (@accum $acc:expr) => {
        $acc
    };
    // parses a unidirectional rule from the token stream
    (@accum[$($acc:tt)*] $name:literal: $lhs:tt => $rhs:tt $(if $cond:expr)*, $($tail:tt)*) => {
        rewrite_rules!{
            @accum [
                $($acc)*
                rewrite!($name; $lhs => $rhs $(if $cond)*), 
            ]
            $($tail)*
        }
    };
    // parses a bidirectional rule from the token stream
    (@accum[$($acc:tt)*] $name:literal: $lhs:tt <=> $rhs:tt $(if $cond:expr)*, $($tail:tt)*) => {
        rewrite_rules!{
            @accum [
                $($acc)*
                rewrite!($name; $lhs => $rhs $(if $cond)*), 
                rewrite!(concat!($name, "-rev"); $rhs => $lhs $(if $cond)*),
            ]
            $($tail)*
        }
    };
    // public entry-point, set up accumulator argument
    ($($args:tt)*) => {
        rewrite_rules!{ @accum[] $($args)* }
    };
}

lazy_static! {
    /// Contains the set of rewrite rules used by the rewriter. Initialized lazily to allow global
    /// definition. 
    static ref RULES: Vec<Rewrite> = rewrite_rules![
        // commutativity
        "add-commutative": 
            "(+ ?a ?b)" => "(+ ?b ?a)",
        "mul-commutative":
            "(* ?a ?b)" => "(* ?b ?a)",

        // associativity
        "add-associative": 
            "(+ ?a (+ ?b ?c))" => "(+ (+ ?a ?b) ?c)",
        "mul-associative":
            "(* ?a (* ?b ?c))" => "(* (* ?a ?b) ?c)",
        "mul-div-associative":
            "(* ?a (/ ?b ?c))" <=> "(/ (* ?a ?b) ?c)",

        // canonicalization
        "neg-canon":
            "(- ?a)" <=> "(* -1 ?a)",
        "sub-canon":
            "(- ?a ?b)" <=> "(+ ?a (- ?b))",
        "tan-canon":
            "(tan ?a)" <=> "(/ (sin ?a) (cos ?a))",
        
        // constant evaluation: "sub0", "pow-1", "tan0" can be derived
        "add0":
            "(+ ?a 0)" => "?a",
        "mul0":
            "(* ?a 0)" => "0",
        "mul1":
            "(* ?a 1)" <=> "?a",
        "div1":
            "(/ ?a 1)" => "?a",
        "pow0":
            "(pow ?a 0)" => "1",
        "pow1-[a]":
            "(pow 1 ?a)" => "1"
            if positive("?a"),
        "pow1-[b]":
            "(pow ?a 1)" <=> "?a",
        "pow2":
            "(pow ?a 2)" <=> "(* ?a ?a)",
        "sin0":
            "(sin 0)" => "0",
        "cos0":
            "(cos 0)" => "1",
        "exp0":
            "(exp 0)" => "1",
        "log1":
            "(log 1)" => "0",

        // cancellation
        "sub-cancel":
            "(- ?a ?a)" => "0",
        "div-cancel":
            "(/ ?a ?a)" => "1"
            if non_zero("?a"),
        "log-exp-cancel":
            "(log (exp ?a))" => "?a",
        "exp-log-cancel":
            "(exp (log ?a))" => "?a",

        // distribution
        "mul-distribute":
            "(* (+ ?a ?b) ?c)" <=> "(+ (* ?c ?a) (* ?c ?b))",
        "pow-distribute":
            "(pow (* ?a ?b) ?c)" <=> "(* (pow ?a ?c) (pow ?b ?c))"
            if positive("?c"),

        // power rules
        "pow-mul":
            "(* (pow ?a ?b) (pow ?a ?c))" <=> "(pow ?a (+ ?b ?c))"
            if positive("?b")  // negative exponent
            if positive("?c"), // negative exponent
        "pow-div-[a]":
            "(/ (pow ?a ?b) (pow ?a ?c))" <=> "(pow ?a (- ?b ?c))"
            if geq("?b", "?c") // negative exponent in RHS
            if non_zero("?a"), // div by 0
        "pow-div-[b]":
            "(/ (pow ?a ?b) (pow ?c ?b))" <=> "(pow (/ ?a ?c) ?b)"
            if non_zero("?c")  // div by 0
            if positive("?b"), // negative exponent
        
        // trigonometry; "tan-neg" can be derived
        "sin-neg-angle":
            "(sin (- ?a))" <=> "(- (sin ?a))",
        "cos-neg-angle":
            "(cos (- ?a))" => "(cos ?a)",
        "sin-double-angle":
            "(sin (* 2 ?a))" <=> "(* 2 (* (sin ?a) (cos ?a)))",
        "cos-double-angle":
            "(cos (* 2 ?a))" <=> "(- 1 (* 2 (pow (sin ?a) 2)))",
    ].into();
}

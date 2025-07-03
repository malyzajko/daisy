//! TODO crate documentation; code structure

use std::{fmt, rc::Rc};
use clap::Parser;
use egg::{Extractor, Id};
use crate::{
    ast::Ast, 
    cli::{Cli, CostKind}, 
    context::Context, 
    analysis::IntervalAnalysis, 
    interval::Interval, 
};

mod analysis;
mod ast;
mod cli;
mod context;
mod cost;
mod interval;
mod rules;

pub type EGraph  = egg::EGraph<Ast, IntervalAnalysis>;
pub type RecExpr = egg::RecExpr<Ast>;
pub type Rewrite = egg::Rewrite<Ast, IntervalAnalysis>;
pub type Runner  = egg::Runner<Ast, IntervalAnalysis>;

struct Evaluation {
    expr: Option<RecExpr>, 
    interval: Interval, 
}

impl fmt::Display for Evaluation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(expr) = &self.expr {
            write!(f, "{expr} : ")?;
        }
        write!(f, "{}", self.interval)?;
        Ok(())
    }
}

fn extract(egraph: &EGraph, root: Id, cost: CostKind, ctx: Rc<Context>) -> RecExpr {
    use cost::*;

    // generic extraction over the cost function. each kind of cost function is indicated by a type tag
    // implementing CostTag (see the cost module for more info)
    fn inner<T: CostTag>(root: Id, egraph: &EGraph, ctx: Rc<Context>) -> RecExpr {
        let function = T::function(ctx);
        let extractor = Extractor::new(egraph, function);
        let (_, expr) = extractor.find_best(root);
        expr
    }
    match cost {
        CostKind::AstSize => inner::<AstSize>(root, egraph, ctx), 
        CostKind::Width => inner::<Width>(root, egraph, ctx), 
        CostKind::Magnitude => inner::<Magnitude>(root, egraph, ctx), 
        CostKind::WidthFirst => inner::<WidthFirst>(root, egraph, ctx), 
        CostKind::MagnitudeFirst => inner::<MagnitudeFirst>(root, egraph, ctx), 
    }
}

/// Performs equality saturation over an expression and computes its interval and, if a cost function is
/// given, extracts a rewriting. 
fn evaluate(expressions: Vec<RecExpr>, ctx: &Rc<Context>) -> Vec<Evaluation> {
    // construct the egraph with all expressions and run equality saturation
    let analysis = IntervalAnalysis::new(ctx);
    let runner = Runner::new(analysis)
        .with_iter_limit(ctx.settings.iter_limit);
    let runner = expressions
        .iter()
        .fold(runner, Runner::with_expr)
        .run(rules::get());
    let egraph = &runner.egraph;

    // for each expression, get its interval and extract a rewriting if a cost function is given
    let extract_root = |root| ctx.settings.cost
        .map(|cost| extract(egraph, root, cost, Rc::clone(ctx)));
    runner.roots
        .into_iter()
        .map(|root| Evaluation {
            expr: extract_root(root), 
            interval: egraph[root].data.clone(), 
        })
        .collect()
}

/// Reads input from command-line arguments, evaluates it as an expression, and prints the resulting
/// expression back to stdout. 
/// 
/// # Returns (or prints)
/// - Error message to stderr if parsing failed; exit code 1. 
/// - Evaluated expression to stdout otherwise; exit code 0. 
fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    let ctx = Context::new(args.settings, args.intervals);
    let output = evaluate(args.expressions, &ctx);

    if args.verbose {
        // todo add debug info
    }

    for eval in output {
        println!("{eval}");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use crate::{cli::{Settings, Variable}, context::Context};

    fn run_case(input: &str, expected: &str, vars: &str) {
        let settings = Settings::default();
        let intervals = vars.split_whitespace()
            .map(Variable::from_str)
            .map(Result::unwrap);
        let ctx = Context::new(settings, intervals);
        let input = input.parse().unwrap();
        let output = super::evaluate(vec![input], &ctx);
        let expr = output[0].expr
            .as_ref()
            .unwrap()
            .to_string();
        assert_eq!(expected, expr)
    }

    #[test]
    fn rewrite_rational() {
        let run = |a, b| run_case(a, b, "");
        run(
            "1",
            "1",
        );
        run(
            "649661220360188016400584487651/528383489232665513077289963261",
            "649661220360188016400584487651/528383489232665513077289963261",
        );
        run(
            "30/5",
            "6",
        );
    }

    #[test]
    fn rewrite_mul_div() {
        let run = |a, b| run_case(a, b, "x:[1,2] y:[5,10] z:[-9/10,100]");
        run(
            "(/ 30 (* 2 4))",
            "15/4",
        );
        run(
            "(/ x x)",
            "1",
        );
        run(
            "(/ (* 1 x) 1)",
            "x",
        );
        run(
            "(/ x (* y 0))",
            "(/ x 0)",
        );
        run(
            "(/ (* x y) y)",
            "x",
        );
        run(
            "(/ (* x y) x)",
            "y",
        );
        run(
            "(/ z z)",
            "(/ z z)", // because z could be 0
        );
        run(
            "(+ (+ (* x 5) (* 10 x)) x)",
            "(* x 16)",
        );
    }

    #[test]
    fn rewrite_add_sub() {
        let run = |a, b| run_case(a, b, "x:[123123123123/3,123123123123/2] y:[5,10]");
        run(
            "(- 30 (+ 2 4))",
            "24",
        );
        run(
            "(- x x)",
            "0",
        );
        run(
            "(- x (+ y 0))",
            "(- x y)",
        );
        run(
            "(+ 1 (+ x (+ 2 (+ y 3))))",
            "(+ y (+ x 6))",
        );
        run(
            "(- 0 x)",
            "(- x)",
        );
    }

    #[test]
    fn rewrite_unary() {
        let run = |a, b| run_case(a, b, "");
        run(
            "(- 3)",
            "-3",
        );
        run(
            "(cos (sin 0))",
            "1",
        );
        run(
            "(exp 0)",
            "1",
        );
    }

    // #[test]
    // fn rewrite_pow() {
    //     let data = [
    //         [
    //             "(pow 15 5)",
    //             "759375",
    //         ],
    //         [
    //             "(pow 0 -5)",
    //             "(pow 0 -5)",
    //         ],
    //         [
    //             "(pow x 0)",
    //             "1",
    //         ],
    //         [
    //             "(pow 0 0)",
    //             "1",
    //         ],
    //         [
    //             "(pow x 1)",
    //             "x",
    //         ],
    //         [
    //             "(pow 1 (cos x))",
    //             "1",
    //         ],
    //         [
    //             "(* (pow a 2) (pow a c))",
    //             "(pow a (+ 2 c))",
    //         ],
    //         [
    //             "(/ (pow a b) (pow a c))",
    //             "(pow a (- b c))",
    //         ],
    //         [
    //             "(/ (pow a b) (pow c b))",
    //             "(pow (/ a c) b)",
    //         ],
    //     ];
    //     for (id, [input, expected]) in data.iter().enumerate() {
    //         test_assert!(id; input;
    //             eval(input) => Ok(e) if e.to_string() == *expected
    //         );
    //     }
    // }
}

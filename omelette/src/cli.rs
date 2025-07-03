use std::str::FromStr;
use anyhow::anyhow;
use clap::{Args, Parser, ValueEnum};
use rug::Rational;
use crate::{ast::Identifier, RecExpr};

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum CostKind {
    /// Number of nodes
    AstSize, 
    /// Interval width; `upper - lower`
    Width, 
    /// Interval magnitude; `max(|lower|, |upper|)`
    Magnitude, 
    /// Interval width first, then AST size
    WidthFirst, 
    /// Interval magnitude first, then AST size
    MagnitudeFirst, 
}

#[derive(Clone, Debug, Args)]
pub struct Variable {
    pub identifier: Identifier, 
    pub min: Rational, 
    pub max: Rational, 
}

impl FromStr for Variable {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let error = || anyhow!("Expected syntax `variable:[min,max]`");
        let (name, interval) = s
            .split_once(':')
            .ok_or_else(error)?;
        let identifier = name.parse()?;
        let (min, max) = interval.strip_prefix("[")
            .and_then(|s| s.strip_suffix(']'))
            .and_then(|s| s.split_once(','))
            .ok_or_else(error)?;
        let min = Rational::from_str(min)?;
        let max = Rational::from_str(max)?;

        Ok(Variable{ identifier, min, max })
    }
}

#[derive(Args, Debug)]
pub struct Settings {
    /// Maximum number of iterations to run equality saturation for. 
    #[arg(long, short, default_value_t = 30)]
    pub iter_limit: usize, 

    /// Cost function used to compare two rewritings during extraction. If no value is given, extraction is
    /// disabled. 
    #[arg(long, short)]
    pub cost: Option<CostKind>, 

    /// Precision in bits of the floating-point arithmetic used for interval evaluation. 
    #[arg(long, short, default_value_t = 128)]
    pub precision: u32, 
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            iter_limit: 30, 
            cost: Some(CostKind::WidthFirst), 
            precision: 128, 
        }
    }
}

/// Term rewriting and interval evaluation engine powered by equality saturation. Rewritten expressions are
/// only extracted if a cost function is given. The output is a line for each evaluated expression;
/// `REWRITTEN_EXPR : [MIN,MAX]` if extraction is enabled; `[MIN,MAX]` otherwise. All expressions are given
/// in prefix notation. 
#[derive(Parser, Debug)]
pub struct Cli {
    /// Expression(s) to process. Given in prefix notation. Terminate list with `:`. 
    #[arg(required=true, value_terminator=":")]
    pub expressions: Vec<RecExpr>, 

    /// Intervals of all variables referenced in the expression(s). Given with the syntax `ID:[MIN,MAX]`. 
    pub intervals: Vec<Variable>, 

    #[command(flatten)]
    pub settings: Settings, 

    /// Set to write debug information to STDERR. 
    #[arg(long, short)]
    pub verbose: bool, 
}

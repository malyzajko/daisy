use std::{fmt, str::FromStr};
use anyhow::anyhow;
use egg::{define_language, Id};
use rug::{float::{OrdFloat, Round}, Float, Rational};

/// Represents a variable identifier. 
/// 
/// This needs to be declared as a new type (and not as a raw string) since illegal tokens would otherwise be
/// parsed as variable identifiers as fallback. This way, we have direct control over what kinds of strings
/// we allow (see [`Identifier::from_str`].) 
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub String);

impl FromStr for Identifier {
    type Err = anyhow::Error;

    /// Attempts to parse string as variable identifier. Parsing succeeds if and only if the string:
    /// - Contains only symbols that are alphanumeric or underscores. 
    /// - Does not begin with a numeric symbol. 
    /// 
    /// Note that unicode is supported, at least superficially. 
    fn from_str(s: &str) -> anyhow::Result<Identifier> {
        if s.contains(|c: char| !c.is_alphanumeric() && c != '_') ||
           s.starts_with(|c: char| c.is_numeric())
        {
            Err(anyhow!("Variable identifier '{s}' contains illegal symbol"))
        } else {
            Ok(Identifier(s.to_owned()))
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Rational(Rational), 
    Float(OrdFloat), 
}

impl Value {
    pub fn clone_rational(&self) -> Rational {
        match self {
            Value::Rational(r) => r.clone(), 
            Value::Float(f) => f
                .as_float()
                .to_rational()
                .expect("Bound is defined"), 
        }
    }

    pub fn clone_float(&self, precision: u32, round: Round) -> Float {
        match self {
            Value::Rational(r) => {
                let (float, _) = Float::with_val_round(precision, r, round);
                float
            }, 
            Value::Float(f) => f.as_float().clone(), 
        }
    }
}

impl FromStr for Value {
    type Err = rug::rational::ParseRationalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Rational::from_str(s).map(Value::Rational)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.clone_rational().fmt(f)
    }
}

define_language! {
    /// The AST based on the nodes used in Daisy. 
    /// 
    /// Note that [`Id`](egg::Id) contains a reference to another e-node,
    /// representing a nested expression. 
    pub enum Ast {
        // unary operators
        "-"    = Neg(Id),
        "sqrt" = Sqrt(Id),
        "sin"  = Sin(Id),
        "cos"  = Cos(Id),
        "tan"  = Tan(Id),
        "exp"  = Exp(Id),
        "log"  = Log(Id),

        // binary operators
        "+"   = Add([Id; 2]),
        "-"   = Sub([Id; 2]),
        "*"   = Mul([Id; 2]),
        "/"   = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        
        // TODO maybe add some constants like pi or e so we can evaluate certain trig function calls

        // literal values
        Literal(Value), 
        Variable(Identifier), 
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use crate::{
        ast::{Identifier, Rational},
        RecExpr,
    };

    #[test]
    fn parse_valid_variable() {
        let run = |string| Identifier::from_str(string).unwrap().0 == string;
        run("alpha");
        run("alphanumeric123");
        run("snake_case");
        run("CamelCase");
        run("_starts_with_underscore");
        run("其他语言");
        run("alpha_then_¾");
        run("");
    }
    
    #[test]
    fn parse_invalid_variable() {
        let run = |string| Identifier::from_str(string).unwrap_err();
        run("1_starts_with_number");
        run("¾_then_alpha");
        run("contains spaces");
        run("contains_symbol!");
        run("\0");
        run("😋");
    }

    #[test]
    fn parse_valid_rational() {
        let run = |string| Rational::from_str(string).unwrap().to_string() == string;
        run("0");
        run("123");
        run("3/2");
        run("10/3");
        run("18446744073709551616"); // u64::MAX + 1
        run("649661220360188016400584487651/528383489232665513077289963261"); // some primes
    }
    
    #[test]
    fn parse_invalid_rational() {
        let run = |string| Rational::from_str(string).unwrap_err();
        run("");
        run("1.5");
    }

    #[test]
    fn parse_valid() {
        let run = |string| RecExpr::from_str(string).unwrap().to_string() == string;
        run("123");
        run("3/2");
        run("variable");
        run("(+ 1 2)");
        run("(* 10/3 3/2)");
        run("(cos (/ a 15/4))");
        run("(sqrt (tan (exp (log x))))");
    }

    #[test]
    fn parser_invalid() {
        let run = |string| RecExpr::from_str(string).unwrap_err();
        run("");
        run("+ 1 2");
        run("😋");
    }
}

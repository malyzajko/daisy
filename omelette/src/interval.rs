use std::{cmp::Ordering, fmt};
use egg::Id;
use rug::{float::{Constant, Round, Special}, ops::*, Assign, Complete, Float, Integer};
use crate::{ast::Ast, context::Context};

/// A closed interval. May be computed from an [`Ast`] expression with [`Interval::of`]. 
#[derive(Clone, Debug, PartialEq)]
pub struct Interval(pub Float, pub Float);

impl Interval {
    pub fn of(expr: &Ast, mut child: impl FnMut(Id) -> Interval, ctx: &Context) -> Interval {
        let precision = ctx.settings.precision;
        let mut child = |&id: &Id| child(id);
        let interval = match expr {
            Ast::Neg(x) => neg(child(x)), 
            Ast::Sqrt(x) => sqrt(child(x)), 
            Ast::Sin(x) => sin(child(x)), 
            Ast::Cos(x) => cos(child(x)), 
            Ast::Tan(x) => tan(child(x)), 
            Ast::Exp(x) => exp(child(x)), 
            Ast::Log(x) => log(child(x)), 
            Ast::Add([x, y]) => add(child(x), child(y)), 
            Ast::Sub([x, y]) => sub(child(x), child(y)), 
            Ast::Mul([x, y]) => mul(child(x), child(y)), 
            Ast::Div([x, y]) => div(child(x), child(y)), 
            Ast::Pow([x, y]) => pow(child(x), child(y)), 
            Ast::Literal(x) => Interval(
                x.clone_float(precision, Round::Down), 
                x.clone_float(precision, Round::Up), 
            ), 
            Ast::Variable(id) => ctx.get_interval(id)
                .cloned()
                .unwrap_or_else(|| panic!("Variable '{id}' not defined in LUT")), 
        };
        let interval = interval.normalise_undefined();
        
        // sanity checks
        assert!(interval.0.prec() == precision);
        assert!(interval.1.prec() == precision);
        assert!(interval.0 <= interval.1);

        interval
    }

    /// Normalises undefined bounds such that they are set to -infinity and +infinity, respectively. 
    /// 
    /// Without this, bounds may be NaN or set to the same infinity due to implementation quirks. 
    fn normalise_undefined(mut self) -> Interval {
        if !self.0.is_finite() {
            self.0.assign(Special::NegInfinity);
        }
        if !self.1.is_finite() {
            self.1.assign(Special::Infinity);
        }
        self
    }

    /// Classifies the interval's relationship to zero. 
    pub fn class(&self) -> Class {
        enum Sign {
            Negative, 
            Zero, 
            Positive, 
        }
        let sign_of = |f: &Float| match (f.is_zero(), f.is_sign_positive()) {
            (true, _)  => Sign::Zero, 
            (_, false) => Sign::Negative, 
            (_, true)  => Sign::Positive, 
        };

        match (sign_of(&self.0), sign_of(&self.1)) {
            (Sign::Zero,     Sign::Zero)     => Class::Zero, 
            (Sign::Negative, Sign::Positive) => Class::Mixed, 
            (Sign::Negative, Sign::Negative) => Class::Negative(true),
            (Sign::Negative, Sign::Zero)     => Class::Negative(false),  
            (Sign::Positive, Sign::Positive) => Class::Positive(true),
            (Sign::Zero,     Sign::Positive) => Class::Positive(false),  
            _ => unreachable!("x.min <= x.max"), 
        }
    }
    
    pub fn width(&self) -> Float {
        (&self.1 - &self.0).complete(self.precision())
    }

    pub fn magnitude(&self) -> Float {
        self.1
            .clone()
            .abs()
            .max(&self.0)
    }

    pub fn contains_zero(&self) -> bool {
        match self.class() {
            Class::Mixed => true, 
            Class::Negative(nonzero) => !nonzero, 
            Class::Positive(nonzero) => !nonzero, 
            Class::Zero => true, 
        }
    }

    pub fn is_point(&self) -> bool {
        self.0 == self.1
    }

    /// Computes the precision of the interval as the highest precision of its bounds. 
    fn precision(&self) -> u32 {
        u32::max(self.0.prec(), self.1.prec())
    }

    /// Short-hand used to inverse monotonicity when used with [`unary`] and [`binary`]. 
    /// 
    /// The returned interval is invalid and should only be used as an intermediary. 
    fn rev(self) -> Interval {
        Interval(self.1, self.0)
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bound = |bound: &Float, inf: &str| bound
            .to_rational()
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| inf.into());
        write!(f, "[{},{}]", bound(&self.0, "-INF"), bound(&self.1, "INF"))
    }
}

/// Classifies an interval's relationship to zero. 
#[derive(Clone, Copy, PartialOrd, PartialEq)]
pub enum Class {
    /// The interval crosses zero. 
    Mixed, 
    /// The interval is all negative. The bool indicates whether the upper bound is non-zero. 
    Negative(bool), 
    /// The interval is all positive. The bool indicates whether the lower bound is non-zero. 
    Positive(bool), 
    /// The interval contains only zero. 
    Zero, 
}

/// Applies a monotonically rising function with correct rounding to an interval. 
/// 
/// Monotonically falling functions may be emulated through [`Interval::rev`]. 
fn unary(op: impl Fn(&mut Float, Round) -> Ordering, mut x: Interval) -> Interval {
    op(&mut x.0, Round::Down);
    op(&mut x.1, Round::Up);
    x
}

/// Applies a function that monotonically rises with both arguments with correct rounding to an interval. 
/// 
/// Monotonically falling functions (for either argument) may be emulated through [`Interval::rev`]. 
fn binary(op: impl Fn(&mut Float, Float, Round) -> Ordering, mut x: Interval, y: Interval) -> Interval {
    op(&mut x.0, y.0, Round::Down);
    op(&mut x.1, y.1, Round::Up);
    x
}

/// Computes the phase of each bound in a sine curve. 
/// 
/// Defined as integer multiples of π/2, rounded toward negative infinity. TODO this does a decent amount of
/// allocations. 
fn sin_phase(x: &Interval) -> [Integer; 2] {
    // compute x/π (halves of the sine curve)
    let pi = {
        let prec = x.precision();
        let (lo, _) = Float::with_val_round(prec, Constant::Pi, Round::Down);
        let (hi, _) = Float::with_val_round(prec, Constant::Pi, Round::Up);
        Interval(lo, hi)
    };
    let Interval(h1, h2) = div(x.clone(), pi);

    // multiply by 2 to compute 2x/π (quarters of the sine curve), and round downward
    let quarters_of = |mut half: Float, round| {
        half.mul_assign_round(2, round);
        let (quarters, _) = half
            .to_integer_round(Round::Down)
            .expect("Bound is defined");
        quarters
    };
    [quarters_of(h1, Round::Down), quarters_of(h2, Round::Up)]
}

/// Utility to set a bound to NaN. 
/// 
/// Note that this should get normalised to +/- infinity by [`Interval::normalise_undefined`] before being
/// used in further calculations to avoid unexpected behaviour. 
fn undefined(mut x: Float) -> Float {
    x.assign(Special::Nan);
    x
}

fn zero(mut x: Float) -> Float {
    x.assign(Special::Zero);
    x
}

/// Computes arithmetic negation. 
fn neg(x: Interval) -> Interval {
    Interval(-x.1, -x.0)
}

/// Computes square root. 
fn sqrt(x: Interval) -> Interval {
    unary(Float::sqrt_round, x)
}

/// Backend for computing sine and cosine. 
/// 
/// `phase_offset` is the number of quarters to add to the [`sin_phase`] of the bounds; 0 for sine and 1 for
/// cosine. `op` is the floating-point function being applied; [`Float::sin_round`] or [`Float::cos_round`]. 
fn sin_cos(x: Interval, phase_offset: u32, op: impl Fn(&mut Float, Round) -> Ordering + Copy) -> Interval {
    // used to assign an outer bounds; [-1, 1]
    let outer1 = |mut x: Float| {
        x.assign(-1);
        x
    };
    let outer2 = |mut x: Float| {
        x.assign(1);
        x
    };

    // compute the quarters of the bounds in a sine curve (non-repeating after a cycle)
    let [abs_q1, abs_q2] = sin_phase(&x);

    // if the bounds are a whole cycle or more apart, the outer bounds are the tightest
    if (&abs_q1 - &abs_q2).complete().abs() >= 4 {
        return Interval(outer1(x.0), outer2(x.1))
    }

    // add the phase offset and get the quarters relative to a cycle by taking mod 4
    let quarters = [abs_q1, abs_q2]
        .map(|q| q + phase_offset)
        .map(|q| q.rem_euc(4) as Integer)
        .map(|q| q.to_u8())
        .map(|q| q.expect("We just took mod 4"));

    // used to apply sin/cos to a bound rounding up or down
    let op1 = |mut f: Float| {
        op(&mut f, Round::Down);
        f
    };
    let op2 = |mut f: Float| {
        op(&mut f, Round::Up);
        f
    };
    
    // compute the output bounds based on the quarters of each input bound
    // |  _|_  |   |   |
    // | / | \ |   |   |
    // |/  |  \|   |   |
    // |   |   |   |   |
    // |   |   |\  |  /|
    // |   |   | \_|_/ |
    // |   |   |   |   |
    //   0   1   2   3
    match quarters {
        // both -1 and +1 contained
        [0, 3] | [2, 1] => Interval(
            outer1(x.0), 
            outer2(x.1), 
        ), 
        // +1 contained, compute lower bound with sin/cos
        [0, 1] | [0, 2] | [3, 1] | [3, 2] => Interval(
            Float::min(op1(x.0), &op1(x.1.clone())), 
            outer2(x.1), 
        ), 
        // -1 contained, compute upper bound with sin/cos
        [1, 0] | [1, 3] | [2, 0] | [2, 3] => Interval(
            outer1(x.0.clone()), 
            Float::max(op2(x.0), &op2(x.1)), 
        ), 
        // sin/cos is monotonically rising in interval
        [0, 0] | [3, 3] | [3, 0] => Interval(
            op1(x.0), 
            op2(x.1), 
        ), 
        // sin/cos is monotonically falling in interval
        [1, 1] | [2, 2] | [1, 2] => Interval(
            op1(x.1), 
            op2(x.0), 
        ), 
        _ => unreachable!("Quarters are mod 4")
    }
}

/// Computes cosine. 
fn cos(x: Interval) -> Interval {
    sin_cos(x, 1, Float::cos_round)
}

/// Computes sine. 
fn sin(x: Interval) -> Interval {
    sin_cos(x, 0, Float::sin_round)
}

/// Computes tangent. 
fn tan(x: Interval) -> Interval {
    // tan is defined iff the bounds are in the same phase, or in consecutive phases with an odd lower phase
    let [p1, p2] = sin_phase(&x);
    let defined = p1 == p2 || (p1.is_odd() && p2 == p1 + 1);

    match defined {
        true => unary(Float::tan_round, x), 
        false => Interval(undefined(x.0), undefined(x.1)), 
    }
}

/// Computes natural exponentiation with floating-point approximation. 
fn exp(x: Interval) -> Interval {
    unary(Float::exp_round, x)
}

/// Computes natural logarithm with floating-point approximation. 
fn log(x: Interval) -> Interval {
    unary(Float::ln_round, x)
}

/// Computes addition. 
fn add(x: Interval, y: Interval) -> Interval {
    binary(Float::add_assign_round, x, y)
}

/// Computes subtraction. 
fn sub(x: Interval, y: Interval) -> Interval {
    binary(Float::sub_assign_round, x, y.rev())
}

/// Computes multiplication. 
fn mul(x: Interval, y: Interval) -> Interval {
    // sort x and y by class to reduce the number of cases considered. the order of Class is carefully chosen
    // to remove cases where unnecessary cloning is required
    let (x, y, class_x, class_y) = {
        let class_x = x.class();
        let class_y = y.class();
        match class_x <= class_y {
            true => (x, y, class_x, class_y), 
            false => (y, x, class_y, class_x), 
        }
    };
    let Interval(mut a, mut b) = x;
    let Interval(c, d) = &y;

    fn multiply(mut lo1: Float, lo2: &Float, mut hi1: Float, hi2: &Float) -> Interval {
        lo1.mul_assign_round(lo2, Round::Down);
        hi1.mul_assign_round(hi2, Round::Up);
        Interval(lo1, hi1)
    }
    match (class_x, class_y) {
        (Class::Mixed, Class::Mixed) => {
            let z = multiply(a.clone(), d, b.clone(), d);
            let w = multiply(b, c, a, c);
            Interval(
                Float::min(z.0, &w.0), 
                Float::max(z.1, &w.1), 
            )
        }
        (Class::Mixed,       Class::Negative(_)) => multiply(b, c, a, c), 
        (Class::Mixed,       Class::Positive(_)) => multiply(a, d, b, d), 
        (Class::Negative(_), Class::Negative(_)) => multiply(b, d, a, c), 
        (Class::Negative(_), Class::Positive(_)) => multiply(a, d, b, c), 
        (Class::Positive(_), Class::Positive(_)) => multiply(a, c, b, d), 
        (Class::Zero, _) |
        (_, Class::Zero) => {
            a.assign(Special::Zero);
            b.assign(Special::Zero);
            Interval(a, b)
        }
        _ => unreachable!("class_x <= class_y")
    }
}

/// Computes division. 
fn div(x: Interval, y: Interval) -> Interval {
    // used to compute the reciprocal of a bound
    let recip = |mut f: Float, round| {
        f.recip_round(round);
        f
    };

    // compute 1/y
    let y_inv = match y.class() {
        Class::Mixed | Class::Zero => return Interval(undefined(y.0), undefined(y.1)), 
        Class::Negative(true) |
        Class::Positive(true) => Interval(
            recip(y.1, Round::Down), 
            recip(y.0, Round::Up), 
        ), 
        Class::Negative(false) => Interval(
            undefined(y.1), 
            recip(y.0, Round::Up), 
        ), 
        Class::Positive(false) => Interval(
            recip(y.1, Round::Down), 
            undefined(y.0),  
        ), 
    };
    // compute x * 1/y
    mul(x, y_inv.normalise_undefined())
}

/// Computes exponentiation. 
fn pow(x: Interval, y: Interval) -> Interval {
    let round = |yi: Float, round| yi
        .to_i32_saturating_round(round)
        .expect("Bound is defined");
    let y1 = round(y.0, Round::Down);
    let y2 = round(y.1, Round::Up);

    // we can be smarter about these cases, but they rarely show up in real-world usage so for now we leave
    // them undefined
    if y1 != y2 || y1.is_negative() {
        return Interval(undefined(x.0), undefined(x.1))
    }
    
    let e = y1;
    let pow1 = |mut b: Float| {
        b.pow_assign_round(e, Round::Down);
        b
    };
    let pow2 = |mut b: Float| {
        b.pow_assign_round(e, Round::Up);
        b
    };
    
    match (e % 2 == 0, x.class()) {
        (true, Class::Mixed) => {
            let high = pow2(Float::max(x.0.abs(), &x.1.as_abs()));
            Interval(zero(x.1), high)
        }
        (true, Class::Negative(_)) => Interval(
            pow1(x.1), 
            pow2(x.0), 
        ), 
        _ => Interval(
            pow1(x.0), 
            pow2(x.1), 
        ), 
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use rug::{float::Round, ops::CompleteRound, Float};
    use crate::{cli::{Settings, Variable}, context::Context};

    use super::Interval;

    const PRECISION: u32 = 128;
    const TOLERANCE: f64 = 0.000001;

    fn to_interval(floats: [f64; 2], precision: u32) -> Interval {
        let (f1, _) = Float::with_val_round(precision, floats[0], Round::Down);
        let (f2, _) = Float::with_val_round(precision, floats[1], Round::Up);
        Interval(f1, f2)
    }

    fn test_op(expected: [f64; 2], op: impl Fn(u32) -> Interval) {
        let lq = op(PRECISION).normalise_undefined();
        let hq = op(PRECISION * 2).normalise_undefined();
        let expected = to_interval(expected, 64);

        assert!(!lq.0.is_nan(), "Lower bound is defined");
        assert!(!lq.1.is_nan(), "Upper bound is defined");
        assert!(lq.0 <= lq.1, "Interval is ordered correctly");
        assert!(lq.0 <= hq.0, "Lower bound is rounded downward");
        assert!(lq.1 >= hq.1, "Upper bound is rounded upward");

        let within_tolerance = |actual: &Float, expected: &Float| {
            if actual.is_infinite() {
                actual == expected
            } else {
                (actual - expected).complete(PRECISION).abs() < TOLERANCE
            }
        };
        assert!(
            within_tolerance(&lq.0, &expected.0), 
            "Lower bound is close to correct (got {} vs expected {})", 
            lq.0, expected.0
        );
        assert!(
            within_tolerance(&lq.1, &expected.1), 
            "Upper bound is close to correct (got {} vs expected {})", 
            lq.1, expected.1
        );
    }

    fn test_binary<F>(x: [f64; 2], y: [f64; 2], expected: [f64; 2], op: F)
    where
        F: Fn(Interval, Interval) -> Interval
    {
        test_op(expected, |p| {
            let x = to_interval(x, p);
            let y = to_interval(y, p);
            op(x, y)
        })
    }

    fn test_unary<F>(x: [f64; 2], expected: [f64; 2], op: F)
    where
        F: Fn(Interval) -> Interval
    {
        test_op(expected, |p| {
            let x = to_interval(x, p);
            op(x)
        })
    }

    #[test]
    fn neg() {
        let test = |x, expected| test_unary(x, expected, super::neg);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [1.0, 2.0],
            [-2.0, -1.0],
        );
        test(
            [0.0, 5034.2],
            [-5034.2, 0.0],
        );
    }

    #[test]
    fn sqrt() {
        let test = |x, expected| test_unary(x, expected, super::sqrt);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [25.0, 25.0],
            [5.0, 5.0],
        );
        test(
            [0.0, 25.0],
            [0.0, 5.0],
        );
        test(
            [-1.0, 1.0],
            [f64::NEG_INFINITY, 1.0],
        );
        test(
            [-1.0, -1.0],
            [f64::NEG_INFINITY, f64::INFINITY],
        );
        test(
            [2970.25, 5640.01],
            [54.5, 75.1],
        );
    }

    #[test]
    fn sin() {
        let test = |x, expected| test_unary(x, expected, super::sin);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [0.5, 1.0],
            [0.4794255386, 0.8414709848],
        );
        test(
            [-1.0, -0.5],
            [-0.8414709848, -0.4794255386],
        );
        test(
            [1.2, 2.0],
            [0.9092974268, 1.0],
        );
        test(
            [-3.5, -2.8],
            [-0.33498815015, 0.35078322769],
        );
        test(
            [-4.0, 4.0],
            [-1.0, 1.0],
        );
        test(
            [-0.5, 0.5],
            [-0.4794255386, 0.4794255386]
        );
        test(
            [0.999, 1.001],
            [0.84093026185, 0.84201086628],
        );
    }

    #[test]
    fn cos() {
        let test = |x, expected| test_unary(x, expected, super::cos);
        test(
            [0.0, 0.0],
            [1.0, 1.0],
        );
        test(
            [0.5, 1.0],
            [0.54030230586, 0.87758256189],
        );
        test(
            [-1.0, -0.5],
            [0.54030230586, 0.87758256189],
        );
        test(
            [1.2, 2.0],
            [-0.41614683654, 0.36235775447],
        );
        test(
            [-3.5, -2.8],
            [-1.0, -0.93645668729],
        );
        test(
            [-4.0, 4.0],
            [-1.0, 1.0],
        );
        test(
            [-0.5, 0.5],
            [0.87758256189, 1.0],
        );
        test(
            [0.999, 1.001],
            [0.53946056487, 0.54114350656],
        );
    }

    #[test]
    fn tan() {
        let test = |x, expected| test_unary(x, expected, super::tan);
        test(
            [-1.0, 1.0], 
            [-1.55740772465, 1.55740772465],   
        );
        test(
            [34.0, 35.0], 
            [-0.623498962716, 0.473814720414], 
        );
        test(
            [-4.5, -3.5], 
            [-4.63733205455, -0.374585640159], 
        );
        test(
            [1.0, 2.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [2.0, 5.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
    }
    
    #[test]
    fn exp() {
        let test = |x, expected| test_unary(x, expected, super::exp);
        test(
            [0.0, 0.0],
            [1.0, 1.0],
        );
        test(
            [1.0, 1.0],
            [2.71828182846, 2.71828182846],
        );
        test(
            [0.5, 2.0],
            [1.6487212707, 7.38905609893],
        );
        test(
            [-1.0, 2.0],
            [0.367879441171, 7.38905609893],
        );
    }

    #[test]
    fn log() {
        let test = |x, expected| test_unary(x, expected, super::log);
        test(
            [1.0, 1.0],
            [0.0, 0.0],
        );
        test(
            [2.0, 2.0],
            [0.69314718056, 0.69314718056],
        );
        test(
            [3.0, 4.0],
            [1.09861228867, 1.38629436112],
        );
        test(
            [0.0, 0.0],
            [f64::NEG_INFINITY, f64::INFINITY],
        );
        test(
            [0.0, 1.0],
            [f64::NEG_INFINITY, 0.0],
        );
        test(
            [-1.0, 1.0],
            [f64::NEG_INFINITY, 0.0],
        );
        test(
            [-2.0, 2.0], 
            [f64::NEG_INFINITY, 0.69314718056], 
        );
        test(
            [-2.0, -1.0],
            [f64::NEG_INFINITY, f64::INFINITY],
        );
    }

    #[test]
    fn add() {
        let test = |x, y, expected| test_binary(x, y, expected, super::add);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [-1.0, 2.0], 
            [3.0, 4.0], 
            [2.0, 6.0], 
        );
        test(
            [6.5, 12.2], 
            [-33.1, 64.6], 
            [-26.6, 76.8], 
        );
        test(
            [0.0, 0.0], 
            [1.0, 2.0], 
            [1.0, 2.0], 
        );
        test(
            [1.0, 2.0], 
            [-2.0, -1.0], 
            [-1.0, 1.0], 
        );
        test(
            [2.0, 3.0], 
            [1.0, 2.0], 
            [3.0, 5.0], 
        );
    }

    #[test]
    fn sub() {
        let test = |x, y, expected| test_binary(x, y, expected, super::sub);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [-1.0, 2.0],
            [3.0, 4.0],
            [-5.0, -1.0],
        );
        test(
            [6.5, 12.2],
            [-33.1, 64.6],
            [-58.1, 45.3],
        );
        test(
            [0.0, 0.0],
            [1.0, 2.0],
            [-2.0, -1.0],
        );
        test(
            [1.0, 2.0],
            [-2.0, -1.0],
            [2.0, 4.0],
        );
        test(
            [2.0, 3.0],
            [1.0, 2.0],
            [0.0, 2.0],
        );
    }

    #[test]
    fn mul() {
        let test = |x, y, expected| test_binary(x, y, expected, super::mul);
        test(
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
        );
        test(
            [-2.0, 3.0],
            [2.0, 4.0],
            [-8.0, 12.0],
        );
        test(
            [-3.5, 5.5],
            [-2.5, 4.5],
            [-15.75, 24.75],
        );
        test(
            [-2.0, -1.0],
            [1.0, 2.0],
            [-4.0, -1.0],
        );
        test(
            [-1.5, 1.5],
            [-2.5, 2.5],
            [-3.75, 3.75],
        );
        test(
            [-2.0, 3.5],
            [-2.5, 2.5],
            [-8.75, 8.75],
        );
    }

    #[test]
    fn div() {
        let test = |x, y, expected| test_binary(x, y, expected, super::div);
        test(
            [1.0, 2.0], 
            [2.0, 3.0], 
            [1.0 / 3.0, 1.0], 
        );
        test(
            [-2.0, 2.0], 
            [1.0, 1.0], 
            [-2.0, 2.0], 
        );
        test(
            [0.5, 2.0], 
            [0.25, 1.0], 
            [0.5, 8.0], 
        );
        test(
            [-6.0, 3.0], 
            [2.0, 8.0], 
            [-3.0, 1.5] 
        );
        test(
            [-2.0, 2.0], 
            [-1.0, -1.0], 
            [-2.0, 2.0], 
        );
        test(
            [0.0, 0.0], 
            [0.0, 0.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [1.0, 1.0], 
            [0.0, 0.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [2.0, 3.0], 
            [0.0, 5.0], 
            [0.4, f64::INFINITY], 
        );
        test(
            [2.0, 3.0], 
            [-5.0, 0.0], 
            [f64::NEG_INFINITY, -0.4], 
        );
    }

    #[test]
    fn pow() {
        let test = |x, y, expected| test_binary(x, y, expected, super::pow);
        test(
            [0.0, 0.0], 
            [1.0, 1.0], 
            [0.0, 0.0], 
        );
        test(
            [1.0, 1.0], 
            [0.0, 0.0], 
            [1.0, 1.0], 
        );
        test(
            [5.0, 5.0], 
            [2.0, 2.0], 
            [25.0, 25.0], 
        );
        test(
            [-5.0, 5.0], 
            [2.0, 2.0], 
            [0.0, 25.0], 
        );
        test(
            [-5.0, -4.0], 
            [4.0, 4.0], 
            [256.0, 625.0], 
        );
        test(
            [1.0, 1.0], 
            [-1.0, -1.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [2.0, 3.0], 
            [0.1, 0.2], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [2.0, 2.0], 
            [-0.9, 0.8], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
        test(
            [-4.0, -3.0], 
            [-2.0, -2.0], 
            [f64::NEG_INFINITY, f64::INFINITY], 
        );
    }

    #[test]
    fn expr() {
        let test = |input: &str, vars: &str, expected| test_op(expected, |precision| {
            let settings = Settings {
                precision, 
                ..Default::default()
            };
            let intervals = vars.split_whitespace()
                .map(Variable::from_str)
                .map(Result::unwrap);
            let ctx = Context::new(settings, intervals);
            let input = input.parse().unwrap();
            let output = crate::evaluate(vec![input], &ctx);
            output[0].interval.clone()
        });
        test(
            "5", "",
            [5.0, 5.0],
        );
        test(
            "(+ 1 (* 2 3))", "",
            [7.0, 7.0],
        );
        test(
            "(/ (* (- 4 2) (+ 3 2)) 2)", "",
            [5.0, 5.0],
        );
        test(
            "u", "u:[0,1]",
            [0.0, 1.0],
        );
        test(
            "(+ (* x y) z)", "x:[1,2] y:[2,3] z:[-2,-1]",
            [0.0, 5.0],
        );
        test(
            "(/ (sqrt -1) 0)", "",
            [f64::NEG_INFINITY, f64::INFINITY],
        );
        test(
            "(/ (sqrt -1) 1)", "",
            [f64::NEG_INFINITY, f64::INFINITY],
        );
        test(
            "(pow (sin x) 2)", "x:[1/10,3]",
            [0.00996671107937, 1.0],
        );
        test(
            "(pow (cos x) 2)", "x:[1/10,3]",
            [0.0, 0.990033288921],
        );
    }
}

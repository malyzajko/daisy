use std::{
    cmp::Ordering, 
    fmt::Debug, 
    marker::PhantomData, 
    rc::Rc, 
};
use egg::{CostFunction as _, Id};
use rug::Float;
use crate::{
    ast::Ast, 
    context::Context, 
    interval::Interval, 
};

/// Cost function used by Egg during extraction. 
/// 
/// The type parameter is a tag implementing [`CostTag`] that defines which cost function (between the
/// several defined by Omelette) to use. The cost function is encoded on the type-level since different
/// cost functions require different types of values to be computed from the AST. 
pub struct CostFunction<T: ?Sized> {
    ctx: Rc<Context>, 
    _phantom: PhantomData<T>, 
}

impl<T: CostTag> egg::CostFunction<Ast> for CostFunction<T>
where
    Cost<T::Cost, T::Data>: Clone + Debug
{
    type Cost = Cost<T::Cost, T::Data>;

    fn cost<C: FnMut(Id) -> Self::Cost>(&mut self, enode: &Ast, mut costs: C) -> Self::Cost {
        T::cost(enode, |id| costs(id).data, &self.ctx)
    }
}

/// The information stored by Egg for each node when calculating the cost. 
#[derive(Clone, Debug)]
pub struct Cost<T, U> {
    /// The actual cost being compared; e.g., the interval width. 
    cost: T, 
    /// Data required to compute the parent cost; e.g., the interval. 
    data: U, 
}

impl<T: PartialEq, U> PartialEq for Cost<T, U> {
    fn eq(&self, other: &Self) -> bool {
        self.cost.eq(&other.cost)
    }
}

impl<T: PartialOrd, U> PartialOrd for Cost<T, U> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cost.partial_cmp(&other.cost)
    }
}

/// Types implementing this trait are tags that indicate what cost function to use. 
/// 
/// A tag may then be used to instantiate the actual [`CostFunction`] used by Egg. This allows us to reuse
/// code that's common between all cost functions. 
pub trait CostTag {
    /// The actual costs being compared; e.g., the interval width. 
    type Cost: Clone + Debug + PartialOrd;
    /// Data required to compute the parent cost; e.g., the interval. 
    type Data: Clone + Debug;

    /// Computes the [`Cost`] for an enode. 
    fn cost<D>(enode: &Ast, data: D, ctx: &Context) -> Cost<Self::Cost, Self::Data>
    where
        D: FnMut(Id) -> Self::Data;

    /// Instantiates the actual [`CostFunction`] used by Egg. 
    fn function(ctx: Rc<Context>) -> CostFunction<Self> {
        CostFunction {
            ctx,  
            _phantom: PhantomData, 
        }
    }
}

/// Tag for the AST size cost function. 
#[derive(Clone, Debug)]
pub struct AstSize;

impl CostTag for AstSize {
    type Cost = usize;
    type Data = usize;

    fn cost<D>(enode: &Ast, data: D, _: &Context) -> Cost<usize, usize>
    where
        D: FnMut(Id) -> Self::Data
    {
        let cost = egg::AstSize.cost(enode, data);

        Cost {
            cost, 
            data: cost, 
        }
    }
}

/// Tag for the interval width cost function. 
#[derive(Clone, Debug)]
pub struct Width;

impl CostTag for Width {
    type Cost = Float;
    type Data = Interval;

    fn cost<D>(enode: &Ast, data: D, ctx: &Context) -> Cost<Float, Interval>
    where
        D: FnMut(Id) -> Self::Data
    {
        let interval = Interval::of(enode, data, ctx);
        let width = interval.width();

        Cost {
            cost: width, 
            data: interval, 
        }
    }
}

/// Tag for the interval magnitude cost function. 
#[derive(Clone, Debug)]
pub struct Magnitude;

impl CostTag for Magnitude {
    type Cost = Float;
    type Data = Interval;

    fn cost<D>(enode: &Ast, data: D, ctx: &Context) -> Cost<Float, Interval>
    where
        D: FnMut(Id) -> Self::Data
    {
        let interval = Interval::of(enode, data, ctx);
        let width = interval.magnitude();

        Cost {
            cost: width, 
            data: interval, 
        }
    }
}

/// Utility for combining two other cost functions and comparing their results lexicographically. 
#[derive(Clone, Debug)]
pub struct Chain<T, U> {
    _phantom0: PhantomData<T>, 
    _phantom1: PhantomData<U>, 
}

impl<T: CostTag, U: CostTag> CostTag for Chain<T, U> {
    type Cost = (T::Cost, U::Cost);
    type Data = (T::Data, U::Data);

    fn cost<D>(enode: &Ast, mut data: D, ctx: &Context) -> Cost<Self::Cost, Self::Data>
    where
        D: FnMut(Id) -> Self::Data
    {
        let cost0 = T::cost(enode, |id| data(id).0, ctx);
        let cost1 = U::cost(enode, |id| data(id).1, ctx);

        Cost {
            cost: (cost0.cost, cost1.cost), 
            data: (cost0.data, cost1.data), 
        }
    }
}

/// Tag for the interval width first cost function. 
pub type WidthFirst = Chain<Width, AstSize>;

/// Tag for the interval magnitude first cost function. 
pub type MagnitudeFirst = Chain<Magnitude, AstSize>;

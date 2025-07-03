use std::rc::Rc;
use egg::*;
use crate::{
    ast::{Ast, Value}, 
    context::Context, 
    interval::*, 
    EGraph, 
};

/// E-class analysis using interval evaluation of the e-nodes. 
pub struct IntervalAnalysis {
    ctx: Rc<Context>, 
}

impl IntervalAnalysis {
    pub fn new(ctx: &Rc<Context>) -> IntervalAnalysis {
        IntervalAnalysis{ ctx: Rc::clone(ctx) }
    }
}

impl Analysis<Ast> for IntervalAnalysis {
    type Data = Interval;

    /// Computes the interval evaluation for the e-node. 
    fn make(egraph: &EGraph, enode: &Ast) -> Interval {
        let ctx = &egraph.analysis.ctx;
        Interval::of(enode, |x| egraph[x].data.clone(), ctx)
    }

    /// Merges e-class analyses by taking the intersection between the intervals. 
    fn merge(&mut self, a: &mut Interval, b: Interval) -> DidMerge {
        let change_min = b.0 > a.0;
        let change_max = b.1 < a.1;

        if change_min {
            a.0 = b.0;
        }
        if change_max {
            a.1 = b.1;
        }
        assert!(
            a.1 >= a.0,
            "Merged non-intersecting intervals; result = [{},{}]",
            a.0, a.1
        );
        DidMerge(change_min || change_max, !(change_min && change_max))
    }

    /// If an e-class interval evaluates to a single point, add the rational inside the interval to the
    /// e-class as a new e-node, and since the e-class can be represented as a single rational, no further
    /// rewrites can improve upon it; all non-leaf e-nodes are removed to save on processing time. 
    fn modify(egraph: &mut EGraph, id: Id) {
        let interval = &egraph[id].data;

        if interval.is_point() {
            // add point to e-class
            let point = interval.0
                .clone()
                .into();
            let added = egraph.add(Ast::Literal(Value::Float(point)));
            egraph.union(id, added);

            // prune e-class
            egraph[id].nodes.retain(Ast::is_leaf);
        }
    }
}

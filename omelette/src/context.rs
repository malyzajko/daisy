use std::{collections::HashMap, rc::Rc};
use rug::{float::Round, Float};
use crate::{
    ast::Identifier, 
    cli::{Settings, Variable}, 
    interval::Interval, 
};

/// Storage for global data. 
/// 
/// A reference is given to all modules. 
pub struct Context {
    /// Various settings used during the evaluation. 
    pub settings: Settings, 
    /// A LUT of the intervals of all variables. Defined as private to allow alternate representations. 
    intervals: HashMap<Identifier, Interval>, 
}

impl Context {
    pub fn new(settings: Settings, intervals: impl IntoIterator<Item = Variable>) -> Rc<Self> {
        let intervals = intervals
            .into_iter()
            .map(|var| {
                let (min, _) = Float::with_val_round(settings.precision, var.min, Round::Down);
                let (max, _) = Float::with_val_round(settings.precision, var.max, Round::Up);
                let interval = Interval(min, max);
                (var.identifier, interval)
            })
            .collect();
        Rc::new(Context{ settings, intervals })
    }
    
    pub fn get_interval(&self, id: &Identifier) -> Option<&Interval> {
        self.intervals.get(id)
    }
}

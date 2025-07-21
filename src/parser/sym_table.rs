use std::collections::HashMap;

use super::Expr;

#[derive(Debug)]
pub struct SymTable {
    syms: HashMap<String, Expr>,
}

impl SymTable {
    pub fn new() -> Self {
        Self { syms: HashMap::new() }
    }
    pub fn put(&mut self, id: &String, val: Expr) -> Option<Expr> {
        self.syms.insert(String::from(id), val)
    }
    pub fn get(&self, id: &String) -> Option<&Expr> {
        self.syms.get(id)
    }
    pub fn get_mut(&mut self, id: &String) -> Option<&mut Expr> {
        self.syms.get_mut(id)
    }
}

use std::collections::HashMap;

use crate::parser::Symbol;

#[derive(Debug, Clone)]
pub struct SymTable {
	syms: HashMap<String, Symbol>,
}

impl SymTable {
	pub fn new() -> Self {
		Self { syms: HashMap::new() }
	}
	pub fn put(&mut self, id: &String, val: Symbol) -> Option<Symbol> {
		self.syms.insert(String::from(id), val)
	}
	pub fn set(&mut self, id: &String, val: Symbol) -> Result<(), String> {
		if self.syms.contains_key(id) {
			self.syms.insert(id.clone(), val);
			Ok(())
		} else {
			Err(format!("Змінна '{}' не задекларована", id))
		}
	}
	pub fn get(&self, id: &String) -> Option<&Symbol> {
		self.syms.get(id)
	}
	pub fn get_mut(&mut self, id: &String) -> Option<&mut Symbol> {
		self.syms.get_mut(id)
	}
	pub fn is_empty(&self) -> bool {
		self.syms.is_empty()
	}
	pub fn clear(&mut self) {
		self.syms.clear();
	}
	pub fn contains(&self, id: &str) -> bool {
		self.syms.contains_key(id.into())
	}
	pub fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
		self.syms.iter()
	}
	pub fn iter_key(&self) -> impl Iterator<Item = &String> {
		self.syms.keys().into_iter()
	}
	pub fn iter_val(&self) -> impl Iterator<Item = &Symbol> {
		self.syms.values().into_iter()
	}
}

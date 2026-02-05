use std::{cell::RefCell, rc::Rc};

use crate::{parser::Statements, sym_table::SymTable};

// Define BinOp enum to match the new token structure
#[derive(Debug, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	EqEq,
	NotEq,
}

#[derive(Debug, Clone)]
pub enum Symbol {
	Variable(Expr),
	Function(Vec<Statements>, Rc<RefCell<SymTable>>)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Not,
}

#[derive(Debug, Clone)]
pub enum Expr {
	U64(u64),
	F64(f64),
	I64(i64),
	Str(String),
	Bool(bool),
	Id(String),
	UnaryOp(Box<Expr>, UnaryOp),

	// function name, arguments
	Call(String, Vec<Expr>),

	Arg(String),

	// lhs, op, rhs
	BinOp(Box<Expr>, BinOp, Box<Expr>),

	// condition,   then block   ,   else block
	If(Box<Expr>, Vec<Statements>, Vec<Statements>),
	Void
}


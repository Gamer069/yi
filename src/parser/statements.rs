use crate::{keywords::{self, TypeKw}, parser::Expr};

pub type AST = Vec<Statements>;

#[derive(Debug, Clone)]
pub enum Statements {
	Let(String, Option<TypeKw>, Option<Box<Expr>>),
	Assign(String, Box<Expr>),
	Func(String, Vec<(String, keywords::TypeKw)>, keywords::TypeKw, Vec<Statements>),
	Expr(Box<Expr>),
	Return(Box<Expr>),
}

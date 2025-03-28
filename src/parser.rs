use std::cell::RefCell;

use sym_table::SymTable;

use crate::tok::*;

mod sym_table;

type AST = Vec<Statements>;

pub struct Parser {
    tokens: Vec<Tok>,
    i: usize,
    pub sym_table: RefCell<SymTable>
}

impl Parser {
    pub fn new(toks: Vec<Tok>) -> Self {
        Self { tokens: toks, i: 0, sym_table: RefCell::new(SymTable::new()) }
    }
    pub fn parse(&mut self) -> Result<AST, String> {
        let mut statements: AST = Vec::new();

        while self.i < self.tokens.len() {
            match self.cur() {
                Some(Tok::Func) => {
                    self.parse_func_statement();
                },
                Some(Tok::Let) => {
                    let statement = self.parse_let_statement();
                    statements.push(statement);
                },
                Some(Tok::Sc) => {
                    // Skip standalone semicolons
                    self.eat();
                    continue;
                },
                Some(Tok::Id(id)) => {
                    self.eat();
                    if self.cur().unwrap() == Tok::Eq {
                        self.eat();
                        let expr = self.parse_expr();
                        self.sym_table.borrow_mut().put(&id, expr).expect("Failed to assign to variable");
                    }
                },
                Some(_) => {
                    let expr = self.parse_expr();
                    statements.push(Statements::Expr(Box::new(expr)));

                    if self.cur() == Some(Tok::Sc) {
                        self.eat();
                    }
                },
                None => break,
            }
        };

        println!("{:#?}", self.sym_table);

        Ok(statements)
    }

    fn parse_expr(&mut self) -> Expr {
        let mut left = self.parse_term();
        while let Some(tok) = self.cur() {
            match tok {
                Tok::Add | Tok::Sub => {
                    let op = match tok {
                        Tok::Add => BinOp::Add,
                        Tok::Sub => BinOp::Sub,
                        _ => unreachable!(),
                    };
                    self.eat();
                    let right = self.parse_term();
                    left = Expr::BinOp(Box::new(left), op, Box::new(right));
                },
                Tok::Sc | Tok::Rp => break, // Stop parsing expression at semicolon or right parenthesis
                _ => break,
            }
        };
        left
    }

    fn parse_term(&mut self) -> Expr {
        let mut left = self.parse_factor();
        while let Some(tok) = self.cur() {
            match tok {
                Tok::Mul | Tok::Div => {
                    let op = match tok {
                        Tok::Mul => BinOp::Mul,
                        Tok::Div => BinOp::Div,
                        _ => unreachable!(),
                    };
                    self.eat();
                    let right = self.parse_factor();
                    left = Expr::BinOp(Box::new(left), op, Box::new(right));
                },
                Tok::Add | Tok::Sub | Tok::Sc | Tok::Rp => break, // Break on operators with lower precedence
                _ => break,
            }
        }
        left
    }

    fn parse_factor(&mut self) -> Expr {
        // ok, for now we WON'T have local/global variations of variables, just global ones but
        // I'll implement that later
        match self.cur() {
            Some(Tok::I64(num)) => {
                let value = num;
                self.eat();
                Expr::I64(value)
            },
            Some(Tok::U64(num)) => {
                let value = num;
                self.eat();
                Expr::U64(value)
            },
            Some(Tok::F64(num)) => {
                let value = num;
                self.eat();
                Expr::F64(value)
            },
            Some(Tok::Lp) => {
                self.eat();
                let expr_node = self.parse_expr();
                if self.cur() == Some(Tok::Rp) {
                    self.eat();
                } else {
                    eprintln!("Expected ')' but got {:?}", self.cur());
                    std::process::exit(-1);
                }
                // self.eat_tok(Tok::Rp);
                expr_node
            },
            Some(Tok::Id(id)) => {
                let id_clone = id.clone();
                self.eat();
                Expr::Id(id_clone)
            },
            Some(Tok::Str(s)) => {
                let s_clone = s.clone();
                self.eat();
                Expr::Str(s_clone)
            },
            Some(Tok::Bool(val)) => {
                self.eat();
                Expr::Bool(val)
            },
            _ => {
                eprintln!("Unexpected token {:?}", self.cur().unwrap());
                std::process::exit(-1);
            }
        }
    }

    fn parse_let_statement(&mut self) -> Statements {
        self.eat_tok(Tok::Let);
        let id = self.parse_id();
        self.eat_tok(Tok::Eq);
        let expr = self.parse_expr();

        // Check for and consume semicolon if present
        if self.cur() == Some(Tok::Sc) {
            self.eat();
        }

        self.sym_table.borrow_mut().put(&id, expr.clone());

        Statements::Let(id, Box::from(expr), false)
    }

    fn parse_func_statement(&mut self) -> Statements {
        self.eat_tok(Tok::Func);
        let id = self.parse_id();
        println!("id: {:?}", id);
        self.eat_tok(Tok::Lp);
        // TODO: args
        self.eat_tok(Tok::Rp);
        self.eat_tok(Tok::Lc);

        let mut statements: AST = Vec::new();

        while self.cur() != Some(Tok::Rc) {
            match self.cur() {
                Some(Tok::Func) => {
                    self.parse_func_statement();
                },
                Some(Tok::Let) => {
                    let statement = self.parse_let_statement();
                    statements.push(statement);
                },
                Some(Tok::Sc) => {
                    // Skip standalone semicolons
                    self.eat();
                    continue;
                },
                Some(Tok::Id(id)) => {
                    self.eat();
                    if self.cur().unwrap() == Tok::Eq {
                        self.eat();
                        let expr = self.parse_expr();
                        self.sym_table.borrow_mut().put(&id, expr).expect("Failed to assign to variable");
                    }
                },
                Some(_) => {
                    let expr = self.parse_expr();
                    statements.push(Statements::Expr(Box::new(expr)));

                    if self.cur() == Some(Tok::Sc) {
                        self.eat();
                    }
                },
                None => break,
            }
        };

        println!("{:#?}", self.sym_table);

        self.eat_tok(Tok::Rc);

        Statements::Func(id, statements)
    }

    fn parse_id(&mut self) -> String {
        if let Some(Tok::Id(id)) = self.cur() {
            let id_clone = id.clone();
            self.eat();
            id_clone
        } else {
            eprintln!("Expected ID but got {:?}", self.cur().unwrap());
            std::process::exit(-1);
        }
    }

    fn cur(&self) -> Option<Tok> {
        self.tokens.get(self.i).cloned()
    }

    fn eat(&mut self) -> Option<Tok> {
        let tok = self.tokens.get(self.i).cloned();
        self.i += 1;
        tok
    }

    fn eat_tok(&mut self, expected_tok: Tok) {
        if let Some(cur_tok) = self.cur() {
            // Use discriminant comparison instead of direct equality
            // This ensures we only check the enum variant, not the contained values
            if std::mem::discriminant::<_>(&cur_tok) == std::mem::discriminant(&expected_tok) {
                self.eat();
            } else {
                eprintln!("Expected {:?} but got {:?}", expected_tok, cur_tok);
                std::process::exit(-1);
            }
        } else {
            eprintln!("Expected {:?} but reached end of tokens", expected_tok);
            std::process::exit(-1);
        }
    }
}

// Define BinOp enum to match the new token structure
#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Expr {
    U64(u64),
    F64(f64),
    I64(i64),
    Str(String),
    Bool(bool),
    Id(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statements {
    Let(String, Box<Expr>, bool),
    Func(String, Vec<Statements>),
    Expr(Box<Expr>),
}

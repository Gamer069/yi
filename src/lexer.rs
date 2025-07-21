use std::fs;
use std::iter::Peekable;
use std::str::Chars;

use crate::tok::Tok;

macro_rules! eeprintln {
    ($($arg:tt)*) => {{
        eprintln!("{}помилка:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
    }};
}

pub struct Lexer {
    code: String
}

impl Lexer {
    pub fn new(fname: String) -> Lexer {
        let codestring = fs::read_to_string(fname).expect("Failed to read file for some reason...");
        Self { code: codestring }
    }

    pub fn lex(&self) -> Vec<Tok> {
        let mut toks: Vec<Tok> = Vec::new();
        let mut chars = self.code.chars().peekable();
        let mut is_comment = false;
        while let Some(c) = chars.next() {
            if !is_comment {
                match c {
                    c if ('\u{0400}' <= c && c <= '\u{04FF}') || c == '_' => {
                        let mut id: String = String::from(c);
                        let mut chars_iter = chars.clone(); //clone the iterator one time.
                        while let Some(peeked_char) = chars_iter.peek() {
                            if ('\u{0400}' <= *peeked_char && *peeked_char <= '\u{04FF}') || *peeked_char == '_' {
                                id.push(chars_iter.next().unwrap()); //now use next from the clone.
                                chars = chars_iter.clone(); //replace the original char iterator with the now advanced clone.
                            } else {
                                break;
                            }
                        }
                        if id == "так" {
                            toks.push(Tok::Bool(true));
                            continue;
                        }
                        if id == "ні" {
                            toks.push(Tok::Bool(false));
                            continue;
                        }
                        if id == "змінна" {
                            toks.push(Tok::Let);
                            continue;
                        }
                        if id == "функ" {
                            toks.push(Tok::Func);
                            continue;
                        }
                        toks.push(Tok::Id(id));
                    },
                    'A'..='z' => {
                        eeprintln!("Англійський тут не працює");
                        std::process::exit(-1);
                    },
                    '=' => {
                        match chars.peek() {
                            Some(&'=')=>{
                                toks.push(Tok::EqEq);
                                chars.next();
                            }
                            _=>{
                                toks.push(Tok::Eq);
                            }
                        }
                    },
                    ';' => {
                        toks.push(Tok::Sc);
                    },
                    '+' => {
                        toks.push(Tok::Add);
                    },
                    '-' => {
                        let peeked = chars.peek();
                        if peeked.is_some() {
                            let peekedchar = *peeked.unwrap();
                            if '0' <= peekedchar && peekedchar <= '9' {
                                self.lex_num(true, c, &mut chars, &mut toks);
                            } else {
                                toks.push(Tok::Sub);
                            }
                        }
                    },
                    '*' => {
                        toks.push(Tok::Mul);
                    },
                    '/' => {
                        let peeked = chars.peek();
                        if peeked.is_some() {
                            if *peeked.unwrap() == '/' {
                                is_comment = true;
                                continue;
                                // is comment
                            }
                        }
                        toks.push(Tok::Div);
                    },
                    '(' => {
                        toks.push(Tok::Lp);
                    },
                    ')' => {
                        toks.push(Tok::Rp);
                    },
                    '{' => {
                        toks.push(Tok::Lc);
                    },
                    '}' => {
                        toks.push(Tok::Rc);
                    },
                    '"' => {
                        let mut str: String = String::new();
                        let mut chars_iter = chars.clone();
                        while let Some(peeked_char) = chars_iter.peek() {
                            if *peeked_char != '"' {
                                str.push(chars_iter.next().unwrap());
                                chars = chars_iter.clone();
                            } else {
                                chars.next();
                                break;
                            }
                        }
                        toks.push(Tok::Str(str));
                    },
                    '0'..='9' => {
                        self.lex_num(false, c, &mut chars, &mut toks);
                    },
                    _ => {}
                };
            } else {
                if c == '\n' {
                    is_comment = false;
                }
            };
        };
        toks
    }
    fn lex_num<'a>(&self, neg: bool, c: char, chars: &mut Peekable<Chars<'a>>, toks: &mut Vec<Tok>) {
        let mut num: String = String::from(c);
        let mut chars_iter = chars.clone();
        while let Some(peeked) = chars_iter.peek() {
            match *peeked {
                '-' => {
                    num.push(chars_iter.next().unwrap());
                    *chars = chars_iter.clone();
                },
                '0'..='9' => {
                    num.push(chars_iter.next().unwrap());
                    *chars = chars_iter.clone();
                },
                '.' => {
                    num.push(chars_iter.next().unwrap());
                    *chars = chars_iter.clone();
                },
                _ => { 
                    break;
                }
            }
        }
        if num.is_empty() {
            println!("Empty...");
            return;
        }

        if num.contains('.') {
            toks.push(Tok::F64(num.parse::<f64>().unwrap()));
        } else {
            if neg {
                toks.push(Tok::I64(num.parse::<i64>().unwrap()));
            } else {
                toks.push(Tok::U64(num.parse::<u64>().unwrap()));
            }
        }
    }
}


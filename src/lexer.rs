use std::fs;
use std::iter::Peekable;
use std::str::Chars;

use crate::tok::{SpannedTok, Tok};

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

    pub fn lex(&self) -> (Vec<SpannedTok>, Vec<String>) {
        let mut toks: Vec<SpannedTok> = Vec::new();
        let mut chars = self.code.chars().peekable();
        let mut is_comment = false;
        let mut line = 1;
        let mut col = 0;
        let lines = self.code.lines().map(|s| s.to_owned()).collect::<Vec<String>>();

        while let Some(c) = chars.next() {
            col += 1;

            if c == '\n' {
                line += 1;
                col = 0;
                continue;
            }

            if !is_comment {
                match c {
                    c if ('\u{0400}' <= c && c <= '\u{04FF}') || c == '_' => {
                        let mut id: String = String::from(c);
                        let mut chars_iter = chars.clone(); //clone the iterator one time.
                        while let Some(peeked_char) = chars_iter.peek() {
                            if ('\u{0400}' <= *peeked_char && *peeked_char <= '\u{04FF}') || *peeked_char == '_' {
                                id.push(chars_iter.next().unwrap()); //now use next from the clone.
                                col += 1;
                                chars = chars_iter.clone(); //replace the original char iterator with the now advanced clone.
                            } else {
                                break;
                            }
                        }
                        if id == "так" {
                            toks.push(Tok::Bool(true).to_spanned(line, col));
                            continue;
                        }
                        if id == "ні" {
                            toks.push(Tok::Bool(false).to_spanned(line, col));
                            continue;
                        }
                        if id == "змінна" {
                            toks.push(Tok::Let.to_spanned(line, col));
                            continue;
                        }
                        if id == "функ" {
                            toks.push(Tok::Func.to_spanned(line, col));
                            continue;
                        }
                        toks.push(Tok::Id(id).to_spanned(line, col));
                    },
                    'A'..='z' => {
                        eeprintln!("Англійський тут не працює");
                        std::process::exit(-1);
                    },
                    '=' => {
                        match chars.peek() {
                            Some(&'=')=>{
                                toks.push(Tok::EqEq.to_spanned(line, col));
                                chars.next();
                            }
                            _=>{
                                toks.push(Tok::Eq.to_spanned(line, col));
                            }
                        }
                    },
                    ';' => {
                        toks.push(Tok::Sc.to_spanned(line, col));
                    },
                    '+' => {
                        toks.push(Tok::Add.to_spanned(line, col));
                    },
                    '-' => {
                        let peeked = chars.peek();
                        if peeked.is_some() {
                            let peekedchar = *peeked.unwrap();
                            if '0' <= peekedchar && peekedchar <= '9' {
                                self.lex_num(true, c, &mut chars, &mut toks, line, &mut col);
                            } else {
                                toks.push(Tok::Sub.to_spanned(line, col));
                            }
                        }
                    },
                    '*' => {
                        toks.push(Tok::Mul.to_spanned(line, col));
                    },
                    '/' => {
                        let peeked = chars.peek();
                        if peeked.is_some() {
                            if *peeked.unwrap() == '/' {
                                is_comment = true;

                                continue;
                            }
                        }
                        toks.push(Tok::Div.to_spanned(line, col));
                    },
                    '(' => {
                        toks.push(Tok::Lp.to_spanned(line, col));
                    },
                    ')' => {
                        toks.push(Tok::Rp.to_spanned(line, col));
                    },
                    '{' => {
                        toks.push(Tok::Lc.to_spanned(line, col));
                    },
                    '}' => {
                        toks.push(Tok::Rc.to_spanned(line, col));
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
                        toks.push(Tok::Str(str).to_spanned(line, col));
                    },
                    '0'..='9' => {
                        self.lex_num(false, c, &mut chars, &mut toks, line, &mut col);
                    },
                    _ => {}
                };
            } else {
                if c == '\n' {
                    is_comment = false;
                }
            };
        };
        (toks, lines)
    }
    fn lex_num<'a>(&self, neg: bool, c: char, chars: &mut Peekable<Chars<'a>>, toks: &mut Vec<SpannedTok>, line: usize, col: &mut usize) {
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
            *col += 1;
        }
        if num.is_empty() {
            println!("Empty...");
            return;
        }

        if num.contains('.') {
            toks.push(Tok::F64(num.parse::<f64>().unwrap()).to_spanned(line, *col));
        } else {
            if neg {
                toks.push(Tok::I64(num.parse::<i64>().unwrap()).to_spanned(line, *col));
            } else {
                toks.push(Tok::U64(num.parse::<u64>().unwrap()).to_spanned(line, *col));
            }
        }
    }
}


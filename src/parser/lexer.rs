use std::{
    collections::VecDeque,
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader, Read},
};

use super::error::{ParseError, FAILED_TO_READ_FILE, INVALID_TOKEN, UNEXPECTED_EOF};

#[derive(Clone, Debug, PartialEq)]
pub enum Tokens {
    TAnd,
    TAssign,
    TAttach,
    TBool,
    TBreak,
    TColon,
    TComma,
    TContinue,
    TDiv,
    TDto,
    TDot,
    TElse,
    TEof,
    TEq,
    TFalse,
    TFor,
    TFn,
    TGt,
    TGte,
    TIf,
    TIn,
    TInt,
    TLbrace,
    TLbrack,
    TLparen,
    TLt,
    TLte,
    TMap,
    TMinus,
    TMod,
    TMult,
    TNeq,
    TNot,
    TOr,
    TPlus,
    TPub,
    TRbrace,
    TRbrack,
    TReal,
    TReturn,
    TRparen,
    TSemi,
    TStr,
    TTo,
    TTrait,
    TTrue,
    TVar,
    TWhile,
    TIdentifier(String),
    TIntegerValue(i64),
    TStringValue(String),
    TRealValue(f64),
}

impl Display for Tokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TAnd => write!(f, "and"),
            Self::TAssign => write!(f, "assign"),
            Self::TAttach => write!(f, "attach"),
            Self::TBool => write!(f, "bool"),
            Self::TBreak => write!(f, "break"),
            Self::TColon => write!(f, "colon"),
            Self::TComma => write!(f, "comma"),
            Self::TContinue => write!(f, "continue"),
            Self::TDiv => write!(f, "div"),
            Self::TDto => write!(f, "dto"),
            Self::TDot => write!(f, "."),
            Self::TElse => write!(f, "else"),
            Self::TEof => write!(f, "<eof>"),
            Self::TEq => write!(f, "=="),
            Self::TFalse => write!(f, "false"),
            Self::TFor => write!(f, "for"),
            Self::TFn => write!(f, "fn"),
            Self::TGt => write!(f, ">"),
            Self::TGte => write!(f, ">="),
            Self::TIf => write!(f, "if"),
            Self::TIn => write!(f, "in"),
            Self::TInt => write!(f, "int"),
            Self::TLbrace => write!(f, "{{"),
            Self::TLbrack => write!(f, "["),
            Self::TLparen => write!(f, "("),
            Self::TLt => write!(f, "<"),
            Self::TLte => write!(f, "<="),
            Self::TMap => write!(f, "map"),
            Self::TMinus => write!(f, "-"),
            Self::TMod => write!(f, "mod"),
            Self::TMult => write!(f, "*"),
            Self::TNeq => write!(f, "!="),
            Self::TNot => write!(f, "not"),
            Self::TOr => write!(f, "or"),
            Self::TPlus => write!(f, "+"),
            Self::TPub => write!(f, "pub"),
            Self::TRbrace => write!(f, "}}"),
            Self::TRbrack => write!(f, "]"),
            Self::TReal => write!(f, "real"),
            Self::TReturn => write!(f, "return"),
            Self::TRparen => write!(f, ")"),
            Self::TSemi => write!(f, ";"),
            Self::TStr => write!(f, "str"),
            Self::TTo => write!(f, "to"),
            Self::TTrait => write!(f, "trait"),
            Self::TTrue => write!(f, "true"),
            Self::TVar => write!(f, "var"),
            Self::TWhile => write!(f, "while"),
            Self::TIdentifier(id) => write!(f, "{id}"),
            Self::TIntegerValue(val) => write!(f, "{val}"),
            Self::TStringValue(val) => write!(f, "{val}"),
            Self::TRealValue(val) => write!(f, "{val}"),
        }
    }
}

pub struct Lexer {
    pub buffer: BufReader<File>,
    pub current_line: usize,
    pub current_char: usize,
    pub token_buffer: VecDeque<Tokens>,
}

impl Lexer {
    pub fn new(buffer: BufReader<File>) -> Self {
        Self {
            buffer,
            current_char: 1,
            current_line: 1,
            token_buffer: VecDeque::new(),
        }
    }

    pub fn next_token(&mut self) -> Result<Tokens, ParseError> {
        let token = self._next_token()?;
        return Ok(token);
    }

    pub fn _next_token(&mut self) -> Result<Tokens, ParseError> {
        match self.token_buffer.pop_back() {
            Some(t) => return Ok(t),
            None => {}
        }

        let mut token = String::new();
        let ch = self.get_next_char()?;

        match ch {
            '\0' => Ok(Tokens::TEof),
            '(' => return Ok(Tokens::TLparen),
            ')' => return Ok(Tokens::TRparen),
            '{' => return Ok(Tokens::TLbrace),
            '}' => return Ok(Tokens::TRbrace),
            '[' => return Ok(Tokens::TLbrack),
            ']' => return Ok(Tokens::TRbrack),
            ';' => return Ok(Tokens::TSemi),
            ':' => return Ok(Tokens::TColon),
            ',' => return Ok(Tokens::TComma),
            '+' => return Ok(Tokens::TPlus),
            '-' => return Ok(Tokens::TMinus),
            '*' => return Ok(Tokens::TMult),
            '/' => {
                let next_ch = self.get_next_char()?;

                if next_ch == '/' {
                    self.get_chars_until(b'\n')?;
                    self.put_back_char()?;
                    return self.next_token();
                } else {
                    return Ok(Tokens::TDiv);
                }
            }
            '%' => return Ok(Tokens::TMod),
            '=' | '<' | '>' => {
                let next_ch = self.get_next_char()?;

                match next_ch {
                    '=' => match ch {
                        '=' => return Ok(Tokens::TEq),
                        '<' => return Ok(Tokens::TLte),
                        '>' => return Ok(Tokens::TGte),
                        _ => {
                            return Err(ParseError {
                                message: String::from(FAILED_TO_READ_FILE),
                            })
                        }
                    },
                    _ => {
                        if next_ch != '\0' {
                            self.put_back_char()?;
                        }

                        match ch {
                            '=' => return Ok(Tokens::TAssign),
                            '<' => return Ok(Tokens::TLt),
                            '>' => return Ok(Tokens::TGt),
                            _ => {
                                return Err(ParseError {
                                    message: String::from(FAILED_TO_READ_FILE),
                                });
                            }
                        }
                    }
                }
            }
            '!' => {
                let next_ch = self.get_next_char()?;

                if next_ch == '=' {
                    return Ok(Tokens::TNeq);
                }

                return Err(ParseError {
                    message: String::from(INVALID_TOKEN),
                });
            }
            '"' => {
                let mut found_string_end = false;

                while found_string_end == false {
                    let string_const = self.get_chars_until(b'"')?;

                    match string_const {
                        None => {
                            return Err(ParseError {
                                message: String::from(UNEXPECTED_EOF),
                            })
                        }
                        Some(s) => {
                            token.push_str(&s);
                        }
                    };

                    if token.len() == 1 {
                        token.pop();
                        found_string_end = true;
                    } else {
                        let second_2_last_char = token.chars().nth(token.len() - 2).unwrap();

                        if second_2_last_char != '\\' {
                            token.pop();
                            found_string_end = true;
                        }
                    }
                }

                return Ok(Tokens::TStringValue(token));
            }
            ch if ch.is_whitespace() => {
                if ch == '\n' {
                    self.current_line += 1;
                    self.current_char = 0;
                }

                return self.next_token();
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                token.push_str(&ch.to_string());

                loop {
                    let ch = self.get_next_char()?;

                    if !ch.is_ascii_alphanumeric() && ch != '_' {
                        self.put_back_char()?;
                        break;
                    }

                    token.push_str(&String::from(ch));
                }

                let matched_token = match token {
                    token if token == "and" => Tokens::TAnd,
                    token if token == "attach" => Tokens::TAttach,
                    token if token == "bool" => Tokens::TBool,
                    token if token == "break" => Tokens::TBreak,
                    token if token == "continue" => Tokens::TContinue,
                    token if token == "dto" => Tokens::TDto,
                    token if token == "else" => Tokens::TElse,
                    token if token == "false" => Tokens::TFalse,
                    token if token == "for" => Tokens::TFor,
                    token if token == "fn" => Tokens::TFn,
                    token if token == "if" => Tokens::TIf,
                    token if token == "in" => Tokens::TIn,
                    token if token == "int" => Tokens::TInt,
                    token if token == "map" => Tokens::TMap,
                    token if token == "mod" => Tokens::TMod,
                    token if token == "not" => Tokens::TNot,
                    token if token == "or" => Tokens::TOr,
                    token if token == "pub" => Tokens::TPub,
                    token if token == "real" => Tokens::TReal,
                    token if token == "return" => Tokens::TReturn,
                    token if token == "str" => Tokens::TStr,
                    token if token == "to" => Tokens::TTo,
                    token if token == "trait" => Tokens::TTrait,
                    token if token == "true" => Tokens::TTrue,
                    token if token == "var" => Tokens::TVar,
                    token if token == "while" => Tokens::TWhile,
                    _ => Tokens::TIdentifier(token),
                };

                return Ok(matched_token);
            }
            ch if ch.is_ascii_digit() => {
                self.put_back_char()?;
                self.read_integer(&mut token)?;

                let maybe_decimal = self.get_next_char()?;

                if maybe_decimal == '.' {
                    token.push_str(&String::from(maybe_decimal));
                    self.read_integer(&mut token)?;

                    let real_const = match token.parse::<f64>() {
                        Ok(val) => val,
                        Err(err) => {
                            return Err(ParseError {
                                message: err.to_string(),
                            })
                        }
                    };

                    return Ok(Tokens::TRealValue(real_const));
                }

                self.put_back_char()?;

                let int_const = match token.parse::<i64>() {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(ParseError {
                            message: err.to_string(),
                        })
                    }
                };

                return Ok(Tokens::TIntegerValue(int_const));
            }
            ch if ch.to_string() == "." => {
                let next_ch = self.get_next_char()?;

                if next_ch.is_ascii_digit() {
                    token.push_str(&String::from(ch));
                    token.push_str(&String::from(next_ch));

                    self.read_integer(&mut token)?;

                    let real_const = match token.parse::<f64>() {
                        Ok(val) => val,
                        Err(err) => {
                            return Err(ParseError {
                                message: err.to_string(),
                            })
                        }
                    };

                    return Ok(Tokens::TRealValue(real_const));
                }

                self.put_back_char()?;
                return Ok(Tokens::TDot);
            }
            _ => {
                return Err(ParseError {
                    message: String::from(FAILED_TO_READ_FILE),
                })
            }
        }
    }

    pub fn get_position_string(&self) -> String {
        format!("{}:{}", self.current_line, self.current_char)
    }

    pub fn peek_token(&mut self) -> Result<Tokens, ParseError> {
        let token = self._next_token()?;
        self.token_buffer.push_front(token.clone());
        return Ok(token);
    }

    pub fn put_back_token(&mut self, token: Tokens) {
        self.token_buffer.push_back(token);
    }

    fn get_chars_until(&mut self, ch: u8) -> Result<Option<String>, ParseError> {
        let mut str_buffer: Vec<u8> = Vec::new();

        let count = match self.buffer.read_until(ch, &mut str_buffer) {
            Ok(count) => count,
            Err(err) => {
                return Err(ParseError {
                    message: err.to_string(),
                })
            }
        };

        if count < 1 {
            return Ok(None);
        }

        self.current_char += count;
        return Ok(Some(String::from_utf8(str_buffer).unwrap()));
    }

    fn get_next_char(&mut self) -> Result<char, ParseError> {
        let mut read_buffer: [u8; 1] = [0];
        let count = match self.buffer.read(&mut read_buffer) {
            Ok(count) => count,
            Err(err) => {
                return Err(ParseError {
                    message: err.to_string(),
                })
            }
        };

        if count < 1 {
            return Ok('\0');
        }

        let ch = read_buffer[0] as char;
        self.current_char += 1;

        return Ok(ch);
    }

    fn put_back_char(&mut self) -> Result<(), ParseError> {
        match self.buffer.seek_relative(-1) {
            Ok(()) => {
                self.current_char -= 1;
                Ok(())
            }
            Err(err) => Err(ParseError {
                message: err.to_string(),
            }),
        }
    }

    fn read_integer(&mut self, token: &mut String) -> Result<(), ParseError> {
        loop {
            let ch = self.get_next_char()?;

            if ch.is_ascii_digit() {
                token.push_str(&String::from(ch));
            } else {
                self.put_back_char()?;
                break;
            }
        }

        Ok(())
    }

    pub fn is_typename(token: Tokens) -> bool {
        match token {
            Tokens::TBool
            | Tokens::TInt
            | Tokens::TMap
            | Tokens::TReal
            | Tokens::TStr
            | Tokens::TVar
            | Tokens::TIdentifier(_) => true,
            _ => false,
        }
    }
}

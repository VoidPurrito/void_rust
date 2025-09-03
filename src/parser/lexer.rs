use std::{
    collections::VecDeque, fmt::Display, fs::File, io::{BufRead, BufReader, Read}
};

use super::error::{ParseError, FAILED_TO_READ_FILE, INVALID_TOKEN, UNEXPECTED_EOF};

#[derive(Clone, Debug, PartialEq)]
pub enum Tokens {
    TAnd,
    TAssign,
    TAttach,
    TBool,
    TBreak,
    TBuiltIn(String),
    TColon,
    TClass,
    TComma,
    TContinue,
    TDiv,
    TDto,
    TDot,
    TElse,
    TEof,
    TEq,
    // TExit,
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

#[derive(Clone)]
pub struct Token {
    pub value: Tokens,
    pub line_number: usize,
    pub char_number: usize,
}

impl Token {
    pub fn new(value: Tokens, line: usize, char: usize) -> Self {
        Self {
            value,
            line_number: line,
            char_number: char,
        }
    }
}

pub struct Character {
    pub value: char,
    pub line_no: usize,
    pub char_no: usize,
}

impl Display for Tokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TAnd => write!(f, "and"),
            Self::TAssign => write!(f, "assign"),
            Self::TAttach => write!(f, "attach"),
            Self::TBool => write!(f, "bool"),
            Self::TBreak => write!(f, "break"),
            Self::TBuiltIn(name) => write!(f, "{name}"),
            Self::TClass => write!(f, "class"),
            Self::TColon => write!(f, "colon"),
            Self::TComma => write!(f, "comma"),
            Self::TContinue => write!(f, "continue"),
            Self::TDiv => write!(f, "div"),
            Self::TDto => write!(f, "dto"),
            Self::TDot => write!(f, "."),
            Self::TElse => write!(f, "else"),
            Self::TEof => write!(f, "<eof>"),
            Self::TEq => write!(f, "=="),
            // Self::TExit => write!(f, "exit"),
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
    pub token_buffer: VecDeque<Token>,
    
    char_buffer: [usize; 2],
    char_buffer_idx: usize,
}

impl Lexer {
    pub fn new(buffer: BufReader<File>) -> Self {
        Self {
            buffer,
            current_char: 1,
            current_line: 1,
            token_buffer: VecDeque::new(),
            char_buffer: [0, 0],
            char_buffer_idx: 1,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, ParseError> {
        let token = self._next_token(false)?;
        return Ok(token);
    }

    pub fn _next_token(&mut self, peek: bool) -> Result<Token, ParseError> {
        if !peek {
            match self.token_buffer.pop_back() {
                Some(t) => return Ok(t),
                None => {}
            }
        }

        let mut token = String::new();
        let ch = self.get_next_char()?;

        match ch.value {
            '\0' => Ok(Token::new(Tokens::TEof, ch.line_no, ch.char_no)),
            '(' => Ok(Token::new(Tokens::TLparen, ch.line_no, ch.char_no)),
            ')' => Ok(Token::new(Tokens::TRparen, ch.line_no, ch.char_no)),
            '{' => Ok(Token::new(Tokens::TLbrace, ch.line_no, ch.char_no)),
            '}' => Ok(Token::new(Tokens::TRbrace, ch.line_no, ch.char_no)),
            '[' => Ok(Token::new(Tokens::TLbrack, ch.line_no, ch.char_no)),
            ']' => Ok(Token::new(Tokens::TRbrack, ch.line_no, ch.char_no)),
            ';' => Ok(Token::new(Tokens::TSemi, ch.line_no, ch.char_no)),
            ':' => Ok(Token::new(Tokens::TColon, ch.line_no, ch.char_no)),
            ',' => Ok(Token::new(Tokens::TComma, ch.line_no, ch.char_no)),
            '%' => Ok(Token::new(Tokens::TMod, ch.line_no, ch.char_no)),
            '+' => Ok(Token::new(Tokens::TPlus, ch.line_no, ch.char_no)),
            '-' => Ok(Token::new(Tokens::TMinus, ch.line_no, ch.char_no)),
            '*' => Ok(Token::new(Tokens::TMult, ch.line_no, ch.char_no)),
            '/' => {
                let next_ch = self.get_next_char()?;

                if next_ch.value == '/' {
                    self.get_chars_until(b'\n')?;
                    //self.put_back_char()?;
                    return self.next_token();
                } else {
                    return Ok(Token::new(Tokens::TDiv, ch.line_no, ch.char_no));
                }
            }
            '=' | '<' | '>' => {
                let next_ch = self.get_next_char()?;

                match next_ch.value {
                    '=' => match ch.value {
                        '=' => return Ok(Token::new(Tokens::TEq, ch.line_no, ch.char_no)),
                        '<' => return Ok(Token::new(Tokens::TLte, ch.line_no, ch.char_no)),
                        '>' => return Ok(Token::new(Tokens::TGte, ch.line_no, ch.char_no)),
                        _ => {
                            return Err(ParseError {
                                message: String::from(FAILED_TO_READ_FILE),
                            })
                        }
                    },
                    _ => {
                        if next_ch.value != '\0' {
                            self.put_back_char()?;
                        }

                        match ch.value {
                            '=' => return Ok(Token::new(Tokens::TAssign, ch.line_no, ch.char_no)),
                            '<' => return Ok(Token::new(Tokens::TLt, ch.line_no, ch.char_no)),
                            '>' => return Ok(Token::new(Tokens::TGt, ch.line_no, ch.char_no)),
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

                if next_ch.value == '=' {
                    return Ok(Token::new(Tokens::TNeq, ch.line_no, ch.char_no));
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

                return Ok(Token::new(Tokens::TStringValue(token), ch.line_no, ch.char_no));
            }
            val if val.is_whitespace() => {
                if val == '\n' {
                    self.current_line += 1;
                    self.current_char = 1;
                }

                return self.next_token();
            }
            val if val.is_ascii_alphabetic() || val == '_' => {
                let lineno = ch.line_no;
                let charno = ch.char_no;
                let mut next_ch = ch;

                let mut has_letter = val.is_ascii_alphabetic();

                loop {
                    if has_letter {
                        if !next_ch.value.is_ascii_alphanumeric() && next_ch.value != '_' {
                            self.put_back_char()?;
                            break;
                        }
                    } else if next_ch.value.is_ascii_alphabetic() {
                        has_letter = true;
                    } else {

                    }

                    token.push_str(&String::from(next_ch.value));
                    next_ch = self.get_next_char()?;
                }

                let matched_token = match token {
                    token if Lexer::get_if_reserved_word(&token) != None => {
                        Lexer::get_if_reserved_word(&token).unwrap()
                    }
                    _ => Tokens::TIdentifier(token),
                };

                return Ok(Token::new(matched_token, lineno, charno));
            }
            val if val.is_ascii_digit() => {
                self.put_back_char()?;
                self.read_integer(&mut token)?;

                let maybe_decimal = self.get_next_char()?;

                if maybe_decimal.value == '.' {
                    token.push_str(&String::from(maybe_decimal.value));
                    self.read_integer(&mut token)?;

                    let real_const = match token.parse::<f64>() {
                        Ok(val) => val,
                        Err(err) => {
                            return Err(ParseError {
                                message: err.to_string(),
                            })
                        }
                    };

                    return Ok(Token::new(Tokens::TRealValue(real_const), ch.line_no, ch.char_no));
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

                return Ok(Token::new(Tokens::TIntegerValue(int_const), ch.line_no, ch.char_no));
            }
            val if val.to_string() == "." => {
                let next_ch = self.get_next_char()?;

                if next_ch.value.is_ascii_digit() {
                    token.push_str(&String::from(val));
                    token.push_str(&String::from(next_ch.value));

                    self.read_integer(&mut token)?;

                    let real_const = match token.parse::<f64>() {
                        Ok(val) => val,
                        Err(err) => {
                            return Err(ParseError {
                                message: err.to_string(),
                            })
                        }
                    };

                    return Ok(Token::new(Tokens::TRealValue(real_const), ch.line_no, ch.char_no));
                }

                self.put_back_char()?;
                return Ok(Token::new(Tokens::TDot, ch.line_no, ch.char_no));
            }
            val if val.to_string() == "@" => {
                match self.get_chars_until(b'(') {
                    Ok(Some(mut built_in_name)) => {
                        self.put_back_char()?;
                        built_in_name.pop();
                        Ok(Token::new(Tokens::TBuiltIn(format!("@{}", built_in_name)), ch.line_no, ch.char_no))
                    }
                    Ok(None) => return Ok(Token::new(Tokens::TBuiltIn(format!("@")), ch.line_no, ch.char_no)),
                    Err(err) => return Err(err),
                }
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

    pub fn peek_token(&mut self) -> Result<Token, ParseError> {
        let token = self._next_token(true)?;
        self.token_buffer.push_front(token.clone());
        return Ok(token);
    }

    pub fn put_back_token(&mut self, token: Token) {
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

    fn get_next_char(&mut self) -> Result<Character, ParseError> {
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
            return Ok(Character { value: '\0', line_no: self.current_line, char_no: self.current_char });
        }

        let ch = Character { value: read_buffer[0] as char, line_no: self.current_line, char_no: self.current_char };
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

            if ch.value.is_ascii_digit() {
                token.push_str(&String::from(ch.value));
            } else {
                self.put_back_char()?;
                break;
            }
        }

        Ok(())
    }

    fn is_valid_identifier_character(ch: &char) -> bool {
        ch.is_ascii_alphabetic() || *ch == '_'
    }

    pub fn is_typename(token: &Tokens) -> bool {
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

    fn get_if_reserved_word(token: &String) -> Option<Tokens> {
        match token {
            token if token == "and" => Some(Tokens::TAnd),
            token if token == "attach" => Some(Tokens::TAttach),
            token if token == "bool" => Some(Tokens::TBool),
            token if token == "break" => Some(Tokens::TBreak),
            token if token == "class" => Some(Tokens::TClass),
            token if token == "continue" => Some(Tokens::TContinue),
            token if token == "dto" => Some(Tokens::TDto),
            token if token == "else" => Some(Tokens::TElse),
            // token if token == "exit" => Some(Tokens::TExit),
            token if token == "false" => Some(Tokens::TFalse),
            token if token == "for" => Some(Tokens::TFor),
            token if token == "fn" => Some(Tokens::TFn),
            token if token == "if" => Some(Tokens::TIf),
            token if token == "in" => Some(Tokens::TIn),
            token if token == "int" => Some(Tokens::TInt),
            token if token == "map" => Some(Tokens::TMap),
            token if token == "mod" => Some(Tokens::TMod),
            token if token == "not" => Some(Tokens::TNot),
            token if token == "or" => Some(Tokens::TOr),
            token if token == "pub" => Some(Tokens::TPub),
            token if token == "real" => Some(Tokens::TReal),
            token if token == "return" => Some(Tokens::TReturn),
            token if token == "str" => Some(Tokens::TStr),
            token if token == "to" => Some(Tokens::TTo),
            token if token == "trait" => Some(Tokens::TTrait),
            token if token == "true" => Some(Tokens::TTrue),
            token if token == "var" => Some(Tokens::TVar),
            token if token == "while" => Some(Tokens::TWhile),
            _ => None,
        }
    }
}

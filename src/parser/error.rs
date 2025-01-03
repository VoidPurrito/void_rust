use std::{error::Error, fmt};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed ")
    }
}

pub const UNEXPECTED_EOF: &str = "unexpected end-of-file";
pub const FAILED_TO_READ_FILE: &str = "failed to read file";
pub const INVALID_TOKEN: &str = "Invalid token";

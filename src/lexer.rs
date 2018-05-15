use arrayvec::{ArrayVec, CapacityError};

use std::str::{self, FromStr};
use std::{io, num};

const HEADER_LENGTH: usize = 2;
const NUMBER_LENGTH: usize = 32;
const STRING_LENGTH: usize = 64;

quick_error!{
    #[derive(Debug)]
    pub enum Error {
        InvalidCharacter(c: u8) {
            from()
            description("Invalid character")
            display("Encountered invalid character \"{}\"", *c as char)
        }
        InvalidChecksum(expected: u8, actual: u8) {
            description("Invalid checksum")
            display("Expected checksum \"{:X}\" , found checksum \"{}\"", expected, actual)
        }
        Io(err: io::Error) {
            from()
            description(err.description())
            display("Encountered I/O error while lexing: {}", err)
            cause(err)
        }
        UnexpectedEof(token: &'static str) {
            from()
            description("Unexpected EOF")
            display("Encountered unexpected EOF in {}", token)
        }
        IncompleteToken(token: &'static str) {
            description("Incomplete token")
            display("Could not complete token of type {}", token)
        }
        ArrayOverflow(err: CapacityError<u8>, capacity: usize) {
            description(err.description())
            display("Tried to push more than {} characters into the buffer: {}", capacity, err)
            cause(err)
        }
        Int(err: num::ParseIntError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
    }
}

// Quick error can't handle from for tuples
impl From<(CapacityError<u8>, usize)> for Error {
    fn from((e, cap): (CapacityError<u8>, usize)) -> Self {
        Error::ArrayOverflow(e, cap)
    }
}

impl From<(u8, u8)> for Error {
    fn from((expected, actual): (u8, u8)) -> Self {
        Error::InvalidChecksum(expected, actual)
    }
}

#[derive(Debug)]
pub enum TokenKind {
    Header([u8; HEADER_LENGTH]),
    StringLiteral(ArrayVec<[u8; STRING_LENGTH]>),
    CommaSeperator,
    FloatLiteral(ArrayVec<[u8; NUMBER_LENGTH]>),
    IntLiteral(i64),
    Checksum(u8),
    LineEnding,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Token { kind }
    }
}

pub struct Tokenizer<R> {
    input: io::Bytes<R>,
    peek_buf: Option<u8>,
}

impl<R: io::Read> Tokenizer<R> {
    pub fn new(input: R) -> Result<Self, io::Error> {
        let mut tk = Tokenizer {
            input: input.bytes(),
            peek_buf: None,
        };

        tk.advance()?;

        Ok(tk)
    }

    pub fn advance(&mut self) -> Result<Option<u8>, io::Error> {
        let prev = self.peek_buf;

        self.peek_buf = match self.input.next() {
            None => None,
            Some(Err(e)) => {
                self.peek_buf = None;
                return Err(e);
            }
            Some(Ok(v)) => Some(v),
        };

        Ok(prev)
    }
}

impl<R: io::Read> Iterator for Tokenizer<R> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_buf {
            None => return None,
            Some(c) if c == b'$' => {
                try_some!(self.advance());
                let mut header = [0u8; HEADER_LENGTH];
                for i in 0..HEADER_LENGTH {
                    match try_some!(self.advance()) {
                        None => return Some(Err("Header".into())),
                        Some(c) => header[i] = c,
                    }
                }
                return Some(Ok(Token::new(TokenKind::Header(header))));
            }
            Some(c) if c.is_ascii_alphabetic() => {
                let mut buf = ArrayVec::<[u8; STRING_LENGTH]>::new();
                while let Some(a) = self.peek_buf {
                    if !a.is_ascii_alphabetic() {
                        break;
                    }
                    if let Err(e) = buf.try_push(a) {
                        return Some(Err((e, buf.capacity()).into()));
                    }
                    try_some!(self.advance());
                }
                return Some(Ok(Token::new(TokenKind::StringLiteral(buf))));
            }
            Some(c) if c.is_ascii_digit() || c == b'-' => {
                let mut buf = ArrayVec::<[u8; NUMBER_LENGTH]>::new();
                if c == b'-' {
                    if let Err(e) = buf.try_push(c) {
                        return Some(Err((e, buf.capacity()).into()));
                    }
                    try_some!(self.advance());
                    if let Some(d) = self.peek_buf {
                        if !d.is_ascii_digit() {
                            return Some(Err(Error::IncompleteToken("Number")));
                        }
                    }
                }

                // start of digit, eat digits until we encounter a non-digit
                while let Some(d) = self.peek_buf {
                    if !d.is_ascii_digit() {
                        break;
                    }
                    if let Err(e) = buf.try_push(d) {
                        return Some(Err((e, buf.capacity()).into()));
                    }
                    try_some!(self.advance());
                }

                if let Some(c) = self.peek_buf {
                    if c == b'.' {
                        // we got a decimal dot!
                        if let Err(e) = buf.try_push(c) {
                            return Some(Err((e, buf.capacity()).into()));
                        }
                        try_some!(self.advance());

                        // read another integer literal
                        while let Some(d) = self.peek_buf {
                            if !d.is_ascii_digit() {
                                break;
                            }
                            if let Err(e) = buf.try_push(d) {
                                return Some(Err((e, buf.capacity()).into()));
                            }
                            try_some!(self.advance());
                        }
                        return Some(Ok(Token::new(TokenKind::FloatLiteral(buf))));
                    }
                }
                // we got an integer
                // we know we only have valid ascii characters
                let string = str::from_utf8(&buf).unwrap();
                let int = match i64::from_str(string) {
                    Ok(i) => i,
                    Err(e) => return Some(Err(e.into())),
                };
                Some(Ok(Token::new(TokenKind::IntLiteral(int))))
            }
            Some(c) if c == b',' => {
                try_some!(self.advance());
                Some(Ok(Token::new(TokenKind::CommaSeperator)))
            }
            _ => unimplemented!(),
        }
    }
}

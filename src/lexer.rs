use std::io;

pub enum TokenKind {
    Header([u8; 2]),
    StringLiteral(String),
    CommaSeperator,
    FloatLiteral(f64),
    IntLiteral(i64),
    ValidChecksum(u8),
    InvalidChecksum { expected: u8, actual: u8 },
    LineEnding,
}

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
    type Item = Result<Token, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_buf {
            None => return None,
            Some(c) if c.is_ascii_digit() || c == b'-' => {
                let sign = if c == b'-' {
                    try_some!(self.advance()); // FIXME: we need a try_some! here
                    -1
                } else {
                    1
                };

                let buf = [0u8; 32];

                // start of digit, eat digits until we encounter a non-digit
                while let Some(d) = self.peek_buf {
                    if !d.is_ascii_digit() {
                        break;
                    }

                    return Some(Ok(Token::new(TokenKind::IntLiteral(-1))));
                }
                unimplemented!();
            }

            _ => unimplemented!(),
        }
    }
}

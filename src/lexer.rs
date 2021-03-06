use arrayvec::ArrayVec;
use std::io;
use std::str::{self, FromStr};

use err::LexError;

pub const CHECKSUM_LENGTH: usize = 2;
pub const HEADER_LENGTH: usize = 2;
pub const NUMBER_LENGTH: usize = 32;
pub const STRING_LENGTH: usize = 64;
/// Excluded chars can be found
/// [here](http://www.catb.org/gpsd/NMEA.html#_nmea_encoding_conventions)
pub const EXCLUDED_CHARS: [u8; 5] = [b'*', b'I', b'$', b'\n', b'\r'];

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Header([u8; HEADER_LENGTH]),
    StringLiteral(ArrayVec<[u8; STRING_LENGTH]>),
    CommaSeparator,
    FloatLiteral(ArrayVec<[u8; NUMBER_LENGTH]>),
    IntLiteral(isize),
    Checksum(u8),
    LineEnding,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Token { kind }
    }

    pub fn is_header(&self) -> bool {
        if let TokenKind::Header(_) = self.kind {
            return true;
        }
        false
    }
}

#[derive(Debug)]
pub struct Tokenizer<R> {
    input: io::Bytes<R>,
    peek_buf: Option<u8>,
    cur_checksum: u8,
}

impl<R: io::Read> Tokenizer<R> {
    pub fn new(input: R) -> Result<Self, io::Error> {
        let mut tk = Tokenizer {
            input: input.bytes(),
            peek_buf: None,
            cur_checksum: 0,
        };

        tk.advance()?;

        Ok(tk)
    }

    fn advance(&mut self) -> Result<Option<u8>, io::Error> {
        let prev = self.peek_buf;

        if let Some(c) = prev {
            if !EXCLUDED_CHARS.contains(&c) {
                self.cur_checksum ^= c;
            } else if c == b'$' {
                self.cur_checksum = 0;
            }
        }

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
    type Item = Result<Token, LexError>;

    #[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_buf {
            None => None,
            Some(c) if c == b'$' => {
                try_some!(self.advance());
                let mut header = [0u8; HEADER_LENGTH];
                for v in &mut header {
                    match try_some!(self.advance()) {
                        None => return Some(Err(LexError::UnexpectedEof("Header"))),
                        Some(c) => *v = c,
                    }
                }
                Some(Ok(Token::new(TokenKind::Header(header))))
            }
            Some(c) if c.is_ascii_alphabetic() => {
                let mut buf = ArrayVec::<[u8; STRING_LENGTH]>::new();
                while let Some(a) = self.peek_buf {
                    if !a.is_ascii_alphabetic() {
                        break;
                    }
                    try_err!(buf.try_push(a));
                    try_some!(self.advance());
                }
                Some(Ok(Token::new(TokenKind::StringLiteral(buf))))
            }
            Some(c) if c.is_ascii_digit() || c == b'-' => {
                let mut buf = ArrayVec::<[u8; NUMBER_LENGTH]>::new();
                if c == b'-' {
                    try_err!(buf.try_push(c));
                    try_some!(self.advance());
                    if let Some(d) = self.peek_buf {
                        if !d.is_ascii_digit() {
                            return Some(Err(LexError::IncompleteToken("Number")));
                        }
                    }
                }

                // start of digit, eat digits until we encounter a non-digit
                while let Some(d) = self.peek_buf {
                    if !d.is_ascii_digit() {
                        break;
                    }
                    try_err!(buf.try_push(d));
                    try_some!(self.advance());
                }

                if let Some(c) = self.peek_buf {
                    if c == b'.' {
                        // we got a decimal dot!
                        try_err!(buf.try_push(c));
                        try_some!(self.advance());

                        // read another integer literal
                        while let Some(d) = self.peek_buf {
                            if !d.is_ascii_digit() {
                                break;
                            }
                            try_err!(buf.try_push(d));
                            try_some!(self.advance());
                        }
                        return Some(Ok(Token::new(TokenKind::FloatLiteral(buf))));
                    }
                }

                // we got an integer
                let string = try_err!(str::from_utf8(&buf));
                let int = try_err!(isize::from_str(string));
                Some(Ok(Token::new(TokenKind::IntLiteral(int))))
            }
            Some(c) if c == b',' => {
                try_some!(self.advance());
                Some(Ok(Token::new(TokenKind::CommaSeparator)))
            }
            Some(c) if c == b'\r' || c == b'\n' => {
                while let Some(nl) = self.peek_buf {
                    if nl != b'\r' && nl != b'\n' {
                        break;
                    }
                    try_some!(self.advance());
                }
                self.cur_checksum = 0;
                Some(Ok(Token::new(TokenKind::LineEnding)))
            }
            Some(c) if c == b'*' => {
                let actual_sum = self.cur_checksum;
                let mut buf = ArrayVec::<[u8; CHECKSUM_LENGTH]>::new();

                try_some!(self.advance());

                for _ in 0..CHECKSUM_LENGTH {
                    match self.peek_buf {
                        None => return Some(Err(LexError::UnexpectedEof("Checksum"))),
                        Some(h) if h.is_ascii_hexdigit() => {
                            try_err!(buf.try_push(h));
                        }
                        Some(_) => return Some(Err(LexError::IncompleteToken("Checksum"))),
                    }
                    try_some!(self.advance());
                }

                let expected_sum = try_err!(u8::from_str_radix(try_err!(str::from_utf8(&buf)), 16));
                if expected_sum ^ actual_sum != 0 {
                    return Some(Err((expected_sum, actual_sum).into()));
                }
                Some(Ok(Token::new(TokenKind::Checksum(actual_sum))))
            }
            Some(c) => {
                try_some!(self.advance());
                Some(Err(LexError::InvalidCharacter(c)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn t_lexer(arg: &str) -> Tokenizer<Cursor<&str>> {
        Tokenizer::new(Cursor::new(arg)).unwrap()
    }

    fn str_array_vec(vec: Vec<u8>) -> ArrayVec<[u8; STRING_LENGTH]> {
        let mut av = ArrayVec::<[u8; STRING_LENGTH]>::new();
        for v in vec {
            av.push(v);
        }
        av
    }

    fn float_array_vec(vec: Vec<u8>) -> ArrayVec<[u8; NUMBER_LENGTH]> {
        let mut av = ArrayVec::<[u8; NUMBER_LENGTH]>::new();
        for v in vec {
            av.push(v);
        }
        av
    }

    #[test]
    fn check_empty() {
        let mut lexer = t_lexer("");
        assert!(lexer.next().is_none());
    }

    mod header {
        use super::{t_lexer, Token, TokenKind};

        #[test]
        fn header_ok() {
            let mut lexer = t_lexer("$GG");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Header([b'G', b'G'])
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn only_dollar() {
            let mut lexer = t_lexer("$");
            assert!(lexer.next().unwrap().is_err());
        }

        #[test]
        fn dollar_plus_char() {
            let mut lexer = t_lexer("$G");
            assert!(lexer.next().unwrap().is_err());
        }
    }

    mod string_lit {
        use super::{str_array_vec, t_lexer, Token, TokenKind};

        #[test]
        fn string_ok() {
            let mut lexer = t_lexer("arg");
            let expected = str_array_vec(vec![b'a', b'r', b'g']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::StringLiteral(ref s),
                }))
                if s == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer("arg.");
            let expected = str_array_vec(vec![b'a', b'r', b'g']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::StringLiteral(ref s),
                }))
                if s == &expected
            );
            assert!(lexer.next().is_some());
        }
    }

    mod int {
        use super::{t_lexer, Token, TokenKind};

        #[test]
        fn check_int() {
            let mut lexer = t_lexer("1234");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::IntLiteral(1234)
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn negative_int() {
            let mut lexer = t_lexer("-1234");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::IntLiteral(-1234)
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer("1234a");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::IntLiteral(1234)
                }))
            );
            assert!(lexer.next().is_some());
        }
    }

    mod float {
        use super::{float_array_vec, t_lexer, Token, TokenKind};

        #[test]
        fn positive_float() {
            let mut lexer = t_lexer("1.23");
            let expected = float_array_vec(vec![b'1', b'.', b'2', b'3']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::FloatLiteral(ref f),
                }))
                if f == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn negative_float() {
            let mut lexer = t_lexer("-1.23");
            let expected = float_array_vec(vec![b'-', b'1', b'.', b'2', b'3']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::FloatLiteral(ref f),
                }))
                if f == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn end_with_dot() {
            let mut lexer = t_lexer("-123.");
            let expected = float_array_vec(vec![b'-', b'1', b'2', b'3', b'.']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::FloatLiteral(ref f),
                }))
                if f == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer("1.23a");
            let expected = float_array_vec(vec![b'1', b'.', b'2', b'3']);
            let left = lexer.next();
            assert_matches!(
                left,
                Some(Ok(Token {
                    kind: TokenKind::FloatLiteral(_),
                }))
            );
            assert_eq!(
                left.unwrap().unwrap(),
                Token {
                    kind: TokenKind::FloatLiteral(expected)
                }
            );
            assert!(lexer.next().is_some());
        }

        #[test]
        fn negative_correct_stop_at_dot() {
            let mut lexer = t_lexer("-123.a");
            let expected = float_array_vec(vec![b'-', b'1', b'2', b'3', b'.']);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::FloatLiteral(ref f),
                }))
                if f == &expected
            );
            assert!(lexer.next().is_some());
        }
    }

    mod comma {
        use super::{t_lexer, Token, TokenKind};
        #[test]
        fn just_comma() {
            let mut lexer = t_lexer(",");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::CommaSeparator
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer(",a");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::CommaSeparator
                }))
            );
            assert!(lexer.next().is_some());
        }
    }

    mod new_line {
        use super::{t_lexer, Token, TokenKind};

        #[test]
        fn lf() {
            let mut lexer = t_lexer("\n");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::LineEnding
                }))
            );
            assert!(lexer.next().is_none());
        }
        #[test]
        fn cr() {
            let mut lexer = t_lexer("\r");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::LineEnding
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn crlf() {
            let mut lexer = t_lexer("\r\n");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::LineEnding
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn multiple_new_line() {
            let mut lexer = t_lexer("\n\r\n\n\r");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::LineEnding
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer("\r\na");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::LineEnding
                }))
            );
            assert!(lexer.next().is_some());
        }
    }

    mod checksum {
        use super::{str_array_vec, t_lexer, LexError, Token, TokenKind, EXCLUDED_CHARS};

        #[test]
        fn empty_message() {
            let mut lexer = t_lexer("*00");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Checksum(0)
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn only_one_hex() {
            let mut lexer = t_lexer("*0");
            assert_matches!(lexer.next(), Some(Err(LexError::UnexpectedEof(_))));
            assert!(lexer.next().is_none());
        }

        #[test]
        fn only_asterisk() {
            let mut lexer = t_lexer("*");
            assert_matches!(lexer.next(), Some(Err(LexError::UnexpectedEof(_))));
            assert!(lexer.next().is_none());
        }

        #[test]
        fn not_a_hex() {
            let mut lexer = t_lexer("*Z5");
            let expected = str_array_vec(vec![b'Z']);
            assert_matches!(lexer.next(), Some(Err(LexError::IncompleteToken(_))));
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::StringLiteral(ref s),
                }))
                if s == &expected
            );
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::IntLiteral(5)
                }))
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn stops_correctly() {
            let mut lexer = t_lexer("*00\n");
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Checksum(0)
                }))
            );
            assert!(lexer.next().is_some());
        }

        #[test]
        fn with_message() {
            let expected: u8 = b't' ^ b'e' ^ b's' ^ b't';
            let mut lexer = t_lexer("test*16").skip(1);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Checksum(ref ch),
                }))
                if ch == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn reset_at_dollar() {
            let expected: u8 = b't' ^ b'e' ^ b's' ^ b't';
            let mut lexer = t_lexer("ab$test*16").skip(3);
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Checksum(ref ch),
                }))
                if ch == &expected
            );
            assert!(lexer.next().is_none());
        }

        #[test]
        fn exclude_chars() {
            let ex_chars = EXCLUDED_CHARS
                .iter()
                .map(|&b| b as char)
                .collect::<String>();
            let input = format!("{}*00", ex_chars);
            let mut lexer = t_lexer(&input).skip(3);
            // This test depends on the ordering of EXCLUDED_CHARS
            assert_matches!(
                lexer.next(),
                Some(Ok(Token {
                    kind: TokenKind::Checksum(0)
                }))
            );
            assert!(lexer.next().is_none());
        }
    }

    mod nmea {
        use super::{float_array_vec, str_array_vec, t_lexer, LexError, Token, TokenKind};

        #[test]
        fn correct_wo_location() {
            let mut lexer = t_lexer("$GPGGA,142013.087,,,,,0,0,,,M,,M,,*42");
            let header = Token::new(TokenKind::Header([b'G', b'P']));
            let string_lit = str_array_vec(vec![b'G', b'G', b'A']);
            let sentence_type = Token::new(TokenKind::StringLiteral(string_lit));
            let comma = Token::new(TokenKind::CommaSeparator);
            let float = float_array_vec(vec![
                b'1', b'4', b'2', b'0', b'1', b'3', b'.', b'0', b'8', b'7',
            ]);
            let float_lit = Token::new(TokenKind::FloatLiteral(float));
            let int_lit = Token::new(TokenKind::IntLiteral(0));
            let m_lit = Token::new(TokenKind::StringLiteral(str_array_vec(vec![b'M'])));
            let checksum = Token::new(TokenKind::Checksum(66));
            assert_matches!(lexer.next(), Some(Ok(ref h)) if h == &header);
            assert_matches!(lexer.next(), Some(Ok(ref st)) if st == &sentence_type);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref f)) if f == &float_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref i)) if i == &int_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref i)) if i == &int_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref m)) if m == &m_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref m)) if m == &m_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &checksum);
        }

        #[test]
        fn incorrect_wo_location() {
            let mut lexer = t_lexer("$GPGGA,142018.087,,,,,0,0,,,,M,,M,,*43");
            let header = Token::new(TokenKind::Header([b'G', b'P']));
            let string_lit = str_array_vec(vec![b'G', b'G', b'A']);
            let sentence_type = Token::new(TokenKind::StringLiteral(string_lit));
            let comma = Token::new(TokenKind::CommaSeparator);
            let float = float_array_vec(vec![
                b'1', b'4', b'2', b'0', b'1', b'8', b'.', b'0', b'8', b'7',
            ]);
            let float_lit = Token::new(TokenKind::FloatLiteral(float));
            let int_lit = Token::new(TokenKind::IntLiteral(0));
            let m_lit = Token::new(TokenKind::StringLiteral(str_array_vec(vec![b'M'])));
            assert_matches!(lexer.next(), Some(Ok(ref h)) if h == &header);
            assert_matches!(lexer.next(), Some(Ok(ref st)) if st == &sentence_type);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref f)) if f == &float_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref i)) if i == &int_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref i)) if i == &int_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref m)) if m == &m_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref m)) if m == &m_lit);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Ok(ref c)) if c == &comma);
            assert_matches!(lexer.next(), Some(Err(LexError::InvalidChecksum(67, 101))));
        }
    }
}

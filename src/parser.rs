use arrayvec::ArrayVec;
use std::{io, iter};

use lexer::{self, Error as LexError, Token, TokenKind};

quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        Lexer(err: LexError) {
            from()
            description(err.description())
            display("Could not parse due to an error in the lexer: {}", err)
        }
        UnexpectedToken {
            description("Unexpected token")
            display("Encountered unexpected token")
        }
        UnexpectedSentenceType {
            description("Sentence type has wrong format")
            display("Encountered unexpected sentence type")
        }
        Incomplete {
            description("Incomplete sentence")
            display("Could not complete the sentence due to EOF")
        }
    }
}

#[derive(Debug)]
pub enum CardinalDir {
    North,
    East,
    South,
    West,
}

pub enum GpsQualityIndicator {

}

#[derive(Debug)]
pub struct GgaSentence {
    talker_id: [u8; lexer::HEADER_LENGTH],
    /// Universal Time Coordinated (UTC)
    utc: f64,
    /// Latitude
    lat: Option<f64>,
    /// North or South
    lat_dir: Option<CardinalDir>,
    /// Longitude
    long: Option<f64>,
    /// East or West
    long_dir: Option<CardinalDir>,
}

#[derive(Debug)]
pub struct GgaParser<R: io::Read> {
    lexer: iter::Peekable<lexer::Tokenizer<R>>,
}

impl<R: io::Read> GgaParser<R> {
    pub fn new(input: R) -> Result<Self, io::Error> {
        Ok(GgaParser {
            lexer: lexer::Tokenizer::new(input)?.peekable(),
        })
    }

    pub fn read_sentence(&mut self) -> Result<Option<GgaSentence>, ParseError> {
        let talker_id = expect!(self, Header, h)?;
        let sen_type = expect!(self, StringLiteral, s)?;
        match sen_type.as_slice() {
            b"GGA" => {
                expect!(self, CommaSeparator)?;
                self.parse_gga(talker_id)
            }
            _ => Err(ParseError::UnexpectedSentenceType),
        }
    }

    fn parse_gga(
        &mut self,
        talker_id: [u8; lexer::HEADER_LENGTH],
    ) -> Result<Option<GgaSentence>, ParseError> {
        // TODO: Clarify if commas should be ignored
        let utc = expect!(self, FloatLiteral, f)?;
        expect!(self, CommaSeparator)?;
        let lat = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let lat_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"N" => Some(CardinalDir::North),
            Some(ref s) if s.as_slice() == b"S" => Some(CardinalDir::South),
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let long = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let long_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"E" => Some(CardinalDir::East),
            Some(ref s) if s.as_slice() == b"W" => Some(CardinalDir::West),
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        Ok(None)
    }

    // pub fn read_sentence() {
    //     // every sentence must start with a header

    //     if let  Token{ kind: TokenKind::Header(talker_id) }= self.lexer.next().ok_or(Err::UnexpectedEOF)?? {
    //         // got a valid header!

    //         // parse the correct structure
    //         if let Token { kind: TokenKind::StringLiteral(s) } = self.lexer.next() {
    //             match s.as_slice() {
    //                 b"GGA" => parse_gga();
    //                 _ => unimplemented!() // err: expected known sentence type
    //             }
    //         } else {
    //             unimplemented!() // err: expected String
    //         }
    //     } else {
    //         unimplemented!(); // err: expected header
    //     }
    // }

    // pub fn read_gga() {
    //     if let Token { kind: TokenKind::FloatLiteral(fv) } = self.lexer.next() {

    //     }
    // }
}

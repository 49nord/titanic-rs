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
    }
}

pub enum CardinalDir {
    North,
    East,
    South,
    West,
}

pub enum GpsQualityIndicator {

}

pub struct GgaSentence {
    header: [u8; lexer::HEADER_LENGTH],
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
        let a = expect!(self, TokenKind::Header(_h), { _h });
        println!("{:?}", a);
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

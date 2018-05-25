use arrayvec::{ArrayVec, CapacityError};
use chrono;
use std::{io, num, str};

use lexer::STRING_LENGTH;

quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        Lexer(err: LexError) {
            from()
            description(err.description())
            display("Could not parse due to an error in the lexer: {}", err)
            cause(err)
        }
        UnexpectedToken {
            description("Unexpected token")
            display("Encountered unexpected token")
        }
        UnexpectedSentenceType {
            description("Unexpected sentence type")
            display("Encountered unexpected sentence type")
        }
        Incomplete {
            description("Incomplete sentence")
            display("Could not complete the sentence due to EOF")
        }
        Time(err: chrono::format::ParseError) {
            from()
            description("Time parsing error")
            display("Failed to parse FloatLiteral as time: {}", err)
            cause(err)
        }
        SatInView(count: i64) {
            description("Unexpected number of satellites in view")
            display("Unexpected number of satellites in view: {}", count)
        }
        UnexpectedDir(dir: ArrayVec<[u8; STRING_LENGTH]>) {
            description("Unexpected direction")
            display("Could not parse {:?} as direction", dir)
        }
        Utf8(err: str::Utf8Error) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        InvalidInput(msg: &'static str) {
            description("Invalid input")
            display("Invalid input, {}", msg)
        }
        Int(err: num::ParseIntError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        Float(err: num::ParseFloatError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        InvalidCoord(val: f64, max: f64) {
            description("Invalid coordinate")
            display("Invalid coordinate: {} should be between {:.0} and {:.0}", val, max*-1.0, max)
        }
    }
}

quick_error!{
    #[derive(Debug)]
    pub enum LexError {
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
        NotEvenUtf8(err: str::Utf8Error) {
            from()
            description(err.description())
            display("Expected ascii but did not even get valid utf8: {}", err)
            cause(err)
        }
    }
}

use arrayvec::CapacityError;
use chrono;
use std::{num, io};

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
            description("Sentence type has wrong format")
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
        Longitude {
            description("Longitude parsing error")
            display("Failed to parse a FloatLiteral as longitude")
        }
        Latitude {
            description("Latitude parsing error")
            display("Failed to parse a FloatLiteral as latitude")
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
    }
}
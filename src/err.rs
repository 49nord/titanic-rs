//! All errors that can happen while parsing can be found here.

use arrayvec::{ArrayVec, CapacityError};
use chrono;
use std::{io, num, str};

use lexer::STRING_LENGTH;

quick_error! {
    /// Errors that can happen while parsing.
    #[derive(Debug)]
    pub enum ParseError {
        /// Encountered an error while lexing.
        Lexer(err: LexError) {
            from()
            description(err.description())
            display("Could not parse due to an error in the lexer: {}", err)
            cause(err)
        }
        /// Found an unexpected token.
        UnexpectedToken {
            description("Unexpected token")
            display("Encountered unexpected token")
        }
        /// Found an unexptected sentence type.
        UnexpectedSentenceType {
            description("Unexpected sentence type")
            display("Encountered unexpected sentence type")
        }
        /// Failed to parse a `FloatLiteral` to a `NaiveTime`.
        Time(err: chrono::format::ParseError) {
            from()
            description("Time parsing error")
            display("Failed to parse FloatLiteral as time: {}", err)
            cause(err)
        }
        /// Failed to parse a `StringLiteral` to a `CardDir`.
        UnexpectedDir(dir: ArrayVec<[u8; STRING_LENGTH]>) {
            description("Unexpected direction")
            display("Could not parse {:?} as direction", dir)
        }
        /// Failed to convert to `str`.
        /// This should not happen if the lexer only returns ascii bytes.
        Utf8(err: str::Utf8Error) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        /// Found an invalid value.
        /// E.g. a station id not between 0 and 1023, a float with the wrong
        /// format for a coordinate, ...
        InvalidValue(msg: &'static str) {
            description("Invalid value")
            display("Invalid value, {}", msg)
        }
        /// Failed to parse an integer.
        Int(err: num::ParseIntError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        /// Failed to parse a float.
        Float(err: num::ParseFloatError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        /// Found an invalid coordinate, e.g. latitude outside of the range -90° to +90°.
        InvalidCoord(val: f64, max: f64) {
            description("Invalid coordinate")
            display("Invalid coordinate: {} should be between {:.0} and {:.0}", val, max*-1.0, max)
        }
        /// Found an unexpected unit.
        InvalidUnit {
            description("Unexpected unit")
            display("Found an unexpected unit")
        }
    }
}

quick_error!{
    /// Errors that can happen while lexing the raw input.
    #[derive(Debug)]
    pub enum LexError {
        /// An invalid character.
        InvalidCharacter(c: u8) {
            from()
            description("Invalid character")
            display("Encountered invalid character \"{}\"", *c as char)
        }
        /// The checksum is invalid.
        /// The two values are the expected and the actual checksum.
        /// `InvalidChecksum(expected: u8, actual: u8)`
        InvalidChecksum(expected: u8, actual: u8) {
            description("Invalid checksum")
            display("Expected checksum \"{:X}\" , found checksum \"{}\"", expected, actual)
        }
        /// I/O Error
        Io(err: io::Error) {
            from()
            description(err.description())
            display("Encountered I/O error while lexing: {}", err)
            cause(err)
        }
        /// The EOF was found in an unexpected location.
        /// This can happen in the header and the checksum, when there aren't
        /// enough bytes after the starting sign.
        UnexpectedEof(token: &'static str) {
            from()
            description("Unexpected EOF")
            display("Encountered unexpected EOF in {}", token)
        }
        /// A token could not be completed.
        /// Some reasons for that are a `'-'` without a following digit and
        /// a non hex-digit in the two chars following `'*'`.
        IncompleteToken(token: &'static str) {
            description("Incomplete token")
            display("Could not complete token of type {}", token)
        }
        /// An array overflowed while trying to push values into it.
        ArrayOverflow(err: CapacityError<u8>, capacity: usize) {
            description(err.description())
            display("Tried to push more than {} characters into the buffer: {}", capacity, err)
            cause(err)
        }
        /// Failed to parse an integer.
        Int(err: num::ParseIntError) {
            from()
            description(err.description())
            display("{}", err)
            cause(err)
        }
        /// Failed to parse some bytes to a `str` because they are not even utf8
        /// even though only ascii is expected.
        NotEvenUtf8(err: str::Utf8Error) {
            from()
            description(err.description())
            display("Expected ascii but did not even get valid utf8: {}", err)
            cause(err)
        }
    }
}

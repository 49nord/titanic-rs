use arrayvec::ArrayVec;
use chrono::{self, NaiveTime};
use std::str::{self, FromStr};
use std::{io, iter};

use err::{CoordinateParseError, ParseError};
use lexer::{self, Token, TokenKind};

#[derive(Debug)]
pub enum LatDir {
    North,
    South,
}

#[derive(Debug)]
pub enum LongDir {
    East = 1,
    West = -1,
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
    /// Longitude in
    long: Option<f64>,
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
        let utc = fl_to_utc(&expect!(self, FloatLiteral, f)?)?;
        println!("utc: {}", utc);
        expect!(self, CommaSeparator)?;
        let lat = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let lat_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"N" => Some(LatDir::North),
            Some(ref s) if s.as_slice() == b"S" => Some(LatDir::South),
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => None,
        };
        let lat = match (lat, lat_dir) {
            (Some(l), Some(d)) => Some(fl_to_lat(&l, &d)?),
            (_, _) => None,
        };
        expect!(self, CommaSeparator)?;
        let long = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let long_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"E" => Some(LongDir::East),
            Some(ref s) if s.as_slice() == b"W" => Some(LongDir::West),
            Some(_) => return Err(ParseError::UnexpectedToken),
            None => None,
        };
        // let long = match (long, long_dir) {
        //     (Some(l), Some(d)) => Some(fl_to_long(l, d)?),
        //     (_, _) => None,
        // };
        expect!(self, CommaSeparator)?;
        Ok(None)
    }
}

/// Converts the data of a `TokenKind::FloatLiteral` to a time.
/// The input has to be in the format `hhmmss.sss`.
#[inline]
fn fl_to_utc(utc: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>) -> Result<NaiveTime, chrono::ParseError> {
    // unwrap can be used since we know that utc is only valid ascii
    NaiveTime::parse_from_str(str::from_utf8(&utc).unwrap(), "%H%M%S%.f")
}

fn fl_to_lat(
    lat: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>,
    dir: &LatDir,
) -> Result<f64, CoordinateParseError> {
    if !lat.is_ascii() {
        return Err(CoordinateParseError::InvalidInput("found non ascii bytes"));
    }
    // This check is needed to ensure we don't panic
    const DEG_SPLIT: usize = 2;
    if DEG_SPLIT > lat.len() {
        return Err(CoordinateParseError::InvalidInput("input is too short"));
    }
    let (deg, dec_min) = lat.split_at(DEG_SPLIT);
    // unwrap can be used since we know that lat is only valid ascii
    let degrees = f64::from(i8::from_str(str::from_utf8(deg).unwrap())?);
    let decimal_min = f64::from_str(str::from_utf8(dec_min).unwrap())? * f64::from(10 / 6);
    let dec_deg = degrees + decimal_min;
    if dec_deg.abs() > 90.0 {
        return Err(CoordinateParseError::InvalidLat(dec_deg));
    }
    match dir {
        LatDir::North => Ok(degrees + decimal_min),
        LatDir::South => Ok((degrees + decimal_min) * -1.0),
    }
}

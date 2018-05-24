use arrayvec::ArrayVec;
use chrono::{self, NaiveTime};
use std::str::{self, FromStr};
use std::{io, iter};

use err::{CoordinateParseError, ParseError};
use lexer::{self, Token, TokenKind};

#[derive(Debug)]
pub enum CardDir {
    North,
    South,
    East,
    West,
}

impl CardDir {
    #[inline]
    fn get_sign(&self) -> isize {
        match self {
            CardDir::North | CardDir::East => 1,
            CardDir::South | CardDir::West => -1,
        }
    }
}

#[derive(Debug)]
pub enum GpsQualityInd {
    FixNotAvailable,
    GpsFix,
    DifferentialGpsFix,
    PpsFix,
    RealTimeKinematic,
    FloatRtk,
    Estimated,
    ManualInputMode,
    SimulationMode,
}

impl GpsQualityInd {
    /// Takes an integer in the range `0..=8` and returns the corresponding
    /// `GpsQualityInd`.
    #[inline]
    fn try_from_int(int: i64) -> Result<Self, ParseError> {
        match int {
            0 => Ok(GpsQualityInd::FixNotAvailable),
            1 => Ok(GpsQualityInd::GpsFix),
            2 => Ok(GpsQualityInd::DifferentialGpsFix),
            3 => Ok(GpsQualityInd::PpsFix),
            4 => Ok(GpsQualityInd::RealTimeKinematic),
            5 => Ok(GpsQualityInd::FloatRtk),
            6 => Ok(GpsQualityInd::Estimated),
            7 => Ok(GpsQualityInd::ManualInputMode),
            8 => Ok(GpsQualityInd::SimulationMode),
            _ => Err(ParseError::UnexpectedToken),
        }
    }
}

#[derive(Debug)]
pub struct GgaSentence {
    talker_id: [u8; lexer::HEADER_LENGTH],
    /// Universal Time Coordinated (UTC)
    utc: NaiveTime,
    /// Latitude in decimal degrees.
    /// A positive value indicates that the coordinate is in the northern hemisphere.
    /// A negative value indicates that the coordinate is in the southern hemisphere.
    lat: Option<f64>,
    /// Longitude in decimal degrees.
    /// A positive value indicates that the coordinate is in the eastern hemisphere.
    /// A negative value indicates that the coordinate is in the western hemisphere.
    long: Option<f64>,
    /// Indicates the quality of the gps data.
    gps_qlty: GpsQualityInd,
    /// Number of satellites in view.
    sat_view: u64,
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

        // Parse utc
        let utc = fl_to_utc(&expect!(self, FloatLiteral, f)?)?;
        expect!(self, CommaSeparator)?;

        // Parse latitude
        let lat = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let lat_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"N" => Some(CardDir::North),
            Some(ref s) if s.as_slice() == b"S" => Some(CardDir::South),
            Some(s) => return Err(ParseError::UnexpectedDir(s)),
            None => None,
        };
        let lat = match (lat, lat_dir) {
            (Some(l), Some(d)) => Some(fl_to_lat(&l, &d)?),
            (_, _) => None,
        };
        expect!(self, CommaSeparator)?;

        // Parse longitude
        let long = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(f),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        let long_dir = match accept!(self, StringLiteral, s)? {
            Some(ref s) if s.as_slice() == b"E" => Some(CardDir::East),
            Some(ref s) if s.as_slice() == b"W" => Some(CardDir::West),
            Some(s) => return Err(ParseError::UnexpectedDir(s)),
            None => None,
        };
        let long = match (long, long_dir) {
            (Some(l), Some(d)) => Some(fl_to_long(&l, &d)?),
            (_, _) => None,
        };
        expect!(self, CommaSeparator)?;

        // Parse quality indicator
        let gps_qlty = GpsQualityInd::try_from_int(expect!(self, IntLiteral, i)?)?;
        expect!(self, CommaSeparator)?;

        // Parse satellites in view
        let sat_view = match expect!(self, IntLiteral, i)? {
            i if i < 0 => return Err(ParseError::SatInView(i)),
            i => i as u64,
        };
        expect!(self, CommaSeparator)?;

        Ok(Some(GgaSentence {
            talker_id,
            utc,
            lat,
            long,
            gps_qlty,
            sat_view,
        }))
    }
}

/// Converts the data of a `TokenKind::FloatLiteral` to a time.
/// The input has to be in the format `hhmmss.sss`.
#[inline]
fn fl_to_utc(utc: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>) -> Result<NaiveTime, chrono::ParseError> {
    // unwrap can be used since we know that utc is only valid ascii
    NaiveTime::parse_from_str(str::from_utf8(&utc).unwrap(), "%H%M%S%.f")
}

#[inline]
fn fl_to_lat(
    lat: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>,
    dir: &CardDir,
) -> Result<f64, CoordinateParseError> {
    const DEG_SPLIT: usize = 2;
    const MAX_ABS_LAT: f64 = 90.0;
    parse_coord(lat, dir, DEG_SPLIT, MAX_ABS_LAT)
}

#[inline]
fn fl_to_long(
    long: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>,
    dir: &CardDir,
) -> Result<f64, CoordinateParseError> {
    const DEG_SPLIT: usize = 3;
    const MAX_ABS_LONG: f64 = 180.0;
    parse_coord(long, dir, DEG_SPLIT, MAX_ABS_LONG)
}

fn parse_coord(
    coord: &ArrayVec<[u8; lexer::NUMBER_LENGTH]>,
    dir: &CardDir,
    deg_split: usize,
    abs_max: f64,
) -> Result<f64, CoordinateParseError> {
    // These checks is needed to ensure we don't panic
    if !coord.is_ascii() {
        return Err(CoordinateParseError::InvalidInput("found non ascii bytes"));
    }
    if deg_split > coord.len() {
        return Err(CoordinateParseError::InvalidInput("input is too short"));
    }

    let (deg, dec_min) = coord.split_at(deg_split);
    // unwrap can be used since we know that lat is only valid ascii
    let degrees = f64::from(i8::from_str(str::from_utf8(deg).unwrap())?);
    let decimal_min = f64::from_str(str::from_utf8(dec_min).unwrap())? * 10.0 / 6.0 / 100.0;
    let dec_deg = degrees + decimal_min;
    if dec_deg.abs() > abs_max {
        return Err(CoordinateParseError::InvalidCoord(dec_deg, abs_max));
    }
    Ok(dec_deg * dir.get_sign() as f64)
}

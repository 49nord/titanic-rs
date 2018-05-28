//! This module provides a parser for the *GGA* sentence of the*NMEA 0183*
//! protocol.

use chrono::NaiveTime;
use std::str::{self, FromStr};
use std::{io, iter};

use err::{LexError, ParseError};
use lexer::{self, Token, TokenKind};

const LAT_SPLIT: usize = 2;
const ABS_MAX_LAT: f64 = 90.0;
const LONG_SPLIT: usize = 3;
const ABS_MAX_LONG: f64 = 180.0;

/// The cardinal directions.
#[derive(Debug)]
enum CardDir {
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

/// Indicator of the quality of gps data.
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
    /// Else `ParseError::UnexpectedToken` is returned.
    #[inline]
    fn try_from_i64(int: i64) -> Result<Self, ParseError> {
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

/// This represents a correct GGA sentence and can be created by a
/// [GgaParser](../parser/struct.GgaParser.html)
#[derive(Debug)]
pub struct GgaSentence {
    /// Talker id contained in the header of every sentence.
    pub talker_id: [u8; lexer::HEADER_LENGTH],
    /// Universal Time Coordinated (UTC)
    pub utc: NaiveTime,
    /// Latitude in decimal degrees.
    /// A positive value indicates that the coordinate is in the northern hemisphere.
    /// A negative value indicates that the coordinate is in the southern hemisphere.
    pub lat: Option<f64>,
    /// Longitude in decimal degrees.
    /// A positive value indicates that the coordinate is in the eastern hemisphere.
    /// A negative value indicates that the coordinate is in the western hemisphere.
    pub long: Option<f64>,
    /// Indicates the quality of the gps data.
    pub gps_qlty: GpsQualityInd,
    /// Number of satellites in view.
    pub sat_view: u64,
    /// Horizontal dilution of precision (meters)
    pub hdop: Option<f64>,
    /// Antenna Altitude above/below mean-sea-level (geoid) (in meters)
    pub altitude: Option<f64>,
    /// Geoidal separation, the difference between the WGS-84 earth ellipsoid
    /// and mean-sea-level (geoid), "-" means mean-sea-level below ellipsoid
    pub geo_sep: Option<f64>,
    /// Age of differential GPS data, time in seconds since last SC104 type
    /// 1 or 9 update, null field when DGPS is not used
    pub age: Option<f64>,
    /// Differential reference station ID, 0000-1023
    pub station_id: Option<u32>,
}

/// The parser for the `NMEA 0183` protocol that parses only GGA sentences.
#[derive(Debug)]
pub struct GgaParser<R: io::Read> {
    lexer: iter::Peekable<lexer::Tokenizer<R>>,
}

impl<R: io::Read> GgaParser<R> {
    /// Create a new parser that parses `input`.
    pub fn new(input: R) -> Result<Self, io::Error> {
        Ok(GgaParser {
            lexer: lexer::Tokenizer::new(input)?.peekable(),
        })
    }

    /// Read a sentence.
    /// The first char has to `'$'`. Returns `ParseError::UnexpectedSentenceType`
    /// if the sentence type is not GGA.
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

    /// Parse a GGA sentence.
    /// The comma before utc has to have already been consumed.
    fn parse_gga(
        &mut self,
        talker_id: [u8; lexer::HEADER_LENGTH],
    ) -> Result<Option<GgaSentence>, ParseError> {
        // TODO: Clarify if commas should be ignored

        // Parse utc
        let utc = Self::fl_to_utc(&expect!(self, FloatLiteral, f)?)?;
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
            Some(s) => return Err(ParseError::InvalidDir(s)),
            None => None,
        };
        let lat = match (lat, lat_dir) {
            (Some(lat), Some(d)) => Some(Self::parse_coord(&lat, &d, LAT_SPLIT, ABS_MAX_LAT)?),
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
            Some(s) => return Err(ParseError::InvalidDir(s)),
            None => None,
        };
        let long = match (long, long_dir) {
            (Some(long), Some(d)) => Some(Self::parse_coord(&long, &d, LONG_SPLIT, ABS_MAX_LONG)?),
            (_, _) => None,
        };
        expect!(self, CommaSeparator)?;

        // Parse quality indicator
        let gps_qlty = GpsQualityInd::try_from_i64(expect!(self, IntLiteral, i)?)?;
        expect!(self, CommaSeparator)?;

        // Parse satellites in view
        let sat_view = match expect!(self, IntLiteral, i)? {
            i if i < 0 => {
                return Err(ParseError::InvalidValue(
                    "number of satellites in view has to be >=0",
                ))
            }
            i => i as u64,
        };
        expect!(self, CommaSeparator)?;

        // Parse horizontal dilution of precision
        let hdop = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(fl_as_f64(f.as_slice())?),
            None => None,
        };
        expect!(self, CommaSeparator)?;

        // Parse antenna altitude and check if the unit is meter
        let altitude = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(fl_as_f64(f.as_slice())?),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        self.expect_meters()?;
        expect!(self, CommaSeparator)?;

        // Parse geoidal separation and check if the unit is meter
        let geo_sep = match accept!(self, FloatLiteral, f)? {
            Some(f) => Some(fl_as_f64(f.as_slice())?),
            None => None,
        };
        expect!(self, CommaSeparator)?;
        self.expect_meters()?;
        expect!(self, CommaSeparator)?;

        // Parse age of differential GPS data seconds
        let age = match accept!(self, FloatLiteral, f)? {
            Some(f) => {
                let f = fl_as_f64(f.as_slice())?;
                if f < 0.0 {
                    return Err(ParseError::InvalidValue(
                        "age of the data cannot be negative",
                    ));
                }
                Some(f)
            }
            None => None,
        };
        expect!(self, CommaSeparator)?;

        // Parse differential reference station id
        let station_id = match accept!(self, IntLiteral, i)? {
            // casting is possible since we know i is in range 0..=1023
            Some(i) if 0 <= i && i <= 1023 => Some(i as u32),
            Some(_) => {
                return Err(ParseError::InvalidValue(
                    "station_id must be between 0 and 1023",
                ))
            }
            None => None,
        };

        // Parse Checksum
        expect!(self, Checksum, c)?;

        Ok(Some(GgaSentence {
            talker_id,
            utc,
            lat,
            long,
            gps_qlty,
            sat_view,
            hdop,
            altitude,
            geo_sep,
            age,
            station_id,
        }))
    }

    /// Skips and consumes all tokens till the next `TokenKind::Header` or EOF.
    /// Nothing will happen if the next token already is a header.
    /// Returns `None` if EOF has been reached.
    pub fn jump_to_header(&mut self) -> Option<Result<(), io::Error>> {
        loop {
            match self.lexer.peek() {
                Some(Ok(v)) if v.is_header() => return Some(Ok(())),
                Some(_) => (),
                None => return None,
            }
            if let Some(Err(LexError::Io(e))) = self.lexer.next() {
                return Some(Err(e));
            }
        }
    }

    /// Parse `coord` as a f64 representing a coordinate.
    /// `dir` will be converted to 1 or -1 to be multiplied with the degrees.
    /// `deg_split` is the number of numbers that make up the degrees.
    /// `abs_max` is maximum value in degree, e.g. 180 for longitude.
    fn parse_coord(
        coord: &[u8],
        dir: &CardDir,
        deg_split: usize,
        abs_max: f64,
    ) -> Result<f64, ParseError> {
        // This check is needed to ensure we don't panic
        if deg_split > coord.len() {
            return Err(ParseError::InvalidValue(
                "the float is too short for a coordinate",
            ));
        }

        let (deg, dec_min) = coord.split_at(deg_split);
        let degrees = f64::from(i8::from_str(str::from_utf8(deg)?)?);
        let decimal_min = fl_as_f64(dec_min)? * 10.0 / 6.0 / 100.0;
        let dec_deg = degrees + decimal_min;
        if dec_deg.abs() > abs_max {
            return Err(ParseError::InvalidCoord(dec_deg, abs_max));
        }
        Ok(dec_deg * dir.get_sign() as f64)
    }

    /// Converts the data of a `TokenKind::FloatLiteral` to a time.
    /// The input has to be in the format `hhmmss.sss`.
    #[inline]
    fn fl_to_utc(utc: &[u8]) -> Result<NaiveTime, ParseError> {
        Ok(NaiveTime::parse_from_str(
            str::from_utf8(&utc)?,
            "%H%M%S%.f",
        )?)
    }

    /// Check if the next token is a `StringLiteral` with the value b'M'.
    /// Else return an error.
    #[inline]
    fn expect_meters(&mut self) -> Result<(), ParseError> {
        if expect!(self, StringLiteral, s)?.as_slice() != b"M" {
            return Err(ParseError::InvalidUnit);
        }
        Ok(())
    }
}

#[inline]
fn fl_as_f64(fl: &[u8]) -> Result<f64, ParseError> {
    Ok(f64::from_str(str::from_utf8(fl)?)?)
}

//! This module provides a parser for the *GGA* sentence of the*NMEA 0183*
//! protocol.

use arrayvec::ArrayVec;
use chrono::NaiveTime;
use std::str::{self, FromStr};
use std::{io, iter};

use err::{LexError, ParseError};
use lexer;

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
    /// The first char has to be `'$'`. Returns `ParseError::UnexpectedSentenceType`
    /// if the sentence type is not GGA.
    pub fn read_sentence(&mut self) -> Result<Option<GgaSentence>, ParseError> {
        let talker_id = self.expect_header()?;
        let sen_type = self.expect_sen_type()?;
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
        // Parse utc
        let utc = self.expect_utc()?;
        expect!(self, CommaSeparator)?;

        // Parse latitude
        let lat = self.accept_lat()?;
        expect!(self, CommaSeparator)?;

        // Parse longitude
        let long = self.accept_long()?;
        expect!(self, CommaSeparator)?;

        // Parse quality indicator
        let gps_qlty = self.expect_qual_ind()?;
        expect!(self, CommaSeparator)?;

        // Parse satellites in view
        let sat_view = self.expect_sat_in_view()?;
        expect!(self, CommaSeparator)?;

        // Parse horizontal dilution of precision
        let hdop = self.accept_hdop()?;
        expect!(self, CommaSeparator)?;

        // Parse antenna altitude
        let altitude = self.accept_altitude()?;
        expect!(self, CommaSeparator)?;
        self.expect_meters()?;
        expect!(self, CommaSeparator)?;

        // Parse geoidal separation
        let geo_sep = self.accept_geo_sep()?;
        expect!(self, CommaSeparator)?;
        self.expect_meters()?;
        expect!(self, CommaSeparator)?;

        // Parse age of differential GPS data in seconds
        let age = self.accept_age()?;
        expect!(self, CommaSeparator)?;

        // Parse differential reference station id
        let station_id = self.accept_station_id()?;

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

    /// Expect the next token to be a header.
    /// The expected format is `$xx`.
    #[inline]
    fn expect_header(&mut self) -> Result<[u8; 2], ParseError> {
        expect!(self, Header, h)
    }

    /// Expect the next token to represent the sentence type.
    #[inline]
    fn expect_sen_type(&mut self) -> Result<ArrayVec<[u8; 64]>, ParseError> {
        expect!(self, StringLiteral, s)
    }

    /// Expect the next token to represent utc.
    /// The expected format is `hhmmss.sss`.
    #[inline]
    fn expect_utc(&mut self) -> Result<NaiveTime, ParseError> {
        Ok(NaiveTime::parse_from_str(
            str::from_utf8(&expect!(self, FloatLiteral, f)?)?,
            "%H%M%S%.f",
        )?)
    }

    /// Accept a latitude, set it to `None` if the number or the direction is empty.
    /// The accepted format is a float with four digits before the comma,
    /// followed by a comma and a `StringLiteral` containing 'N' or 'S'.
    fn accept_lat(&mut self) -> Result<Option<f64>, ParseError> {
        let raw_lat = match accept!(self, FloatLiteral, f)? {
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
        match (raw_lat, lat_dir) {
            (Some(lat), Some(d)) => Ok(Some(Self::parse_coord(&lat, &d, LAT_SPLIT, ABS_MAX_LAT)?)),
            (_, _) => Ok(None),
        }
    }

    /// Accept a longitude, set it to `None` if the number or the direction is empty.
    /// The accepted format is a float with five digits before the comma,
    /// followed by a comma and a `StringLiteral` containing 'N' or 'S'.
    fn accept_long(&mut self) -> Result<Option<f64>, ParseError> {
        let raw_long = match accept!(self, FloatLiteral, f)? {
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
        match (raw_long, long_dir) {
            (Some(long), Some(d)) => Ok(Some(Self::parse_coord(
                &long,
                &d,
                LONG_SPLIT,
                ABS_MAX_LONG,
            )?)),
            (_, _) => Ok(None),
        }
    }

    /// Expect the next token to represent a `GpsQualityInd`.
    #[inline]
    fn expect_qual_ind(&mut self) -> Result<GpsQualityInd, ParseError> {
        GpsQualityInd::try_from_i64(expect!(self, IntLiteral, i)?)
    }

    /// Expect the next token to represent the number of sattelites in view
    /// Returns `Err(ParseError::InvalidValue(_))` if the number is < 0.
    #[inline]
    fn expect_sat_in_view(&mut self) -> Result<u64, ParseError> {
        match expect!(self, IntLiteral, i)? {
            i if i < 0 => Err(ParseError::InvalidValue(
                "number of satellites in view has to be >=0",
            )),
            i => Ok(i as u64),
        }
    }

    /// Accept the next token as horizontal dilution of precision, set to `None` if the field is
    /// empty.
    #[inline]
    fn accept_hdop(&mut self) -> Result<Option<f64>, ParseError> {
        match accept!(self, FloatLiteral, f)? {
            Some(f) => Ok(Some(fl_as_f64(f.as_slice())?)),
            None => Ok(None),
        }
    }

    /// Accept the next token as antenna altitude, set to `None` if the field is empty.
    #[inline]
    fn accept_altitude(&mut self) -> Result<Option<f64>, ParseError> {
        match accept!(self, FloatLiteral, f)? {
            Some(f) => Ok(Some(fl_as_f64(f.as_slice())?)),
            None => Ok(None),
        }
    }

    /// Accept the next token as geoidal separation, set to `None` if the field is empty.
    #[inline]
    fn accept_geo_sep(&mut self) -> Result<Option<f64>, ParseError> {
        match accept!(self, FloatLiteral, f)? {
            Some(f) => Ok(Some(fl_as_f64(f.as_slice())?)),
            None => Ok(None),
        }
    }

    /// Accept the next token as age of the data in seconds, set to `None` if the field is empty.
    fn accept_age(&mut self) -> Result<Option<f64>, ParseError> {
        match accept!(self, FloatLiteral, f)? {
            Some(f) => {
                let f = fl_as_f64(f.as_slice())?;
                if f < 0.0 {
                    return Err(ParseError::InvalidValue(
                        "age of the data cannot be negative",
                    ));
                }
                Ok(Some(f))
            }
            None => Ok(None),
        }
    }

    /// Accept the next token as station id, set to `None` if the field is empty.
    /// Returns `Err(ParseError::InvalidValue(_))` if the id is not between 0 and 1023 (inclusive).
    fn accept_station_id(&mut self) -> Result<Option<u32>, ParseError> {
        match accept!(self, IntLiteral, i)? {
            // casting is possible since we know i is in range 0..=1023
            Some(i) if 0 <= i && i <= 1023 => Ok(Some(i as u32)),
            Some(_) => Err(ParseError::InvalidValue(
                "station_id must be between 0 and 1023",
            )),
            None => Ok(None),
        }
    }

    /// Parse `coord` as a f64 representing a coordinate.
    /// The coordinate will be multiplied by 1 or -1 depending on the direction.
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

#[cfg(test)]
mod tests {
    use super::{CardDir, GgaParser, GgaSentence, GpsQualityInd, ParseError};
    use std::io::Cursor;

    fn t_parser(arg: &str) -> GgaParser<Cursor<&str>> {
        GgaParser::new(Cursor::new(arg)).unwrap()
    }

    fn str_array_vec(vec: Vec<u8>) -> ::arrayvec::ArrayVec<[u8; ::lexer::STRING_LENGTH]> {
        let mut av = ::arrayvec::ArrayVec::<[u8; ::lexer::STRING_LENGTH]>::new();
        for v in vec {
            av.push(v);
        }
        av
    }

    #[test]
    fn expect_methods_on_empty() {
        let mut parser = t_parser("");
        assert_matches!(parser.expect_header(), Err(ParseError::UnexpectedToken));
        assert_matches!(parser.expect_sen_type(), Err(ParseError::UnexpectedToken));
        assert_matches!(parser.expect_utc(), Err(ParseError::UnexpectedToken));
        assert_matches!(parser.expect_qual_ind(), Err(ParseError::UnexpectedToken));
        assert_matches!(
            parser.expect_sat_in_view(),
            Err(ParseError::UnexpectedToken)
        );
    }

    #[test]
    fn accept_methods_on_empty() {
        let mut parser = t_parser("");
        assert_matches!(parser.accept_age(), Ok(None));
        assert_matches!(parser.accept_altitude(), Ok(None));
        assert_matches!(parser.accept_geo_sep(), Ok(None));
        assert_matches!(parser.accept_hdop(), Ok(None));
        assert_matches!(parser.accept_lat(), Err(ParseError::UnexpectedToken));
        assert_matches!(parser.accept_long(), Err(ParseError::UnexpectedToken));
        assert_matches!(parser.accept_station_id(), Ok(None));
    }

    mod header {
        use super::{t_parser, ParseError};

        #[test]
        fn header_ok() {
            let mut parser = t_parser("$GP");
            assert_matches!(parser.expect_header(), Ok([b'G', b'P']));
            assert_matches!(parser.lexer.next(), None);
        }

        #[test]
        fn header_too_short() {
            let mut parser = t_parser("$a");
            assert_matches!(parser.expect_header(), Err(ParseError::Lexer(_)));
            assert_matches!(parser.lexer.next(), None);
        }
    }
}

//! This module provides a parser for the *GGA* sentence of the *NMEA 0183*
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

#[derive(Debug, PartialEq)]
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
    /// Takes an integer in the range `0..=8` and returns the corresponding `GpsQualityInd`.
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

/// Represents a correct GGA sentence and will usually be created by a
/// [GgaParser](../parser/struct.GgaParser.html)
#[derive(Debug, PartialEq)]
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

/// Parser for the `NMEA 0183` protocol that parses only GGA sentences.
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
    pub fn read_sentence(&mut self) -> Result<GgaSentence, ParseError> {
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
    ) -> Result<GgaSentence, ParseError> {
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

        // Consume new line
        accept!(self, LineEnding)?;

        Ok(GgaSentence {
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
        })
    }

    /// Skips and consumes all tokens till the next `TokenKind::Header` or EOF.
    /// Nothing will happen if the next token already is a header.
    /// Returns `Ok(None)` if EOF has been reached.
    pub fn jump_to_header(&mut self) -> Result<Option<()>, io::Error> {
        loop {
            match self.lexer.peek() {
                Some(Ok(v)) if v.is_header() => return Ok(Some(())),
                Some(_) => (),
                None => return Ok(None),
            }
            if let Some(Err(LexError::Io(e))) = self.lexer.next() {
                return Err(e);
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
            Some(s) => return Err(ParseError::UnexpectedDir(s)),
            None => None,
        };
        match (raw_lat, lat_dir) {
            (Some(lat), Some(d)) => Ok(Some(parse_coord(&lat, &d, LAT_SPLIT, ABS_MAX_LAT)?)),
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
            Some(s) => return Err(ParseError::UnexpectedDir(s)),
            None => None,
        };
        match (raw_long, long_dir) {
            (Some(long), Some(d)) => Ok(Some(parse_coord(&long, &d, LONG_SPLIT, ABS_MAX_LONG)?)),
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
                "number of satellites in view has to be larger than or equal to 0",
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

/// Parse `coord` as a f64 representing a coordinate.
/// The coordinate will be multiplied by 1 or -1 depending on the direction.
/// `deg_split` is the number of digits that represent the degrees.
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

    let (deg, min) = coord.split_at(deg_split);
    let degrees = f64::from(u8::from_str(str::from_utf8(deg)?)?);
    let min = fl_as_f64(min)?;
    if min >= 60.0 {
        return Err(ParseError::InvalidValue("minutes have to be less than 60"));
    }
    let decimal_min = min * 10.0 / 6.0 / 100.0;
    let dec_deg = degrees + decimal_min;
    if dec_deg.abs() > abs_max {
        return Err(ParseError::InvalidCoord(dec_deg, abs_max));
    }
    Ok(dec_deg * dir.get_sign() as f64)
}

#[inline]
fn fl_as_f64(fl: &[u8]) -> Result<f64, ParseError> {
    Ok(f64::from_str(str::from_utf8(fl)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_matches!(parser.expect_meters(), Err(ParseError::UnexpectedToken));
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

    #[test]
    fn direction_sign() {
        assert_eq!(CardDir::North.get_sign(), 1);
        assert_eq!(CardDir::East.get_sign(), 1);
        assert_eq!(CardDir::South.get_sign(), -1);
        assert_eq!(CardDir::West.get_sign(), -1);
    }

    #[test]
    fn gps_quality_from_i64() {
        assert_matches!(
            GpsQualityInd::try_from_i64(-1),
            Err(ParseError::UnexpectedToken)
        );
        assert_matches!(
            GpsQualityInd::try_from_i64(0),
            Ok(GpsQualityInd::FixNotAvailable)
        );
        assert_matches!(GpsQualityInd::try_from_i64(1), Ok(GpsQualityInd::GpsFix));
        assert_matches!(
            GpsQualityInd::try_from_i64(2),
            Ok(GpsQualityInd::DifferentialGpsFix)
        );
        assert_matches!(GpsQualityInd::try_from_i64(3), Ok(GpsQualityInd::PpsFix));
        assert_matches!(
            GpsQualityInd::try_from_i64(4),
            Ok(GpsQualityInd::RealTimeKinematic)
        );
        assert_matches!(GpsQualityInd::try_from_i64(5), Ok(GpsQualityInd::FloatRtk));
        assert_matches!(GpsQualityInd::try_from_i64(6), Ok(GpsQualityInd::Estimated));
        assert_matches!(
            GpsQualityInd::try_from_i64(7),
            Ok(GpsQualityInd::ManualInputMode)
        );
        assert_matches!(
            GpsQualityInd::try_from_i64(8),
            Ok(GpsQualityInd::SimulationMode)
        );
        assert_matches!(
            GpsQualityInd::try_from_i64(9),
            Err(ParseError::UnexpectedToken)
        );
    }

    mod header {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("$GP");
            assert_matches!(parser.expect_header(), Ok([b'G', b'P']));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn too_short() {
            let mut parser = t_parser("$a");
            assert_matches!(parser.expect_header(), Err(ParseError::Lexer(_)));
            assert!(parser.lexer.next().is_none());
        }
    }

    mod sentence_type {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("GGA");
            let exp = str_array_vec(vec![b'G', b'G', b'A']);
            assert_matches!(parser.expect_sen_type(), Ok(ref s) if s == &exp);
        }

        #[test]
        fn ok_long() {
            let mut parser = t_parser("aaaaa");
            let exp = str_array_vec(vec![b'a', b'a', b'a', b'a', b'a']);
            assert_matches!(parser.expect_sen_type(), Ok(ref s) if s == &exp);
        }

        #[test]
        fn ok_short() {
            let mut parser = t_parser("a");
            let exp = str_array_vec(vec![b'a']);
            assert_matches!(parser.expect_sen_type(), Ok(ref s) if s == &exp);
        }
    }

    mod utc {
        use super::*;

        #[test]
        fn max() {
            let mut parser = t_parser("235959.999999999");
            let left = parser.expect_utc();
            let expected = NaiveTime::from_hms_nano(23, 59, 59, 999999999);
            assert!(left.is_ok());
            assert_eq!(left.unwrap(), expected);
        }

        #[test]
        fn min() {
            let mut parser = t_parser("000000.0");
            let left = parser.expect_utc();
            assert!(left.is_ok());
            assert_eq!(left.unwrap(), NaiveTime::from_hms(0, 0, 0));
        }

        #[test]
        fn negative() {
            let mut parser = t_parser("-00000.0");
            let left = parser.expect_utc();
            assert_matches!(left, Err(ParseError::Time(_)));
        }

        #[test]
        fn no_dot() {
            let mut parser = t_parser("111111");
            assert_matches!(parser.expect_utc(), Err(ParseError::UnexpectedToken));
        }

        #[test]
        fn wrong_fmt() {
            let mut parser = t_parser("1111111.11");
            assert_matches!(parser.expect_utc(), Err(ParseError::Time(_)));
        }
    }

    mod parse_coord {
        use super::*;

        #[test]
        fn coord_ok() {
            let left = parse_coord(b"18000.000", &CardDir::East, 3, 180.0);
            assert_matches!(left, Ok(coord) if coord == 180.0);
        }

        #[test]
        fn short_coord() {
            let left = parse_coord(b"111.0", &CardDir::East, 3, 180.0);
            assert_matches!(left, Ok(coord) if coord == 111.0);
        }

        #[test]
        fn higher_than_max() {
            let left = parse_coord(b"18000.3", &CardDir::East, 3, 180.0);
            assert_matches!(left,
                            Err(ParseError::InvalidCoord(c, max))
                            if c == 180.005 && max == 180.0);
        }

        #[test]
        fn negative_coord() {
            let left = parse_coord(b"-0000.001", &CardDir::East, 3, 180.0);
            assert_matches!(left, Err(ParseError::Int(_)));
        }

        #[test]
        fn minutes_too_high() {
            let left = parse_coord(b"00060.0", &CardDir::East, 3, 180.0);
            assert_matches!(left, Err(ParseError::InvalidValue(_)));
        }

        #[test]
        fn direction_handling() {
            let coord = b"01612.369";
            let north = parse_coord(coord, &CardDir::North, 3, 180.0);
            let east = parse_coord(coord, &CardDir::East, 3, 80.0);
            let south = parse_coord(coord, &CardDir::South, 3, 180.0);
            let west = parse_coord(coord, &CardDir::West, 3, 80.0);
            assert!(north.is_ok() && east.is_ok() && south.is_ok() && west.is_ok());
            let north = north.unwrap();
            let east = east.unwrap();
            let south = south.unwrap();
            let west = west.unwrap();
            assert_eq!(north, east);
            assert_eq!(south, west);
            assert_eq!(north, south * -1.0);
        }
    }

    mod accept_lat {
        use super::*;

        #[test]
        fn some_lat() {
            let mut parser = t_parser("1612.369,N");
            let left = parser.accept_lat();
            assert_matches!(left, Ok(Some(lat)) if lat == 16.20615);
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn negative_lat() {
            let mut parser = t_parser("1612.369,S");
            let left = parser.accept_lat();
            assert_matches!(left, Ok(Some(lat)) if lat == -16.20615);
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_coordinate() {
            let mut parser = t_parser(",N");
            assert_matches!(parser.accept_lat(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_direction() {
            let mut parser = t_parser("1612.369,");
            assert_matches!(parser.accept_lat(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_coord_or_dir() {
            let mut parser = t_parser(",");
            assert_matches!(parser.accept_lat(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn wrong_direction() {
            let mut parser = t_parser("01612.369,W");
            let left = parser.accept_lat();
            assert_matches!(left, Err(ParseError::UnexpectedDir(_)));
            assert!(parser.lexer.next().is_none());
        }
    }

    mod accept_long {
        use super::*;

        #[test]
        fn some_long() {
            let mut parser = t_parser("01612.369,E");
            let left = parser.accept_long();
            assert_matches!(left, Ok(Some(long)) if long == 16.20615);
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn negative_long() {
            let mut parser = t_parser("01612.369,W");
            let left = parser.accept_long();
            assert_matches!(left, Ok(Some(long)) if long == -16.20615);
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_coordinate() {
            let mut parser = t_parser(",E");
            assert_matches!(parser.accept_long(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_direction() {
            let mut parser = t_parser("01612.369,");
            assert_matches!(parser.accept_long(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn no_coord_or_dir() {
            let mut parser = t_parser(",");
            assert_matches!(parser.accept_long(), Ok(None));
            assert!(parser.lexer.next().is_none());
        }

        #[test]
        fn wrong_direction() {
            let mut parser = t_parser("01612.369,N");
            let left = parser.accept_long();
            assert_matches!(left, Err(ParseError::UnexpectedDir(_)));
            assert!(parser.lexer.next().is_none());
        }
    }

    mod quality_indicator {
        use super::*;

        #[test]
        fn lowest_value() {
            let mut parser = t_parser("0");
            let left = parser.expect_qual_ind();
            assert_matches!(left, Ok(GpsQualityInd::FixNotAvailable));
        }

        #[test]
        fn highest_value() {
            let mut parser = t_parser("8");
            let left = parser.expect_qual_ind();
            assert_matches!(left, Ok(GpsQualityInd::SimulationMode));
        }

        #[test]
        fn too_low() {
            let mut parser = t_parser("-1");
            let left = parser.expect_qual_ind();
            assert_matches!(left, Err(ParseError::UnexpectedToken));
        }

        #[test]
        fn too_high() {
            let mut parser = t_parser("9");
            let left = parser.expect_qual_ind();
            assert_matches!(left, Err(ParseError::UnexpectedToken));
        }
    }

    mod sattelites {
        use super::*;

        #[test]
        fn lowest_value() {
            let mut parser = t_parser("0");
            assert_matches!(parser.expect_sat_in_view(), Ok(0));
        }

        #[test]
        fn too_low() {
            let mut parser = t_parser("-1");
            let left = parser.expect_sat_in_view();
            assert_matches!(left, Err(ParseError::InvalidValue(_)));
        }
    }

    mod hdop {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("12345.6789");
            let left = parser.accept_hdop();
            assert_matches!(left, Ok(Some(hdop)) if hdop == 12345.6789);
        }
    }

    mod altitude {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("12345.6789");
            let left = parser.accept_altitude();
            assert_matches!(left, Ok(Some(f)) if f == 12345.6789);
        }
    }

    mod geoidal_separation {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("12345.6789");
            let left = parser.accept_geo_sep();
            assert_matches!(left, Ok(Some(f)) if f == 12345.6789);
        }
    }

    mod age {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("12345.6789");
            let left = parser.accept_age();
            assert_matches!(left, Ok(Some(age)) if age == 12345.6789);
        }

        #[test]
        fn lowest_value() {
            let mut parser = t_parser("0.0");
            let left = parser.accept_age();
            assert_matches!(left, Ok(Some(age)) if age == 0.0);
        }

        #[test]
        fn negative() {
            let mut parser = t_parser("-0.0001");
            let left = parser.accept_age();
            assert_matches!(left, Err(ParseError::InvalidValue(_)));
        }
    }

    mod station_id {
        use super::*;

        #[test]
        fn lowest() {
            let mut parser = t_parser("0");
            let left = parser.accept_station_id();
            assert_matches!(left, Ok(Some(id)) if id == 0);
        }

        #[test]
        fn highest() {
            let mut parser = t_parser("1023");
            let left = parser.accept_station_id();
            assert_matches!(left, Ok(Some(id)) if id == 1023);
        }

        #[test]
        fn too_low() {
            let mut parser = t_parser("-1");
            let left = parser.accept_station_id();
            assert_matches!(left, Err(ParseError::InvalidValue(_)));
        }

        #[test]
        fn too_high() {
            let mut parser = t_parser("1024");
            let left = parser.accept_station_id();
            assert_matches!(left, Err(ParseError::InvalidValue(_)));
        }
    }

    mod units {
        use super::*;

        #[test]
        fn meters() {
            let mut parser = t_parser("M");
            let left = parser.expect_meters();
            assert!(left.is_ok());
        }
    }

    mod parse_gga {
        use super::*;

        #[test]
        fn with_location() {
            let gga = "$GPGGA,142212.000,1956.9418,S,06938.0163,W,1,3,5.74,102.1,M,47.9,M,,*57";
            let talker_id = [b'G', b'P'];

            let mut parser = t_parser(gga);
            // "$GPGGA," is needed for the correct checksum, but will be skipped.
            parser.expect_header().expect("header");
            parser.expect_sen_type().expect("sentence type");
            expect!(parser, CommaSeparator).expect("comma");

            let left = parser.parse_gga(talker_id);
            let expected = GgaSentence {
                talker_id,
                utc: NaiveTime::from_hms(14, 22, 12),
                lat: Some(
                    parse_coord(b"1956.9418", &CardDir::South, 2, 90.0).expect("lat: -19.949030"),
                ),
                long: Some(
                    parse_coord(b"06938.0163", &CardDir::West, 3, 180.0).expect("long: -69.633605"),
                ),
                gps_qlty: GpsQualityInd::try_from_i64(1).expect("GpsFix"),
                sat_view: 3,
                hdop: Some(5.74),
                altitude: Some(102.1),
                geo_sep: Some(47.9),
                age: None,
                station_id: None,
            };

            assert!(left.is_ok());
            assert_eq!(left.unwrap(), expected);
        }

        #[test]
        fn without_location() {
            let talker_id = [b'G', b'P'];

            let mut parser = t_parser("$GPGGA,142054.304,,,,,0,0,,,M,,M,,*49");
            // "$GPGGA," is needed for the correct checksum, but will be skipped.
            parser.expect_header().expect("header");
            parser.expect_sen_type().expect("sentence type");
            expect!(parser, CommaSeparator).expect("comma");

            let left = parser.parse_gga(talker_id);
            let expected = GgaSentence {
                talker_id,
                utc: NaiveTime::from_hms_milli(14, 20, 54, 304),
                lat: None,
                long: None,
                gps_qlty: GpsQualityInd::try_from_i64(0).expect("FixNotAvailable"),
                sat_view: 0,
                hdop: None,
                altitude: None,
                geo_sep: None,
                age: None,
                station_id: None,
            };

            assert!(left.is_ok());
            assert_eq!(left.unwrap(), expected);
        }

        #[test]
        fn comma_not_consumed() {
            let talker_id = [b'G', b'P'];

            let mut parser = t_parser("$GPGGA,142054.304,,,,,0,0,,,M,,M,,*49");
            parser.expect_header().expect("header");
            parser.expect_sen_type().expect("sentence type");

            let left = parser.parse_gga(talker_id);
            assert_matches!(left, Err(ParseError::UnexpectedToken));
        }
    }

    mod read_sentence {
        use super::*;

        #[test]
        fn with_location() {
            let gga = "$GPGGA,142212.000,1956.9418,S,06938.0163,W,1,3,5.74,102.1,M,47.9,M,,*57";
            let mut parser = t_parser(gga);

            let left = parser.read_sentence();
            let expected = GgaSentence {
                talker_id: [b'G', b'P'],
                utc: NaiveTime::from_hms(14, 22, 12),
                lat: Some(
                    parse_coord(b"1956.9418", &CardDir::South, 2, 90.0).expect("lat: -19.949030"),
                ),
                long: Some(
                    parse_coord(b"06938.0163", &CardDir::West, 3, 180.0).expect("long: -69.633605"),
                ),
                gps_qlty: GpsQualityInd::try_from_i64(1).expect("GpsFix"),
                sat_view: 3,
                hdop: Some(5.74),
                altitude: Some(102.1),
                geo_sep: Some(47.9),
                age: None,
                station_id: None,
            };

            assert!(left.is_ok());
            assert_eq!(left.unwrap(), expected);
        }

        #[test]
        fn without_location() {
            let mut parser = t_parser("$GPGGA,142054.304,,,,,0,0,,,M,,M,,*49");

            let left = parser.read_sentence();
            let expected = GgaSentence {
                talker_id: [b'G', b'P'],
                utc: NaiveTime::from_hms_milli(14, 20, 54, 304),
                lat: None,
                long: None,
                gps_qlty: GpsQualityInd::try_from_i64(0).expect("FixNotAvailable"),
                sat_view: 0,
                hdop: None,
                altitude: None,
                geo_sep: None,
                age: None,
                station_id: None,
            };

            assert!(left.is_ok());
            assert_eq!(left.unwrap(), expected);
        }

        #[test]
        fn wrong_sentence_type() {
            let mut parser = t_parser("$GPGLL");

            let left = parser.read_sentence();
            assert_matches!(left, Err(ParseError::UnexpectedSentenceType));
        }
    }

    mod jump_to_header {
        use super::*;

        #[test]
        fn ok() {
            let mut parser = t_parser("abc123$aa");
            assert!(parser.jump_to_header().is_ok());
            assert_matches!(parser.expect_header(), Ok([b'a', b'a']));
        }

        #[test]
        fn stay_at_header() {
            let mut parser = t_parser("$aa123$bb");
            assert!(parser.jump_to_header().is_ok());
            assert!(parser.jump_to_header().is_ok());
            assert_matches!(parser.expect_header(), Ok([b'a', b'a']));
            assert!(parser.jump_to_header().is_ok());
            assert!(parser.jump_to_header().is_ok());
            assert_matches!(parser.expect_header(), Ok([b'b', b'b']));
        }

        #[test]
        fn no_header() {
            let mut parser = t_parser("$aa");
            assert_matches!(parser.expect_header(), Ok([b'a', b'a']));
            assert_matches!(parser.jump_to_header(), Ok(None));
            assert_matches!(parser.jump_to_header(), Ok(None));
        }

        #[test]
        fn invalid_ascii() {
            let sentences =
                "$GPGGA,142130.220,4900.7350,N,00825.5268,E,1,3,5.53,102.1,M,47.9,M,,*5B\n\n\
                 -�OAz�\n\
                 �\"AAliCGRA�B؇=&J]���b����?$GPRMC,142132.000,A,\
                 4900.7350,N,00825.5269,E,0.00,182.46,150518,,,A*63\n\n\
                 $GPVTG,182.46,T,,M,0.00,N,0.00,K,A*34\n\n\

                 $GPGGA,142132.000,4900.7350,N,00825.5269,E,1,3,5.53,102.1,M,47.9,M,,*58";

            let mut parser = t_parser(sentences);
            assert!(parser.read_sentence().is_ok());
            assert_matches!(
                parser.read_sentence(),
                Err(ParseError::Lexer(::err::LexError::IncompleteToken(_)))
            );
            assert!(parser.jump_to_header().is_ok());
            assert_matches!(
                parser.read_sentence(),
                Err(ParseError::UnexpectedSentenceType)
            );
            assert!(parser.jump_to_header().is_ok());
            assert_matches!(
                parser.read_sentence(),
                Err(ParseError::UnexpectedSentenceType)
            );
            assert!(parser.jump_to_header().is_ok());
            assert!(parser.read_sentence().is_ok());
        }
    }
}

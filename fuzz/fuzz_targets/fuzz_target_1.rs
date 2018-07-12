#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate nmea_gps;

use std::io::Cursor;
use nmea_gps::GgaParser;

fuzz_target!(|data: &[u8]| {
    let data = Cursor::new(data);
    let parser = GgaParser::new(data);

    for _ in parser {
        ();
    }
});

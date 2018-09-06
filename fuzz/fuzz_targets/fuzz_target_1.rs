#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate titanic;

use std::io::Cursor;
use titanic::GgaParser;

fuzz_target!(|data: &[u8]| {
    let data = Cursor::new(data);
    let parser = GgaParser::new(data);

    for _ in parser {
        ();
    }
});

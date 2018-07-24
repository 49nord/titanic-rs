# Nmea 0183 GGA Parser

This is a parser for the GGA sentence of the NMEA 0183 protocol.

More sentences may be added in the future, but are *not planned* at the moment.

Accepts data as described [here](http://www.catb.org/gpsd/NMEA.html#_gga_global_positioning_system_fix_data).

## Setup

Put this to your `Cargo.toml`:

```toml
[dependencies]
nmea_gps = "0.1.0"
```

Then put this in your crate root:

```rust
extern crate nmea_gps;
```

## Usage

[`GgaParser`](./parser/struct.GgaParser.html) can be used like an
[`Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html).
Calling `next()` on `GgaParser` **blocks** until it finds `'$'`,
reaches EOF or an I/O error occurs. `'$'` signals the beginning of a new sentence.
If the new sentence is of the type GGA, it will be parsed if possible.
The parser iterates over `Result<GgaSentence, ParseError>`.

EOF signals the end of the iterator.

```rust
# extern crate nmea_gps;
# use std::io::Cursor;
use nmea_gps::GgaParser;

let data = Cursor::new("$GPGGA,142212.000,1956.9418,S,06938.0163,W,1,3,5.74,102.1,M,47.9,M,,*57");
let parser = GgaParser::new(data).unwrap();

for gga in parser {
    let gga = gga.unwrap();
    println!(
       "Time: {}, we are here: {}°, {}°",
       gga.utc.format("%H:%M:%S"),
       gga.lat.unwrap(),
       gga.long.unwrap()
   );
}
// Prints "Time: 14:22:12, we are here: -19.94903°, -69.633605°"
```
#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! base64-url = "1.1"
//! ```

use std::env;
use std::io;
use std::io::{Read, Write};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 || (args[1] != "decode" && args[1] != "encode") {
        println!("Error: Specify either 'decode' or 'encode' as first and only argument");
        ::std::process::exit(1);
    }

    if args[1] == "decode" {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        let decoded = base64_url::decode(&buffer).expect("Error decoding");
        io::stdout().write(&decoded).unwrap();
        io::stdout().flush().unwrap();
        println!("");
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        let encoded = base64_url::encode(&buffer);
        println!("{}", encoded);
    }

    Ok(())
}

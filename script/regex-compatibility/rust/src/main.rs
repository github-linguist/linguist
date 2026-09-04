use regex::Regex;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;
use std::str;

fn main() {
    let args: Vec<_> = env::args_os().collect();
    if args.len() != 2 {
        eprintln!("usage: linguist-regex-compatibility PATTERNS_FILE");
        process::exit(2);
    }

    let data = match fs::read(&args[1]) {
        Ok(data) => data,
        Err(error) => {
            eprintln!("{error}");
            process::exit(2);
        }
    };
    if data.is_empty() {
        return;
    }
    let mut patterns: Vec<_> = data.split(|byte| *byte == 0).collect();
    if patterns.last() == Some(&&[][..]) {
        patterns.pop();
    }

    let stdout = io::stdout();
    let mut output = io::BufWriter::new(stdout.lock());
    for (index, bytes) in patterns.iter().enumerate() {
        let pattern = match str::from_utf8(bytes) {
            Ok(pattern) => pattern,
            Err(error) => {
                eprintln!("pattern {index} is not UTF-8: {error}");
                process::exit(2);
            }
        };
        if let Err(error) = Regex::new(pattern) {
            if write!(output, "{index}\0{error}\0").is_err() {
                eprintln!("failed to write compiler error");
                process::exit(2);
            }
        }
    }

    if let Err(error) = output.flush() {
        eprintln!("{error}");
        process::exit(2);
    }
}

use std::ascii::AsciiCast;
use std::str::from_utf8;

static A: u8 = 'A' as u8;
static a: u8 = 'a' as u8;

fn uppercase_and_filter(input: &str) -> ~[u8] {
  let mut result = ~[];

  for b in input.bytes() {
    if b.is_ascii() {
      let ascii = b.to_ascii();
      if ascii.is_lower() {
        // We know it's ascii, so just do the math directly
        result.push((b + (A - a)))
      } else if ascii.is_upper() {
        result.push(b);
      }
    }
  }

  return result;
}

fn vigenere(key: &str, text: &str, is_encoding: bool) -> ~str {

  let key_bytes = uppercase_and_filter(key);
  let text_bytes = uppercase_and_filter(text);

  let mut result_bytes = ~[];

  for (i, c) in text_bytes.iter().enumerate() {
    let c2 = if is_encoding {
      (c + key_bytes[i % key_bytes.len()] - 2 * A) % 26 + A
    } else {
      (c - key_bytes[i % key_bytes.len()] + 26) % 26 + A
    };
    result_bytes.push(c2);
  }

  return from_utf8(result_bytes).to_owned();
}

fn main() {
  let text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
  let key = "VIGENERECIPHER";

  println!("Text: {:s}", text);
  println!("Key:  {:s}", key);

  let encoded = vigenere(key, text, true);
  println!("Code: {:s}", encoded);
  let decoded = vigenere(key, encoded, false);
  println!("Back: {:s}", decoded);
}

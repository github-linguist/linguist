use std::iter::range_inclusive;
use std::rand::{task_rng, Rng};
use std::io;
use std::str;
use std::option::collect;
use std::io::stdio::StdReader;
use std::str::from_char;
use std::iter::FromIterator;
use std::hashmap::HashSet;

static NUMBER_OF_DIGITS: uint = 4;
static LOWEST_DIGIT: uint = 1;
static HIGHEST_DIGIT: uint = 9;

fn generate_digits() -> ~[uint] {
  task_rng().sample(range_inclusive(LOWEST_DIGIT, HIGHEST_DIGIT), NUMBER_OF_DIGITS)
}

fn read_input(reader: &mut StdReader) -> ~str {
  let mut buf = [0u8, ..32];
  let num_bytes = reader.read(buf).unwrap();
  return str::from_utf8(buf).slice_to(num_bytes).trim().to_owned();
}

fn parse_guess_string(guess: &str) -> Option<~[uint]> {
  let digit_chars = guess.chars();
  let digit_options = digit_chars.map(|c| from_str::<uint>(from_char(c)));

  // std::option::collect is a cool little function that goes through an iterator
  // and returns a None if it hits a None. If not it will return a Some that
  // contains a vector containing the unwrapped values of the iterator
  let digits_maybe: Option<~[uint]> = collect(digit_options);

  match digits_maybe {
    Some(ref digits) => {
      if digits.len() != NUMBER_OF_DIGITS {
        println!("you need to guess with {:u} digits", NUMBER_OF_DIGITS);
        return None;
      }
      if digits.iter().any(|&digit| digit < LOWEST_DIGIT || digit > HIGHEST_DIGIT) {
        println("digit out of range");
        return None;
      }
      let digits_set: HashSet<&uint> = FromIterator::from_iterator(&mut digits.iter());
      if digits_set.len() != NUMBER_OF_DIGITS {
        println("no duplicates!");
        return None;
      }
    },
    None => println("non-numeric input, please try again")
  }
  return digits_maybe;
}

fn calculate_score(digits: &[uint], guess: &[uint]) -> (uint, uint) {
  let mut bulls = 0;
  let mut cows = 0;
  for i in range(0, digits.len()) {
    let mut j = guess.iter();
    let pos_maybe: Option<uint> = j.position(|&a| -> bool {a == digits[i]});
    match  pos_maybe {
      None                  => (),
      Some(pos) if pos == i => bulls += 1,
      Some(_)               => cows += 1
    }
  }
  return (bulls, cows);
}

fn main() {
  let mut reader = io::stdin();

  loop {
    let digits = generate_digits();
    println!("I have chosen my {} digits. Please guess what they are", NUMBER_OF_DIGITS);
    loop {
      let guess_string = read_input(&mut reader);
      let digits_maybe = parse_guess_string(guess_string);
      match digits_maybe {
        None => continue,
        Some(guess_digits) => {
          let (bulls, cows) = calculate_score(digits, guess_digits);
          if bulls == NUMBER_OF_DIGITS {
            println("you win!");
            break;
          } else {
            println!("bulls: {:u}, cows: {:u}", bulls, cows);
          }
        }
      }
    }
  }
}

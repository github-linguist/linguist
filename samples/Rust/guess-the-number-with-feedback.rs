use std::io::stdio::stdin;
use std::str;
use std::rand::{task_rng, Rng};

static LOWEST: int = 0;
static HIGHEST: int = 100;

fn main() {
  let mut rng = task_rng();
  let mut stdin = stdin();
  let mut num_guesses = 0;

  loop {

    let number = rng.gen_range(LOWEST, HIGHEST);

    println!("I have chosen my number between {:d} and {:d}. You know what to do", LOWEST, HIGHEST);

    loop {

      let buf: &mut [u8] = [0u8, ..64];
      let num_bytes = stdin.read(buf).unwrap();
      let guess_str = str::from_utf8(buf).slice_to(num_bytes).trim();

      match from_str::<int>(guess_str) {
        None => println("not a number! please try again"),
        Some(guess) => {
          num_guesses += 1;
          if guess == number {
            println!("you got it in {:d} tries!", num_guesses);
            num_guesses = 0;
            break;
          } else if guess < number {
            println("too low!");
          } else {
            println("too high!");
          }
        }
      }
    }
  }
}

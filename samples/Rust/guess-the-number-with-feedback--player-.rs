use std::io::stdio::stdin;
use std::str;

fn main() {
  let mut low = 1;
  let mut high = 100;
  let mut current_guess = calculate_guess(low, high);
  let mut number_of_guesses = 1;
  let mut stdin = stdin();

  let take_a_guess = |new_low: int, new_high: int| {
    low = new_low;
    high = new_high;
    number_of_guesses += 1;
    current_guess = calculate_guess(low, high);
  };
	
  println!("Hello, please choose a number between {} and {} and remember it.", low, high);
  println("Got it? Good. Now I shall guess it using only the power of my mind.\n");

  loop {
    println!("My guess is {:d}. Is that too [h]igh, too [l]ow or is it [e]qual to it? ", current_guess);
    println("(please type h, l or e and then hit enter)");
    let buf: &mut [u8] = [0u8, ..8];
    let num_bytes = stdin.read(buf);
    let answer = str::from_utf8(buf).slice_to(num_bytes.unwrap()).trim();
    match answer {
      "e" | "E" => {
        println!("Jolly good! I got it in only {} tries!", number_of_guesses);
        break;
      }
      "l" | "L" => take_a_guess(current_guess, high),
      "h" | "H" => take_a_guess(low, current_guess),
      _         => println("sorry, I didn't quite get that")

    }
    if low == high {
      println!("Your number must be {}. Or maybe you forgot it? It happens!", low);
      break;
    }
    if low > high {
      println("Uh, I think something went wrong. Entirely my fault, I'm sure!");
      break;
    }
  }
}

fn calculate_guess(low: int, high: int) -> int {
  (low + high) / 2
}

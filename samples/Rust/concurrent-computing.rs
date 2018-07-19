use std::io::timer::sleep;
use std::rand::{rng, Rng};

fn main() {
  let mut random = rng();
  for word in "Enjoy Rosetta Code".words() {
    let local_word = word.to_owned();
    let snooze_time = random.gen_range::<u64>(0, 1000);

    spawn(proc() {
      sleep(snooze_time);
      println!("{}", local_word);
    });
  }
}

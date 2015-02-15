// rust 0.8

fn main() {
  let x = levenshtein_distance("kitten", "sitting");
  println!("{}", x);
}

fn levenshtein_distance(word1: &str, word2: &str) -> uint {
  let word1_length = word1.len() + 1;
  let word2_length = word2.len() + 1;

  let mut matrix = ~[~[0]];

  for i in range(1, word1_length) { matrix[0].push(i); }
  for j in range(1, word2_length) { matrix.push(~[j]); }

  for j in range(1, word2_length) {
    for i in range(1, word1_length) {
      let x: uint = if word1[i - 1] == word2[j - 1] {
        matrix[j-1][i-1]
      }
      else {
        let min_distance = [matrix[j][i-1], matrix[j-1][i], matrix[j-1][i-1]];
        *min_distance.iter().min().unwrap() + 1
      };

      matrix[j].push(x);
    }
  }

  matrix[word2_length-1][word1_length-1]
}

// rust 0.9-pre (1b12dca 2013-12-11 12:56:22 -0800)

fn is_ordered(s: &str) -> bool {
    let mut prev = '\x00';
    for c in s.chars() {
        if c < prev {
            return false;
        }
        prev = c;
    }

    return true;
}

fn find_longest_ordered_words(dict: ~[&str]) -> ~[~str] {
    let mut result = ~[];
    let mut longest_length = 0;

    for &s in dict.iter() {
        if is_ordered(s) {
            let n = s.len();
            if n > longest_length {
                longest_length = n;
                result.truncate(0);
            }
            if n == longest_length {
                result.push(s.to_owned());
            }
        }
    }

    return result;
}

fn main() {
    let raw = std::io::File::open(&Path::new("unixdict.txt")).read_to_end();
    let lines:~[&str] = std::str::from_utf8(raw).lines_any().collect();

    let longest_ordered = find_longest_ordered_words(lines);

    for s in longest_ordered.iter() {
        println(s.to_str());
    }
}

use std::iter;

fn next_sequence(in_seq: &[int]) -> ~[int] {
    assert!(!in_seq.is_empty());

    let mut result = ~[];
    let mut current_number = in_seq[0];
    let mut current_runlength = 1;

    for i in in_seq.slice_from(1).iter() {
        if current_number == *i {
            current_runlength += 1;
        } else {
            result.push(current_runlength);
            result.push(current_number);
            current_runlength = 1;
            current_number = *i;
        }
    }
    result.push(current_runlength);
    result.push(current_number);

    result
}

fn main() {
    let mut seq = ~[1];

    for i in iter::range(0, 10) {
        println!("Sequence {}: {:?}", i, seq);
        seq = next_sequence(seq);
    }
}

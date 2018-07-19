extern mod std;
use std::io;

fn main() {
    let input_file = &Path("input.txt");
    let input = io::read_whole_file_str(input_file);
    match input {
        Err(e) => fail!(e),
        Ok(_) => {}
    }

    let output_file = &Path("output.txt");
    let output = io::file_writer(output_file, [io::Create, io::Truncate]);
    match output {
        Err(e) => fail!(e),
        Ok(_) => {}
    }

    output.unwrap().write_str(input.unwrap());
}

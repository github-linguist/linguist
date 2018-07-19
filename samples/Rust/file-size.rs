fn main() {
    let path_wd = Path::new("input.txt");
    println!("{}", path_wd.stat().size);

    let path_root = Path::new("/input.txt");
    println!("{}", path_root.stat().size);
}

// rust 0.9

fn main()
{
    let args = std::os::args();
    for arg in args.tail().iter()
    {
        let n = from_str::<u64>(*arg).unwrap();
        do std::task::spawn
        {
            std::io::timer::sleep(n);
            println!("{}", n);
        }
    }
}

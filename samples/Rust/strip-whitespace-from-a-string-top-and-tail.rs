// rust 0.9-pre

fn main()
{
    let string = "String without spaces";
    let spaces = " \x0B\t\r\n \xA0 \u2000 \u3000 ";
    let string_with_spaces = spaces + string + spaces;

    assert_eq!(string_with_spaces.trim(), string);
    assert_eq!(string_with_spaces.trim_left().to_owned(), string + spaces);
    assert_eq!(string_with_spaces.trim_right().to_owned(), spaces + string);
}

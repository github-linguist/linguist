import gleam/io
import gleam/io.{println}

/// triple slash comment
pub fn main() {
  "this is a string
  👩‍💻 こんにちは Gleam 💫
  \n\r\t\"\\"

  Nil

  let i: Int = -3 + 5_000 / 2 * 1 % 2
  let b = i > 0 || False && True
  let f = 3.14 -. 1.73;
  ;;

  let value: Bool = {
    "Hello"
    42 + 12
    False
  }
  let x = { i - 32 } * 5 / 9

  [1, 2, 3, 4]
  [1, ..[2, 3]]
  let x = [2, 3]
  let y = [1, ..x]

  let my_tuple = #(10, "hello")

  case my_tuple.0 {
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    n -> "Some other number" // This matches anything
  }

  let description =
    case y {
      [] -> "This list is empty"
      [a] -> "This list has 1 element"
      [a, b] -> "This list has 2 elements"
      _other -> "This list has more than 2 elements"
    }

  let [a, b] = [1, 5]
  case a, b {
    1, 1 -> "both are 1"
    1, _ -> "x is 1"
    _, 1 -> "y is 1"
    _, _ -> "neither is 1"
  }

  case [[0]] {
    [[a]] | [[a, _]] if a < 0 -> [0 - a]
    [[_, ..] as inner_list] -> inner_list
    other -> []
  }

  let add = fn(x, y) { x + y }
  let add1 = add(1, _)
  add1(5)
  assert 0 = { 5 |> add1 } - { 5 |> add(1) }

  println("hello world")
  io.print("hello world\n")
}

pub fn add(x: Int, y: Int) -> Int {
  x + y
}

pub fn twice(f: fn(t) -> t, x: t) -> t {
  x |> f |> f
}

fn list_of_two(my_value: a) -> List(a) {
  [my_value, my_value]
}

fn list_of_two_inferred(my_value) {
  [my_value, my_value]
}

pub fn labeled(
  in string: String,
  each pattern: String,
  with replacement: String,
) {
  labeled(in: string, each: pattern, with: replacement)
}

pub type Cat {
  Cat(name: String, cuteness: Int)
}

fn cats() {
  // Labelled fields can be given in any order
  let cat1 = Cat(name: "Nubi", cuteness: 2001)
  let cat2 = Cat(cuteness: 1805, name: "Biffy")

  // Alternatively fields can be given without labels
  let cat3 = Cat("Ginny", 1950)

  [cat1, cat2, cat3]
}

type User {
  LoggedIn(name: String)
  Guest
}

fn get_name(user) {
  case user {
    LoggedIn(name) -> name
    Guest -> "Guest user"
  }
}

type Score {
  Points(Int)
}

fn double_score(score) {
  let Points(p) = score
  p + score.0
}

pub opaque type Box(inner_type) {
  Box(inner: inner_type)
}

type Option(t) = Result(t, Nil)

pub type Person {
  Person(
    name: String,
    gender: Option(String),
    shoe_size: Int,
    age: Int,
    is_happy: Bool,
  )
}

pub fn have_birthday(person) {
  Person(..person, age: person.age + 1, is_happy: True)
}

pub fn unwrap(value) {
  assert Ok(value) = value
  value
}

pub fn try_twice(v1, v2) {
  try v1 = v1()
  try v2 = v2()
  Ok(#(v1, v2))
}

fn favourite_number() -> Int {
  todo("unsure") + todo
}

pub const start_year = 2101
pub const end_year: Int = 2111

fn bit_strings() {
  <<>>
  <<3>>
  <<3:8>>
  <<3:size(8)>>
  <<3:16>>
  <<0:4, 1:3, 1:1>> == <<3>>
  <<3:size(4)-unit(4)>> == <<3:size(16)>>
  <<"Hello Gleam 💫":utf8>>
  let a = <<0:1, 1:1, 1:1>>
  <<a:bit_string, 1:5>> == <<"a":utf8>>

  assert <<"hello":utf8, exclamation_point:8>> = <<"hello!":utf8>>
}

pub external fn random_float() -> Float = "rand" "uniform"

pub external fn inspect(a) -> a = "Elixir.IO" "inspect"

pub external fn any(in: List(a), satisfying: fn(a) -> Bool) =
  "my_external_module" "any"

pub external type Queue(a)

pub external fn new() -> Queue(a) = "queue" "new"

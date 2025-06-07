# Example showing Nickel functions and string interpolation
let greet = fun name => "Hello, %{name}!" in
let add = fun x y => x + y in
let list_example = [1, 2, 3] |> std.array.map (fun x => x * 2) in

{
  greeting = greet "World",
  sum = add 5 10,
  doubled_list = list_example,
  environment = "development",
  version = "1.0.0",
}
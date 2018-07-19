def map := [
  "a" => 1,
  "b" => 2,
  "c" => 3,
]

for key => value in map {
  println(`$key $value`)
}

for value in map {     # ignore keys
  println(`. $value`)
}

for key => _ in map {  # ignore values
  println(`$key .`)
}

for key in map.domain() {     # iterate over the set whose values are the keys
  println(`$key .`)
}

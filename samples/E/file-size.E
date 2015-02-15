for file in [<file:input.txt>, <file:///input.txt>] {
  println(`The size of $file is ${file.length()} bytes.`)
}

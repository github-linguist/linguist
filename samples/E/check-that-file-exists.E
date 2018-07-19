for file in [<file:input.txt>,
             <file:///input.txt>] {
  require(file.exists(),       fn { `$file is missing!` })
  require(!file.isDirectory(), fn { `$file is a directory!` })
}

for file in [<file:docs>,
             <file:///docs>] {
  require(file.exists(),      fn { `$file is missing!` })
  require(file.isDirectory(), fn { `$file is not a directory!` })
}

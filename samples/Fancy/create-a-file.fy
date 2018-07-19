["/", "./"] each: |dir| {
  # create '/docs', then './docs'
  Directory create: (dir ++ "docs")
  # create files /output.txt, then ./output.txt
  File open: (dir ++ "output.txt") modes: ['write] with: |f| {
    f writeln: "hello, world!"
  }
}

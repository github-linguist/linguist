set(list one.c two.c three.c)

foreach(file ${list})
  message(${file})
endforeach(file)

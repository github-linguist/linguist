makeItHail := method(n,
  stones := list(n)
  while (n != 1,
    if(n isEven,
      n = n / 2,
      n = 3 * n + 1
    )
    stones append(n)
  )
)

out := makeItHail(27)
writeln("For the sequence beginning at 27, the number of elements generated is ", out size, ".")
write("The first four elements generated are ")
for(i, 0, 3,
  write(out at(i), " ")
)
writeln(".")

write("The last four elements generated are ")
for(i, out size - 4, out size - 1,
  write(out at(i), " ")
)
writeln(".")

numOfElems := 0
nn := 3
for(x, 3, 100000,
  out = makeItHail(x)
  if(out size > numOfElems,
    numOfElems = out size
    nn = x
  )
)

writeln("For numbers less than or equal to 100,000, ", nn,
" has the longest sequence of ", numOfElems, " elements.")

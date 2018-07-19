inf := File with("input.txt") openForReading
outf := File with("output.txt") openForUpdating

while(l := inf readLine,
  outf write(l, "\n")
)

inf close
outf close

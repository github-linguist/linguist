def ls := makeCommand("ls")
ls("-l")

def [results, _, _] := ls.exec(["-l"])
when (results) -> {
  def [exitCode, out, err] := results
  print(out)
} catch problem {
  print(`failed to execute ls: $problem`)
}

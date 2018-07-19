import tables

proc sort(s: string): string =
  var
    i,j: int
    t: char

  result = s
  for i in 0 .. result.len - 1:
    j = i
    t = result[j]
    while(j > 0) and (result[j - 1] > t):
      result[j] = result[j - 1]
      dec(j)
    result[j] = t
# end sort

proc maxCount(an: TTable[string,seq[string]]): int =
  result = 0
  for v in an.values:
    if v.len > result:
      result = v.len
#end maxCount

proc showAnagrams(s: seq[string]) =
  for v in s:
    write(stdout,v)
    write(stdout," ")
  writeln(stdout,"")
#end showAnagrams

proc processFile: TTable[string,seq[string]] =
  var
    fd: TFile
    sline,line: string

  result = initTable[string,seq[string]]()
  if Open(fd,"unixdict.txt"):
    while not EndOfFile(fd):
      line = fd.readLine()
      sline = sort(line)
      if result.hasKey(sline):
        result[sline] = result[sline] & line
      else:
        result[sline] = @[line]

var
  anagrams:TTable[string,seq[string]] = processFile()
  max = anagrams.maxCount

for v in anagrams.values:
  if v.len == max: showAnagrams(v)

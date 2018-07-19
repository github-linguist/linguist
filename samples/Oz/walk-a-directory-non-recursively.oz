declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  Files = {Filter {Path.readdir "."} Path.isFile}
  Pattern = ".*\\.oz$"
  MatchingFiles = {Filter Files fun {$ File} {Regex.search Pattern File} \= false end}
in
  {ForAll MatchingFiles System.showInfo}

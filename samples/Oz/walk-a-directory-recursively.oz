declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  proc {WalkDirTree Root Pattern Proc}
     proc {Walk R}
        Entries = {Path.readdir R}
        Files = {Filter Entries Path.isFile}
        MatchingFiles = {Filter Files fun {$ File} {Regex.search Pattern File} \= false end}
        Subdirs = {Filter Entries Path.isDir}
     in
        {ForAll MatchingFiles Proc}
        {ForAll Subdirs Walk}
     end
  in
     {Walk Root}
  end
in
  {WalkDirTree "." ".*\\.oz$" System.showInfo}

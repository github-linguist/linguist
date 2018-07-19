declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
in
  {Show {Path.size "input.txt"}}
  {Show {Path.size "/input.txt"}}

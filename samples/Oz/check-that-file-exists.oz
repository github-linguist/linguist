declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
in
  {Show {Path.exists "docs"}}
  {Show {Path.exists "input.txt"}}
  {Show {Path.exists "/docs"}}
  {Show {Path.exists "/input.txt"}}

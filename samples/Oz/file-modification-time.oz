declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  Modified = {Path.mtime "input.txt"} %% posix time
in
  {Show {OsTime.localtime Modified}} %% human readable record

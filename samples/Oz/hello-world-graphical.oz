declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Window = {QTk.build td(label(text:"Goodbye, World!"))}
in
  {Window show}

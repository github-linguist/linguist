declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Button
  Window = {QTk.build td(button(text:"Click me" handle:Button))}
in
  {Window show}
  {Delay 500}
  {Tk.send event(generate Button "<ButtonPress-1>")}
  {Delay 500}
  {Tk.send event(generate Button "<ButtonRelease-1>")}

declare
  [QTk] = {Module.link ['x-oz://system/wp/QTk.ozf']}
  Entry
  Window = {QTk.build td(entry(handle:Entry))}
in
  {Window show}
  {Entry getFocus(force:true)}

  for C in "Hello, world!" do
     Key = if C == 32 then "<space>" else [C] end
  in
     {Delay 100}
     {Tk.send event(generate Entry Key)}
  end

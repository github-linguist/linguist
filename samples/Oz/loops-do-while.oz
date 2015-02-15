declare
  I = {NewCell 0}
in
  for until:@I mod 6 == 0 do
     I := @I + 1
     {Show @I}
  end

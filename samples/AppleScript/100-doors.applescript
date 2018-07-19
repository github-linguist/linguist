set is_open to {}
repeat 100 times
   set end of is_open to false
end
repeat with pass from 1 to 100
  repeat with door from pass to 100 by pass
    set item door of is_open to not item door of is_open
  end
end
set open_doors to {}
repeat with door from 1 to 100
   if item door of is_open then
     set end of open_doors to door
   end
end
set text item delimiters to ", "
display dialog "Open doors: " & open_doors

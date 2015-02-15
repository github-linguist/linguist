CopyFile := function(src, dst)
  local f, g, line;
  f := InputTextFile(src);
  g := OutputTextFile(dst, false);
  while true do
    line := ReadLine(f);
    if line = fail then
      break
    else
      WriteLine(g, Chomp(line));
    fi;
  od;
  CloseStream(f);
  CloseStream(g);
end;

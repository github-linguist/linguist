declare
  class TextFile from Open.file Open.text end

  In = {New TextFile init(name:"input.txt")}
  Out = {New TextFile init(name:"output.txt" flags:[write text create truncate])}

  proc {CopyAll In Out}
     case {In getS($)} of false then skip
     [] Line then
        {Out putS(Line)}
        {CopyAll In Out}
     end
  end
in
  {CopyAll In Out}
  {Out close}
  {In close}

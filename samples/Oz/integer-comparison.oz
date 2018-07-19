functor
import
  Application(exit)
  Open(text file)
define

Txt = class from Open.file Open.text end
Stdout = {New Open.file init(name:stdout)}
Stdin  = {New Txt init(name:stdin)}

proc{Print Msg}
  {Stdout write(vs:Msg)}
end

fun{GetInt Prompt}
  {Print Prompt}
  {StringToInt {Stdin getS($)}}
end

Int1 = {GetInt "Enter 1st Integer:"}
Int2 = {GetInt "Enter 2nd Integer:"}

if(Int1  < Int2) then {Print Int1#" less than "#Int2} end
if(Int1  > Int2) then {Print Int1#" greater than "#Int2} end
if(Int1 == Int2) then {Print Int1#" equal to "#Int2} end

{Application.exit 0}
end

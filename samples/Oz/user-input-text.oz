declare
  StdIn = {New class $ from Open.file Open.text end init(name:stdin)}
  StringInput
  Num = {NewCell 0}
in
  {System.printInfo "Enter a string: "}
  StringInput = {StdIn getS($)}

  for until:@Num == 75000 do
     {System.printInfo "Enter 75000: "}
     Line = {StdIn getS($)}
  in
     Num := try {String.toInt Line} catch _ then 0 end
  end

declare
  fun {Select Prompt Items}
     case Items of nil then ""
     else
	for
	   Item in Items
	   Index in 1..{Length Items}
	do
	   {System.showInfo Index#") "#Item}
	end
	{System.printInfo Prompt}
	try
	   {Nth Items {ReadInt}}
	catch _ then
	   {Select Prompt Items}
	end
     end
  end

  fun {ReadInt}
     class TextFile from Open.file Open.text end
     StdIo = {New TextFile init(name:stdin)}
  in
     {String.toInt {StdIo getS($)}}
  end

  Item = {Select "Which is from the three pigs: "
	  ["fee fie" "huff and puff" "mirror mirror" "tick tock"]}
	
in
  {System.showInfo "You chose: "#Item}

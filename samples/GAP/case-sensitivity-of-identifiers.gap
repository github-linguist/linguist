# GAP is case sensitive
ThreeDogs := function()
	local dog, Dog, DOG;
	dog := "Benjamin";
	Dog := "Samba";
	DOG := "Bernie";
	if dog = DOG then
		Print("There is just one dog named ", dog, "\n");
	else
		Print("The three dogs are named ", dog, ", ", Dog, " and ", DOG, "\n");
	fi;
end;

ThreeDogs();
# The three dogs are named Benjamin, Samba and Bernie

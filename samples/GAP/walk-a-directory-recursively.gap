Walk := function(name, op)
	local dir, file, e;
	dir := Directory(name);
	for e in SortedList(DirectoryContents(name)) do
		file := Filename(dir, e);
		if IsDirectoryPath(file) then
			if not (e in [".", ".."]) then
				Walk(file, op);
			fi;
		else
			op(file);
		fi;
	od;
end;

# This will print filenames
Walk(".", Display);

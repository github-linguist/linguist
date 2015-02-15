ReadByLines := function(name)
	local file, line, count;
	file := InputTextFile(name);
	count := 0;
	while true do
		line := ReadLine(file);
		if line = fail then
			break;
		fi;
		count := count + 1;
	od;
	CloseStream(file);
	return count;
end;

# With [http://www.ibiblio.org/pub/docs/misc/amnesty.txt amnesty.txt]
ReadByLines("amnesty.txt");
# 384

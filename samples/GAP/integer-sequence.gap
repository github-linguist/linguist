InfiniteLoop := function()
	local n;
	n := 1;
	while true do
		Display(n);
		n := n + 1;
	od;
end;

# Prepare some coffee
InfiniteLoop();

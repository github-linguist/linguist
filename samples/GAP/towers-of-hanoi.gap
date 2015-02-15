Hanoi := function(n)
	local move;
	move := function(n, a, b, c) # from, through, to
		if n = 1 then
			Print(a, " -> ", c, "\n");
		else
			move(n - 1, a, c, b);
			move(1, a, b, c);
			move(n - 1, b, a, c);
		fi;
	end;
	move(n, "A", "B", "C");
end;

Hanoi(1);
# A -> C

Hanoi(2);
# A -> B
# A -> C
# B -> C

Hanoi(3);
# A -> C
# A -> B
# C -> B
# A -> C
# B -> A
# B -> C
# A -> C

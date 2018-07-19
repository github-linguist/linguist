# Solution in '''RPN'''
Play24 := function()
	local input, digits, line, c, chars, stack, stackptr, cur, p, q, ok, a, b, run;
	input := InputTextUser();
	run := true;
	while run do
		digits := List([1 .. 4], n -> Random(1, 9));
		while true do
			Display(digits);
			line := ReadLine(input);
			line := Chomp(line);
			if line = "end" then
				run := false;
				break;
			elif line = "next" then
				break;
			else
				ok := true;
				stack := [ ];
				stackptr := 0;
				chars := "123456789+-*/ ";
				cur := ShallowCopy(digits);
				for c in line do
					if c = ' ' then
						continue;
					fi;
					p := Position(chars, c);
					if p = fail then
						ok := false;
						break;
					fi;
					if p < 10 then
						q := Position(cur, p);
						if q = fail then
							ok := false;
							break;
						fi;
						Unbind(cur[q]);
						stackptr := stackptr + 1;
						stack[stackptr] := p;
					else
						if stackptr < 2 then
							ok := false;
							break;
						fi;
						b := stack[stackptr];
						a := stack[stackptr - 1];
						stackptr := stackptr - 1;
						if c = '+' then
							a := a + b;
						elif c = '-' then
							a := a - b;
						elif c = '*' then
							a := a * b;
						elif c = '/' then
							if b = 0 then
								ok := false;
								break;
							fi;
							a := a / b;
						else
							ok := false;
							break;
						fi;
						stack[stackptr] := a;
					fi;
				od;
				if ok and stackptr = 1 and Size(cur) = 0 then
					if stack[1] = 24 then
						Print("Good !\n");
						break;
					else
						Print("Bad value: ", stack[1], "\n");
						continue;
					fi;
				fi;
				Print("Invalid expression\n");
			fi;
		od;
	od;
	CloseStream(input);
end;

# example session
# type "end" to quit the game, "next" to try another list of digits
gap> Play24();
[ 7, 6, 8, 5 ]
86*75-/
Good !
[ 5, 9, 2, 7 ]
end
gap>

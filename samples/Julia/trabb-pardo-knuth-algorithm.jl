f(x) = abs(x)^.5 + 5x^3
for i in map(parse_int,reverse(split(chomp(readline(STDIN)),' ')))
	println("$i: ",f(i)>400?"TOO LARGE":f(i))
end
1 2 3 4 5 6 7 8 9 10 11
11: TOO LARGE
10: TOO LARGE
9: TOO LARGE
8: TOO LARGE
7: TOO LARGE
6: TOO LARGE
5: TOO LARGE
4: 322.0
3: 136.73205080756887
2: 41.41421356237309
1: 6.0

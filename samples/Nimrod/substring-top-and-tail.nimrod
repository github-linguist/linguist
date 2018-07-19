var s = "The quick μ brown fox"
echo(s.substr(1))
echo(s.substr(0,s.len-2))
echo(s.substr(1,s.len-2))
# using slices
echo(s[1 .. -2])

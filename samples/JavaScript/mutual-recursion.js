function F(n)
{
 return n === 0 ? 1 : n - M(F(n - 1));
}

function M(n)
{
 return n === 0 ? 0 : n - F(M(n - 1));
}

var
 out = {F: [], M: []},
 i;
for (i = 0; i < 20; i++)
{
 out.F.push(F(i));
 out.M.push(M(i));
}
print(out.F + "\n" + out.M);

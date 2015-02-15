--if-then-elseif-then-else
if a then
  b()
elseif c then
  d()
else
  e()
end

for var = start, _end, step do --note: end is a reserved word
  something()
end

for var, var2, etc in iteratorfunction do
  something()
end

while somethingistrue() do
  something()
end

repeat
  something()
until somethingistrue()

cases = {
key1 = dothis,
key2 = dothat,
key3 = dotheother
}

cases[key]() --equivalent to dothis(), dothat(), or dotheother() respectively

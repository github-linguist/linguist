romans = {
{1000, "M"},
{900, "CM"}, {500, "D"}, {400, "CD"}, {100, "C"},
{90, "XC"}, {50, "L"}, {40, "XL"}, {10, "X"},
{9, "IX"}, {5, "V"}, {4, "IV"}, {1, "I"} }

k = io.read() + 0
for _, v in ipairs(romans) do --note that this is -not- ipairs.
  val, let = unpack(v)
  while k >= val do
    k = k - val
	io.write(let)
  end
end
print()

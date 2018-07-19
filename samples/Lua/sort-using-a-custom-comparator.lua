function pair(a, b) return a[1] < b[1] end

t = {
{2, 5}, {1, 6}, {4, 8}, {3, 2}
}
table.sort(t, pair)
for i, v in ipairs(t) do print(unpack(v)) end

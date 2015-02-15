local table = {
    ["foo"] = "bar",
    ["baz"] = 6,
    42 = 7,
}
for key,val in pairs(table) do
    print(string.format("%s: %s\n", key, val)
end

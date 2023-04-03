local array = {1, 2, 3, 4}
assert(array[1] == 1)

table.insert(array, "hello")
table.insert(array, "world")
print(array[5], array[6])

for k, v in array {
	print(k, v)
}

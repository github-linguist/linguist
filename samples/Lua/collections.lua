collection = {0, '1'}
print(collection[1]) -- prints 0

collection = {["foo"] = 0, ["bar"] = '1'} -- a collection of key/value pairs
print(collection["foo"]) -- prints 0
print(collection.foo) -- syntactic sugar, also prints 0

collection = {0, '1', ["foo"] = 0, ["bar"] = '1'}

collection = [0, '1']                 # Lists are mutable (editable) and can be sorted in place
x = collection[0]                     # accessing an item (which happens to be a numeric 0 (zero)
collection.append(2)                  # adding something to the end of the list
collection.insert(0, '-1')            # inserting a value into the beginning
y = collection[0]                     # now returns a string of "-1"
collection.extend([2,'3'])            # same as [collection.append(i) for i in [2,'3']] ... but faster
collection += [2,'3']                 # same as previous line
collection[2:6]                       # a "slice" (collection of the list elements from the third up to but not including the sixth)
len(collection)                       # get the length of (number of elements in) the collection
collection = (0, 1)                   # Tuples are immutable (not editable)
collection[:]                         # ... slices work on these too; and this is equivalent to collection[0:len(collection)]
collection[-4:-1]                     # negative slices count from the end of the string
collection[::2]                       # slices can also specify a stride --- this returns all even elements of the collection
collection="some string"              # strings are treated as sequences of characters
x = collection[::-1]                  # slice with negative step returns reversed sequence (string in this case).
collection[::2] == "some string"[::2] # True, literal objects don't need to be bound to name/variable to access slices or object methods
collection.__getitem__(slice(0,len(collection),2))  # same as previous expressions.
collection = {0: "zero", 1: "one"}    # Dictionaries (Hash)
collection['zero'] = 2                # Dictionary members accessed using same syntax as list/array indexes.
collection = set([0, '1'])            # sets (Hash)

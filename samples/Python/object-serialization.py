# Object Serialization in Python
# serialization in python is accomplished via the Pickle module.
# Alternatively, one can use the cPickle module if speed is the key,
# everything else in this example remains the same.

import pickle

class Entity:
	def __init__(self):
		self.name = "Entity"
	def printName(self):
		print self.name

class Person(Entity): #OldMan inherits from Entity
	def __init__(self): #override constructor
		self.name = "Cletus"

instance1 = Person()
instance1.printName()

instance2 = Entity()
instance2.printName()

target = file("objects.dat", "w") # open file

#  Serialize
pickle.dump((instance1, instance2), target) # serialize `instance1` and `instance2`to `target`
target.close() # flush file stream
print "Serialized..."

# Unserialize
target = file("objects.dat") # load again
i1, i2 = pickle.load(target)
print "Unserialized..."

i1.printName()
i2.printName()

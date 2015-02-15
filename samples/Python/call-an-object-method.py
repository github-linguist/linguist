class MyClass(object):
	@classmethod
	def myClassMethod(self, x):
		pass
	@staticmethod
	def myStaticMethod(x):
		pass
	def myMethod(self, x):
		return 42 + x

myInstance = MyClass()

# Instance method
myInstance.myMethod(someParameter)
# A method can also be retrieved as an attribute from the class, and then explicitly called on an instance:
MyClass.myMethod(myInstance, someParameter)


# Class or static methods
MyClass.myClassMethod(someParameter)
MyClass.myStaticMethod(someParameter)
# You can also call class or static methods on an instance, which will simply call it on the instance's class
myInstance.myClassMethod(someParameter)
myInstance.myStaticMethod(someParameter)

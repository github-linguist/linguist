class myClass
{
	Method(someParameter){
		MsgBox % SomeParameter
	}
}

myClass.method("hi")
myInstance := new myClass
myInstance.Method("bye")

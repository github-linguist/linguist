#pragma rtGlobals=3

Function FooBar()
	return 0
End

Function FooBarSubType() : ButtonControl
	return 0
End

Function/D FooBarVar()
	return 0
End

static Function FooBarStatic()
	return 0
End

threadsafe static Function FooBarStaticThreadsafe()
	return 0
End

threadsafe Function FooBarThread()
	return 0
End

Function CallOperationsAndBuiltInFuncs(string var)

	string someDQString = "abcd"

	Make/N=(1,2,3,4) root:myWave/WAVE=myWave
	Redimension/N=(-1,-1,-1,5) myWave

	print strlen(someDQString)

	return 0
End


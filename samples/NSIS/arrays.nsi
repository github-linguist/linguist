!include NSISArray.nsh
Function ArrayTest
	Push $0
	; Declaring an array
	NSISArray::New TestArray 1 2
	NSISArray::Push TestArray "Hello"
	; NSISArray arrays are dynamic by default.
	NSISArray::Push TestArray "World"
	NSISArray::Read TestArray 1
	Pop $0
	DetailPrint $0
	Pop $0
FunctionEnd

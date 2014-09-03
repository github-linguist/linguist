#pragma rtGlobals=3

StrConstant myConstString="abcd"
// some comment
constant myConst=123

Structure struct1
	string str
	variable var
EndStructure

static Structure struct2
	string str
	variable var
EndStructure

#include "someFile"

#ifdef NOT_DEFINED
	// conditional compilation
#endif

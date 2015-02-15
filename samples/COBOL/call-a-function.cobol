CALL "No-Arguments"

*> Fixed number of arguments.
CALL "2-Arguments" USING Foo Bar

CALL "Optional-Arguments" USING Foo
CALL "Optional-Arguments" USING Foo Bar
*> If an optional argument is omitted and replaced with OMITTED, any following
*> arguments can still be specified.
CALL "Optional-Arguments" USING Foo OMITTED Bar
*> Interestingly, even arguments not marked as optional can be omitted without
*> a compiler warning. It is highly unlikely the function will still work,
*> however.
CALL "2-Arguments" USING Foo

*> COBOL does not support a variable number of arguments, or named arguments.

*> Values to return can be put in either one of the arguments or, in OpenCOBOL,
*> the RETURN-CODE register.
*> A standard function call cannot be done in another statement.
CALL "Some-Func" USING Foo
MOVE Return-Code TO Bar

*> Intrinsic functions can be used in any place a literal value may go (i.e. in
*> statements) and are optionally preceded by FUNCTION.
*> Intrinsic functions that do not take arguments may optionally have a pair of
*> empty parentheses.
*> Intrinsic functions cannot be defined by the user.
MOVE FUNCTION PI TO Bar
MOVE FUNCTION MEDIAN(4, 5, 6) TO Bar

*> Built-in functions/subroutines typically have prefixes indicating which
*> compiler originally incorporated it:
*>  - C$      - ACUCOBOL-GT
*>  - CBL_    - Micro Focus
*>  - CBL_OC_ - OpenCOBOL
*> Note: The user could name their functions similarly if they wanted to.
CALL "C$MAKEDIR" USING Foo
CALL "CBL_CREATE_DIR" USING Foo
CALL "CBL_OC_NANOSLEEP" USING Bar
*> Although some built-in functions identified by numbers.
CALL X"F4" USING Foo Bar

*> Parameters can be passed in 3 different ways:
*>  - BY REFERENCE - this is the default way in OpenCOBOL and this clause may
*>       be omitted. The address of the argument is passed to the function.
*>       The function is allowed to modify the variable.
*>  - BY CONTENT - a copy is made and the function is passed the address
*>      of the copy, which it can then modify. This is recomended when
*>      passing a literal to a function.
*>  - BY VALUE - the function is passed the address of the argument (like a
*>      pointer). This is mostly used to provide compatibility with other
*>      languages, such as C.
CALL "Modify-Arg" USING BY REFERENCE Foo *> Foo is modified.
CALL "Modify-Arg" USING BY CONTENT Foo   *> Foo is unchanged.
CALL "C-Func" USING BY VALUE Bar

*> Partial application is impossible as COBOL does not support first-class
*> functions.
*> However, as functions are called using a string of their PROGRAM-ID,
*> you could pass a 'function' as an argument to another function, or store
*> it in a variable, or get it at runtime.
ACCEPT Foo *> Get a PROGRAM-ID from the user.
CALL "Use-Func" USING Foo
CALL Foo USING Bar

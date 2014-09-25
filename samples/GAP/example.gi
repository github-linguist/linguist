#############################################################################
##
#W  example.gd
##
##  This file contains a sample of a GAP implementation file.
##


#############################################################################
##
#M  SomeOperation( <val> )
##
##  performs some operation on <val>
##
InstallMethod( SomeProperty,
    "for left modules",
    [ IsLeftModule ], 0,
    function( M )
    if IsFreeLeftModule( M ) and not IsTrivial( M ) then
      return true;
    fi;
    TryNextMethod();
    end );



#############################################################################
##
#F  SomeGlobalFunction( )
##
##  A global variadic funfion.
##
InstallGlobalFunction( SomeGlobalFunction, function( arg )
    if Length( arg ) = 3 then
      return arg[1] + arg[2] * arg[3];
    elif Length( arg ) = 2 then
      return arg[1] - arg[2]
    else
      Error( "usage: SomeGlobalFunction( <x>, <y>[, <z>] )" );
    fi;
    end );


#
# A plain function.
#
SomeFunc := function(x, y)
    local z, func, tmp, j;
    z := x * 1.0;
    y := 17^17 - y;
    func := a -> a mod 5;
    tmp := List( [1..50], func );
    while y > 0 do
        for j in tmp do
            Print(j, "\n");
        od;
        repeat
            y := y - 1;
        until 0 < 1;
        y := y -1;
    od;
    return z;
end;
        
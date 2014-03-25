/*
APPROACH
0 - start point
1 - end point
2 - max change
Returns 0 shifted toward 1 by 2, without crossing 0.
Note that if argument2 is negative, you will move AWAY from 0.
Ex:
approach( 5, 0, 2 )     = 3
approach( -5, 0, 2 )    = -3
approach( 1, 0, 1 )     = 0
approach( 1, 0, 2 )     = 0
*/
if (argument0 < argument1)
    return min(argument0 + argument2, argument1); 
else
    return max(argument0 - argument2, argument1);
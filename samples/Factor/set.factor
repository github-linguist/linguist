( scratchpad ) USE: sets
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } union .
HS{ 2 3 4 5 6 7 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } intersect .
HS{ 5 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } diff .
HS{ 2 3 4 }
( scratchpad ) HS{ 2 5 4 3 } HS{ 5 6 7 } subset? .
f
( scratchpad ) HS{ 5 6 } HS{ 5 6 7 } subset? .
t
( scratchpad ) HS{ 5 6 } HS{ 5 6 7 } set= .
f
( scratchpad ) HS{ 6 5 7 } HS{ 5 6 7 } set= .
t

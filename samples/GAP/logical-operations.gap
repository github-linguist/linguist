Logical := function(a, b)
    return [ a or b, a and b, not a ];
end;

Logical(true, true);
# [ true, true, false ]

Logical(true, false);
# [ true, false, false ]

Logical(false, true);
# [ true, false, true ]

Logical(false, false);
# [ false, false, true ]

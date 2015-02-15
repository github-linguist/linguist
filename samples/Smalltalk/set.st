#(1 2 3) asSet union: #(2 3 4) asSet.
"a Set(1 2 3 4)"

#(1 2 3) asSet intersection: #(2 3 4) asSet.
"a Set(2 3)"

#(1 2 3) asSet difference: #(2 3 4) asSet.
"a Set(1)"

#(1 2 3) asSet includesAllOf: #(1 3) asSet.
"true"

#(1 2 3) asSet includesAllOf: #(1 3 4) asSet.
"false"

#(1 2 3) asSet = #(2 1 3) asSet.
"true"

#(1 2 3) asSet = #(1 2 4) asSet.
"false"

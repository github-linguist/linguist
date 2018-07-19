(* solution 1 *)
rep[n_Integer,s_String]:=Apply[StringJoin,ConstantArray[s,{n}]]

(* solution 2 -- @@ is the infix form of Apply[] *)
rep[n_Integer,s_String]:=StringJoin@@Table[s,{n}]

(* solution 3 -- demonstrating another of the large number of looping constructs available *)
rep[n_Integer,s_String]:=Nest[StringJoin[s, #] &,s,n-1]

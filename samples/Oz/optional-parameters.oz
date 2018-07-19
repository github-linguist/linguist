declare
  class Table
     attr
        rows

     meth init(Rows)
        rows := Rows
     end

     meth sort(ordering:O<=Lexicographic  column:C<=1  reverse:R<=false)
        fun {Predicate Row1 Row2}
           Res = {O {Nth Row1 C} {Nth Row2 C}}
        in
           if R then {Not Res} else Res end
        end
     in
        rows := {Sort @rows Predicate}
     end
  end

  fun {Lexicographic As Bs}  %% omitted for brevity
  end

  T = {New Table init([["a" "b" "c"] ["" "q" "z"] ["zap" "zip" "Zot"]])}
in
  {T sort}
  {T sort(column:3)}
  {T sort(column:2)}
  {T sort(column:2 reverse:true)}
  {T sort(ordering:fun {$ A B} {Length B} < {Length A} end)}

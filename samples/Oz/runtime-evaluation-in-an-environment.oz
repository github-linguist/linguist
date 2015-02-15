declare
   fun {EvalWithX Program A B}
      {Compiler.evalExpression Program env('X':B) _}
      -
      {Compiler.evalExpression Program env('X':A) _}
   end
in
   {Show {EvalWithX "{Exp X}" 0.0 1.0}}

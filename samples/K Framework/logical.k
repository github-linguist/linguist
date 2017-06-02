module C-EXPR-LOGICAL
     imports INT
     imports K-EQUAL
     imports BITS-SYNTAX
     imports C-DYNAMIC-SYNTAX
     imports C-SEQUENCE-POINT-SYNTAX
     imports C-SYNTAX
     imports C-TYPING-SYNTAX

     /*@ Here, we wrapped the controlling expressions with
     \kinline{simplifyTruth} when heating them, so that we are guaranteed
     the values in those locations are either \kinline{tv(0, int)} or
     \kinline{tv(1, int)}.*/

     /*@ \fromStandard{\source[n1570]{\para{6.5.13}{3--4}}}{
     The \cinline{&&} operator shall yield 1 if both of its operands compare
     unequal to 0; otherwise, it yields 0. The result has type \cinline{int}.

     Unlike the bitwise binary \cinline{&} operator, the \cinline{&&} operator
     guarantees left-to-right evaluation; if the second operand is evaluated,
     there is a sequence point between the evaluations of the first and second
     operands. If the first operand compares equal to 0, the second operand is
     not evaluated.
     } */
     context (HOLE:KItem => reval(HOLE)) && _ [ndheat, result(RValue)]
     rule tv(0, ut(_, int)) && _ => tv(0, utype(int))
          [structural]
     rule tv(1, ut(_, int)) && E:K
          => sequencePoint ~> E
          [structural]
     rule tv((unknown(I:Int) => #if I ==Int 0 #then 0 #else 1 #fi), ut(_, int)) && _

     /*@ \fromStandard{\source[n1570]{\para{6.5.14}{3--4}}}{
     The \cinline{||} operator shall yield 1 if either of its operands compare
     unequal to 0; otherwise, it yields 0. The result has type \cinline{int}.

     Unlike the bitwise \cinline{|} operator, the \cinline{||} operator
     guarantees left-to-right evaluation; if the second operand is evaluated,
     there is a sequence point between the evaluations of the first and second
     operands. If the first operand compares unequal to 0, the second operand
     is not evaluated.
     }*/
     context (HOLE:KItem => reval(HOLE)) || _ [ndheat, result(RValue)]
     rule tv(0, ut(_, int)) || E:K
          => sequencePoint ~> E
          [structural]
     rule tv(1, ut(_, int)) || _ => tv(1, utype(int))
          [structural]
     rule tv((unknown(I:Int) => #if I ==Int 0 #then 0 #else 1 #fi), ut(_, int)) || _

endmodule

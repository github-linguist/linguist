module C-EXPR-INC-AND-DEC
     imports C-DYNAMIC-SYNTAX

     /*@ \fromStandard{\source[n1570]{\para{6.5.2.4}{2}}}{
     The result of the postfix \cinline{++} operator is the value of the
     operand. As a side effect, the value of the operand object is incremented
     (that is, the value 1 of the appropriate type is added to it). See the
     discussions of additive operators and compound assignment for information
     on constraints, types, and conversions and the effects of operations on
     pointers. The value computation of the result is sequenced before the side
     effect of updating the stored value of the operand. With respect to an
     indeterminately-sequenced function call, the operation of postfix
     \cinline{++} is a single evaluation. Postfix \cinline{++} on an object
     with atomic type is a read-modify-write operation with
     \cinline{memory_order_seq_cst} memory order semantics.
     }*/

     /*@ \fromStandard{\source[n1570]{\para{6.5.2.4}{3}}}{
     The postfix \cinline{--} operator is analogous to the postfix \cinline{++}
     operator, except that the value of the operand is decremented (that is,
     the value 1 of the appropriate type is subtracted from it).
     }*/

     rule postOp(LV:KResult, Op:K) => #postOp(LV, fillLHoles(LV, Op))
          [structual]

     syntax KItem ::= #postOp(K, K)
     context #postOp((HOLE:KItem => reval(HOLE)), _) [result(RValue)]
     rule #postOp(V:RValue, Op:K) => fillRHoles(V, Op) ~> discard ~> V
          [structural]

endmodule


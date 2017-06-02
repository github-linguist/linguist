module C-EXPR-SEQUENCING
     imports C-SEQUENCE-POINT-SYNTAX
     imports C-DYNAMIC-SYNTAX

     /*@ \fromStandard{\source[n1570]{\para{6.5.17}{2}}}{
     The left operand of a comma operator is evaluated as a void expression;
     there is a sequence point between its evaluation and that of the right
     operand. Then the right operand is evaluated; the result has its type and
     value.
     }*/

     rule _:RValue ~> comma => sequencePoint
          [structural]

endmodule

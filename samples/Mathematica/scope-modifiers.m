Module -> localize names of variables (lexical scoping)
Block  -> localize values of variables (dynamic scoping)

Module creates new symbols:

Module[{x}, Print[x];
 Module[{x}, Print[x]]
]

->x$119
->x$120

Block localizes values only; it does not create new symbols:

x = 7;
Block[{x=0}, Print[x]]
Print[x]
->0
->7

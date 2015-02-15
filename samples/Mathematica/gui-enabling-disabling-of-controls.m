Manipulate[Null, {{value, 0},
  InputField[Dynamic[value], Number,
    Enabled -> Dynamic[value == 0]] &},
 Row@{Button["increment", value++, Enabled -> Dynamic[value < 10]],
   Button["decrement", value--, Enabled -> Dynamic[value > 0]]}]

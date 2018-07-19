Manipulate[Null, {{value, 0}, InputField[Dynamic[value], Number] &},
 Row@{Button["increment", value++],
   Button["random",
    If[DialogInput[
      Column@{"Are you sure?",
        Row@{Button["Yes", DialogReturn[True]],
          Button["No", DialogReturn[False]]}}],
     value = RandomInteger@10000], Method -> "Queued"]}]

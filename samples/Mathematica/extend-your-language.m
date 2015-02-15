if2[c1_,c2_]:=Which[
          c1&&c2, bothConditionsAreTrue[],
          c1, firstConditionIsTrue[],
          c2, secondConditionIsTrue[],
          True,noConditionIsTrue]

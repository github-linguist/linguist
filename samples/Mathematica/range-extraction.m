rangeExtract[data_List] := ToString[Row[
                               Riffle[
                                  Flatten[Split[Sort[data], #2 - #1 == 1 &] /. {a_Integer, __, b_} :> Row[{a, "-", b}]],
                                     ","]
                                     ]];

MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

SomeFile := DATASET([   {'C', 'G'},
                        {'C', 'C'},
                        {'A', 'X'},
                        {'B', 'G'},
                        {'A', 'B'}], MyRec);

Val1Sort := SORT(SomeFile, Value1);
Val2Sort := SORT(SomeFile, Value2);

Dedup1   := DEDUP(Val1Sort, LEFT.Value1 = RIGHT.Value1);

/* Result set is:
    Rec#    Value1    Value2
    1        A        X
    2        B        G
    3        C        G
*/

Dedup2 := DEDUP(Val2Sort, LEFT.Value2 = RIGHT.Value2);

/* Result set is:
    Rec#    Value1    Value2
    1        A        B
    2        C        C
    3        C        G
    4        A        X
*/

Dedup3 := DEDUP(Val1Sort, LEFT.Value1 = RIGHT.Value1,RIGHT);

/* Result set is:
    Rec#    Value1    Value2
    1        A        B
    2        B        G
    3        C        C
*/

Dedup4 := DEDUP(Val2Sort, LEFT.Value2 = RIGHT.Value2,RIGHT);

/* Result set is:
    Rec#    Value1    Value2
    1        A        B
    2        C        C
    3        B        G
    4        A        X
*/

output(Dedup1);
output(Dedup2);
output(Dedup3);
output(Dedup4);

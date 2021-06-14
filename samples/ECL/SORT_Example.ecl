MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

SomeFile := DATASET([{'C', 'G'}, 
                     {'C', 'C'}, 
                     {'A', 'X'}, 
                     {'B', 'G'}, 
                     {'A', 'B'}], MyRec);

SortedRecs1 := SORT(SomeFile, Value1, Value2);
SortedRecs2 := SORT(SomeFile, -Value1, Value2);
SortedRecs3 := SORT(SomeFile, Value1, -Value2);
SortedRecs4 := SORT(SomeFile, -Value1, -Value2);
SortedRecs5 := SORT(SomeFile, Value2, Value1);
SortedRecs6 := SORT(SomeFile, -Value2, Value1);
SortedRecs7 := SORT(SomeFile, Value2, -Value1);
SortedRecs8 := SORT(SomeFile, -Value2, -Value1);

OUTPUT(SortedRecs1);
OUTPUT(SortedRecs2);
OUTPUT(SortedRecs3);
OUTPUT(SortedRecs4);
OUTPUT(SortedRecs5, {Value2, Value1});
OUTPUT(SortedRecs6, {Value2, Value1});
OUTPUT(SortedRecs7, {Value2, Value1});
OUTPUT(SortedRecs8, {Value2, Value1});

/*
SortedRecs1 results in:
    Rec#    Value1    Value2
    1        A        B
    2        A        X
    3        B        G
    4        C        C
    5        C        G

SortedRecs2 results in:
    Rec#    Value1    Value2
    1        C        C
    2        C        G
    3        B        G
    4        A        B
    5        A        X

SortedRecs3 results in:
    Rec#    Value1    Value2
    1        A        X
    2        A        B
    3        B        G
    4        C        G
    5        C        C

SortedRecs4 results in:
    Rec#    Value1    Value2
    1        C        G
    2        C        C
    3        B        G
    4        A        X
    5        A        B
*/

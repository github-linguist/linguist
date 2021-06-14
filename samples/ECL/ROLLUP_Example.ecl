MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
    UNSIGNED1 Value3;
END;

SomeFile := DATASET([   {'C', 'G', 1}, 
                        {'C', 'C', 2}, 
                        {'A', 'X', 3}, 
                        {'B', 'G', 4}, 
                        {'A', 'B', 5}], MyRec);

SortedTable := SORT(SomeFile, Value1);
OUTPUT(SortedTable);

RECORDOF(SomeFile) RollThem(SomeFile L, MyRec R) := TRANSFORM
    SELF.Value3 := IF(L.Value3 < R.Value3, L.Value3, R.Value3);
    SELF.Value2 := IF(L.Value2 < R.Value2, L.Value2, R.Value2);
    SELF := L; 
END;

RolledUpRecs := ROLLUP( SortedTable, 
                        LEFT.Value1 = RIGHT.Value1, 
                        RollThem(LEFT, RIGHT));

OUTPUT(RolledUpRecs );
 
/*
Processes as:
    LEFT    vs.     RIGHT
    1 (AX3)         2 (AB5)        - match, run transform, output AB3
    1 (AB3)         3 (BG4)        - no match, output BG4
    3 (BG4)         4 (CX1)        - no match 
    4 (CX1)         5 (CC2)        - match, run transform, output CC1

Result set is:
    Rec#    Value1    Value2    Value3
    1       A         B         3
    2       B         G         4
    3       C         C         1
*/

FlatRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
    STRING1 CVal2_1;
    STRING1 CVal2_2;
END;

FlatFile := DATASET([{'C', 'A', 'X', 'W'}, 
                     {'B', 'B', 'S', 'Y'}, 
                     {'A', 'C', 'Z', 'T'}], FlatRec);

OutRec := RECORD
    FlatFile.Value1;
    FlatFile.Value2;
END;
P_Recs := TABLE(FlatFile, OutRec);

OUTPUT(P_Recs, NAMED('ParentData'));
/*
P_Recs result set is:
    Rec#     Value1   Value2
    1        C        A
    2        B        B    
    3        A        C
*/

OutRec NormThem(FlatRec L, INTEGER C) := TRANSFORM
    SELF.Value2 := CHOOSE(C, L.CVal2_1, L.CVal2_2);
    SELF := L;
END;
ChildRecs := NORMALIZE(FlatFile, 2, NormThem(LEFT, COUNTER));

OUTPUT(ChildRecs, NAMED('ChildData'));
/*
ChildRecs result set is:
    Rec#     Value1   Value2
    1        C        X
    2        C        W
    3        B        S
    4        B        Y    
    5        A        Z
    6        A        T
*/
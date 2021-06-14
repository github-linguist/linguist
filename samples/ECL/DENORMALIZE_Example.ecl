MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

ParentFile := DATASET([ {'C', 'A'}, 
                        {'B', 'B'}, 
                        {'A', 'C'}], MyRec);
ChildFile := DATASET([  {'C', 'X'}, 
                        {'B', 'S'}, 
                        {'C', 'W'}, 
                        {'B', 'Y'}, 
                        {'A', 'Z'}, 
                        {'A', 'T'}], MyRec);

MyOutRec := RECORD
    ParentFile.Value1;
    ParentFile.Value2;
    STRING1 CVal2_1 := '';
    STRING1 CVal2_2 := '';
END;

P_Recs := TABLE(ParentFile, MyOutRec);
OUTPUT(P_Recs, NAMED('ParentDataReady'));
/* P_Recs result set is:
    Rec#    Value1 PVal2    CVal2_1 CVal2_2
    1       C               A
    2       B               B
    3       A               C
*/

MyOutRec DeNormThem(MyOutRec L, MyRec R, INTEGER C) := TRANSFORM
    SELF.CVal2_1 := IF(C = 1, R.Value2, L.CVal2_1);
    SELF.CVal2_2 := IF(C = 2, R.Value2, L.CVal2_2);
    SELF := L;
END;

DeNormedRecs := DENORMALIZE(P_Recs, ChildFile, 
                            LEFT.Value1 = RIGHT.Value1, 
                            DeNormThem(LEFT, RIGHT, COUNTER));

OUTPUT(DeNormedRecs, NAMED('NestedChildDataset'));

/* DeNormedRecs result set is:
    Rec#    Value1  PVal2   CVal2_1 CVal2_2
    1       A       C       Z       T
    2       B       B       S       Y
    3       C       A       X       W
*/

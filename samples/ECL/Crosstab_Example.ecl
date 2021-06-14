MyRec := RECORD
    STRING1  Value1;
    STRING1  Value2;
    INTEGER1 Value3;
END;

SomeFile := DATASET([{'C', 'G', 1},
                     {'C', 'C', 2},
                     {'A', 'X', 3},
                     {'B', 'G', 4},
                     {'A', 'B', 5}], MyRec);

MyOutRec := RECORD
    SomeFile.Value1;
    GrpCount := COUNT(GROUP);
    GrpSum   := SUM(GROUP, SomeFile.Value3);
    AveSum   := AVE(GROUP, SomeFile.Value3);
END;

MyTable := TABLE(SomeFile, MyOutRec, Value1);

OUTPUT(MyTable);

/* MyTable result set is:
    Rec#    Value1  GrpCount    GrpSum
    1       C       2           3
    2       A       2           8
    3       B       1           4
*/

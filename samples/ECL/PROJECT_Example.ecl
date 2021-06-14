MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

SomeFile := DATASET([{'C', 'G'}, 
                     {'C', 'C'}, 
                     {'A', 'X'}, 
                     {'B', 'G'}, 
                     {'A', 'B'}], MyRec);

MyOutRec := RECORD
    myRec.Value1;
    SomeFile.Value2;
    STRING4 CatValues;
END;

MyOutRec CatThem(SomeFile L, INTEGER C) := TRANSFORM
    SELF.CatValues := L.Value1 + L.Value2 + '-' + (STRING)C;
    SELF := L;
END;

CatRecs := PROJECT(SomeFile, 
                   CatThem(LEFT, COUNTER));

OUTPUT(CatRecs);

/* CatRecs result set is:
    Rec#     Value1   Value2   CatValues
    1        C        G        CG-1
    2        C        C        CC-2
    3        A        X        AX-3
    4        B        G        BG-4
    5        A        B        AB-5
*/

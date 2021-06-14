MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
    INTEGER1 Value3;
END;

SomeFile := DATASET([{'C', 'G', 1}, 
                     {'C', 'C', 2}, 
                     {'A', 'X', 3}, 
                     {'B', 'G', 4}, 
                     {'A', 'B', 5}], MyRec);

SortedRecs1 := SORT(SomeFile, Value1, Value2);
SortedRecs2 := SORT(SomeFile, -Value1, Value2);
SortedRecs3 := SORT(SomeFile, Value1, -Value2);
SortedRecs4 := SORT(SomeFile, -Value1, -Value2);

SortedRecs1[1].Value3;                    //result = 5
SortedRecs2[1].Value3;                    //result = 2
SortedRecs3[1].Value3;                    //result = 3
SortedRecs4[1].Value3;                    //result = 1
EVALUATE(SortedRecs1[1], Value3 + 10);    //result = 15
EVALUATE(SortedRecs2[1], Value3 + 10);    //result = 12
EVALUATE(SortedRecs3[1], Value3 + 10);    //result = 13
EVALUATE(SortedRecs4[1], Value3 + 10);    //result = 11

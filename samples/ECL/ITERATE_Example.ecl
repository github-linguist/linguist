MyRec := RECORD
    INTEGER2 Value1;
    INTEGER2 Value2;
END;

SomeFile := DATASET([{10, 0}, 
                     {20, 0}, 
                     {30, 0}, 
                     {40, 0}, 
                     {50, 0}], MyRec);

MyRec AddThem(MyRec L, MyRec R) := TRANSFORM
    SELF.Value2 := L.Value2 + R.Value1;
    SELF := R;
END;

AddedRecs := ITERATE(SomeFile, AddThem(LEFT, RIGHT));

output(AddedRecs);

/* Processes as:
    LEFT.Value2     RIGHT.Value1
    0 (0)           1 (10)            - 0 + 10 = 10
    1 (10)          2 (20)            - 10 + 20 = 30
    2 (30)          3 (30)            - 30 + 30 = 60
    3 (60)          4 (40)            - 60 + 40 = 100
    4 (100)         5 (50)            - 100 + 50 = 150

AddedRecs result set is:
    Rec#    Value1    Value2
    1        10        10
    2        20        30    
    3        30        60
    4        40        100
    5        50        150
*/

MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

SomeFile := GROUP(DATASET([    {'C', 'G'},
                            {'C', 'C'},
                            {'A', 'X'},
                            {'B', 'G'},
                            {'A', 'B'}], MyRec), TRUE);

Dedup1 := DEDUP(SomeFile, 
                LEFT.Value2 IN ['G', 'C', 'X'] AND 
                RIGHT.Value2 IN ['X', 'B', 'C'], ALL);

/*
Processes as:   LEFT    vs.     RIGHT
                1 (G)           2 (C)    - lose 2 (RIGHT rec)
                1 (G)           3 (X)    - lose 3 (RIGHT rec)
                1 (G)           4 (G)    - keep RIGHT rec 4
                1 (G)           5 (B)    - lose 5 (RIGHT rec)

                4 (G)           1 (G)    - keep RIGHT rec 1 

Result set is:
    Rec#    Value1    Value2
    1        C        G
    4        B        G
*/

Dedup2 := DEDUP(SomeFile, 
                LEFT.Value2 IN ['G', 'C'] AND 
                RIGHT.Value2 IN ['X', 'B'], ALL);

/*
Processes as:   LEFT    vs.     RIGHT
                1 (G)           2 (C)   - keep RIGHT rec 2
                1 (G)           3 (X)   - lose 3 (RIGHT rec)
                1 (G)           4 (G)   - keep RIGHT rec 4
                1 (G)           5 (B)   - lose 5 (RIGHT rec)

                2 (C)           1 (G)   - keep RIGHT rec 1
                2 (C)           4 (G)   - keep RIGHT rec 4

                4 (G)           1 (G)   - keep RIGHT rec 1
                4 (G)           2 (C)   - keep RIGHT rec 2

Result set is:
    Rec#    Value1    Value2
    1        C        G
    2        C        C
    4        B        G
*/

Dedup3 := DEDUP(SomeFile, 
                LEFT.Value2 IN ['X', 'B'] AND 
                RIGHT.Value2 IN ['G', 'C'], ALL);

/*
Processes as:    LEFT   vs.     RIGHT
                1 (G)           2 (C)        - keep RIGHT rec 2
                1 (G)           3 (X)        - keep RIGHT rec 3 
                1 (G)           4 (G)        - keep RIGHT rec 4
                1 (G)           5 (B)        - keep RIGHT rec 5

                2 (C)           1 (G)        - keep RIGHT rec 1
                2 (C)           3 (X)        - keep RIGHT rec 3
                2 (C)           4 (G)        - keep RIGHT rec 4
                2 (C)           5 (B)        - keep RIGHT rec 5

                3 (X)           1 (G)        - lose 1 (RIGHT rec)
                3 (X)           2 (C)        - lose 2 (RIGHT rec)
                3 (X)           4 (G)        - lose 4 (RIGHT rec)
                3 (X)           5 (B)        - keep RIGHT rec 5

                5 (B)           3 (X)        - keep RIGHT rec 3

Result set is:
    Rec#    Value1    Value2
    3        A        X
    5        A        B
*/

output(Dedup1);
output(Dedup2);
output(Dedup3);

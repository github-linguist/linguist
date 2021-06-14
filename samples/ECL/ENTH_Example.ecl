SomeFile := DATASET([   {'A'},{'B'},{'C'},{'D'},{'E'},
                        {'F'},{'G'},{'H'},{'I'},{'J'},
                        {'K'},{'L'},{'M'},{'N'},{'O'},
                        {'P'},{'Q'},{'R'},{'S'},{'T'},
                        {'U'},{'V'},{'W'},{'X'},{'Y'}
                    ], {STRING1 Letter});

Set1 := ENTH(SomeFile,2,10,1);
Set2 := ENTH(SomeFile,2,10,2);
Set3 := ENTH(SomeFile,2,10,3);
Set4 := ENTH(SomeFile,2,10,4);
Set5 := ENTH(SomeFile,2,10,5);

OUTPUT(Set1);
OUTPUT(Set2);
OUTPUT(Set3);
OUTPUT(Set4);
OUTPUT(Set5);

/* The expected results are:
            Set1:       Set2:       Set3:       Set4:       Set5:
    Rec#    Letter      Letter      Letter      Letter      Letter
    1       E           D           C           B           A
    2       J           I           H           G           F
    3       O           N           M           L           K
    4       T           S           R           Q           P
    5       Y           X           W           V           U
*/

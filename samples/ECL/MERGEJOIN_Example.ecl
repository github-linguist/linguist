//NOTE: This example must be run on HThor
Rec := RECORD,MAXLENGTH(4096)
    STRING1 Letter;
    UNSIGNED1 DS;
END;	

ds1 := DATASET([{'A',1},{'B',1},{'C',1},{'D',1},{'E',1}],Rec);
ds2 := DATASET([{'A',2},{'B',2},{'h',2},{'I',2},{'J',2}],Rec);
ds3 := DATASET([{'B',3},{'C',3},{'M',3},{'N',3},{'O',3}],Rec);

SetDS := [ds1,ds2,ds3];

j1 := MERGEJOIN(SetDS,
                STEPPED(LEFT.Letter=RIGHT.Letter),
                SORTED(Letter));

j2 := MERGEJOIN(SetDS,
                STEPPED(LEFT.Letter=RIGHT.Letter),
                SORTED(Letter),MOFN(3,4));

OUTPUT(j1);
OUTPUT(j2);

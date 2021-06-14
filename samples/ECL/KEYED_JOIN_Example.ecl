//RUN on HTHOR
//RUN on HTHOR
PtblRec := RECORD
    INTEGER8 sequence;
    STRING2  State;
    STRING20 City;
    STRING25 Lname;
    STRING15 Fname;
END;

Temp := DATASET([   {3000, 'FL', 'BOCA RATON', 'LONDON', 'BARIDGE'}, 
                    {35, 'FL', 'BOCA RATON', 'SMITH', 'FRANK'}, 
                    {50, 'FL', 'BOCA RATON', 'SMITH', 'SUE'}, 
                    {135, 'FL', 'BOCA RATON', 'SMITH', 'NANCY'}, 
                    {235, 'FL', 'BOCA RATON', 'SMITH', 'FRED'}, 
                    {335, 'FL', 'BOCA RATON', 'TAYLOR', 'FRANK'}, 
                    {3500, 'FL', 'BOCA RATON', 'JONES', 'FRANK'}, 
                    {30, 'FL', 'BOCA RATON', 'TAYLOR', 'RICHARD'}], PtblRec);

Proj := SORT(temp, sequence);

DataFile := '~RTTEMP::TestKeyedJoin';
KeyFile1 := '~RTTEMP::lname.fnameKey';
KeyFile2 := '~RTTEMP::lname.fnameKeyPay';
KeyFile3 := '~RTTEMP::sequenceKey';
KeyFile4 := '~RTTEMP::sequenceKeyPay';

PtblOut     := OUTPUT(Proj, , DataFile, OVERWRITE);
Ptbl1       := DATASET( DataFile, 
                        { PtblRec, UNSIGNED8 __filepos { virtual(fileposition) } }, FLAT);
Ptbl2       := DATASET(DataFile, PtblRec, FLAT);

AlphaKey    := INDEX(Ptbl1, {lname, fname, __filepos}, KeyFile1);
AlphaPay    := INDEX(Ptbl2, {lname, fname}, {Ptbl2}, KeyFile2); //payload

Bld1        := BUILD(AlphaKey, OVERWRITE);
Bld2        := BUILD(AlphaPay, OVERWRITE);

SequenceKey := INDEX(Ptbl1, {sequence, __filepos}, KeyFile3);
SequencePay := INDEX(Ptbl2, {sequence}, {Ptbl2}, KeyFile4); //payload
Bld3        := BUILD(SequenceKey, OVERWRITE);
Bld4        := BUILD(SequencePay, OVERWRITE);

peopleRecord := RECORD
    INTEGER8 id;
    STRING20 lastname;
END;

peopleDataset := DATASET([  {3000, 'LONDON'}, 
                            {3500, 'SMITH'}, 
                            {30, 'TAYLOR'}], peopleRecord);

joinedRecord := RECORD
    PtblRec;
    peopleRecord AND NOT [id];
END;

joinedRecord doJoin(peopleRecord L, Ptbl1 R) := TRANSFORM
    SELF := L;
    SELF := R;
END;

FilledRecs1 := JOIN(peopleDataset, Ptbl1, 
                    LEFT.id = RIGHT.sequence, 
                    doJoin(LEFT, RIGHT), KEYED(SequenceKey)); //FULL Keyed

FilledRecs2 := JOIN(peopleDataset, Ptbl1, 
                    LEFT.lastname = RIGHT.Lname, 
                    doJoin(LEFT, RIGHT), KEYED(AlphaKey)); //FULLKeyed

FilledRecs3 := JOIN(peopleDataset, SequencePay, 
                    KEYED(LEFT.id = RIGHT.sequence), 
                    TRANSFORM(joinedRecord, SELF := LEFT, SELF := RIGHT)); //HALF-KEYED

FilledRecs4 := JOIN(peopleDataset, AlphaPay, 
                    KEYED(LEFT.lastname = RIGHT.Lname), 
                    TRANSFORM(joinedRecord, SELF := LEFT, SELF := RIGHT)); //HALF-KEYED

Out1 := OUTPUT(FilledRecs1, NAMED('FULLKEYRecs1'));
Out2 := OUTPUT(FilledRecs2, NAMED('FULLKEYRecs2'));
Out3 := OUTPUT(FilledRecs3, NAMED('HALFKEYRecs3'));
Out4 := OUTPUT(FilledRecs4, NAMED('HALFKEYRecs4'));

SEQUENTIAL( PtblOut, 
            PARALLEL(Bld1, Bld2, Bld3, Bld4), 
            PARALLEL(Out1, Out2, Out3, Out4));

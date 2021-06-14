ParentRec := RECORD
    INTEGER1  NameID;
    STRING20  Name;
END;

ChildRec := RECORD
    INTEGER1  NameID;
    STRING20  Addr;
END;

DenormedRec := RECORD
    ParentRec;
    INTEGER1 NumRows;
    DATASET(ChildRec) Children {MAXCOUNT(5)};
END;

NamesTable := DATASET([ {1, 'Gavin'}, 
                        {2, 'Liz'}, 
                        {3, 'Mr Nobody'}, 
                        {4, 'Anywhere'}], 
                      ParentRec);            

NormAddrs := DATASET([{1, '10 Malt Lane'},     
                      {2, '10 Malt Lane'},     
                      {2, '3 The cottages'},     
                      {4, 'Here'},     
                      {4, 'There'},     
                      {4, 'Near'},     
                      {4, 'Far'}], 
                     ChildRec);    

DenormedRec ParentLoad(ParentRec L) := TRANSFORM
    SELF.NumRows := 0;
    SELF.Children := [];
    SELF := L;
END;

//Ptbl := TABLE(NamesTable, DenormedRec);
Ptbl := PROJECT(NamesTable, ParentLoad(LEFT));
OUTPUT(Ptbl, NAMED('ParentDataReady'));

DenormedRec DeNormThem(DenormedRec L, ChildRec R, INTEGER C) := TRANSFORM
    SELF.NumRows := C;
    SELF.Children := L.Children + R;
    SELF := L;
END;

DeNormedRecs := DENORMALIZE(Ptbl, NormAddrs, 
                            LEFT.NameID = RIGHT.NameID, 
                            DeNormThem(LEFT, RIGHT, COUNTER));

OUTPUT(DeNormedRecs, NAMED('NestedChildDataset'));

// *******************************

ParentRec ParentOut(DenormedRec L) := TRANSFORM
    SELF := L;
END;

Pout := PROJECT(DeNormedRecs, ParentOut(LEFT));
OUTPUT(Pout, NAMED('ParentExtracted'));

// /* Using Form 1 of NORMALIZE */
ChildRec NewChildren(DenormedRec L, INTEGER C) := TRANSFORM
    SELF := L.Children[C];
END;
NewChilds := NORMALIZE(DeNormedRecs, LEFT.NumRows, NewChildren(LEFT, COUNTER));

// /* Using Form 2 of NORMALIZE */
// ChildRec NewChildren(ChildRec L) := TRANSFORM
//     SELF := L;
// END;

// NewChilds := NORMALIZE(DeNormedRecs, LEFT.Children, NewChildren(RIGHT));

// /* Using Form 2 of NORMALIZE with inline TRANSFORM*/
// NewChilds := NORMALIZE(DeNormedRecs, LEFT.Children, TRANSFORM(RIGHT));

OUTPUT(NewChilds, NAMED('ChildrenExtracted'));

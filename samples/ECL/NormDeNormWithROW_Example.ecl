LinkRec := RECORD
    INTEGER1  NameID;
END;

ParentRec := RECORD(LinkRec)
    STRING20  Name;
END;

ChildRec := RECORD(LinkRec)
    STRING20  Addr;
    INTEGER4  Phone;
END;

ChildNestedRec := RECORD
    ChildRec AND NOT [NameID];
END;

DenormedRec := RECORD
    ParentRec;
    INTEGER1 NumRows;
    DATASET(ChildNestedRec) Children{MAXCOUNT(5)};
END;

NamesTable := DATASET([ {1, 'Gavin'}, 
                        {2, 'Liz'}, 
                        {3, 'Mr Nobody'}, 
                        {4, 'Anywhere'}], 
                      ParentRec);            

NormAddrs := DATASET([  {1, '10 Malt Lane', 12345}, 	
                        {2, '10 Malt Lane', 54321}, 	
                        {2, '3 The cottages', 45678}, 	
                        {4, 'Here', 87654}, 	
                        {4, 'There', 97531}, 	
                        {4, 'Near', 13579}, 	
                        {4, 'Far', 99999}], 
                     ChildRec);	

DenormedRec ParentLoad(ParentRec L) := TRANSFORM
    SELF.NumRows := 0;
    SELF.Children := [];
    SELF := L;
END;

Ptbl := PROJECT(NamesTable, ParentLoad(LEFT));
OUTPUT(Ptbl, NAMED('ParentDataReady'));

DenormedRec DeNormThem(DenormedRec L, ChildRec R, INTEGER C) := TRANSFORM
    SELF.NumRows := C;
    SELF.Children := L.Children + ROW({R.Addr, R.Phone}, ChildNestedRec);
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

/* Form 1 of NORMALIZE is easiest for this style */
ChildRec NewChildren(DenormedRec L, INTEGER C) := TRANSFORM
    SELF.NameID := L.NameID;
    SELF := L.Children[C];
END;
NewChilds := NORMALIZE(DeNormedRecs, LEFT.NumRows, NewChildren(LEFT, COUNTER));

/* Form 2 of NORMALIZE requires an extra "hoop" to jump through to get the NameID linking field*/
// ChildRec NewChildren(ChildNestedRec L, INTEGER1 pNameID) := TRANSFORM
//     SELF.NameID := pNameID;
//     SELF := L;
// END;

// NewChilds := NORMALIZE(DeNormedRecs, LEFT.Children, NewChildren(RIGHT, LEFT.NameID));

OUTPUT(NewChilds, NAMED('ChildrenExtracted'));

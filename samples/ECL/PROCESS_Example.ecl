
DSrec := RECORD
    STRING4 Letter;
    STRING4 LeftRecIn := '';
    STRING4 RightRecIn := '';
END;

StateRec := RECORD
    STRING2 Letter;
END;

ds := DATASET([{'AA'}, {'BB'}, {'CC'}, {'DD'}, {'EE'}], DSrec);

DSrec DSxform(DSrec L, StateRec R) := TRANSFORM
    SELF.Letter     := L.Letter[1..2] + R.Letter;
    SELF.LeftRecIn  := L.Letter;
    SELF.RightRecIn := R.Letter;
END;

StateRec ROWxform(DSrec L, StateRec R) := TRANSFORM
    SELF.Letter := L.Letter[1] + R.Letter[1];
END;                    

p := PROCESS(ds, ROW({'ZZ'}, StateRec), DSxform(LEFT, RIGHT), ROWxform(LEFT, RIGHT));

OUTPUT(p);
/* Result:
    AAZZ        AA      ZZ  
    BBAZ        BB      AZ  
    CCBA        CC      BA  
    DDCB        DD      CB  
    EEDC        EE      DC  
*/

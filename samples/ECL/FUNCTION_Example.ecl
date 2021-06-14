MyFunc(STRING DataIn, STRING1 SearchChar) := FUNCTION

StrLen := LENGTH(TRIM(dataIn));
    ds := DATASET([{DataIn}], {STRING chars});

    OutRec := RECORD
        UNSIGNED1 flag;
    END;

    OutRec Xform(ds L, INTEGER C) := TRANSFORM
        SELF.flag := IF(L.chars[C] = SearchChar, 1, 0);
    END;

    n := NORMALIZE(ds, StrLen, Xform(LEFT, COUNTER));

    RETURN COUNT(n(flag=1));
END;
    
OUTPUT(MyFunc('abc~xyz~def~fred', '~'));

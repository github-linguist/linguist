IMPORT STD;    

fibRecord := RECORD
    INTEGER fib1 := 1;
    INTEGER fib2 := 1;
    INTEGER mycounter := 100;
    STRING20 extra := 'xx';
END;

fibTable := DATASET([{9}, {8}, {7}, {6}], fibrecord);

fibRecord makeFibs(fibRecord l, fibRecord r) := TRANSFORM
    SELF.fib1 := if(l.fib1=0, r.fib1, r.fib1 + l.fib1);
    SELF.fib2 := r.fib2 + l.fib1 + l.fib2;
    SELF.mycounter := l.mycounter + 1;
    SELF := r;
END;

doDisplay(STRING DispStr) := std.system.Log.addWorkunitInformation(DispStr);

ret := ITERATE(fibTable, makeFibs(LEFT, RIGHT));

APPLY(ret, 
    doDisplay((STRING)fib1 + ','),
    doDisplay((STRING)fib2 + ','),
    doDisplay((STRING)mycounter),
    doDisplay(extra),
    BEFORE(doDisplay('Begin APPLY....')),
    AFTER(doDisplay('...End APPLY'))
    );

OUTPUT(ret);

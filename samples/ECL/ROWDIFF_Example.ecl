Name := {STRING30 fname, STRING20 lname};
in1rec := {UNSIGNED1 id, Name name1, UNSIGNED1 age, STRING5 title};
in2rec := {UNSIGNED1 id, Name name1, REAL4 age, BOOLEAN dead };

in1 := DATASET([{1, 'Gavin', 'Halliday', 33, 'Mr'}, 
                {2, 'Liz', 'Halliday', 33, 'Dr'}, 
                {3, 'Elizabeth', 'Windsor', 99, 'Queen'}], in1rec);

in2 := DATASET([{1, 'Gavin', 'Halliday', 33, false}, 
                {2, 'Liz', '', 33, false}, 
                {3, 'Elizabeth', 'Windsor', 99.1, false}], in2rec);
                                     
//only matching field values will be compared by ROWDIFF

outrec := {UNSIGNED1 id, STRING35 diff1, STRING35 diff2};

outrec t1(in1 L, in2 R) := TRANSFORM
    SELF.id := L.id;
    SELF.diff1 := ROWDIFF(L, R, COUNT);
    SELF.diff2 := ROWDIFF(L, R);
END;

OUTPUT(JOIN(in1, in2, LEFT.id = RIGHT.id, t1(LEFT, RIGHT)));
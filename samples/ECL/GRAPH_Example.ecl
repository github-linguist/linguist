namesRec := RECORD
    STRING20    lname;
    STRING10    fname;
    UNSIGNED2   age := 25;
    UNSIGNED2   ctr := 0;
END;

namesTable2 := DATASET([{'Flintstone', 'Fred', 35}, 
                        {'Flintstone', 'Wilma', 33}, 
                        {'Jetson', 'Georgie', 10}, 
                        {'Mr. T', 'Z-man'}], namesRec);

loopBody(SET OF DATASET(namesRec) ds, UNSIGNED4 c) :=
         PROJECT(   ds[c-1],                                //ds[0]=original input 
                    TRANSFORM(  namesRec, 
                                SELF.age := LEFT.age + c;   //c is graph COUNTER (+55: 1+2+3..+10)
                                SELF.ctr := COUNTER;        //PROJECT’s COUNTER
                                SELF := LEFT));

g1 := GRAPH(namesTable2, 10, loopBody(ROWSET(LEFT), COUNTER));

OUTPUT(g1);

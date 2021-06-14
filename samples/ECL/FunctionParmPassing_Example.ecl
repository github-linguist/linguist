//a Function prototype:
INTEGER actionPrototype(INTEGER v1, INTEGER v2) := 0;

INTEGER aveValues(INTEGER v1, INTEGER v2)	:= (v1 + v2) DIV 2;
INTEGER addValues(INTEGER v1, INTEGER v2)	:= v1 + v2;
INTEGER multiValues(INTEGER v1, INTEGER v2)	:= v1 * v2;

//a Function prototype using a function prototype:
INTEGER applyPrototype(INTEGER v1, actionPrototype actionFunc) := 0;

//using the Function prototype and a default value:
INTEGER applyValue2(INTEGER v1, actionPrototype actionFunc = aveValues) 
        := actionFunc(v1, v1+1) * 2;

//Defining the Function parameter inline, witha default value:
INTEGER applyValue4(INTEGER v1, 
                    INTEGER actionFunc(INTEGER v1, 
                    INTEGER v2) = aveValues)
        := actionFunc(v1, v1+1)*4;
INTEGER doApplyValue(	INTEGER v1, 
                        INTEGER actionFunc(INTEGER v1, INTEGER v2)) 
        := applyValue2(v1+1, actionFunc);

//producing simple results:
OUTPUT(applyValue2(1)); // 2
OUTPUT(applyValue2(2)); // 4
OUTPUT(applyValue2(1, addValues)); // 6
OUTPUT(applyValue2(2, addValues)); // 10
OUTPUT(applyValue2(1, multiValues)); // 4
OUTPUT(applyValue2(2, multiValues)); // 12
OUTPUT(doApplyValue(1, multiValues)); // 12
OUTPUT(doApplyValue(2, multiValues)); // 24

//An attribute taking function parameters which themselves
//have parameters that are functions...
STRING doMany(	INTEGER v1, 
                INTEGER firstAction(INTEGER v1, 
                                    INTEGER actionFunc(INTEGER v1, INTEGER v2)), 
                INTEGER secondAction(	INTEGER v1, 
                                        INTEGER actionFunc(INTEGER v1, INTEGER v2)), 
                INTEGER actionFunc(INTEGER v1, INTEGER v2)) 
        := (STRING)firstAction(v1, actionFunc) + ':' + (STRING)secondaction(v1, actionFunc);

OUTPUT(doMany(1, applyValue2, applyValue4, addValues));
// produces "6:12"

OUTPUT(doMany(2, applyValue4, applyValue2, multiValues));
// produces "24:12"

//run on hthor
//If you are running without the Java plugin installed, you will get syntax errors

IMPORT java;

INTEGER add1(INTEGER val)   := IMPORT(java, 'JavaCat.add1:(I)I');
STRING add2(STRING val)     := IMPORT(java, 'JavaCat.add2:(Ljava/lang/String;)Ljava/lang/String;');
STRING add3(VARSTRING val)  := IMPORT(java, 'JavaCat.add2:(Ljava/lang/String;)Ljava/lang/String;');
UTF8 add4(UTF8 val)         := IMPORT(java, 'JavaCat.add2:(Ljava/lang/String;)Ljava/lang/String;');
UNICODE add5(UNICODE val)   := IMPORT(java, 'JavaCat.add2:(Ljava/lang/String;)Ljava/lang/String;');

STRING addChar(STRING c)         := IMPORT(java, 'JavaCat.addChar:(C)C');
STRING cat(STRING s1, STRING s2) := IMPORT(java, 'JavaCat.cat:(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;');
DATA testData(DATA indata)       := IMPORT(java, 'JavaCat.testData:([B)[B');
INTEGER testArrays(SET OF BOOLEAN b, SET OF INTEGER2 s, SET OF INTEGER4 i, SET OF REAL8 d) 
                                 := IMPORT(java, 'JavaCat.testArrays:([Z[S[I[D)I');
SET OF STRING testStringArray1(SET OF STRING s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF VARSTRING testStringArray2(SET OF VARSTRING s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF STRING8 testStringArray3(SET OF STRING8 s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF VARSTRING8 testStringArray4(SET OF VARSTRING8 s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF UTF8 testStringArray5(SET OF UTF8 s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF UNICODE8 testStringArray6(SET OF UNICODE8 s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');
SET OF UNICODE testStringArray7(SET OF UNICODE s) 
                                 := IMPORT(java, 'JavaCat.testStringArray:([Ljava/lang/String;)[Ljava/lang/String;');

add1(10);
add2('Hello');
add3('World');
add4(U'Leovenaðes');
add5(U'你好世界');
addChar('A');

cat('Hello', ' world');

testData(d'aa');
testArrays([true], [2, 3], [4, 5, 6, 7], [8.0, 9.0]);
testArrays([], [], [], []);
testStringArray1(['one', 'two', 'three']);
testStringArray2(['one', 'two', 'three']);
testStringArray3(['one', 'two', 'three']);
testStringArray4(['one', 'two', 'three']);
testStringArray5(['one', 'two', 'three']);
testStringArray6(['one', 'two', 'three']);
testStringArray7(['one', 'two', 'three']);

s1 :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := add1(COUNTER)));
s2 :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := add1(COUNTER/2)));
SUM(NOFOLD(s1 + s2), a);

s1a :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := (integer) add2((STRING)COUNTER)));
s2a :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := (integer) add3((STRING)(COUNTER/2))));
SUM(NOFOLD(s1a + s2a), a);

s1b :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := COUNTER+1));
s2b :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := (COUNTER/2)+1));
SUM(NOFOLD(s1b + s2b), a);

s1c :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := (integer) ((STRING) COUNTER + '1')));
s2c :=DATASET(250000, TRANSFORM({ integer a }, SELF.a := (integer) ((STRING)(COUNTER/2) + '1')));
SUM(NOFOLD(s1c + s2c), a);

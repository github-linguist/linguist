/* 
 * Multi-line comment
 */
#option ('slidingJoins', true);

namesRecord :=
            RECORD
string20        surname;
string10        forename;
integer2        age;
integer2        dadAge;
integer2        mumAge;
            END;

namesRecord2 :=
            record
string10        extra;
namesRecord;
            end;

namesTable := dataset('x',namesRecord,FLAT);
namesTable2 := dataset('y',namesRecord2,FLAT);

integer2 aveAgeL(namesRecord l) := (l.dadAge+l.mumAge)/2;
integer2 aveAgeR(namesRecord2 r) := (r.dadAge+r.mumAge)/2;

// Standard join on a function of left and right
output(join(namesTable, namesTable2, aveAgeL(left) = aveAgeR(right)));

//Several simple examples of sliding join syntax
output(join(namesTable, namesTable2, left.age >= right.age - 10 and left.age <= right.age +10));
output(join(namesTable, namesTable2, left.age between right.age - 10 and right.age +10));
output(join(namesTable, namesTable2, left.age between right.age + 10 and right.age +30));
output(join(namesTable, namesTable2, left.age between (right.age + 20) - 10 and (right.age +20) + 10));
output(join(namesTable, namesTable2, aveAgeL(left) between aveAgeR(right)+10 and aveAgeR(right)+40));

//Same, but on strings.  Also includes age to ensure sort is done by non-sliding before sliding.
output(join(namesTable, namesTable2, left.surname between right.surname[1..10]+'AAAAAAAAAA' and right.surname[1..10]+'ZZZZZZZZZZ' and left.age=right.age));
output(join(namesTable, namesTable2, left.surname between right.surname[1..10]+'AAAAAAAAAA' and right.surname[1..10]+'ZZZZZZZZZZ' and left.age=right.age,all));

//This should not generate a self join
output(join(namesTable, namesTable, left.age between right.age - 10 and right.age +10));

Field_Population(infile, infield, compareval) := FUNCTIONMACRO
    c1 := COUNT(infile(infield=compareval));
    c2 := COUNT(infile);
    RETURN DATASET([{'Total Records', c2}, 
                    {'Recs=' + #TEXT(compareval), c1}, 
                    {'Population Pct', (INTEGER)(c1/c2 * 100.0)}], 
                    {STRING15 valuetype, INTEGER val});
ENDMACRO;

GenRec := RECORD
    STRING1 Gender;
END;

ds1 := DATASET([{'M'}, {'M'}, {'M'}, {''}, {''}, {'M'}, {''}, {'M'}, {'M'}, {''}], GenRec);
ds2 := DATASET([{''}, {'M'}, {'M'}, {''}, {''}, {'M'}, {''}, {''}, {'M'}, {''}], GenRec);
OUTPUT(Field_Population(ds1, Gender, ''));
OUTPUT(Field_Population(ds2, Gender, ''));

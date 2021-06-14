r := RECORD
    STRING20 Subject;
    INTEGER4 Result;
END;

d := DATASET([  {'English', 92}, 
                {'French', 86}, 
                {'Irish', 80}, 
                {'Math', 98}, 
                {'Geography', 55}, 
                {'Computers', 25}], r);

OUTPUT(d, {Label := Subject, Value := Result}, NAMED('BarChart'));

SomeFile1 := DATASET([  {1, 'A'}, {1, 'B'}, {1, 'C'}, {1, 'D'}, {1, 'E'}, 
                        {1, 'F'}, {1, 'G'}, {1, 'H'}, {1, 'I'}, {1, 'J'}], 
                     {INTEGER1 number, STRING1 Letter});
SomeFile2 := DATASET([  {2, 'A'}, {2, 'B'}, {2, 'C'}, {2, 'D'}, {2, 'E'}, 
                        {2, 'F'}, {2, 'G'}, {2, 'H'}, {2, 'I'}, {2, 'J'}], 
                     {INTEGER1 number, STRING1 Letter});
SomeFile4 := DATASET([  {3, 'A'}, {3, 'B'}, {3, 'C'}, {3, 'D'}, {3, 'E'}, 
                        {3, 'F'}, {3, 'G'}, {3, 'H'}, {3, 'I'}, {3, 'J'}], 
                     {INTEGER1 number, STRING1 Letter});
SomeFile3 := MERGE(SORTED(SomeFile1, letter, number), 
                   SORTED(SomeFile2, letter, number), 
                   SORTED(SomeFile4, letter, number));

OUTPUT(SomeFile3);
OUTPUT(SomeFile1 + SomeFile2);

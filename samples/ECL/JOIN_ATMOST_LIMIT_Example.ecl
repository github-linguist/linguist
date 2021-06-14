SomeFile1 := DATASET([  {1, 'A'}, {2, 'B'}, {3, 'C'}, {4, 'D'}, {5, 'E'}], 
                        {INTEGER1 number, STRING1 Letter1});

SomeFile2 := DATASET([  {1, 'A'}, {2, 'B'}, {2, 'C'}, {2, 'Y'}, 
                        {3, 'C'}, {3, 'Z'}, {4, 'D'}, {5, 'E'}], 
                        {INTEGER1 number, STRING1 Letter2});

//Y := LIMIT(SomeFile1, 4, FAIL(2011, 'Too many results!'));    //throws an exception
//C := LIMIT(SomeFile1, 2);                                     //throws an exception
//Y := IF(COUNT(SomeFile1)>= 2, Somefile1(true), C);            //throws an exception
/*
    Y := JOIN(  SomeFile1, SomeFile2, 
                LEFT.number=RIGHT.number //AND LEFT.letter1=RIGHT.letter2, 
                ATMOST(LEFT.number=RIGHT.number, 2));
*/

Y := JOIN(  SomeFile1, SomeFile2, 
            LEFT.number=RIGHT.number, 
            LIMIT(2, skip));

Y;      // C;

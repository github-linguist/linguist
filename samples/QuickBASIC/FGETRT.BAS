'*********** FGetRT.Bas - demonstrates FGetRT and FPutRT in context

'Copyright (c) 1989 Ethan Winer


DEFINT A-Z
DECLARE SUB FClose (Handle)
DECLARE SUB FCreate (FileName$)
DECLARE SUB FGetRT (Handle, Destination AS ANY, RecNumber&, RecLength)
DECLARE SUB FOpen (FileName$, Handle)
DECLARE SUB FPutRT (Handle, Source AS ANY, RecNumber&, RecLength)
DECLARE SUB KillFile (FileName$)

DECLARE FUNCTION DOSError% ()
DECLARE FUNCTION WhichError% ()
DECLARE FUNCTION ErrorMsg$ (ErrNumber)

TYPE FTest                      'this is the sample type for the file test
   FirstName  AS STRING * 15
   LastName   AS STRING * 15
   Company    AS STRING * 25
   AccountNum AS LONG
   WhatNot    AS DOUBLE
   WhyNot     AS SINGLE
END TYPE
DIM TestRec AS FTest            'TestRec will hold the data to/from the file

CLS
F$ = "Random.Tst"               'this will be our test file
RecLength = LEN(TestRec)        'this sets the record length for gets and puts

FCreate F$                      'create the file
IF DOSError% THEN               'see if an error occurred creating the file
   PRINT ErrorMsg$(WhichError%)
   END
END IF

FOpen F$, Handle                        'open the file for QuickPak Pro Binary

FOR Record& = 1 TO 100                  'create one hundred records
    TestRec.FirstName = "Testing" + STR$(Record&)
    TestRec.WhatNot = Record&
    FPutRT Handle, TestRec, Record&, RecLength
    IF DOSError% THEN                   'check for possible full disk
       PRINT ErrorMsg$(WhichError%)
       END
    END IF
NEXT

FOR Record& = 99 TO 1 STEP -10  'read records backwards to prove it all works
    FGetRT Handle, TestRec, Record&, RecLength
    PRINT "Record"; Record&, TestRec.FirstName; TestRec.WhatNot
NEXT

FClose Handle                   'close the file
KillFile F$                     'why clutter up the disk with this nonsense?


_CStringClass_ EQUATE(1)

NOT_CASE_SENSITIVE           EQUATE(FALSE)
DEFAULT_CS_BUFFER_SIZE       EQUATE(1024)
!--------------------------------------------------------------------------------
!Class CStringClass
!
!--------------------------------------------------------------------------------
CStringClass     Class(),Type,Module('CStringClass.Clw'),LINK('CStringClass.Clw',1)
! Properties
cs                           &CSTRING, PROTECTED
bufferSize                   LONG
newStrSize                   LONG ,PROTECTED
strLength                    LONG ,PROTECTED
nextPointer                  LONG
! Methods
Cat                    PROCEDURE (STRING pStr) ,*CSTRING,PROC ,VIRTUAL 
Construct              PROCEDURE   
Contains               PROCEDURE (STRING pFind, BYTE pCaseSensitive=TRUE) ,BYTE ,VIRTUAL 
Destruct               PROCEDURE () ,VIRTUAL 
Len                    PROCEDURE ,LONG ,VIRTUAL 
Lower                  PROCEDURE () ,STRING ,VIRTUAL 
Replace                PROCEDURE (STRING pFind, STRING pReplace) ,*CSTRING,PROC ,VIRTUAL 
Str                    PROCEDURE (STRING pStr) ,*CSTRING, PROC   ,VIRTUAL 
Str                    PROCEDURE () ,*CSTRING ,VIRTUAL 
SubString              PROCEDURE (LONG pPosition, LONG pLength) ,STRING,PROC ,VIRTUAL 
ToLower                PROCEDURE () ,*CSTRING,PROC ,VIRTUAL 
ToUpper                PROCEDURE () ,*CSTRING,PROC ,VIRTUAL 
Trim                   PROCEDURE () ,*CSTRING,PROC ,VIRTUAL 
Upper                  PROCEDURE () ,STRING ,VIRTUAL 
IndexOf                PROCEDURE (STRING pLookIn, BYTE pCaseSensitive=TRUE) ,LONG ,VIRTUAL 
FoundIn                PROCEDURE (STRING pLookIn, BYTE pCaseSensitive=TRUE) ,BYTE ,VIRTUAL 
SetBuffer              PROCEDURE (LONG pNewBuffer)  ,VIRTUAL 
EscapeXml              PROCEDURE (<STRING pStr>) ,STRING ,VIRTUAL 
                     END 
!_EndOfInclude_

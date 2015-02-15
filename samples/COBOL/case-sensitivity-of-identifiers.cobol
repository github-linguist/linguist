* Case sensitivity of identifiers
       *>* Commented-out lines in the working storage
       *>* are considered as invalid redefinitions
       *>* of ''dog'' that can only be ambiguously
       *>* referenced in the procedure body.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. case-sensitivity.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *>* 01  dog PICTURE X(8) VALUE IS "Benjamin".
       *>* 01  Dog PICTURE X(5) VALUE IS "Samba".
       01  DOG PICTURE X(6) VALUE IS "Bernie".
       PROCEDURE DIVISION.
         DISPLAY
       *>*     "The three dogs are named "
       *>*     dog ", " Dog " and " DOG "."
           "There is just one dog named " DOG "."
         END-DISPLAY
         STOP RUN.
       END PROGRAM case-sensitivity.

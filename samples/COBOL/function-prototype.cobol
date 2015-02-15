       *> A subprogram taking no arguments and returning nothing.
       PROGRAM-ID. no-args PROTOTYPE.
       END PROGRAM no-args.

       *> A subprogram taking two 8-digit numbers as arguments, and returning
       *> an 8-digit number.
       PROGRAM-ID. two-args PROTOTYPE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  arg-1 PIC 9(8).
       01  arg-2 PIC 9(8).
       01  ret   PIC 9(8).
       PROCEDURE DIVISION USING arg-1, arg-2 RETURNING ret.
       END PROGRAM two-args.

       *> A subprogram taking two optional arguments which are 8-digit
       *> numbers (passed by reference (the default and compulsory for
       *> optional arguments)).
       PROGRAM-ID. optional-args PROTOTYPE.
       DATA DIVISION.
       LINKAGE SECTION.
       01  arg-1 PIC 9(8).
       01  arg-2 PIC 9(8).
       PROCEDURE DIVISION USING OPTIONAL arg-1, OPTIONAL arg-2.
       END PROGRAM optional-args.

       *> Standard COBOL does not support varargs or named parameters.

       *> A function from another language, taking a 32-bit integer by
       *> value and returning a 32-bit integer (in Visual COBOL).
       PROGRAM-ID. foreign-func PROTOTYPE.
       OPTIONS.
           ENTRY-CONVENTION some-langauge.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  arg PIC S9(9) USAGE COMP-5.
       01  ret PIC S9(9) USAGE COMP-5.
       PROCEDURE DIVISION USING arg RETURNING ret.
       END PROGRAM foreign-func.

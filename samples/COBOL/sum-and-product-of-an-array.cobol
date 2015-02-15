       IDENTIFICATION DIVISION.
       PROGRAM-ID. array-sum-and-product.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Array-Size              VALUE 10.
       01  array-area              VALUE "01020304050607080910".
           03  array               PIC 99 OCCURS Array-Size TIMES.

       01  array-sum               PIC 9(8).
       01  array-product           PIC 9(10) VALUE 1.

       01  i                       PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL Array-Size < i
               ADD array (i) TO array-sum
               MULTIPLY array (i) BY array-product
           END-PERFORM

           DISPLAY "Sum:     " array-sum
           DISPLAY "Product: " array-product

           GOBACK
           .

       INTERFACE-ID. Shape.

       PROCEDURE DIVISION.

       METHOD-ID. perimeter.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ret USAGE FLOAT-LONG.
       PROCEDURE DIVISION RETURNING ret.
       END METHOD perimeter.

       METHOD-ID. shape-area.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ret USAGE FLOAT-LONG.
       PROCEDURE DIVISION RETURNING ret.
       END METHOD shape-area.

       END INTERFACE Shape.


       CLASS-ID. Rectangle.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           INTERFACE Shape.

       OBJECT IMPLEMENTS Shape.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01  width  USAGE FLOAT-LONG PROPERTY.
           01  height USAGE FLOAT-LONG PROPERTY.

           PROCEDURE DIVISION.

           METHOD-ID. perimeter.
           DATA DIVISION.
           LINKAGE SECTION.
           01  ret USAGE FLOAT-LONG.
           PROCEDURE DIVISION RETURNING ret.
               COMPUTE ret = width * 2.0 + height * 2.0
               GOBACK
               .
           END METHOD perimeter.

           METHOD-ID. shape-area.
           DATA DIVISION.
           LINKAGE SECTION.
           01  ret USAGE FLOAT-LONG.
           PROCEDURE DIVISION RETURNING ret.
               COMPUTE ret = width * height
               GOBACK
               .
           END METHOD shape-area.
       END OBJECT.

       END CLASS Rectangle.

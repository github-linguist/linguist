       IDENTIFICATION DIVISION.
       CLASS-ID. my-class INHERITS base.

       *> The 'INHERITS base' and the following ENVIRONMENT DIVISION
       *> are optional (in Visual COBOL).
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS base.

           *> There is no way (as far as I can tell) of creating a
           *> constructor. However, you could wrap it with another
           *> method to achieve the desired effect.
           *>...

           OBJECT.
               *> Instance data
               DATA DIVISION.
               WORKING-STORAGE SECTION.
               01  instance-variable PIC 9(8).

               *> Properties can have getters and setters automatically
               *> generated.
               01  a-property        PIC 9(8) PROPERTY.

               PROCEDURE DIVISION.

               METHOD-ID. some-method.
               PROCEDURE DIVISION.
                   *> ...
               END METHOD some-method.
           END OBJECT.
       END CLASS my-class.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. example-class-use.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           *> These declarations brings the class and property into
           *> scope.
           CLASS my-class
           PROPERTY a-property.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Declaring a my-class reference variable.
       01  instance USAGE OBJECT REFERENCE my-class.

       PROCEDURE DIVISION.

           *> Invoking a static method or (in this case) a constructor.
           INVOKE my-class "new" RETURNING instance

           *> Invoking an instance method.
           INVOKE instance "some-method"

           *> Using the setter and getter of a-property.
           MOVE 5 TO a-property OF instance
           DISPLAY a-property OF instance

           GOBACK
           .

       END PROGRAM example-class-use.

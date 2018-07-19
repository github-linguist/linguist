       CLASS-ID. Animal.
           *> ...
       END CLASS Animal.

       CLASS-ID. Dog INHERITS Animal.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Animal.

           *> ...
       END CLASS Dog.

       CLASS-ID. Cat INHERITS Animal.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Animal.

           *> ...
       END CLASS Cat.

       CLASS-ID. Lab INHERITS Dog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Dog.

           *> ...
       END CLASS Lab.

       CLASS-ID. Collie INHERITS Dog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           CLASS Dog.

           *> ...
       END CLASS Collie.

USING: io ;
IN: rosetacode
: argument-function1 ( -- ) "Hello World!" print ;
: argument-function2 ( -- ) "Goodbye World!" print ;

! normal words have to know the stack effect of the input parameters they execute
: calling-function1 ( another-function -- ) execute( -- ) ;

! unlike normal words, inline words do not have to know the stack effect.
: calling-function2 ( another-function -- ) execute ; inline

! Stack effect has to be written for runtime computed values :
: calling-function3 ( bool -- ) \ argument-function1 \ argument-function2 ? execute( -- ) ;

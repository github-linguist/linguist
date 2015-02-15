       DELEGATE-ID func.
       PROCEDURE DIVISION USING VALUE t AS FLOAT-LONG
           RETURNING ret AS FLOAT-LONG.
       END DELEGATE.

       CLASS-ID. MainClass.

       78  T0                     VALUE 100.0.
       78  TR                     VALUE 20.0.
       78  k                      VALUE 0.07.

       01  delta-t                INITIALIZE ONLY STATIC
                                  FLOAT-LONG OCCURS 3 VALUES 2.0, 5.0, 10.0.

       78  n                      VALUE 100.

       METHOD-ID NewtonCooling STATIC.
       PROCEDURE DIVISION USING VALUE t AS FLOAT-LONG
               RETURNING ret AS FLOAT-LONG.
           COMPUTE ret = - k * (t - TR)
       END METHOD.

       METHOD-ID Main STATIC.
           DECLARE f AS TYPE func
           SET f TO METHOD self::NewtonCooling

           DECLARE delta-t-len AS BINARY-LONG
           MOVE delta-t::Length TO delta-t-len
           PERFORM VARYING i AS BINARY-LONG FROM 1 BY 1
                   UNTIL i > delta-t-len
               DECLARE elt AS FLOAT-LONG = delta-t (i)
               INVOKE TYPE Console::WriteLine("delta-t = {0:F4}", elt)
               INVOKE self::Euler(f, T0, n, elt)
           END-PERFORM
       END METHOD.

       METHOD-ID Euler STATIC.
       PROCEDURE DIVISION USING VALUE f AS TYPE func, y AS FLOAT-LONG,
               n AS BINARY-LONG, h AS FLOAT-LONG.
           PERFORM VARYING x AS BINARY-LONG FROM 0 BY h UNTIL x >= n
               INVOKE TYPE Console::WriteLine("x = {0:F4}, y = {1:F4}", x, y)
               COMPUTE y = y + h * RUN f(y)
           END-PERFORM
       END METHOD.
       END CLASS.

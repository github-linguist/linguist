      PROGRAM MAIN

      END

C comment
* comment

      SUBROUTINE foo( i, x, b )
      INTEGER            i
      REAL               x
      LOGICAL            b

      IF( i.NE.0 ) THEN
         CALL bar( -i )
      END IF

      RETURN
      END

      DOUBLE COMPLEX FUNCTION baz()

      baz = (0.0d0,0.0d0)

      RETURN 
      END

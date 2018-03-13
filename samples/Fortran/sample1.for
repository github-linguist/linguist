c comment
* comment

      program main

      end

      subroutine foo( i, x, b )
      INTEGER            i
      REAL               x
      LOGICAL            b

      if( i.ne.0 ) then
         call bar( -i )
      end if

      return
      end

      double complex function baz()

      baz = (0.0d0,0.0d0)

      return 
      end

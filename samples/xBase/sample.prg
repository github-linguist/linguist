#require "hbtest"

#pragma linenumber=on

#include "hbclass.ch"

#stdout "stdout"
#warning "warning"

#define MYCONST  100
#undef MYCONST

#ifdef __HARBOUR__
#else
#endif

#if defined( __HARBOUR__ ) .OR. .T.
#elif defined( __HARBOUR__ )
#endif

THREAD STATIC t_var := "thread"

REQUEST AllTrim
ANNOUNCE my_module

PROCEDURE Main()

   MEMVAR p_var, m_var
   FIELD fld

   STATIC s_test := "static"
   LOCAL o := TTest():New( "one", "two" ), tmp
   LOCAL oError
   LOCAL bBlock := {| tmp | QOut( tmp ) }
   LOCAL hHash := { "name" => "value", "name2" => "value2", 2 => 1 }
   PUBLIC p_var := "public"
   PRIVATE m_var := "private"
   PARAMETERS p1

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   CLS
   @ 10, 10 SAY "Hello world!"

   ? hb_ValToExp( o )
   ? m->p1
   ? hHash[ "name" ], hHash[ 2 ]

   FOR tmp := 1 TO 10 STEP 2
      ? tmp
   NEXT

   FOR EACH tmp IN { "a", "b" } DESCEND
      ? tmp
   NEXT

   ? 10.01 + 2 - 3 / 4 * 5 ^ 6 ** 1
   ? 2 < 1, 2 > 1, 2 >= 1, 2 <= 1, 2 == 1, 2 = 1, 2 != 1, 2 <> 1, 2 # 1
   ? -( 1 + 2 ), "a" $ "ab", 10 % 2

   tmp := 0
   DO WHILE tmp < 2
      ? tmp++
   ENDDO

   tmp := 0
   WHILE tmp < 2
      ? ++tmp
      IF tmp >= 0xFF
         LOOP
      ENDIF
      EXIT
   ENDDO

   --tmp
   tmp--

   IF tmp < -10.0
      ? NIL
   ELSEIF .F.
      ? 0d19800101
   ELSE
      ? "string"
   ENDIF

   DO CASE
   CASE tmp == 1
      ? "A"
   OTHERWISE
      ? "B"
   ENDCASE

   SWITCH tmp
   CASE 1
      ? "A"
      EXIT
   OTHERWISE
      ? "B"
   ENDSWITCH

   BEGIN SEQUENCE WITH __BreakBlock()
      BREAK
   RECOVER USING oError
   END /* SEQUENCE */

   local_func( @hHash )

   RETURN

INIT PROCEDURE init_proc()
   RETURN

EXIT PROCEDURE exit_proc()
   RETURN

PROCEDURE returning_nothing()
   RETURN

FUNCTION pub_func()
   RETURN .T.

STATIC FUNCTION local_func()
   RETURN .F.

CREATE CLASS TTest INHERIT TParent
   VAR One, Two
   METHOD New( One )
   METHOD Test() INLINE QOut( "Hello" )
   METHOD MethProc()
ENDCLASS

METHOD PROCEDURE MethProc()
   RETURN

METHOD New( One ) CLASS TTest

   ::super:New()
   ::One := One

   RETURN Self

CREATE CLASS TParent
   VAR One
   METHOD New()
ENDCLASS

METHOD New() CLASS TParent
   ? "TParent:New()"
   RETURN Self

// This is a comment
/* This is a comment */
/* This is
a comment */
* This is a comment
&& This is a comment
NOTE This is a comment
note This is a comment
NOTE

FUNCTION pub_func2()
   ? "hello world!"
   ? 'hello world!'
   ? "hello\world!"
   ? "\"
   ? "hello world!"
   RETURN .T.

!
! $Id: descrip.mms 35 2008-01-08 21:37:42Z tmr $
!
! Project:  LISP -- The LISP Interpreter
! Created:  22-DEC-2008 18:35
! Author:   tmr

cc = cc
cflags = /define="_VMS_=1" -
/WARN=DISABLE=(ZERODIV,FLOATOVERFL,NOMAINUFLO) -
/IEEE_MODE=UNDERFLOW_TO_ZERO/FLOAT=IEEE
core = LISP_CORE
main = LISP_MAIN
exec = [.bin]LISP
clib = SYS$LIBRARY:VAXCRTL
head = LISP_CORE
objs = $(core).obj, $(main).obj

$(exec) : $(objs)
        DEFINE/NOLOG LNK$LIBRARY $(clib)
        LINK/EXEC=$(exec) $(objs)
        DEASSIGN LNK$LIBRARY

$(core).obj : $(core).c, $(head).h

$(main).obj : $(main).c, $(head).h

clean :
        del *.obj;*
        del *.exe;*


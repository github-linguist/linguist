/* Display information about Blitz Basic .shapes file,
   optionally displaying the shape's cookiecut
$AUTHOR: Iggy Drougge 2016
$VER: 1.1
*/
PARSE ARG argument
template = 'FROM/N TO/N SHOW/S FILE/A'
IF argument = '' | argument = '?' THEN DO
   SAY 'ShapesInfo' template
   EXIT 0
   END

CALL ReadArgs()

IF ~Open(fh,args.file,READ) then DO
   SAY "Couldn't Open file:" args.file
   EXIT 10
   END

shape#=0
filebad=0
IF args.to=0 THEN args.to=9999

IF args.from>1 THEN SAY 'Seeking...'
DO WHILE ~EOF(fh)
   header=ReadCh(fh,32)
   IF EOF(fh) THEN DO
      SAY 'Reached end of file.'
      EXIT 0
      END
   PARSE VALUE header WITH pixwidth +2 pixheight +2 depth +2 ebwidth +2 bltsize +2 xhandle +2 yhandle +2 . +4 . +4 onebpmem +2 onebpmemx +2 allbpmem +2 allbpmemx +2 .
   CALL CheckHeader
   IF filebad THEN DO
      SAY 'Not a valid shapes file.'
      SAY C2X(header)
      EXIT 10
      END
   shape#=shape#+1
   bitplanesize = C2D(ebwidth) * C2D(pixheight)
   bitmapsize = bitplanesize * C2D(depth)
   IF shape# < args.from THEN DO
      CALL Seek(fh,bitmapsize,CURRENT)
      ITERATE
      END
   IF shape# > args.to THEN LEAVE
   CALL PrintHeader
   IF args.show THEN CALL ShowCookiecut
   ELSE CALL Seek(fh,bitmapsize,CURRENT)
   END
EXIT 0

CheckHeader:
   IF C2D(pixwidth)>C2D(ebwidth)*8 THEN filebad=1
   IF Left(C2B(bltsize),10)~=C2B(pixheight) THEN filebad=1
RETURN

PrintHeader:
   SAY 'Shape #' || shape# || ':'
   SAY '  Width:    ' C2D(pixwidth) 'pixels' '('C2D(ebwidth) 'bytes)'
   SAY '  Height:   ' C2D(pixheight) 'pixels'
   SAY '  Depth:    ' C2D(depth) 'bitplanes'
   SAY '  BLTSIZE:  ' '$'C2X(bltsize) '('||,
       C2D(B2C(Left(C2B(bltsize),10))) 'x',
       C2D(B2C(Right(C2B(bltsize),6)))')'
   SAY '  Handle:   ' C2D(xhandle)','C2D(yhandle)
/*
   SAY 'Onebpmem: ' C2D(onebpmem)
   SAY 'OnebpmemX:' C2D(onebpmemx)
   SAY 'Allbpmem: ' C2D(allbpmem)
   SAY 'AllbpmemX:' C2D(allbpmemx)
*/
RETURN



ShowCookiecut:
   depth=C2D(depth)
   bmap=Copies('00'x,bitplanesize)
   DO FOR depth
      bmap=BitOr(bmap,readch(fh,bitplanesize))
      END
   ln=1 ; pixheight=C2D(pixheight) ; ebwidth=C2D(ebwidth)
   DO FOR pixheight
      SAY C2B(SubStr(bmap,ln,ebwidth))
      ln=ln+ebwidth
      END
RETURN

EXIT 0

ReadArgs:
/* ReadArgs()-like evaluation of command line arguments */
DO key# = 1 TO Words(Template) /* Initialise the keywords */
   key=Word(template,key#)
   PARSE VAR key key "/" keytype
   SELECT
      WHEN keytype='S'|keytype='N' THEN args.key=0
      WHEN keytype='K'|keytype='A' THEN args.key=''
      OTHERWISE NOP     /* Error in template */
      END
   END

DO WHILE argument ~= ''
   PARSE VAR argument arg1 argument
   arg2=''
   DO key# = 1 TO Words(template)
      key = Word(template,key#)
      PARSE VAR key key '/' keytype
      IF Upper(Left(arg1,Length(key))) = key THEN DO
         SELECT
            WHEN keytype='S' THEN DO
               args.key=1
               END
            WHEN keytype='K' | keytype='N' | keytype='A' THEN DO
               IF Index(arg1,'=')>0
                  THEN DO
                     SAY 'Innehaller ='
                     PARSE VAR arg1 '=' arg2
                     SAY 'arg2:' arg2
                     END
                  ELSE PARSE VAR argument arg2 argument
               args.key=arg2
               IF keytype='N' & DataType(arg2)~==NUM THEN DO
                  SAY 'Illegal numerical argument' key arg2
                  EXIT 10
                  END
               END
            END
            arg1=''
            LEAVE key#
         END
      END
      IF arg1~='' THEN args.file=arg1
   END
RETURN
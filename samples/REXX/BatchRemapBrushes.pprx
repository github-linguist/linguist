/* Ga igenom lador med IFF-filer rekursivt och skapa:
   1: Spritekartor om 320x256 med alla spritar
   2: En tabell med enumererade spritenummer
   Aterstar:
   *: Att skriva shapesfiler direkt
   $VER: 1.0
*/
SIGNAL ON BREAK_C
PARSE ARG argument
template = 'START/N COLOURS/N REMAP/S RECURSIVE/S PALETTE/K DIRECTORY/A'
CALL Init

ADDRESS 'PPAINT'
OPTIONS RESULTS
GetBestVideoMode WIDTH 320 HEIGHT 256 COLORS args.colours
modeid=Word(RESULT,1)
say modeid
Set 'FORCE "DISPLAY=' || modeid || '"'
IF RC~=0 THEN EXIT 20
Set 'FORCE "IMAGEW=320" "IMAGEH=256" "COLORS='args.colours'"'
ClearImage
LockGUI
ScreenToFront
x=0 ; y=0 ; maxhoejd=0 ; fil#=0 ; blad#=0
CALL WriteLn konstfil, ';Blad' blad#
DO UNTIL Lines()==0 & fillista==''
   DO WHILE fillista~=''
      PARSE VAR fillista filnamn 'y' fillista
      IF Right(filnamn,5)=='.info' THEN ITERATE
      IF Word(StateF(dir||filnamn),1)=='DIR' THEN DO
         PUSH fillista
         PUSH dir
         dir=dir||filnamn||'/'
         SAY 'Entering directory' dir'...'
         fillista=ShowDir(dir,'ALL','y')
         ITERATE
         END
      LoadBrush FILE dir || filnamn NOPROGRESS
      IF RC~==0 THEN DO
         SAY 'Skipping file:' dir || filnamn
         ITERATE
         END
      IF args.remap THEN RemapBrush NOPROGRESS
      GetBrushAttributes WIDTH  ; bredd=RESULT
      GetBrushAttributes HEIGHT ; hoejd=RESULT
      GetBrushAttributes COLORS ; djup=RESULT
      IF bredd//16==0 THEN ebwidth=bredd
      ELSE ebwidth=bredd+(16-(bredd//16))
      maxhoejd=Max(maxhoejd,hoejd)
      SAY 'File:' Left(filnamn,29) 'Width:' bredd ' Height:' hoejd ' Depth:' djup ' ebwidth:' ebwidth
      SetCurrentBrush RECTANGULAR WIDTH 1 HEIGHT 1
      DrawRectangle x y x+ebwidth y+hoejd
      Text fil# 'X' x+2 'Y' y+2
      SetCurrentBrush 1 ; SetBrushHandle 0 0
      PutBrush x y
      CALL WriteLn(konstfil,'; '||fil# ||': '|| filnamn ||' ('||ebwidth||'x'||hoejd||'x'||djup||')')
      CALL WriteLn(konstfil,'#'||Upper(Left(filnamn,Min(Length(filnamn),Max(LastPos('.',filnamn)-1,0))))||'='fil#)
      x=x+ebwidth
      IF x+ebwidth>319 THEN DO
         x=0
         y=y+maxhoejd
         IF y+maxhoejd>255 THEN DO
            SaveImage FILE 'Spritesheet' || blad# || '.ilbm' FORCE
            IF RC==0 THEN SAY 'Saved sheet' blad#
            ELSE SAY "Couldn't save spritesheet"
            ClearImage
            y=0
            blad#=blad#+1
            CALL WriteLn konstfil, ';Blad' blad#
            END
         maxhoejd=0
         END
      fil#=fil#+1
      END /* WHILE fillista */
   IF Lines()>0 THEN DO
      PARSE PULL dir
      PARSE PULL fillista
      SAY 'Going back to' dir'...'
      END
END /* UNTIL Lines() */

BREAK_C:
FreeBrush FORCE
UnLockGUI
medd=fil# 'files processed in' blad#+1 'sheets'
SAY medd
RequestNotify 'TITLE SpriteSheet.pprx PROMPT "'medd'"'
ScreenToBack
CALL Close(konstfil)
DO WHILE Lines()>0; PULL .; END
EXIT 0

Init:
   IF argument = '' | argument = '?' THEN DO
      SAY template
      EXIT 0
      END

   CALL ReadArgs()

   IF ~Show('L',"rexxsupport.library") THEN DO
      IF ~AddLib("rexxsupport.library",0,-30,0) THEN DO
         SAY 'Hittade inte rexxsupport.library'
         EXIT 20
         END
      END
   IF ~Open(konstfil,'SpriteConstants.txt','WRITE') THEN EXIT 10

   IF ~SHOW('P', 'PPAINT') THEN DO
      SAY "Couldn't find PPaint. Please start the program first."
      EXIT 5
      END
   dir=args.directory
   IF dir='""' THEN dir=Pragma('DIRECTORY')
   IF dir='' THEN dir='Ram:Megamanv6/Graphics/'
   IF Right(dir,1)~=='/' THEN dir=dir || '/'
   fillista=ShowDir(dir,'FILES','y')
   fillista=ShowDir(dir,'ALL','y')
   IF fillista="" THEN DO
      SAY "Found no files"
      EXIT 5
      END
   IF args.colours==0 THEN args.colours=16
   ADDRESS 'PPAINT'
   OPTIONS RESULTS
   IF args.palette~='' THEN LoadPalette args.palette
RETURN

ReadArgs:
/* ReadArgs()-like evaluation of command line arguments */
SAY 'ReadArgs'
DO key# = 1 TO Words(Template)
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
      IF arg1~='' THEN args.directory=arg1
   END
RETURN
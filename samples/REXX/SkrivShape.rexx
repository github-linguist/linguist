/* rexx */
PARSE ARG filnamn
IF filnamn='' THEN DO
   filnamn='raw'
   filnamn='font.shapes'
   end
IF ~open(fil,filnamn,r) THEN EXIT 10
pixwidth=48
ebwidth=pixwidth/8
pixheight=48
depth=4
SAY "Skriver utfil..."
CALL open utfil,"RAM:utfil",W
CALL skriv pixwidth,2
CALL skriv pixheight,2
CALL skriv depth,2
CALL skriv ebwidth,2
bltsize=Right(C2B(D2C(pixheight)),10,"00")
bltsize=bltsize || Right(C2B(D2C(ebwidth)),6,"00")
/* SAY bltsize */
CALL skriv C2D(B2C(bltsize)),2
CALL skriv 0,4 /* xhandle, yhandle*/
CALL skriv 0,4 /* datapekare */
CALL skriv 0,4 /* cookiepekare */
CALL skriv ebwidth*pixheight,2 /* onebpmem */
CALL skriv ebwidth*pixheight+pixheight*2,2 /* onebpmemx */
CALL skriv ebwidth*pixheight*depth,2 /* allbpmem */
CALL skriv ebwidth*pixheight*depth+pixheight*2*depth,2 /* allbpmemx */
CALL skriv 0,2 /* padding */
CALL Close utfil
EXIT

skriv:
say "Skriver $"D2X(arg(1)) "("arg(2) "byte)"
call writech utfil,right(D2C(ARG(1)),ARG(2),"00"x)
return

visacookie:
   rad=copies('00'x,pixheight*ebwidth)
   say "Initierar bitmap till" pixheight*ebwidth*depth
   say "Ett bitplan =" pixheight*ebwidth
   bmap.=''
   say "laser in"
   do bitplan=1 to depth
      say "laser plan" bitplan
      rad=bitor(rad,readch(fil,pixheight*ebwidth))
      end
   ln=1
   say "skriver ut"
   do for pixheight
      say c2b(substr(rad,ln,bredd/8))
      ln=ln+bredd/8
      end
return
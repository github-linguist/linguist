; ColorQuantization.pb

Structure bestA_ ; table for our histogram
nn.i ; 16,32,...
rc.i ; red   count within (0,1,...,255)/(number of colors)
gc.i ; green count within (0,1,...,255)/(number of colors)
bc.i ; blue  count within (0,1,...,255)/(number of colors)
EndStructure

; these two functions appear to be rather self-explanatory
UsePNGImageDecoder()
UsePNGImageEncoder()

Procedure.i ColorQuantization(Filename$,ncol)
Protected x,y,c

; load our original image or leave the procedure
If not LoadImage(0,Filename$)      :ProcedureReturn 0:endif

; we are not going to actually draw on the original image...
; but we need to use the drawing library to load up
; the pixel information into our arrays...
; if we can't do that, what's the point of going any further?
; so then we would be wise to just leave the procedure [happy fred?]
If not StartDrawing(ImageOutput(0)):ProcedureReturn 0:endif

iw=ImageWidth(0)
ih=ImageHeight(0)

dim cA(iw,ih) ; color array to hold at a given (x,y)
dim rA(iw,ih) ; red   array to hold at a given (x,y)
dim gA(iw,ih) ; green array to hold at a given (x,y)
dim bA(iw,ih) ; blue  array to hold at a given (x,y)
dim tA(iw,ih) ; temp  array to hold at a given (x,y)

; map each pixel from the original image to our arrays
; don't overrun the ranges ie. use {ih-1,iw-1}
for y=0 to ih-1
  for x=0 to iw-1
  c = Point(x,y)
  cA(x,y)=c
  rA(x,y)=Red(c)
  gA(x,y)=Green(c)
  bA(x,y)=Blue(c)
  next
next

StopDrawing() ; don't forget to... StopDrawing()

N=ih*iw
; N is the total number if pixels
if not N:ProcedureReturn 0:endif ; to avoid a division by zero

; stuctured array ie. a table to hold the frequency distribution
dim bestA.bestA_(ncol)

; the "best" red,green,blue based upon frequency
dim rbestA(ncol/3)
dim gbestA(ncol/3)
dim bbestA(ncol/3)

; split the (0..255) range up
xoff=256/ncol   ;256/16=16
xrng=xoff       ;xrng=16

; store these values in our table: bestA(i)\nn= 16,32,...
for i=1 to ncol
xrng+xoff
bestA(i)\nn=xrng
next

; scan by row [y]
for y=0 to ih-1
; scan by col [x]
for x=0 to iw-1

; retrieve the rgb values from each pixel
r=rA(x,y)
g=gA(x,y)
b=bA(x,y)

; sum up the numbers that fall within our subdivisions of (0..255)
for i=1 to ncol
if r>=bestA(i)\nn and r<bestA(i+1)\nn:bestA(i)\rc+1:endif
if g>=bestA(i)\nn and g<bestA(i+1)\nn:bestA(i)\gc+1:endif
if b>=bestA(i)\nn and b<bestA(i+1)\nn:bestA(i)\bc+1:endif
next
next
next

; option and type to: Sort our Structured Array
opt=#PB_Sort_Descending
typ=#PB_Sort_Integer

; sort to get most frequent reds
off=OffsetOf(bestA_\rc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] reds
for i=1 to ncol/3
rbestA(i)=bestA(i)\nn
next

; sort to get most frequent greens
off=OffsetOf(bestA_\gc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] greens
for i=1 to ncol/3
gbestA(i)=bestA(i)\nn
next

; sort to get most frequent blues
off=OffsetOf(bestA_\bc)
SortStructuredArray(bestA(),opt, off, typ,1, ncol)

; save the best [ for number of colors =16 this is int(16/3)=5 ] blues
for i=1 to ncol/3
bbestA(i)=bestA(i)\nn
next

; reset the best low value to 15 and high value to 240
; this helps to ensure there is some contrast when the statistics bunch up
; ie. when a single color tends to predominate... such as perhaps green?
rbestA(1)=15:rbestA(ncol/3)=240
gbestA(1)=15:gbestA(ncol/3)=240
bbestA(1)=15:bbestA(ncol/3)=240

; make a copy of our original image or leave the procedure
If not CopyImage(0,1)              :ProcedureReturn 0:endif

; draw on that copy of our original image or leave the procedure
If not StartDrawing(ImageOutput(1)):ProcedureReturn 0:endif

for y=0 to ih-1
for x=0 to iw-1
c = Point(x,y)

; get the rgb value from our arrays
rt=rA(x,y)
gt=gA(x,y)
bt=bA(x,y)

; given a particular red value say 123 at point x,y
; which of our rbestA(i's) is closest?
; then for green and blue?
; ==============================
r=255
for i=1 to ncol/3
rdiff=abs(rbestA(i)-rt)
if rdiff<=r:ri=i:r=rdiff:endif
next

g=255
for i=1 to ncol/3
gdiff=abs(gbestA(i)-gt)
if gdiff<=g:gi=i:g=gdiff:endif
next

b=255
for i=1 to ncol/3
bdiff=abs(bbestA(i)-bt)
if bdiff<=b:bi=i:b=bdiff:endif
next
; ==============================


; get the color value so we can plot it at that pixel
Color=RGB(rbestA(ri),gbestA(gi),bbestA(bi))

; plot it at that pixel
Plot(x,y,Color)

; save that info to tA(x,y) for our comparison image
tA(x,y)=Color

next
next
StopDrawing() ; don't forget to... StopDrawing()

; create a comparison image of our original vs 16-color or leave the procedure
If not CreateImage(2,iw*2,ih)      :ProcedureReturn 0:endif
; draw on that image both our original image and our 16-color image or leave the procedure
If not StartDrawing(ImageOutput(2)):ProcedureReturn 0:endif

; plot original image
; 0,0 .... 511,0
; .
; .
; 511,0 .. 511,511
for y=0 to ih-1
  for x=0 to iw-1
  c = cA(x,y)
  Plot(x,y,c)
  next
  next

; plot 16-color image to the right of original image
; 512,0 .... 1023,0
; .
; .
; 512,511 .. 1023,511
for y=0 to ih-1
  for x=0 to iw-1
  c = tA(x,y)
  Plot(x+iw,y,c)
  next
  next

StopDrawing() ; don't forget to... StopDrawing()

; save the single 16-color image
SaveImage(1, "_single_"+str(ncol)+"_"+Filename$,#PB_ImagePlugin_PNG )

; save the comparison image
SaveImage(2, "_compare_"+str(ncol)+"_"+Filename$,#PB_ImagePlugin_PNG )
ProcedureReturn 1
EndProcedure

ColorQuantization("Quantum_frog.png",16)

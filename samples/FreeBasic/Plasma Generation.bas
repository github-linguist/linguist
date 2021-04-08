'+++++ IMPORTANT +++++
' - The only reason this
'   code doesnt generate
'   plasma instantaneously
'   is because it draws
'   each iteration instead
'   of the final product.
'   It looks cooler this
'   way : )


Dim Shared As Integer Grad(0 To 255)


'+=============================================================+
Sub Rainbow()
    Dim As Integer i, b, badd, g, gadd, r, radd, rx, gx, bx
    rx =  2
    gx = -2
    bx = -2
    b=Int(Rnd * 256):badd= bx
    g=Int(Rnd * 256):gadd= gx
    r=Int(Rnd * 256):radd= rx
    For i = 0 To 255
        b += badd
        g += gadd
        r += radd
        If b < 0   Then badd = -bx: b = 0
        If b > 255 Then badd =  bx: b = 255
        If g < 0   Then gadd = -gx: g = 0
        If g > 255 Then gadd =  gx: g = 255
        If r > 255 Then radd = -rx: r = 255
        If r < 0   Then radd =  rx: r = 0
        Grad(i) = RGB(r,g,b)
    Next i
End Sub
'+==============+MAKE THIS WHATEVER YOU WANT : )+==============+


Rainbow


Sub GenPlasma(byval w     as integer, byval h     as integer, _
              byval crnr1 as integer, byval crnr2 as integer, _
              byval crnr3 as integer, byval crnr4 as integer, _
              byval rough as integer, byval iter  as integer, _
              byval prs   as double)
    Dim as double prex, prey, d1, d2, d3, d4, hr, fv, Image(0 to w,0 to h)
    hr = rough * 2
    prex = w / 2: prey = h / 2
    Image(0,0)=crnr1
    Image(w,0)=crnr2: d1 = (crnr1+crnr2) / 2: Image(prex,0)=d1
    Image(w,h)=crnr3: d2 = (crnr2+crnr3) / 2: Image(w,prey)=d2
    Image(0,h)=crnr4: d3 = (crnr3+crnr4) / 2: Image(prex,h)=d3
                      d4 = (crnr4+crnr1) / 2: Image(0,prey)=d4
    fv = ((d1+d2+d3+d4) / 4) + (Int(rnd * hr) - rough)
    If fv>255 Then
        fv=255
    ELseif fv<0 Then
        fv=0
    Endif
    Image(Cint(prex),Cint(prey))=fv
    Dim as double divisor, mdivx, mdivy, i, xs, ys, c1,c2,c3,c4, cx,cy, dx,dy
    mdivx = w / 2: mdivy = h / 2
    w -= 1: h -= 1
    For i = 1 to iter
        For ys = 0 To h Step mdivy
            For xs = 0 To w Step mdivx
                prex = mdivx / 2: prey = mdivy / 2
                cx   = xs + mdivx : cy   = ys + mdivy
                c1 = Image(Cint(xs),Cint(ys)): c2 = Image(Cint(cx),Cint(ys))
                c3 = Image(Cint(cx),Cint(cy)): c4 = Image(Cint(xs),Cint(cy))
                d1 = (c1+c2) / 2: d2 = (c2+c3) / 2
                d3 = (c3+c4) / 2: d4 = (c4+c1) / 2
                dx = xs + prex: dy = ys + prey
                Image(Cint(dx),Cint(ys))=d1
                Image(Cint(cx),Cint(dy))=d2
                Image(Cint(dx),Cint(cy))=d3
                Image(Cint(xs),Cint(dy))=d4
                fv = ((d1+d2+d3+d4) / 4) + (Int(rnd * hr) - rough)
                If fv>255 Then
                    fv=255
                ELseif fv<0 Then
                    fv=0
                Endif
                Image(Cint(dx),Cint(dy))=fv
            Next xs
        Next ys
        mdivx = mdivx / 2
        mdivy = mdivy / 2
        hr    = rough
        rough = rough * prs
        ScreenLock
        For ys = 0 To h Step mdivy
            For xs = 0 To w Step mdivx
                Line (xs,ys)-(xs+mdivx,ys+mdivy), Grad(Image(xs,ys)), BF
            Next xs
        Next ys
        ScreenUnlock
    Next i
End Sub



'test code

#include "fbgfx.bi"
Using FB
#define ri(x) (Int(Rnd*x))
ScreenRes 1280,1024,32,,1
Randomize Timer

Do
    Rainbow
    GenPlasma 1280,1024,ri(256),ri(256),ri(256),ri(256),300,8,Rnd
    Locate 1,1: Print "Press the spacebar for another pattern. Press ESC to quit"
    Do
        If MultiKey(&h01) Then
            End
        ElseIf MultiKey(SC_SPACE) Then
            Goto ExitDo
        EndIf
    Loop
    ExitDo:
Loop

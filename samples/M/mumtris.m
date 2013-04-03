;
;  Mumtris
;  Copyright (C) 2012 Piotr Koper <piotr.koper@gmail.com>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU Affero General Public License as
;  published by the Free Software Foundation, either version 3 of the
;  License, or (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU Affero General Public License for more details.
;
;  You should have received a copy of the GNU Affero General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

; Mumtris
; This is a tetris game in MUMPS, for GT.M, have fun.
;
; Resize your terminal (e.g. maximize your PuTTY window), restart GT.M so that
; it can report true size of your terminal, and d ^mumtris.
;
; Try setting ansi=0 for GT.M compatible cursor positioning.
;
; NOTICE: Mumtris uses "active waiting" for making delays lower that 1s.
;         That means that one of your CPU will be used at 99%. It's not a bug,
;         the Mumtris and GT.M will be fully responsive. Take care when
;         running on production system ;-)
;

mumtris
	n ansi,e,n,w,h,gr,fl,hl,sc,lv,lc,sb,st,ml,dh,dw,mx,my,mt,r,y,x,t10m,c,ne,i,q

	s ansi=1	; use (faster) ANSI CSI instead of USE $P:X=x positioning
	s w=10		; matrix width
	s h=22		; matrix height (see below)
	s gr=1		; grid
	s fl=1		; fill
	s hl=1		; help
	s sc=0		; score
	s lv=1		; level
	s lc=0		; lines cleared at current level
	s sb=70		; step base
	s st=$$step	; current step
	s ml=3		; move/rotate hold limit (without fall)

	d dev		; defines dw, dh (device width, device height)
	s h=dh-2	; comment out to disable auto height
	s mx=dw/2-(3*w/2)	; matrix left coordinate
	s my=dh/2-(h/2)-1	; matrix top coordinate
	s mt="3 5_9 8 2_9 .2_02 /5 \2 2_ 2_2 6_/2 |8_|2_| 6_0 /2 \ /2 \|2 |2 \/5 \3 2_\_2 2_ \2 |/2 3_/0/4 Y4 \2 |2 /2 Y Y2 \2 |2 |2 | \/2 |\3_ \0\4_|2_2 /4_/|2_|_|2 /2_|2 |2_|2 |2_/4_2 >08 \/9 3 \/9 9 2 \/0" ; Mumtris

	u $p:noecho
	u $p:escape
	d cls

	d intro

	d elements
	s ne=$r(e)+1	; new element
	d change,new(),preview
	d score(),help,redraw

	s (i,q)=0
	f  q:q  d
	. d pos(0,0)
	. s c=$$key
	. i c=1 d exit s q=1 q
	. s i=$s('c:0,1:i+1)
	. s:i'<ml (i,c)=0
	. i c'=3,$$fall d lock,clear,change,preview i $$new d over,exit s q=1 q	; short-circuit and in first if
	. d redraw
	q

key() ; 0 - timeout, 1 - exit, 2 - harddrop, 3 - other char
	n q,c,d,ex,hd
	s (q,d,ex,hd)=0
	n i
	n l s l=1
	f  q:q  d
	. r *c:0
	. i c<0&'d d
	.. f i=1:1:st*t10m r *c:0 q:c>-1  i $h
	. i c<0 s q=1 q
	. s d=2
	. i c=27 d  q:q
	.. i $l($zb)=1 s (q,ex)=1 q
	.. s c=$a($e($zb,3))
	.. d:c=65 rotate
	.. d:c=66 fall(1)
	.. d:c=67 right
	.. d:c=68 left
	. i c=70!(c=102) s fl=fl+1#3 d preview
	. s:c=71!(c=103) gr='gr
	. i c=72!(c=104) s hl='hl d help
	. d:c=73!(c=105) rotate
	. d:c=74!(c=106) left
	. d:c=75!(c=107) fall(1)
	. d:c=76!(c=108) right
	. s:c=81!(c=113) (q,ex)=1
	. i c=32 d drop s hd=1
	q $s(ex:1,hd:2,d:3,1:0)

redraw
	d matrix
	d stack
	d draw(n,r,y,x)
	q

ticks
	n x,h,b,e,q
	s h=$h,(b,e,q)=0 f i=1:1:1000000000 r *x:0 i h'=$h s h=$h d  q:q
	. i 'b s b=i
	. e  s e=i,q=1
	s t10m=(e-b)\100
	q

change
	s n=ne
	s ne=$r(e)+1
	s x=0,y=0,r=1
	q

new()
	s r=1,x=w/2-2,y=1-e(n,r)
	q:$q $$collision(r,y,x) q

drop
	n i
	s i=0 f  q:$$fall  s i=i+2
	d score(i)
	q

rotate
	n k
	s k=r#e(n)+1
	q:$$collision(k,y,x)
	s r=k
	q

fall(k)
	n c
	i $$collision(r,y+1,x) q:$q 1 q
	s y=y+1
	d:$g(k) score(1)
	q:$q 0 q

right	q:$$collision(r,y,x+1)  s x=x+1 q
left	q:$$collision(r,y,x-1)  s x=x-1 q

collision(r,y,x)
	n i,j,q
	s q=0
	f i=1:1:4 q:q  f j=1:1:4 q:q  s:$g(e(n,r,j,i))&($g(n(y+j,x+i))!(y+j>h!(x+i>w!(x+i<1)))) q=1
	q q

lock
	n i,j
	f i=1:1:4 q:q  f j=1:1:4 q:q  s:$g(e(n,r,j,i)) n(y+j,x+i)=1
	q

clear
	n c,i,j,q
	s c=0
	f j=h:-1:1 d
	. s q=0
	. f i=1:1:w i '$g(n(j,i)) s q=1 q
	. q:q
	. f i=j:-1:1 k n(i) m n(i)=n(i-1)
	. s j=j+1,c=c+1
	. d redraw
	i c d
	. d score($s(c=4:800,1:i*200-100*lv))
	. s lc=lc+c
	. i lv*10'>lc d score(,1) s lc=0
	q

exit
	n s
	s s=mt_"09  Piotr Koper <piotr.koper@gmail.com>09 8 h2tps:2/github.com/pkoper"
	d cls d write(.s,dh/2-3,dw/2-24) h 1 r *s:0 r *s:4
	d cls u $p:echo
	q

intro
	n s
	s s=mt_"9 9 8 Mumtris for GT.M0"
	d cls h 1 d write(.s,dh/2-3,dw/2-24) h 1
	d ticks
	d cls
	r s:0
	q

cls
	d pos(0,0,1)
	q

pos(y,x,c)
	i ansi d
	. ; workaround for ANSI driver: NL in some safe place (1,1)
	. w $c(27)_"[1;1f",!,$c(27)_"["_(y\1+1)_";"_(x\1+1)_"f"
	. w:$g(c) $c(27)_"[2J"
	e  d
	. u $p:(x=x:y=y)
	. u:$g(c) $p:clearscreen
	q

over
	n s
	s s="2 8_9 9 6 8_0 /2 5_/5_4 5_3 4_3 \5_2 \3_2 2_ 9_2_0/3 \2 3_\2_2 \2 /5 \_/ 2_ \3 /3 |3 \2 \/ 2/ 2_ \_2 2_ \0\4 \_\2 \/ 2_ \|2 Y Y2 \2 3_/2 /4 |4 \3 /\2 3_/|2 | \/0 \6_2 (4_2 /2_|_|2 /\3_2 > \7_2 /\_/2 \3_2 >2_|08 \/5 \/6 \/5 \/9  \/9  \/0"
	d cls,write(.s,dh/2-3,dw/2-32) h 1 r *s:0 r *s:2
	q

write(s,y,x)
	n i,j,l,c,d
	d pos(y,x)
	s l=$l(s) f i=1:1:l d
	. s c=$e(s,i)
	. i c?1N d
	.. i 'c s y=y+1 d pos(y,x) q
	.. s d=$e(s,i+1) f j=1:1:c w d
	.. s i=i+1
	. e  w c
	d pos(0,0)
	q

help
	n i,x,l,j
	s i=9 f x="MOVE: LEFT, RIGHT","TURN: UP","DROP: SPACE","","FILL: F","GRID: G","HELP: H","","QUIT: ESC, Q" d pos(dh/2-(h/2)+i,dw/2+(3*w/2+3)) d  s i=i+1
	. i hl w x
	. e  s l=$l(x) f j=1:1:l w " "
	q

fill() q $s(fl=1:"[#]",fl=2:"[+]",1:"[ ]")

draw(n,r,y,x,o)
	n i,j
	s x=3*x+mx+1,y=y+my
	f i=1:1:4 i y+i>my f j=1:1:4 d pos(y+i-1,3*(j-1)+x) w $s($g(e(n,r,i,j)):$$fill,$g(o):"   ",1:"")
	q

step() q 0.85**lv*sb+(0.1*lv)

score(s,l)
	s:$g(s) sc=sc+s
	i $g(l) s lv=lv+l,st=$$step
	d pos(dh/2-(h/2)+2,dw/2+(3*w/2+3)) w "SCORE: ",sc
	d pos(dh/2-(h/2)+3,dw/2+(3*w/2+3)) w "LEVEL: ",lv
	q

preview
	d draw(ne,1,4-e(ne,1),-5,1)
	q

stack
	n i,j,x,y
	s x=mx+1,y=my
	f i=1:1:h f j=1:1:w i $g(n(i,j)) d pos(y+i-1,3*(j-1)+x) w $$fill
	q

matrix
	n i,j
	f i=0:1:h-1 d
	. d pos(my+i,mx) w "|" f j=1:1:w w $s(gr:" . ",1:"   ")
	. w "|"
	d pos(my+h,mx) w "|" f j=1:1:w*3 w "~"
	w "|",!
	q

dev
	n x,i
	zsh "d":x
	s i="" f  s i=$o(x("D",i)) q:i=""  d:(x("D",i)[$p)
	. s dw=$p($p(x("D",i),"WIDTH=",2)," ",1),dh=$p($p(x("D",i),"LENG=",2)," ",1)
	q

elements
	; e - elements
	; e(elemId) - rotateVersions
	; e(elemId,rotateVersion) - bottom coordinate
	; e(elemId,rotateVersion,y,x) - point
	;
	s e=7
	; ____
	s e(1)=2,e(1,1)=2
	s (e(1,1,2,1),e(1,1,2,2),e(1,1,2,3),e(1,1,2,4))=1
	s (e(1,2,1,2),e(1,2,2,2),e(1,2,3,2),e(1,2,4,2))=1
	; |__
	s e(2)=4,e(2,1)=2
	s (e(2,1,1,1),e(2,1,2,1),e(2,1,2,2),e(2,1,2,3))=1
	s (e(2,2,1,2),e(2,2,1,3),e(2,2,2,2),e(2,2,3,2))=1
	s (e(2,3,2,1),e(2,3,2,2),e(2,3,2,3),e(2,3,3,3))=1
	s (e(2,4,1,2),e(2,4,2,2),e(2,4,3,1),e(2,4,3,2))=1
	; __|
	s e(3)=4,e(3,1)=2
	s (e(3,1,1,3),e(3,1,2,1),e(3,1,2,2),e(3,1,2,3))=1
	s (e(3,2,1,2),e(3,2,2,2),e(3,2,3,2),e(3,2,3,3))=1
	s (e(3,3,2,1),e(3,3,2,2),e(3,3,2,3),e(3,3,3,1))=1
	s (e(3,4,1,1),e(3,4,1,2),e(3,4,2,2),e(3,4,3,2))=1
	; ||
	s e(4)=1,e(4,1)=2
	s (e(4,1,1,1),e(4,1,1,2),e(4,1,2,1),e(4,1,2,2))=1
	; _-
	s e(5)=2,e(5,1)=3
	s (e(5,1,2,2),e(5,1,2,3),e(5,1,3,1),e(5,1,3,2))=1
	s (e(5,2,1,2),e(5,2,2,2),e(5,2,2,3),e(5,2,3,3))=1
	; _|_
	s e(6)=4,e(6,1)=2
	s (e(6,1,1,2),e(6,1,2,1),e(6,1,2,2),e(6,1,2,3))=1
	s (e(6,2,1,2),e(6,2,2,2),e(6,2,2,3),e(6,2,3,2))=1
	s (e(6,3,2,1),e(6,3,2,2),e(6,3,2,3),e(6,3,3,2))=1
	s (e(6,4,1,2),e(6,4,2,1),e(6,4,2,2),e(6,4,3,2))=1
	; -_
	s e(7)=2,e(7,1)=3
	s (e(7,1,2,1),e(7,1,2,2),e(7,1,3,2),e(7,1,3,3))=1
	s (e(7,2,1,2),e(7,2,2,1),e(7,2,2,2),e(7,2,3,1))=1
	q

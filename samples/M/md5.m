;
;  MD5 Implementation in M
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

; It works in GT.M with ZCHSET=M, but please, don't use it. It's only a joke.
; Serves well as a reverse engineering example on obtaining boolean functions
; from integer addition, modulo and division.


md5(msg)
	; http://en.wikipedia.org/wiki/MD5
	n m,r,k,h,i,j,a,b,c,d,f,g,w,t,p,q
	s r(0)=7,r(1)=12,r(2)=17,r(3)=22,r(4)=7,r(5)=12,r(6)=17,r(7)=22,r(8)=7,r(9)=12,r(10)=17,r(11)=22,r(12)=7,r(13)=12,r(14)=17,r(15)=22,r(16)=5,r(17)=9,r(18)=14,r(19)=20,r(20)=5,r(21)=9,r(22)=14,r(23)=20,r(24)=5,r(25)=9,r(26)=14,r(27)=20,r(28)=5,r(29)=9,r(30)=14,r(31)=20,r(32)=4,r(33)=11,r(34)=16,r(35)=23,r(36)=4,r(37)=11,r(38)=16,r(39)=23,r(40)=4,r(41)=11,r(42)=16,r(43)=23,r(44)=4,r(45)=11,r(46)=16,r(47)=23,r(48)=6,r(49)=10,r(50)=15,r(51)=21,r(52)=6,r(53)=10,r(54)=15,r(55)=21,r(56)=6,r(57)=10,r(58)=15,r(59)=21,r(60)=6,r(61)=10,r(62)=15,r(63)=21
	s k(0)=3614090360,k(1)=3905402710,k(2)=606105819,k(3)=3250441966,k(4)=4118548399,k(5)=1200080426,k(6)=2821735955,k(7)=4249261313,k(8)=1770035416,k(9)=2336552879,k(10)=4294925233,k(11)=2304563134,k(12)=1804603682,k(13)=4254626195,k(14)=2792965006,k(15)=1236535329,k(16)=4129170786,k(17)=3225465664,k(18)=643717713,k(19)=3921069994,k(20)=3593408605,k(21)=38016083,k(22)=3634488961,k(23)=3889429448,k(24)=568446438,k(25)=3275163606,k(26)=4107603335,k(27)=1163531501,k(28)=2850285829,k(29)=4243563512,k(30)=1735328473,k(31)=2368359562,k(32)=4294588738,k(33)=2272392833,k(34)=1839030562,k(35)=4259657740,k(36)=2763975236,k(37)=1272893353,k(38)=4139469664,k(39)=3200236656,k(40)=681279174,k(41)=3936430074,k(42)=3572445317,k(43)=76029189,k(44)=3654602809,k(45)=3873151461,k(46)=530742520,k(47)=3299628645,k(48)=4096336452,k(49)=1126891415,k(50)=2878612391,k(51)=4237533241,k(52)=1700485571,k(53)=2399980690,k(54)=4293915773,k(55)=2240044497,k(56)=1873313359,k(57)=4264355552,k(58)=2734768916,k(59)=1309151649,k(60)=4149444226,k(61)=3174756917,k(62)=718787259,k(63)=3951481745
	s h(0)=1732584193,h(1)=4023233417,h(2)=2562383102,h(3)=271733878
	s $p(m,$c(0),(55-$l(msg))#64+1)="",m=msg_$c(128)_m_$$n64($l(msg)*8),p=1,q=0
	f  q:q  d
	. f j=0:1:15 s w(j)=$$read(.m,.p)
	. i w(0)<0 s q=1 q
	. s a=h(0),b=h(1),c=h(2),d=h(3)
	. f i=0:1:63 d
	.. i i<16 d
	... s f=$$or($$and(b,c),$$and($$not(b),d)),g=i
	.. e  i i<32 d
	... s f=$$or($$and(d,b),$$and($$not(d),c)),g=(5*i+1)#16
	.. e  i i<48 d
	... s f=$$xor($$xor(b,c),d),g=(3*i+5)#16
	.. e  s f=$$xor(c,$$or(b,$$not(d))),g=(7*i)#16
	.. s t=d,d=c,c=b,b=(b+$$rotate((a+f+k(i)+w(g))#4294967296,r(i)))#4294967296,a=t
	. s h(0)=(h(0)+a)#4294967296,h(1)=(h(1)+b)#4294967296,h(2)=(h(2)+c)#4294967296,h(3)=(h(3)+d)#4294967296
	q $$n32h(h(0))_$$n32h(h(1))_$$n32h(h(2))_$$n32h(h(3))

not(a) ; 32bit
	q 4294967295-a

xor(a,b) ; 32bit
	n x,i s x=0 f i=1:1:32 s x=(x\2)+(((a+b)#2)*2147483648),a=a\2,b=b\2
	q x

and(a,b) ; 32bit
	n x,i s x=0 f i=1:1:32 s x=(x\2)+((((a#2)+(b#2))\2)*2147483648),a=a\2,b=b\2
	q x

or(a,b) ; 32bit
	q $$not($$and($$not(.a),$$not(.b)))

rotate(a,n) ; 32bit, rol
	n c s c=a*(2**n)
	q c#4294967296+(c\4294967296)

read(b,i)
	n n,j s n=0 f j=3:-1:0 s n=256*n+$a($e(b,i+j))
	s i=i+4
	q n

n64(n)
	n s,i f i=1:1:8 s $e(s,i)=$c(n#256),n=n\256
	q s

n32h(n)
	n h,s,i s h="0123456789abcdef" f i=1:2:8 s $e(s,i+1)=$e(h,n#16+1),n=n\16,$e(s,i)=$e(h,n#16+1),n=n\16
	q s

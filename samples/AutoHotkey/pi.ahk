#NoEnv
#SingleInstance, Force
SetBatchLines, -1
#Include mpl.ahk
dot:=".", i:=0
, MP_SET(q, "1")
, MP_SET(r, "0")
, MP_SET(t, "1")
, MP_SET(k, "1")
, MP_SET(n, "3")
, MP_SET(l, "3")
, MP_SET(ONE, "1")
, MP_SET(TWO, "2")
, MP_SET(THREE, "3")
, MP_SET(FOUR, "4")
, MP_SET(SEVEN, "7")
, MP_SET(TEN, "10")

Loop
{
	MP_MUL(q4, q, FOUR)
	, MP_ADD(q4r, q4, r)
	, MP_SUB(q4rt, q4r, t)
	, MP_MUL(tn, t, n)
	If (MP_CMP(q4rt,tn) = -1)
	{
		s := MP_DEC(n) . dot
		OutputDebug %s%
		  dot := ""
		, i++
		, MP_MUL(tn, t, n)
		, MP_SUB(rtn, r, tn)
		, MP_MUL(nr, rtn, TEN)
		, MP_MUL(q3, q, THREE)
		, MP_ADD(q3r, q3, r)
		, MP_DIV(q3rt, remainder, q3r, t)
		, MP_SUB(q3rtn, q3rt, n)
		, MP_MUL(n, q3rtn, TEN)
		, MP_MUL(tmp, q, TEN)
		, MP_CPY(q, tmp)
		, MP_CPY(r, nr)
	}
	Else
	{
		MP_MUL(q2, q, TWO)
		, MP_ADD(q2r, q2, r)
		, MP_MUL(nr, q2r, l)
		, MP_MUL(k7, k, SEVEN)
		, MP_ADD(k72, k7, TWO)
		, MP_MUL(qk, q, k72)
		, MP_MUL(rl, r, l)
		, MP_ADD(qkrl, qk, rl)
		, MP_MUL(tl, t, l)
		, MP_DIV(nn, remainder, qkrl, tl)
		, MP_MUL(tmp, q, k)
		, MP_CPY(q, tmp)
		, MP_MUL(tmp, t, l)
		, MP_CPY(t, tmp)
		, MP_ADD(tmp, l, TWO)
		, MP_CPY(l, tmp)
		, MP_ADD(tmp, k, ONE)
		, MP_CPY(k, tmp)
		, MP_CPY(n, nn)
		, MP_CPY(r, nr)
	}
}

#NoEnv
#SingleInstance, Force
SetBatchLines, -1
#Include mpl.ahk

  MP_SET(base, "2988348162058574136915891421498819466320163312926952423791023078876139")
, MP_SET(exponent, "2351399303373464486466122544523690094744975233415544072992656881240319")
, MP_SET(modulus, "10000000000000000000000000000000000000000")

, NumGet(exponent,0,"Int") = -1 ? return : ""
, MP_SET(result, "1")
, MP_SET(TWO, "2")
while !MP_IS0(exponent)
	MP_DIV(q, r, exponent, TWO)
	, (MP_DEC(r) = 1
		? (MP_MUL(temp, result, base)
		, MP_DIV(q, result, temp, modulus))
		: "")
	, MP_DIV(q, r, exponent, TWO)
	, MP_CPY(exponent, q)
	, MP_CPY(base1, base)
	, MP_MUL(base2, base1, base)
	, MP_DIV(q, base, base2, modulus)

msgbox % MP_DEC(result)
Return

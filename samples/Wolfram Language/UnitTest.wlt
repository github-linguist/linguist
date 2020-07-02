BeginTestSection["Untitled-5"]

VerificationTest[(* 1 *)
	RotationMatrix[phi]
	,
	List[List[Cos[phi], Times[-1, Sin[phi]]], List[Sin[phi], Cos[phi]]]	
]

VerificationTest[(* 2 *)
	Times[1, Power[Plus[a, Times[-1, a]], -1]]
	,
	ComplexInfinity
	,
	{Power::infy}
]

EndTestSection[]

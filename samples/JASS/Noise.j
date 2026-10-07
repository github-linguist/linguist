//
//	Noise JASS v1.2.1
//
//	Port by Glint, https://github.com/eGlint/Warcraft-III-Noise-Library
//	Perlin Noise by Kenneth Perlin, https://mrl.nyu.edu/~perlin/noise/
//	Open Simplex by Kurt Spencer, https://gist.github.com/KdotJPG/b1270127455a94ac5d19
//
//	ADDITIONAL INSTALLATION INSTRUCTIONS
//	In the Trigger Editor, create a new global variable by either using: 
//  	'Variables...', the yellow "X" icon or press 'Ctrl + B'.
//			or
//		'New Global Variable', the green "X" icon or press 'Ctrl + L'
//
//	REQUIRES THE FOLLOWING GLOBAL VARIABLES
//	integer array 	NoisePermutation 
//	integer array 	GradientTable2DX  
//	integer array 	GradientTable2DY
//	real			NoiseGetRandomIntEvent
//	integer			NoiseGetRandomIntLow
//	integer			NoiseGetRandomIntHigh
//	integer			NoiseGetRandomIntReturn
//

constant function NoiseVersion takes nothing returns string 
	return "1.2.1"
endfunction

function Floor takes real value returns integer
	local integer n = R2I(value)
	if value < 0 and value - n != 0. then
		set n = n - 1
	endif
	return n
endfunction

function RealTernaryOp takes boolean cond, real a, real b returns real
	if cond then
		return a
	else
		return b
	endif
endfunction

function Fade takes real t returns real
	return t * t * t * (t * (t * 6. - 15.) + 10.)
endfunction

function Lerp takes real t, real a, real b returns real
	return a + t * (b - a)
endfunction

function Gradient1D takes integer hash, real x returns real
	local integer h = BlzBitAnd(hash, 15)
	return RealTernaryOp(BlzBitAnd(h, 1) == 0, x, -x)
endfunction

function PerlinNoise1D takes real x returns real
	local integer X = BlzBitAnd(Floor(x), 255)
	set x = x - Floor(x)
	return Lerp(Fade(x), Gradient1D(udg_NoisePermutation[X], x), Gradient1D(udg_NoisePermutation[X + 1], x - 1)) * 2
endfunction

function Gradient2D takes integer hash, real x, real y returns real
	local integer h = BlzBitAnd(hash, 15)
	local real u = RealTernaryOp(h < 8, x, y)
	local real v = RealTernaryOp(h < 4, y, x)
	return RealTernaryOp(BlzBitAnd(h, 1) == 0, u, -u) + RealTernaryOp(BlzBitAnd(h, 2) == 0, v, -v)
endfunction

function PerlinNoise2D takes real x, real y returns real
	local integer X = BlzBitAnd(Floor(x), 255)
	local integer Y = BlzBitAnd(Floor(y), 255)
	local real u
	local real v
	local integer A
	local integer B
	local real lerpA1
	local real lerpA2
	set x = x - Floor(x)
	set y = y - Floor(y)
	set u = Fade(x)
	set v = Fade(y)
	set A = udg_NoisePermutation[X] + Y
	set B = udg_NoisePermutation[X + 1] + Y
	set lerpA1 = Lerp(u, Gradient2D(udg_NoisePermutation[A], x, y), Gradient2D(udg_NoisePermutation[B], x - 1., y))
	set lerpA2 = Lerp(u, Gradient2D(udg_NoisePermutation[A + 1], x, y - 1.), Gradient2D(udg_NoisePermutation[B + 1], x - 1., y - 1.))
	return Lerp(v, lerpA1, lerpA2)
endfunction

function Gradient3D takes integer hash, real x, real y, real z returns real
	local integer h = BlzBitAnd(hash, 15)
	local real u = RealTernaryOp(h < 8, x, y)
	local real v = RealTernaryOp(h < 4, y, RealTernaryOp(h == 12 or h == 14, x, z))
	return RealTernaryOp(BlzBitAnd(h, 1) == 0, u, -u) + RealTernaryOp(BlzBitAnd(h, 2) == 0, v, -v)
endfunction

function PerlinNoise3D takes real x, real y, real z returns real
	local integer X = BlzBitAnd(Floor(x), 255)
	local integer Y = BlzBitAnd(Floor(y), 255)
	local integer Z = BlzBitAnd(Floor(z), 255)
	local real u
	local real v
	local real w
	local integer A
	local integer AA
	local integer AB
	local integer B
	local integer BA
	local integer BB
	local real lerpA1
	local real lerpA2
	local real lerpB1
	local real lerpB2
	set x = x - Floor(x)
	set y = y - Floor(y)
	set z = z - Floor(z)
	set u = Fade(x)
	set v = Fade(y)
	set w = Fade(z)
	set A = udg_NoisePermutation[X] + Y
	set AA = udg_NoisePermutation[A] + Z
	set AB = udg_NoisePermutation[A + 1] + Z
	set B = udg_NoisePermutation[X + 1] + Y
	set BA = udg_NoisePermutation[B] + Z
	set BB = udg_NoisePermutation[B + 1] + Z
	set lerpA1 = Lerp(u, Gradient3D(udg_NoisePermutation[AA], x, y, z), Gradient3D(udg_NoisePermutation[BA], x - 1., y, z))
	set lerpA2 = Lerp(u, Gradient3D(udg_NoisePermutation[AB], x, y - 1., z), Gradient3D(udg_NoisePermutation[BB], x - 1., y - 1., z))
	set lerpB1 = Lerp(u, Gradient3D(udg_NoisePermutation[AA + 1], x, y, z - 1.), Gradient3D(udg_NoisePermutation[BA + 1], x - 1., y, z - 1.))
	set lerpB2 = Lerp(u, Gradient3D(udg_NoisePermutation[AB + 1], x, y - 1., z - 1.), Gradient3D(udg_NoisePermutation[BB + 1], x - 1., y - 1., z - 1.))
	return Lerp(w, Lerp(v, lerpA1, lerpA2), Lerp(v, lerpB1, lerpB2))
endfunction

function GeneratePermutationTableCustom takes code GetRandomIntInterface returns nothing 
	local integer i = 0
	local trigger trig = CreateTrigger()
	call TriggerRegisterVariableEvent(trig, "udg_NoiseGetRandomIntEvent", EQUAL, 1)
	call TriggerAddAction(trig, GetRandomIntInterface)
	loop
		exitwhen i > 255
		set udg_NoiseGetRandomIntLow = 0  
		set udg_NoiseGetRandomIntHigh = 255
		set udg_NoiseGetRandomIntEvent = 1
		set udg_NoiseGetRandomIntEvent = 0
		set udg_NoisePermutation[i] = udg_NoiseGetRandomIntReturn
		set udg_NoisePermutation[i + 256] = udg_NoisePermutation[i]
		set i = i + 1
	endloop
	call DestroyTrigger(trig)
endfunction

function GeneratePermutationTable takes nothing returns nothing
	local integer i = 0
	loop
		exitwhen i > 255
		set udg_NoisePermutation[i] = GetRandomInt(0, 255)
		set udg_NoisePermutation[i + 256] = udg_NoisePermutation[i]
		set i = i + 1
	endloop
endfunction

function InitGradientTable2D takes nothing returns nothing 
	set udg_GradientTable2DX[0] = 5
	set udg_GradientTable2DY[0] = 2
	set udg_GradientTable2DX[1] = 2
	set udg_GradientTable2DY[1] = 5
	set udg_GradientTable2DX[2] = -2
	set udg_GradientTable2DY[2] = 5
	set udg_GradientTable2DX[3] = -5
	set udg_GradientTable2DY[3] = 2
	set udg_GradientTable2DX[4] = -5
	set udg_GradientTable2DY[4] = -2
	set udg_GradientTable2DX[5] = -2
	set udg_GradientTable2DY[5] = -5
	set udg_GradientTable2DX[6] = 2
	set udg_GradientTable2DY[6] = -5
	set udg_GradientTable2DX[7] = 5
	set udg_GradientTable2DY[7] = -2
endfunction

constant function StretchConstant2D takes nothing returns real 
	return -0.21132486
endfunction

constant function SquishConstant2D takes nothing returns real 
	return 0.36602540
endfunction 

constant function NormConstant2D takes nothing returns integer 
	return 47
endfunction

constant function PMASK takes nothing returns integer 
	return 255
endfunction

constant function SquishConstant2DX takes nothing returns real 
	return 2 * SquishConstant2D()
endfunction

constant function SquishConstant2DY takes nothing returns real 
	return 2 * SquishConstant2D()
endfunction

function Extrapolate2D takes integer xsb, integer ysb, real dx, real dy returns real 
	local integer index = BlzBitAnd(udg_NoisePermutation[BlzBitXor(udg_NoisePermutation[BlzBitAnd(xsb, PMASK())], BlzBitAnd(ysb, PMASK()))], 7)
	return udg_GradientTable2DX[index] * dx + udg_GradientTable2DY[index] * dy
endfunction

function OpenSimplex2D takes real x, real y returns real 
	local real strechOffset = (x + y) * StretchConstant2D()
	local real xs = x + strechOffset
	local real ys = y + strechOffset
	local integer xsb = Floor(xs)
	local integer ysb = Floor(ys)
	local real squishOffset = (xsb + ysb) * SquishConstant2D()
	local real xb = xsb + squishOffset
	local real yb = ysb + squishOffset
	local real xins = xs - xsb 
	local real yins = ys - ysb
	local real inSum = xins + yins
	local real dx0 = x - xb
	local real dy0 = y - yb
	local real dx_ext
	local real dy_ext
	local integer xsv_ext
	local integer ysv_ext
	local real value = 0
	local real dx1 = dx0 - 1. - SquishConstant2D()
	local real dy1 = dy0 - SquishConstant2D()
	local real attn1 = 2. - dx1 * dx1 - dy1 * dy1
	local real dx2 = dx0 - SquishConstant2D()
	local real dy2 = dy0 - 1 - SquishConstant2D()
	local real attn2 = 2. - dx2 * dx2 - dy2 * dy2
	local real zins
	local real attn0 
	local real attn_ext
	if attn1 > 0 then
		set attn1 = attn1 * attn1
		set value = attn1 * attn1 * Extrapolate2D(xsb + 1, ysb, dx1, dy1)
	endif
	if attn2 > 0 then 
		set attn2 = attn2 * attn2
		set value = value + attn2 * attn2 * Extrapolate2D(xsb, ysb + 1, dx2, dy2)
	endif
	if inSum <= 1 then 
		set zins = 1. - inSum
		if zins > xins or zins > yins then 
			if xins > yins then
				set xsv_ext = xsb + 1
				set ysv_ext = ysb - 1
				set dx_ext = dx0 - 1.
				set dy_ext = dy0 + 1.
			else
				set xsv_ext = xsb - 1
				set ysv_ext = ysb + 1
				set dx_ext = dx0 + 1.
				set dy_ext = dy0 - 1.
			endif
		else 
			set xsv_ext = xsb + 1
			set ysv_ext = ysb + 1
			set dx_ext = dx0 - 1 - SquishConstant2DX()
			set dy_ext = dy0 - 1 - SquishConstant2DY()
		endif
	else 
		set zins = 2. - inSum
		if zins < xins or zins < yins then 
			if xins > yins then 
				set xsv_ext = xsb + 2
				set ysv_ext = ysb
				set dx_ext = dx0 - 2 - SquishConstant2DX()
				set dy_ext = dy0 - 2 - SquishConstant2DY()
			else 
				set xsv_ext = xsb
				set ysv_ext = ysb + 2
				set dx_ext = dx0 - SquishConstant2DX()
				set dy_ext = dy0 - 2 - SquishConstant2DY()
			endif
		else
			set dx_ext = dx0 
			set dy_ext = dy0
			set xsv_ext = xsb 
			set ysv_ext = ysb
		endif
		set xsb = xsb + 1
		set ysb = ysb + 1
		set dx0 = dx0 - 1 - SquishConstant2DX()
		set dy0 = dy0 - 1 - SquishConstant2DY()
	endif
	set attn0 = 2 - dx0 * dx0 - dy0 * dy0
	if attn0 > 0 then 
		set attn0 = attn0 * attn0
		set value = value + attn0 * attn0 * Extrapolate2D(xsb, ysb, dx0, dy0)
	endif
	set attn_ext = 2 - dx_ext * dx_ext - dy_ext * dy_ext
	if attn_ext > 0 then 
		set attn_ext = attn_ext * attn_ext
		set value = value + attn_ext * attn_ext * Extrapolate2D(xsv_ext, ysv_ext, dx_ext, dy_ext)
	endif
	return value / NormConstant2D()
endfunction

function InitNoise takes nothing returns nothing
	call GeneratePermutationTable()
	call InitGradientTable2D()
endfunction

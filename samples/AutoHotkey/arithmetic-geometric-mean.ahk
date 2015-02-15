agm(a, g, tolerance=1.0e-15){
	While abs(a-g) > tolerance
	{
		an := .5 * (a + g)
		g  := sqrt(a*g)
		a  := an
	}
	return a
}
SetFormat, FloatFast, 0.15
MsgBox % agm(1, 1/sqrt(2))

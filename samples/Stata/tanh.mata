numeric matrix tanh(numeric matrix u)
{
		numeric matrix  eu, emu

		eu = exp(u)
		emu = exp(-u)
		return( (eu-emu):/(eu+emu) )
}

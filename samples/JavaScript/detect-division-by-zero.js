function divByZero(dividend,divisor)
{
	var quotient=dividend/divisor;
        if(isNaN(quotient)) return 0; //Can be changed to whatever is desired by the programmer to be 0, false, or Infinity
        return quotient; //Will return Infinity or -Infinity in cases of, for example, 5/0 or -7/0 respectively
}
alert(divByZero(0,0));

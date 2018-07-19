function bitwise(a:int, b:int):void
{
	trace("And: ", a & b);
	trace("Or: ", a | b);
	trace("Xor: ", a ^ b);
	trace("Not: ", ~a);
	trace("Left Shift: ", a << b);
	trace("Right Shift(Arithmetic): ", a >> b);
	trace("Right Shift(Logical): ", a >>> b);
}

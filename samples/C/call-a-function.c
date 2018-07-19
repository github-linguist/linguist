/* function with no argument */
f();

/* fix number of arguments */
g(1, 2, 3);

/* Optional arguments: err...
   Feel free to make sense of the following.  I can't. */
int op_arg();
int main()
{
	op_arg(1);
	op_arg(1, 2);
	op_arg(1, 2, 3);
	return 0;
}
int op_arg(int a, int b)
{
	printf("%d %d %d\n", a, b, (&b)[1]);
	return a;
}  /* end of sensible code */

/* Variadic function: how the args list is handled solely depends on the function */
void h(int a, ...)
{
	va_list ap;
	va_start(ap);
	...
}
/* call it as: (if you feed it something it doesn't expect, don't count on it working) */
h(1, 2, 3, 4, "abcd", (void*)0);

/* named arguments: no such thing */
/* statement context: is that a real phrase? */

/* as a first-class object (i.e. function pointer) */
printf("%p", f); /* that's the f() above */

/* return value */
double a = asin(1);

/* built-in functions: no such thing. Compiler may interally give special treatment
   to bread-and-butter functions such as memcpy(), but that's not a C built-in per se */

/* subroutines: no such thing. You can goto places, but I doubt that counts. */

/* Scalar values are passed by value by default. However, arrays are passed by reference. */
/* Pointers *sort of* work like references, though. */

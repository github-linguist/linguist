/* using pointer arithmetic (because we can, I guess) */
int arg[] = { 1,2,3,4,5 };
int arg_length = sizeof(arg)/sizeof(arg[0]);
int *end = arg+arg_length;
int sum = 0, prod = 1;
int *p;

for (p = arg; p!=end; ++p) {
   sum += *p;
   prod *= *p;
}

int noargs(); /* Declare a function with no arguments that returns an integer */
int twoargs(int a,int b); /* Declare a function with no arguments that returns an integer */
int twoargs(int ,int); /* Parameter names are optional in a prototype definition */
int anyargs(...); /* An ellipsis can be used to declare a function that accepts varargs */
int atleastoneargs(int, ...); /* One mandatory integer argument followed by varargs */

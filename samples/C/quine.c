#include <stdio.h>

static char sym[] = "\n\t\\\"";

int main(void) {
	const char *code = "#include <stdio.h>%c%cstatic char sym[] = %c%cn%ct%c%c%c%c%c;%c%cint main(void) {%c%cconst char *code = %c%s%c;%c%cprintf(code, sym[0], sym[0], sym[3], sym[2], sym[2], sym[2], sym[2], sym[2], sym[3], sym[3], sym[0], sym[0], sym[0], sym[1], sym[3], code, sym[3], sym[0], sym[1], sym[0], sym[0], sym[1], sym[0], sym[0]);%c%c%creturn 0;%c}%c";
	printf(code, sym[0], sym[0], sym[3], sym[2], sym[2], sym[2], sym[2], sym[2], sym[3], sym[3], sym[0], sym[0], sym[0], sym[1], sym[3], code, sym[3], sym[0], sym[1], sym[0], sym[0], sym[1], sym[0], sym[0]);

	return 0;
}

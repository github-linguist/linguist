#include <stdio.h>
#include <stdlib.h>

int main()
{
	int i;
	printf("<table style=\"text-align:center; border: 1px solid\"><th></th>"
		"<th>X</th><th>Y</th><th>Z</th>");
	for (i = 0; i < 4; i++) {
		printf("<tr><th>%d</th><td>%d</td><td>%d</td><td>%d</td></tr>", i,
			rand() % 10000, rand() % 10000, rand() % 10000);
	}
	printf("</table>");

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	const char *name, *id, *dept;
	int sal;
} person;

person ppl[] = {
	{"Tyler Bennett",	"E10297", "D101", 32000},
	{"John Rappl",		"E21437", "D050", 47000},
	{"George Woltman",	"E00127", "D101", 53500},
	{"Adam Smith",		"E63535", "D202", 18000},
	{"Claire Buckman",	"E39876", "D202", 27800},
	{"David McClellan",	"E04242", "D101", 41500},
	{"Rich Holcomb",	"E01234", "D202", 49500},
	{"Nathan Adams",	"E41298", "D050", 21900},
	{"Richard Potter",	"E43128", "D101", 15900},
	{"David Motsinger",	"E27002", "D202", 19250},
	{"Tim Sampair",		"E03033", "D101", 27000},
	{"Kim Arlich",		"E10001", "D190", 57000},
	{"Timothy Grove",	"E16398", "D190", 29900},
};

int pcmp(const void *a, const void *b)
{
	const person *aa = a, *bb = b;
	int x = strcmp(aa->dept, bb->dept);
	if (x) return x;
	return aa->sal > bb->sal ? -1 : aa->sal < bb->sal;
}

#define N sizeof(ppl)/sizeof(person)
void top(int n)
{
	int i, rank;
	qsort(ppl, N, sizeof(person), pcmp);

	for (i = rank = 0; i < N; i++) {
		if (i && strcmp(ppl[i].dept, ppl[i - 1].dept)) {
			rank = 0;
			printf("\n");
		}

		if (rank++ < n)
			printf("%s %d: %s\n", ppl[i].dept, ppl[i].sal, ppl[i].name);
	}
}

int main()
{
	top(2);
	return 0;
}

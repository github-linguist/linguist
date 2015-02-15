#include <stdio.h>

#define N 4
const char *perms[] = {
	"ABCD", "CABD", "ACDB", "DACB", "BCDA", "ACBD", "ADCB", "CDAB",
	"DABC", "BCAD", "CADB", "CDBA", "CBAD", "ABDC", "ADBC", "BDCA",
	"DCBA", "BACD", "BADC", "BDAC", "CBDA", "DBCA", "DCAB",
};

int main()
{
	int i, j, n, cnt[N];
	char miss[N];

	for (n = i = 1; i < N; i++) n *= i; /* n = (N-1)!, # of occurrence */

	for (i = 0; i < N; i++) {
		for (j = 0; j < N; j++) cnt[j] = 0;

		/* count how many times each letter occur at position i */
		for (j = 0; j < sizeof(perms)/sizeof(const char*); j++)
			cnt[perms[j][i] - 'A']++;

		/* letter not occurring (N-1)! times is the missing one */
		for (j = 0; j < N && cnt[j] == n; j++);

		miss[i] = j + 'A';
	}
	printf("Missing: %.*s\n", N, miss);

	return 0;
		
}

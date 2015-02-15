#include <stdio.h>
#include <unistd.h>
#include <omp.h>

typedef struct printer printer;
struct printer { int id, ink; };
printer pnt_main = { 1, 5 };
printer pnt_backup = { 2, 5 };

int print(const char * text, const char **error)
{
	#pragma omp critical
	{
		printer *p = &pnt_main;
		if (!p->ink) p = &pnt_backup;
		if (!p->ink)
			*error = "Out of ink";
		else {
			*error = 0;
			p->ink--;
			printf("%d | ", p->id, p->ink);
			while (*text != '\0') {
				putchar(*(text++));
				fflush(stdout);
				usleep(30000);
			}
			putchar('\n');
		}
	}
	return 0 != *error;
}

const char *humpty[] = {
	"Humpty Dumpty sat on a wall.",
	"Humpty Dumpty had a great fall.",
	"All the king's horses and all the king's men,",
	"Couldn't put Humpty together again."
};

const char *goose[] = {
	"Old Mother Goose,",
	"When she wanted to wander,",
	"Would ride through the air,",
	"On a very fine gander.",
	"Jack's mother came in,",
	"And caught the goose soon,",
	"And mounting its back,",
	"Flew up to the moon."
};

int main()
{
	int i, j, len;
	const char *msg, **text;

	omp_set_num_threads(2);

	#pragma omp parallel for private(text, msg, len, j)
	for (i = 0; i < 2; i++) {
		text = i ? goose : humpty;
		len = (i ? sizeof(goose) : sizeof(humpty) ) / sizeof(const char*);
		for (j = 0; j < len; j++) {
			usleep(100000);
			if (print(text[j], &msg)) {
				fprintf(stderr, "Error: %s\n", msg);
				break;
			}
		}
	}

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *menu_select(const char *const *items, const char *prompt);

int
main(void)
{
	const char *items[] = {"fee fie", "huff and puff", "mirror mirror", "tick tock", NULL};
	const char *prompt = "Which is from the three pigs?";

	printf("You chose %s.\n", menu_select(items, prompt));

	return EXIT_SUCCESS;
}

const char *
menu_select(const char *const *items, const char *prompt)
{
	char buf[BUFSIZ];
	int i;
	int choice;
	int choice_max;

	if (items == NULL)
		return NULL;

	do {
		for (i = 0; items[i] != NULL; i++) {
			printf("%d) %s\n", i + 1, items[i]);
		}
		choice_max = i;
		if (prompt != NULL)
			printf("%s ", prompt);
		else
			printf("Choice? ");
		if (fgets(buf, sizeof(buf), stdin) != NULL) {
			choice = atoi(buf);
		}
	} while (1 > choice || choice > choice_max);

	return items[choice - 1];
}

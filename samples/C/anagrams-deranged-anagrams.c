#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>

// Letter lookup by frequency.  This is to reduce word insertion time.
const char *freq = "zqxjkvbpygfwmucldrhsnioate";
int char_to_idx[128];

// Trie structure of sorts
struct word {
	const char *w;
	struct word *next;
};

union node {
	union node *down[10];
	struct word *list[10];
};

int deranged(const char *s1, const char *s2)
{
	int i;
	for (i = 0; s1[i]; i++)
		if (s1[i] == s2[i]) return 0;
	return 1;
}

int count_letters(const char *s, unsigned char *c)
{
	int i, len;
	memset(c, 0, 26);
	for (len = i = 0; s[i]; i++) {
		if (s[i] < 'a' || s[i] > 'z')
			return 0;
		len++, c[char_to_idx[(unsigned char)s[i]]]++;
	}
	return len;
}

const char * insert(union node *root, const char *s, unsigned char *cnt)
{
	int i;
	union node *n;
	struct word *v, *w = 0;

	for (i = 0; i < 25; i++, root = n) {
		if (!(n = root->down[cnt[i]]))
			root->down[cnt[i]] = n = calloc(1, sizeof(union node));
	}

	w = malloc(sizeof(struct word));
	w->w = s;
	w->next = root->list[cnt[25]];
	root->list[cnt[25]] = w;

	for (v = w->next; v; v = v->next) {
		if (deranged(w->w, v->w))
			return v->w;
	}
	return 0;
}

int main(int c, char **v)
{
	int i, j = 0;
	char *words;
	struct stat st;

	int fd = open(c < 2 ? "unixdict.txt" : v[1], O_RDONLY);
	if (fstat(fd, &st) < 0) return 1;

	words = malloc(st.st_size);
	read(fd, words, st.st_size);
	close(fd);

	union node root = {{0}};
	unsigned char cnt[26];
	int best_len = 0;
	const char *b1, *b2;

	for (i = 0; freq[i]; i++)
		char_to_idx[(unsigned char)freq[i]] = i;

	/* count words, change newline to null */
	for (i = j = 0; i < st.st_size; i++) {
		if (words[i] != '\n') continue;
		words[i] = '\0';

		if (i - j > best_len) {
			count_letters(words + j, cnt);
			const char *match = insert(&root, words + j, cnt);

			if (match) {
				best_len = i - j;
				b1 = words + j;
				b2 = match;
			}
		}

		j = ++i;
	}

	if (best_len) printf("longest derangement: %s %s\n", b1, b2);

	return 0;
}

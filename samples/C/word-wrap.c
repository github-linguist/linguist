#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* nonsensical hyphens to make greedy wrapping method look bad */
const char *string = "In olden times when wishing still helped one, there lived a king "
	"whose daughters were all beautiful, but the youngest was so beautiful "
	"that the sun itself, which has seen so much, was astonished whenever "
	"it shone-in-her-face.  Close-by-the-king's castle lay a great dark "
	"forest, and under an old lime-tree in the forest was a well, and when "
	"the day was very warm, the king's child went out into the forest and "
	"sat down by the side of the cool-fountain, and when she was bored she "
	"took a golden ball, and threw it up on high and caught it, and this "
	"ball was her favorite plaything.";

/*	Each but the last of wrapped lines comes with some penalty as the square
	of the diff between line length and desired line length.  If the line
	is longer than desired length, the penalty is multiplied by 100.  This
	pretty much prohibits the wrapping routine from going over right margin.
	If is ok to exceed the margin just a little, something like 20 or 40 will
	do.

	Knuth uses a per-paragraph penalty for line-breaking in TeX, which is--
	unlike what I have here--probably bug-free.
*/

#define PENALTY_LONG	100
#define PENALTY_SHORT	1

typedef struct word_t {
	const char *s;
	int len;
} *word;

word make_word_list(const char *s, int *n)
{
	int max_n = 0;
	word words = 0;

	*n = 0;
	while (1) {
		while (*s && isspace(*s)) s++;
		if (!*s) break;

		if (*n >= max_n) {
			if (!(max_n *= 2)) max_n = 2;
			words = realloc(words, max_n * sizeof(*words));
		}
		words[*n].s = s;
		while (*s && !isspace(*s)) s++;
		words[*n].len = s - words[*n].s;
		(*n) ++;
	}

	return words;
}

int greedy_wrap(word words, int count, int cols, int *breaks)
{
	int score = 0, line, i, j, d;

	i = j = line = 0;
	while (1) {
		if (i == count) {
			breaks[j++] = i;
			break;
		}

		if (!line) {
			line = words[i++].len;
			continue;
		}

		if (line + words[i].len < cols) {
			line += words[i++].len + 1;
			continue;
		}

		breaks[j++] = i;
		if (i < count) {
			d = cols - line;
			if (d > 0)	score += PENALTY_SHORT * d * d;
			else if (d < 0)	score += PENALTY_LONG * d * d;
		}

		line = 0;
	}
	breaks[j++] = 0;

	return score;
}

/* tries to make right margin more even; pretty sure there's an off-by-one bug
	here somewhere */
int balanced_wrap(word words, int count, int cols, int *breaks)
{
	int *best = malloc(sizeof(int) * (count + 1));

	/* do a greedy wrap to have some baseline score to work with, else
	   we'll end up with O(2^N) behavior */
	int best_score = greedy_wrap(words, count, cols, breaks);

	void test_wrap(int line_no, int start, int score) {
		int line = 0, current_score = -1, d;

		while (start <= count) {
			if (line) line ++;
			line += words[start++].len;
			d = cols - line;
			if (start < count || d < 0) {
				if (d > 0)
					current_score = score + PENALTY_SHORT * d * d;
				else
					current_score = score + PENALTY_LONG * d * d;
			} else {
				current_score = score;
			}

			if (current_score >= best_score) {
				if (d <= 0) return;
				continue;
			}

			best[line_no] = start;
			test_wrap(line_no + 1, start, current_score);
		}
		if (current_score >= 0 && current_score < best_score) {
			best_score = current_score;
			memcpy(breaks, best, sizeof(int) * (line_no));
		}
	}
	test_wrap(0, 0, 0);
	free(best);

	return best_score;
}

void show_wrap(word list, int count, int *breaks)
{
	int i, j;
	for (i = j = 0; i < count && breaks[i]; i++) {
		while (j < breaks[i]) {
			printf("%.*s", list[j].len, list[j].s);
			if (j < breaks[i] - 1)
				putchar(' ');
			j++;
		}
		if (breaks[i]) putchar('\n');
	}
}

int main(void)
{
	int len, score, cols;
	word list = make_word_list(string, &len);
	int *breaks = malloc(sizeof(int) * (len + 1));

	cols = 80;
	score = greedy_wrap(list, len, cols, breaks);
	printf("\n== greedy wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	score = balanced_wrap(list, len, cols, breaks);
	printf("\n== balanced wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);


	cols = 32;
	score = greedy_wrap(list, len, cols, breaks);
	printf("\n== greedy wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	score = balanced_wrap(list, len, cols, breaks);
	printf("\n== balanced wrap at %d (score %d) ==\n\n", cols, score);
	show_wrap(list, len, breaks);

	return 0;
}

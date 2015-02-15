#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int *board, *flood, *known, top = 0, w, h;

static inline int idx(int y, int x) { return y * w + x; }

int neighbors(int c, int *p)
/*
@c cell
@p list of neighbours
@return amount of neighbours
*/
{
	int i, j, n = 0;
	int y = c / w, x = c % w;

	for (i = y - 1; i <= y + 1; i++) {
		if (i < 0 || i >= h) continue;
		for (j = x - 1; j <= x + 1; j++)
			if (!(j < 0 || j >= w
				|| (j == x && i == y)
				|| board[ p[n] = idx(i,j) ] == -1))
				n++;
	}

	return n;
}

void flood_fill(int c)
/*
fill all free cells around @c with “1” and write output to variable “flood”
@c cell
*/
{
	int i, n[8], nei;

	nei = neighbors(c, n);
	for (i = 0; i < nei; i++) { // for all neighbours
		if (board[n[i]] || flood[n[i]]) continue; // if cell is not free, choose another neighbour

		flood[n[i]] = 1;
		flood_fill(n[i]);
	}
}

/* Check all empty cells are reachable from higher known cells.
   Should really do more checks to make sure cell_x and cell_x+1
   share enough reachable empty cells; I'm lazy. Will implement
   if a good counter example is presented. */
int check_connectity(int lowerbound)
{
	int c;
	memset(flood, 0, sizeof(flood[0]) * w * h);
	for (c = lowerbound + 1; c <= top; c++)
		if (known[c]) flood_fill(known[c]); // mark all free cells around known cells

	for (c = 0; c < w * h; c++)
		if (!board[c] && !flood[c]) // if there are free cells which could not be reached from flood_fill
			return 0;

	return 1;
}

void make_board(int x, int y, const char *s)
{
	int i;

	w = x, h = y;
        top = 0;
	x = w * h;

        known = calloc(x + 1, sizeof(int));
        board = calloc(x,     sizeof(int));
        flood = calloc(x,     sizeof(int));

	while (x--) board[x] = -1;

	for (y = 0; y < h; y++)
	for (x = 0; x < w; x++) {
		i = idx(y, x);

		while (isspace(*s)) s++;

		switch (*s) {
		case '_':	board[i] = 0;
		case '.':	break;
		default:
			known[ board[i] = strtol(s, 0, 10) ] = i;
			if (board[i] > top) top = board[i];
		}

		while (*s && !isspace(*s)) s++;
	}
}

void show_board(const char *s)
{
	int i, j, c;

	printf("\n%s:\n", s);

	for (i = 0; i < h; i++, putchar('\n'))
	for (j = 0; j < w; j++) {
		c = board[ idx(i, j) ];
		printf(!c ? " __" : c == -1 ? "   " : " %2d", c);
	}
}

int fill(int c, int n)
{
	int i, nei, p[8], ko, bo;

	if ((board[c] && board[c] != n) || (known[n] && known[n] != c))
		return 0;

	if (n == top) return 1;

	ko = known[n];
	bo = board[c];
	board[c] = n;

	if (check_connectity(n)) {
		nei = neighbors(c, p);
		for (i = 0; i < nei; i++)
			if (fill(p[i], n + 1))
				return 1;
	}

	board[c] = bo;
	known[n] = ko;
	return 0;
}

int main()
{
	make_board(
#define USE_E 0
#if (USE_E == 0)
		8,8,	" __ 33 35 __ __ .. .. .."
			" __ __ 24 22 __ .. .. .."
			" __ __ __ 21 __ __ .. .."
			" __ 26 __ 13 40 11 .. .."
			" 27 __ __ __  9 __  1 .."
			" .   . __ __ 18 __ __ .."
			" .  ..  .  . __  7 __ __"
			" .  .. .. ..  .  .  5 __"
#elif (USE_E == 1)
	3, 3,	" . 4 ."
		" _ 7 _"
		" 1 _ _"
#else
	50, 3,
	" 1 _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . 74"
	" . . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ ."
	" . . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ ."
#endif
	);

	show_board("Before");
	fill(known[1], 1);
	show_board("After"); /* "40 lbs in two weeks!" */

	return 0;
}

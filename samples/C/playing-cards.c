#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

int locale_ok = 0;

wchar_t s_suits[] = L"♠♥♦♣";
/* if your file can't contain unicode, use the next line instead */
//wchar_t s_suits[] = L"\x2660\x2665\x2666\x2663";

const char *s_suits_ascii[] = { "S", "H", "D", "C" };
const char *s_nums[] = { "WHAT",
	"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K",
	"OVERFLOW"
};

typedef struct { int suit, number, _s; } card_t, *card;
typedef struct { int n; card_t cards[52]; } deck_t, *deck;

void show_card(card c)
{
	if (locale_ok)
		printf(" %lc%s", s_suits[c->suit], s_nums[c->number]);
	else
		printf(" %s%s", s_suits_ascii[c->suit], s_nums[c->number]);
}

deck new_deck()
{
	int i, j, k;
	deck d = malloc(sizeof(deck_t));
	d->n = 52;
	for (i = k = 0; i < 4; i++)
		for (j = 1; j <= 13; j++, k++) {
			d->cards[k].suit = i;
			d->cards[k].number = j;
		}
	return d;
}

void show_deck(deck d)
{
	int i;
	printf("%d cards:", d->n);
	for (i = 0; i < d->n; i++)
		show_card(d->cards + i);
	printf("\n");
}

int cmp_card(const void *a, const void *b)
{
	int x = ((card)a)->_s, y = ((card)b)->_s;
	return x < y ? -1 : x > y;
}

card deal_card(deck d)
{
	if (!d->n) return 0;
	return d->cards + --d->n;
}

void shuffle_deck(deck d)
{
	int i;
	for (i = 0; i < d->n; i++)
		d->cards[i]._s = rand();
	qsort(d->cards, d->n, sizeof(card_t), cmp_card);
}

int main()
{
	int i, j;
	deck d = new_deck();

	locale_ok = (0 != setlocale(LC_CTYPE, ""));

	printf("New deck, "); show_deck(d);

	printf("\nShuffle and deal to three players:\n");
	shuffle_deck(d);
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 5; j++)
			show_card(deal_card(d));
		printf("\n");
	}
	printf("Left in deck "); show_deck(d);

	/* freeing the data struct requires just free(), but it depends on the
	 * situation: there might be cards dealt out somewhere, which is not
	 * in the scope of this task.
	 */
	//free(d);
	return 0;
}

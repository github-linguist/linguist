#include <sys/types.h>
#include <regex.h>
#include <stdio.h>

typedef struct {
	const char *s;
	int len, prec, assoc;
} str_tok_t;

typedef struct {
	const char * str;
	int assoc, prec;
	regex_t re;
} pat_t;

enum assoc { A_NONE, A_L, A_R };
pat_t pat_eos = {"", A_NONE, 0};

pat_t pat_ops[] = {
	{"^\\)",	A_NONE, -1},
	{"^\\*\\*",	A_R, 3},
	{"^\\^",	A_R, 3},
	{"^\\*",	A_L, 2},
	{"^/",		A_L, 2},
	{"^\\+",	A_L, 1},
	{"^-",		A_L, 1},
	{0}
};

pat_t pat_arg[] = {
	{"^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"},
	{"^[a-zA-Z_][a-zA-Z_0-9]*"},
	{"^\\(", A_L, -1},
	{0}
};

str_tok_t stack[256]; /* assume these are big enough */
str_tok_t queue[256];
int l_queue, l_stack;
#define qpush(x) queue[l_queue++] = x
#define spush(x) stack[l_stack++] = x
#define spop()   stack[--l_stack]

void display(const char *s)
{
	int i;
	printf("\033[1;1H\033[JText | %s", s);
	printf("\nStack| ");
	for (i = 0; i < l_stack; i++)
		printf("%.*s ", stack[i].len, stack[i].s); // uses C99 format strings
	printf("\nQueue| ");
	for (i = 0; i < l_queue; i++)
		printf("%.*s ", queue[i].len, queue[i].s);
	puts("\n\n<press enter>");
	getchar();
}

int prec_booster;

#define fail(s1, s2) {fprintf(stderr, "[Error %s] %s\n", s1, s2); return 0;}

int init(void)
{
	int i;
	pat_t *p;

	for (i = 0, p = pat_ops; p[i].str; i++)
		if (regcomp(&(p[i].re), p[i].str, REG_NEWLINE|REG_EXTENDED))
			fail("comp", p[i].str);

	for (i = 0, p = pat_arg; p[i].str; i++)
		if (regcomp(&(p[i].re), p[i].str, REG_NEWLINE|REG_EXTENDED))
			fail("comp", p[i].str);

	return 1;
}

pat_t* match(const char *s, pat_t *p, str_tok_t * t, const char **e)
{
	int i;
	regmatch_t m;

	while (*s == ' ') s++;
	*e = s;

	if (!*s) return &pat_eos;

	for (i = 0; p[i].str; i++) {
		if (regexec(&(p[i].re), s, 1, &m, REG_NOTEOL))
			continue;
		t->s = s;
		*e = s + (t->len = m.rm_eo - m.rm_so);
		return p + i;
	}
	return 0;
}

int parse(const char *s) {
	pat_t *p;
	str_tok_t *t, tok;

	prec_booster = l_queue = 0;
	display(s);
	while (*s) {
		p = match(s, pat_arg, &tok, &s);
		if (!p || p == &pat_eos) fail("parse arg", s);

		/* Odd logic here. Don't actually stack the parens: don't need to. */
		if (p->prec == -1) {
			prec_booster += 100;
			continue;
		}
		qpush(tok);
		display(s);

re_op:		p = match(s, pat_ops, &tok, &s);
		if (!p) fail("parse op", s);

		tok.assoc = p->assoc;
		tok.prec = p->prec;

		if (p->prec > 0)
			tok.prec = p->prec + prec_booster;
		else if (p->prec == -1) {
			if (prec_booster < 100)
				fail("unmatched )", s);
			tok.prec = prec_booster;
		}

		while (l_stack) {
			t = stack + l_stack - 1;
			if (!(t->prec == tok.prec && t->assoc == A_L)
					&& t->prec <= tok.prec)
				break;
			qpush(spop());
			display(s);
		}

		if (p->prec == -1) {
			prec_booster -= 100;
			goto re_op;
		}

		if (!p->prec) {
			display(s);
			if (prec_booster)
				fail("unmatched (", s);
			return 1;
		}

		spush(tok);
		display(s);
	}

	return 1;
}

int main()
{
	int i;
	const char *tests[] = {
		"3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3",	/* RC mandated: OK */
		"123",					/* OK */
		"3+4 * 2 / ( 1 - 5 ) ^ 2 ^ 3.14",	/* OK */
		"(((((((1+2+3**(4 + 5))))))",		/* bad parens */
		"a^(b + c/d * .1e5)!",			/* unknown op */
		"(1**2)**3",				/* OK */
		0
	};

	if (!init()) return 1;
	for (i = 0; tests[i]; i++) {
		printf("Testing string `%s'   <enter>\n", tests[i]);
		getchar();

		printf("string `%s': %s\n\n", tests[i],
			parse(tests[i]) ? "Ok" : "Error");
	}

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct str_t {
	size_t len, alloc;
	unsigned char *s;
} bstr_t, *bstr;

#define str_len(s) ((s)->len)
bstr str_new(size_t len)
{
	bstr s = malloc(sizeof(bstr_t));
	if (len < 8) len = 8;
	s->alloc = len;
	s->s = malloc(len);
	s->len = 0;
	return s;
}

void str_extend(bstr s)
{
	size_t ns = s->alloc * 2;
	if (ns - s->alloc > 1024) ns = s->alloc + 1024;
	s->s = realloc(s->s, ns);
	s->alloc = ns;
}

void str_del(bstr s)
{
	free(s->s), free(s);
}

int str_cmp(bstr l, bstr r)
{
	int res, len = l->len;
	if (len > r->len) len = r->len;

	if ((res = memcmp(l->s, r->s, len))) return res;
	return l->len > r->len ? 1 : -1;
}

bstr str_dup(bstr src)
{
	bstr x = str_new(src->len);
	memcpy(x->s, src->s, src->len);
	x->len = src->len;
	return x;
}

bstr str_from_chars(const char *t)
{
	if (!t) return str_new(0);
	size_t l = strlen(t);
	bstr x = str_new(l + 1);
	x->len = l;
	memcpy(x->s, t, l);
	return x;
}

void str_append(bstr s, unsigned char b)
{
	if (s->len >= s->alloc) str_extend(s);
	s->s[s->len++] = b;
}

bstr str_substr(bstr s, int from, int to)
{
	if (!to) to = s->len;
	if (from < 0) from += s->len;
	if (from < 0 || from >= s->len)
		return 0;
	if (to < from) to = from + 1;
	bstr x = str_new(to - from);
	x->len = to - from;
	memcpy(x->s, s->s + from, x->len);
	return x;
}

bstr str_cat(bstr s, bstr s2)
{
	while (s->alloc < s->len + s2->len) str_extend(s);
	memcpy(s->s + s->len, s2->s, s2->len);
	s->len += s2->len;
	return s;
}

void str_swap(bstr a, bstr b)
{
	size_t tz;
	unsigned char *ts;
	tz = a->alloc; a->alloc = b->alloc; b->alloc = tz;
	tz = a->len; a->len = b->len; b->len = tz;
	ts = a->s; a->s = b->s; b->s = ts;
}

bstr str_subst(bstr tgt, bstr pat, bstr repl)
{
	bstr tmp = str_new(0);
	int i;
	for (i = 0; i + pat->len <= tgt->len;) {
		if (memcmp(tgt->s + i, pat->s, pat->len)) {
			str_append(tmp, tgt->s[i]);
			i++;
		} else {
			str_cat(tmp, repl);
			i += pat->len;
			if (!pat->len) str_append(tmp, tgt->s[i++]);
		}
	}
	while (i < tgt->len) str_append(tmp, tgt->s[i++]);
	str_swap(tmp, tgt);
	str_del(tmp);
	return tgt;
}

void str_set(bstr dest, bstr src)
{
	while (dest->len < src->len) str_extend(dest);
	memcpy(dest->s, src->s, src->len);
	dest->len = src->len;
}

int main()
{
	bstr s = str_from_chars("aaaaHaaaaaFaaaaHa");
	bstr s2 = str_from_chars("___.");
	bstr s3 = str_from_chars("");

	str_subst(s, s3, s2);
	printf("%.*s\n", s->len, s->s);

	str_del(s);
	str_del(s2);
	str_del(s3);

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char * lang_url = "http://www.rosettacode.org/w/api.php?action=query&"
		"list=categorymembers&cmtitle=Category:Programming_Languages&"
		"cmlimit=500&format=json";
const char * cat_url = "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000";

#define BLOCK 1024
char *get_page(const char *url)
{
	char cmd[1024];
	char *ptr, *buf;
	int bytes_read = 1, len = 0;
	sprintf(cmd, "wget -q \"%s\" -O -", url);
	FILE *fp = popen(cmd, "r");
	if (!fp) return 0;
	for (ptr = buf = 0; bytes_read > 0; ) {
		buf = realloc(buf, 1 + (len += BLOCK));
		if (!ptr) ptr = buf;
		bytes_read = fread(ptr, 1, BLOCK, fp);
		if (bytes_read <= 0) break;
		ptr += bytes_read;
	}
	*++ptr = '\0';
	return buf;
}

char ** get_langs(char *buf, int *l)
{
	char **arr = 0;
	for (*l = 0; (buf = strstr(buf, "Category:")) && (buf += 9); ++*l)
		for (	(*l)[arr = realloc(arr, sizeof(char*)*(1 + *l))] = buf;
			*buf != '"' || (*buf++ = 0);
			buf++);

	return arr;
}

typedef struct { const char *name; int count; } cnt_t;
cnt_t * get_cats(char *buf, char ** langs, int len, int *ret_len)
{
	char str[1024], *found;
	cnt_t *list = 0;
	int i, llen = 0;
	for (i = 0; i < len; i++) {
		sprintf(str, "/wiki/Category:%s", langs[i]);
		if (!(found = strstr(buf, str))) continue;
		buf = found + strlen(str);

		if (!(found = strstr(buf, "</a> ("))) continue;
		list = realloc(list, sizeof(cnt_t) * ++llen);
		list[llen - 1].name = langs[i];
		list[llen - 1].count = strtol(found + 6, 0, 10);
	}
	*ret_len = llen;
	return list;
}

int _scmp(const void *a, const void *b)
{
	int x = ((const cnt_t*)a)->count, y = ((const cnt_t*)b)->count;
	return x < y ? -1 : x > y;
}

int main()
{
	int len, clen;
	char ** langs = get_langs(get_page(lang_url), &len);
	cnt_t *cats = get_cats(get_page(cat_url), langs, len, &clen);
	qsort(cats, clen, sizeof(cnt_t), _scmp);
	while (--clen >= 0)
		printf("%4d %s\n", cats[clen].count, cats[clen].name);

	return 0;
}

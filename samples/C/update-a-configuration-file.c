#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define strcomp(X, Y) strcasecmp(X, Y)

struct option
{ const char *name, *value;
  int flag; };

/* TODO: dynamically obtain these */
struct option updlist[] =
{ { "NEEDSPEELING", NULL },
  { "SEEDSREMOVED", "" },
  { "NUMBEROFBANANAS", "1024" },
  { "NUMBEROFSTRAWBERRIES", "62000" },
  { NULL, NULL } };

int output_opt(FILE *to, struct option *opt)
{ if (opt->value == NULL)
    return fprintf(to, "; %s\n", opt->name);
  else if (opt->value[0] == 0)
    return fprintf(to, "%s\n", opt->name);
  else
    return fprintf(to, "%s %s\n", opt->name, opt->value); }

int update(FILE *from, FILE *to, struct option *updlist)
{ char line_buf[256], opt_name[128];
  int i;
  for (;;)
  { size_t len, space_span, span_to_hash;
    if (fgets(line_buf, sizeof line_buf, from) == NULL)
      break;
    len = strlen(line_buf);
    space_span = strspn(line_buf, "\t ");
    span_to_hash = strcspn(line_buf, "#");
    if (space_span == span_to_hash)
      goto line_out;
    if (space_span == len)
      goto line_out;
    if ((sscanf(line_buf, "; %127s", opt_name) == 1) ||
        (sscanf(line_buf, "%127s", opt_name) == 1))
    { int flag = 0;
      for (i = 0; updlist[i].name; i++)
      { if (strcomp(updlist[i].name, opt_name) == 0)
        { if (output_opt(to, &updlist[i]) < 0)
            return -1;
          updlist[i].flag = 1;
          flag = 1; } }
      if (flag == 0)
        goto line_out; }
    else
  line_out:
      if (fprintf(to, "%s", line_buf) < 0)
        return -1;
    continue; }
  { for (i = 0; updlist[i].name; i++)
    { if (!updlist[i].flag)
        if (output_opt(to, &updlist[i]) < 0)
          return -1; } }
  return feof(from) ? 0 : -1; }

int main(void)
{ if (update(stdin, stdout, updlist) < 0)
  { fprintf(stderr, "failed\n");
    return (EXIT_FAILURE); }
  return 0; }

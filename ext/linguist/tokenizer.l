%{
  char *tokenizer_token;

  static void eat_until_eol();
  static void eat_until_unescaped(char q);
%}

%option never-interactive
%x sgml

%%

^#![ \t]*([[:alnum:]\/]*\/)?env([ \t]+([^ \t=]*=[^ \t]*))*[ \t]+[[:alpha:]]+ {
    const char *off = strrchr(yytext, ' ');
    if (!off)
      off = yytext;
    else
      ++off;
    tokenizer_token = strdup(off);

    eat_until_eol();
    return 2;
  }

^#![ \t]*[[:alpha:]\/]+ {
    const char *off = strrchr(yytext, '/');
    if (!off)
      off = yytext;
    else
      ++off;
    tokenizer_token = strdup(off);

    eat_until_eol();
    return 2;
  }

^[ \t]*(\/\/|--|\#[^!]|%|\").*  { /* nothing */ }
\"\"|''                         { /* nothing */ }
\"                              { eat_until_unescaped('"'); }
'                               { eat_until_unescaped('\''); }
(0x[0-9a-fA-F]([0-9a-fA-F]|\.)*|[0-9]([0-9]|\.)*)([uU][lL]{0,2}|([eE][-+][0-9]*)?[fFlL]*) { /* nothing */ }
\<[^ \t<>]+                     { tokenizer_token = strdup(yytext); BEGIN(sgml); return 3; }
<sgml>[[:alnum:]]+=/\"          { tokenizer_token = strdup(yytext); input(); eat_until_unescaped('"'); return 1; }
<sgml>[[:alnum:]]+=/'           { tokenizer_token = strdup(yytext); input(); eat_until_unescaped('\''); return 1; }
<sgml>[[:alnum:]]+=[[:alnum:]]* { tokenizer_token = strdup(yytext); *(strchr(tokenizer_token, '=') + 1) = 0; return 1; }
<sgml>[[:alnum:]]+              { tokenizer_token = strdup(yytext); return 1; }
<sgml>\>                        { BEGIN(INITIAL); }
<sgml>.|\n                      { /* nothing */ }
;|\{|\}|\(|\)|\[|\]             { tokenizer_token = strdup(yytext); return 1; }
[[:alnum:].@#/*]+               { tokenizer_token = strdup(yytext); return 1; }
\<\<?|\+|\-|\*|\/|%|&&?|\|\|?   { tokenizer_token = strdup(yytext); return 1; }
.|\n                            { /* nothing */ }

%%

static void eat_until_eol() {
  int c;
  while ((c = input()) != '\n' && c != EOF);
}

static void eat_until_unescaped(char q) {
  int c;
  while ((c = input()) != EOF) {
    if (c == '\\') {
      c = input();
      if (c == EOF)
        return;
    } else if (c == q)
      return;
  }
}

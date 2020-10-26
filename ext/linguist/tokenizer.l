%{

#include "ruby.h"

// Anything longer is unlikely to be useful.
#define MAX_TOKEN_LEN 32

#define FEED2(s, l) do { \
    const char* __s = (s); \
    const size_t __l = (l); \
    if ((__l) > MAX_TOKEN_LEN) \
      break; \
    *yyextra = rb_str_new(__s, __l); \
  } while(0)

#define FEED1(s) FEED2(s, strlen(s))

#define FEED() FEED2(yytext, yyleng)

#define FEED_SHEBANG(s) do { \
    const size_t __l = strlen(s); \
    if (__l > MAX_TOKEN_LEN) \
      break; \
    *yyextra = rb_str_new("SHEBANG#!", sizeof("SHEBANG#!") - 1); \
    rb_str_cat(*yyextra, s, __l); \
  } while(0)

#define FEED_SGML() do { \
    if (yyleng > MAX_TOKEN_LEN) \
      break; \
    *yyextra = rb_str_new(yytext, yyleng); \
    rb_str_cat(*yyextra, ">", 1); \
  } while(0)

#define eat_until_eol() do { \
    int c; \
    while ((c = input(yyscanner)) != '\n' && c != EOF && c); \
    if (c == EOF || !c) \
      return 0; \
  } while (0)

#define eat_until_unescaped(q) do { \
    int c; \
    while ((c = input(yyscanner)) != EOF && c) { \
      if (c == '\n') \
        break; \
      if (c == '\\') { \
        c = input(yyscanner); \
        if (c == EOF || !c) \
          return 0; \
      } else if (c == q) \
        break; \
    } \
    if (c == EOF || !c) \
      return 0; \
  } while (0)

%}

%option never-interactive yywrap reentrant nounput warn nodefault header-file="lex.linguist_yy.h" extra-type="VALUE*" prefix="linguist_yy"
%x sgml c_comment xml_comment haskell_comment ocaml_comment python_dcomment python_scomment

%%

^#![ \t]*([[:alnum:]_\/]*\/)?env([ \t]+([^ \t=]*=[^ \t]*))*[ \t]+[[:alpha:]_]+ {
  const char *off = strrchr(yytext, ' ');
  if (!off)
    off = yytext;
  else
    ++off;
  FEED_SHEBANG(off);
  eat_until_eol();
  return 1;
}

^#![ \t]*[[:alpha:]_\/]+  {
  const char *off = strrchr(yytext, '/');
  if (!off)
    off = yytext;
  else
   ++off;
  if (strcmp(off, "env") == 0) {
    eat_until_eol();
  } else {
    FEED_SHEBANG(off);
    eat_until_eol();
    return 1;
  }
}

^[ \t]*(\/\/|--|\#|%|\")" ".*   { /* nothing */ }

"/*"                              { BEGIN(c_comment); }
  /* See below for xml_comment start. */
"{-"                              { BEGIN(haskell_comment); }
"(*"                              { BEGIN(ocaml_comment); }
"\"\"\""                          { BEGIN(python_dcomment); }
"'''"                             { BEGIN(python_scomment); }

<c_comment,xml_comment,haskell_comment,ocaml_comment,python_dcomment,python_scomment>.|\n { /* nothing */ }
<c_comment>"*/"                   { BEGIN(INITIAL); }
<xml_comment>"-->"                { BEGIN(INITIAL); }
<haskell_comment>"-}"             { BEGIN(INITIAL); }
<ocaml_comment>"*)"               { BEGIN(INITIAL); }
<python_dcomment>"\"\"\""         { BEGIN(INITIAL); }
<python_scomment>"'''"            { BEGIN(INITIAL); }

\"\"|''                           { /* nothing */ }
\"                                { eat_until_unescaped('"'); }
'                                 { eat_until_unescaped('\''); }
(0x[0-9a-fA-F]([0-9a-fA-F]|\.)*|[0-9]([0-9]|\.)*)([uU][lL]{0,2}|([eE][-+][0-9]*)?[fFlL]*) { /* nothing */ }
\<[[:alnum:]_!./?-]+              {
    if (strcmp(yytext, "<!--") == 0) {
     BEGIN(xml_comment);
    } else {
      FEED_SGML();
      BEGIN(sgml);
      return 1;
    }
  }
<sgml>[[:alnum:]_]+=\"            { FEED2(yytext, yyleng - 1); eat_until_unescaped('"'); return 1; }
<sgml>[[:alnum:]_]+='             { FEED2(yytext, yyleng - 1); eat_until_unescaped('\''); return 1; }
<sgml>[[:alnum:]_]+=[[:alnum:]_]* { FEED2(yytext, strchr(yytext, '=') - yytext + 1); return 1; }
<sgml>[[:alnum:]_]+               { FEED(); return 1; }
<sgml>\>                          { BEGIN(INITIAL); }
<sgml>.|\n                        { /* nothing */ }
;|\{|\}|\(|\)|\[|\]               { FEED(); return 1; }
[[:alnum:]_.@#/*]+                {
    if (strncmp(yytext, "/*", 2) == 0) {
      if (yyleng >= 4 && strcmp(yytext + yyleng - 2, "*/") == 0) {
        /* nothing */
      } else {
        BEGIN(c_comment);
      }
    } else {
      FEED();
      return 1;
    }
  }
\<\<?|\+|\-|\*|\/|%|&&?|\|\|?     { FEED(); return 1; }
.|\n                              { /* nothing */ }

%%


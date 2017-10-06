%{

#include "linguist.h"

#define feed_token(tok, typ) do { \
    yyextra->token = (tok); \
    yyextra->type = (typ); \
  } while (0)

#define eat_until_eol() do { \
    int c; \
    while ((c = input(yyscanner)) != '\n' && c != EOF); \
    if (c == EOF) \
      yyterminate(); \
  } while (0)

#define eat_until_unescaped(q) do { \
    int c; \
    while ((c = input(yyscanner)) != EOF) { \
      if (c == '\n') \
        break; \
      if (c == '\\') { \
        c = input(yyscanner); \
        if (c == EOF) \
          yyterminate(); \
      } else if (c == q) \
        break; \
    } \
    if (c == EOF) \
      yyterminate(); \
  } while (0)

%}

%option never-interactive yywrap reentrant nounput warn nodefault header-file="yy.lex.h" extra-type="struct tokenizer_extra *"
%x sgml c_comment xml_comment haskell_comment ocaml_comment python_dcomment python_scomment

%%

^#![ \t]*([[:alnum:]_\/]*\/)?env([ \t]+([^ \t=]*=[^ \t]*))*[ \t]+[[:alpha:]_]+ {
    const char *off = strrchr(yytext, ' ');
    if (!off)
      off = yytext;
    else
      ++off;
    feed_token(strdup(off), SHEBANG_TOKEN);
    eat_until_eol();
    return 1;
  }

^#![ \t]*[[:alpha:]_\/]+ {
    const char *off = strrchr(yytext, '/');
    if (!off)
      off = yytext;
    else
      ++off;
    if (strcmp(off, "env") == 0) {
      eat_until_eol();
    } else {
      feed_token(strdup(off), SHEBANG_TOKEN);
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
\<[^ \t\n\r<>]+/>|" "[^<>\n]{0,2048}>               {
    if (strcmp(yytext, "<!--") == 0) {
     BEGIN(xml_comment);
    } else {
      feed_token(strdup(yytext), SGML_TOKEN);
      BEGIN(sgml);
      return 1;
    }
  }
<sgml>[[:alnum:]_]+=/\"           { feed_token(strdup(yytext), REGULAR_TOKEN); input(yyscanner); eat_until_unescaped('"'); return 1; }
<sgml>[[:alnum:]_]+=/'            { feed_token(strdup(yytext), REGULAR_TOKEN); input(yyscanner); eat_until_unescaped('\''); return 1; }
<sgml>[[:alnum:]_]+=[[:alnum:]_]* { feed_token(strdup(yytext), REGULAR_TOKEN); *(strchr(yyextra->token, '=') + 1) = 0; return 1; }
<sgml>[[:alnum:]_]+               { feed_token(strdup(yytext), REGULAR_TOKEN); return 1; }
<sgml>\>                          { BEGIN(INITIAL); }
<sgml>.|\n                        { /* nothing */ }
;|\{|\}|\(|\)|\[|\]               { feed_token(strdup(yytext), REGULAR_TOKEN); return 1; }
[[:alnum:]_.@#/*]+                {
    if (strncmp(yytext, "/*", 2) == 0) {
      if (strlen(yytext) >= 4 && strcmp(yytext + strlen(yytext) - 2, "*/") == 0) {
        /* nothing */
      } else {
        BEGIN(c_comment);
      }
    } else {
      feed_token(strdup(yytext), REGULAR_TOKEN);
      return 1;
    }
  }
\<\<?|\+|\-|\*|\/|%|&&?|\|\|?     { feed_token(strdup(yytext), REGULAR_TOKEN); return 1; }
.|\n                              { /* nothing */ }

%%


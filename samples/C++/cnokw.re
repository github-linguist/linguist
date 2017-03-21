#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define	ADDEQ	257
#define	ANDAND	258
#define	ANDEQ	259
#define	ARRAY	260
#define	ASM	261
#define	AUTO	262
#define	BREAK	263
#define	CASE	264
#define	CHAR	265
#define	CONST	266
#define	CONTINUE	267
#define	DECR	268
#define	DEFAULT	269
#define	DEREF	270
#define	DIVEQ	271
#define	DO	272
#define	DOUBLE	273
#define	ELLIPSIS	274
#define	ELSE	275
#define	ENUM	276
#define	EQL	277
#define	EXTERN	278
#define	FCON	279
#define	FLOAT	280
#define	FOR	281
#define	FUNCTION	282
#define	GEQ	283
#define	GOTO	284
#define	ICON	285
#define	ID	286
#define	IF	287
#define	INCR	288
#define	INT	289
#define	LEQ	290
#define	LONG	291
#define	LSHIFT	292
#define	LSHIFTEQ	293
#define	MODEQ	294
#define	MULEQ	295
#define	NEQ	296
#define	OREQ	297
#define	OROR	298
#define	POINTER	299
#define	REGISTER	300
#define	RETURN	301
#define	RSHIFT	302
#define	RSHIFTEQ	303
#define	SCON	304
#define	SHORT	305
#define	SIGNED	306
#define	SIZEOF	307
#define	STATIC	308
#define	STRUCT	309
#define	SUBEQ	310
#define	SWITCH	311
#define	TYPEDEF	312
#define	UNION	313
#define	UNSIGNED	314
#define	VOID	315
#define	VOLATILE	316
#define	WHILE	317
#define	XOREQ	318
#define	EOI	319

typedef unsigned int uint;
typedef unsigned char uchar;

#define	BSIZE	8192

#define	YYCTYPE		uchar
#define	YYCURSOR	cursor
#define	YYLIMIT		s->lim
#define	YYMARKER	s->ptr
#define	YYFILL(n)	{cursor = fill(s, cursor);}

#define	RET(i)	{s->cur = cursor; return i;}

typedef struct Scanner {
    int			fd;
    uchar		*bot, *tok, *ptr, *cur, *pos, *lim, *top, *eof;
    uint		line;
} Scanner;

uchar *fill(Scanner *s, uchar *cursor){
    if(!s->eof){
	uint cnt = s->tok - s->bot;
	if(cnt){
	    memcpy(s->bot, s->tok, s->lim - s->tok);
	    s->tok = s->bot;
	    s->ptr -= cnt;
	    cursor -= cnt;
	    s->pos -= cnt;
	    s->lim -= cnt;
	}
	if((s->top - s->lim) < BSIZE){
	    uchar *buf = (uchar*) malloc(((s->lim - s->bot) + BSIZE)*sizeof(uchar));
	    memcpy(buf, s->tok, s->lim - s->tok);
	    s->tok = buf;
	    s->ptr = &buf[s->ptr - s->bot];
	    cursor = &buf[cursor - s->bot];
	    s->pos = &buf[s->pos - s->bot];
	    s->lim = &buf[s->lim - s->bot];
	    s->top = &s->lim[BSIZE];
	    free(s->bot);
	    s->bot = buf;
	}
	if((cnt = read(s->fd, (char*) s->lim, BSIZE)) != BSIZE){
	    s->eof = &s->lim[cnt]; *(s->eof)++ = '\n';
	}
	s->lim += cnt;
    }
    return cursor;
}

int scan(Scanner *s){
	uchar *cursor = s->cur;
std:
	s->tok = cursor;
/*!re2c
any	= [\000-\377];
O	= [0-7];
D	= [0-9];
L	= [a-zA-Z_];
H	= [a-fA-F0-9];
E	= [Ee] [+-]? D+;
FS	= [fFlL];
IS	= [uUlL]*;
ESC	= [\\] ([abfnrtv?'"\\] | "x" H+ | O+);
*/

/*!re2c
	"/*"			{ goto comment; }
	
	L (L|D)*		{ RET(ID); }
	
	("0" [xX] H+ IS?) | ("0" D+ IS?) | (D+ IS?) |
	(['] (ESC|any\[\n\\'])* ['])
				{ RET(ICON); }
	
	(D+ E FS?) | (D* "." D+ E? FS?) | (D+ "." D* E? FS?)
				{ RET(FCON); }
	
	(["] (ESC|any\[\n\\"])* ["])
				{ RET(SCON); }
	
	"..."                   { RET(ELLIPSIS); }
	">>="			{ RET(RSHIFTEQ); }
	"<<="			{ RET(LSHIFTEQ); }
	"+="			{ RET(ADDEQ); }
	"-="			{ RET(SUBEQ); }
	"*="			{ RET(MULEQ); }
	"/="			{ RET(DIVEQ); }
	"%="			{ RET(MODEQ); }
	"&="			{ RET(ANDEQ); }
	"^="			{ RET(XOREQ); }
	"|="			{ RET(OREQ); }
	">>"			{ RET(RSHIFT); }
	"<<"			{ RET(LSHIFT); }
	"++"			{ RET(INCR); }
	"--"			{ RET(DECR); }
	"->"			{ RET(DEREF); }
	"&&"			{ RET(ANDAND); }
	"||"			{ RET(OROR); }
	"<="			{ RET(LEQ); }
	">="			{ RET(GEQ); }
	"=="			{ RET(EQL); }
	"!="			{ RET(NEQ); }
	";"			{ RET(';'); }
	"{"			{ RET('{'); }
	"}"			{ RET('}'); }
	","			{ RET(','); }
	":"			{ RET(':'); }
	"="			{ RET('='); }
	"("			{ RET('('); }
	")"			{ RET(')'); }
	"["			{ RET('['); }
	"]"			{ RET(']'); }
	"."			{ RET('.'); }
	"&"			{ RET('&'); }
	"!"			{ RET('!'); }
	"~"			{ RET('~'); }
	"-"			{ RET('-'); }
	"+"			{ RET('+'); }
	"*"			{ RET('*'); }
	"/"			{ RET('/'); }
	"%"			{ RET('%'); }
	"<"			{ RET('<'); }
	">"			{ RET('>'); }
	"^"			{ RET('^'); }
	"|"			{ RET('|'); }
	"?"			{ RET('?'); }


	[ \t\v\f]+		{ goto std; }

	"\n"
	    {
		if(cursor == s->eof) RET(EOI);
		s->pos = cursor; s->line++;
		goto std;
	    }

	any
	    {
		printf("unexpected character: %c\n", *s->tok);
		goto std;
	    }
*/

comment:
/*!re2c
	"*/"			{ goto std; }
	"\n"
	    {
		if(cursor == s->eof) RET(EOI);
		s->tok = s->pos = cursor; s->line++;
		goto comment;
	    }
        any			{ goto comment; }
*/
}

main(){
    Scanner in;
    int t;
    memset((char*) &in, 0, sizeof(in));
    in.fd = 0;
    while((t = scan(&in)) != EOI){
/*
	printf("%d\t%.*s\n", t, in.cur - in.tok, in.tok);
	printf("%d\n", t);
*/
    }
    close(in.fd);
}

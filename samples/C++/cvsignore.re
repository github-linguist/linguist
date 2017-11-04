
#define YYFILL(n) if (cursor >= limit) break;
#define YYCTYPE char
#define YYCURSOR cursor
#define YYLIMIT limit
#define YYMARKER marker

/*!re2c
any     = (.|"\n");
value	= (":" (.\"$")+)?;
cvsdat	= "Date";
cvsid	= "Id";
cvslog	= "Log";
cvsrev	= "Revision";
cvssrc	= "Source";
*/

#define APPEND(text) \
	append(output, outsize, text, sizeof(text) - sizeof(YYCTYPE))

inline void append(YYCTYPE *output, size_t & outsize, const YYCTYPE * text, size_t len)
{
	memcpy(output + outsize, text, len);
	outsize += (len / sizeof(YYCTYPE));
}

void scan(YYCTYPE *pText, size_t *pSize, int *pbChanged)
{
	// rule
	// scan lines
	// find $ in lines
	//   compact $<keyword>: .. $ to $<keyword>$
  
	YYCTYPE *output;
	const YYCTYPE *cursor, *limit, *marker;

	cursor = marker = output = *pText;

	size_t insize = *pSize;
	size_t outsize = 0;

	limit = cursor + insize;

	while(1) {
loop:
/*!re2c

"$" cvsdat value "$"	{ APPEND(L"$" L"Date$"); goto loop; }
"$" cvsid  value "$"	{ APPEND(L"$" L"Id$"); goto loop; }
"$" cvslog value "$"	{ APPEND(L"$" L"Log$"); goto loop; }
"$" cvsrev value "$"	{ APPEND(L"$" L"Revision$"); goto loop; }
"$" cvssrc value "$"	{ APPEND(L"$" L"Source$"); goto loop; }
any						{ output[outsize++] = cursor[-1]; if (cursor >= limit) break; goto loop; }

*/
	}
	output[outsize] = '\0';

	// set the new size
	*pSize = outsize;
	
	*pbChanged = (insize == outsize) ? 0 : 1;
}

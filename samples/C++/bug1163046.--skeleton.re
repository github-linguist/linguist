#include <iostream>

#define YYCTYPE unsigned char
#define YYCURSOR cursor
#define YYLIMIT cursor
#define YYMARKER marker
#define YYFILL(n)

bool scan(const char *text)
{
	YYCTYPE *start = (YYCTYPE *)text;
	YYCTYPE *cursor = (YYCTYPE *)text;
	YYCTYPE *marker = (YYCTYPE *)text;
next:
	YYCTYPE *token = cursor;
/*!re2c
'(This file must be converted with BinHex 4.0)'
	{
		if (token == start || *(token - 1) == '\n')
		return true; else goto next;
	}
[\001-\377]
	{ goto next; }
[\000]
	{ return false; }
*/
	return false;
}

#define do_scan(str, expect) \
	res = scan(str) == expect ? 0 : 1; \
	std::cerr << str << "\t-\t" << (res ? "fail" : "ok") << std::endl; \
	result += res

/*!max:re2c */

int main(int,void**)
{
	int res, result = 0;
	do_scan("(This file must be converted with BinHex 4.0)", 1);
	do_scan("x(This file must be converted with BinHex 4.0)", 0);
	do_scan("(This file must be converted with BinHex 4.0)x", 1);
	do_scan("x(This file must be converted with BinHex 4.0)x", 0);
	
	return result;
}

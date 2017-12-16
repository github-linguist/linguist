#include "ruby.h"
#include "linguist.h"
#include "lex.linguist_yy.h"

// Anything longer is unlikely to be useful.
#define MAX_TOKEN_LEN 32

int linguist_yywrap(yyscan_t yyscanner) {
	return 1;
}

static VALUE rb_tokenizer_extract_tokens(VALUE self, VALUE rb_data) {
	YY_BUFFER_STATE buf;
	yyscan_t scanner;
	struct tokenizer_extra extra;
	VALUE ary, s;
	long len;
	int r;

	Check_Type(rb_data, T_STRING);

	len = RSTRING_LEN(rb_data);
	if (len > 100000)
		len = 100000;

	linguist_yylex_init_extra(&extra, &scanner);
	buf = linguist_yy_scan_bytes(RSTRING_PTR(rb_data), (int) len, scanner);

	ary = rb_ary_new();
	do {
		extra.type = NO_ACTION;
		extra.token = NULL;
		r = linguist_yylex(scanner);
		switch (extra.type) {
		case NO_ACTION:
			break;
		case REGULAR_TOKEN:
			len = strlen(extra.token);
			if (len <= MAX_TOKEN_LEN)
				rb_ary_push(ary, rb_str_new(extra.token, len));
			free(extra.token);
			break;
		case SHEBANG_TOKEN:
			len = strlen(extra.token);
			if (len <= MAX_TOKEN_LEN) {
				s = rb_str_new2("SHEBANG#!");
				rb_str_cat(s, extra.token, len);
				rb_ary_push(ary, s);
			}
			free(extra.token);
			break;
		case SGML_TOKEN:
			len = strlen(extra.token);
			if (len <= MAX_TOKEN_LEN) {
				s = rb_str_new(extra.token, len);
				rb_str_cat2(s, ">");
				rb_ary_push(ary, s);
			}
			free(extra.token);
			break;
		}
	} while (r);

	linguist_yy_delete_buffer(buf, scanner);
	linguist_yylex_destroy(scanner);

	return ary;
}

__attribute__((visibility("default"))) void Init_linguist() {
	VALUE rb_mLinguist = rb_define_module("Linguist");
	VALUE rb_cTokenizer = rb_define_class_under(rb_mLinguist, "Tokenizer", rb_cObject);

	rb_define_method(rb_cTokenizer, "extract_tokens", rb_tokenizer_extract_tokens, 1);
}

#include "ruby.h"
#include "lex.linguist_yy.h"

int linguist_yywrap(yyscan_t yyscanner) {
	return 1;
}

static VALUE rb_tokenizer_extract_tokens(VALUE self, VALUE rb_data) {
	YY_BUFFER_STATE buf;
	yyscan_t scanner;
	VALUE extra;
	VALUE ary;
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
		extra = 0;
		r = linguist_yylex(scanner);
		if (extra) {
			rb_ary_push(ary, extra);
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

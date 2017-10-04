#include "ruby.h"
#include "yy.lex.h"
#include "linguist.h"

int yywrap(yyscan_t yyscanner) {
	return 1;
}

static VALUE rb_tokenizer_extract_tokens(VALUE self, VALUE rb_data) {
	YY_BUFFER_STATE buf;
	yyscan_t scanner;
	VALUE ary, s;
	long len;
	int r;

	Check_Type(rb_data, T_STRING);

	len = RSTRING_LEN(rb_data);
	if (len > 100000)
		len = 100000;

	yylex_init(&scanner);
	buf = yy_scan_bytes(RSTRING_PTR(rb_data), len, scanner);

	ary = rb_ary_new();
	do {
		tokenizer_type = NO_ACTION;
		tokenizer_token = NULL;
		r = yylex(scanner);
		switch (tokenizer_type) {
		case NO_ACTION:
			break;
		case REGULAR_TOKEN:
			rb_ary_push(ary, rb_str_new2(tokenizer_token));
			free(tokenizer_token);
			break;
		case SHEBANG_TOKEN:
			s = rb_str_new2("SHEBANG#!");
			rb_str_cat2(s, tokenizer_token);
			rb_ary_push(ary, s);
			free(tokenizer_token);
			break;
		case SGML_TOKEN:
			s = rb_str_new2(tokenizer_token);
			rb_str_cat2(s, ">");
			rb_ary_push(ary, s);
			free(tokenizer_token);
			break;
		}
	} while (r);

	yy_delete_buffer(buf, scanner);
	yylex_destroy(scanner);

	return ary;
}

__attribute__((visibility("default"))) void Init_linguist() {
	VALUE rb_mLinguist = rb_define_module("Linguist");
	VALUE rb_cTokenizer = rb_define_class_under(rb_mLinguist, "Tokenizer", rb_cObject);

	rb_define_method(rb_cTokenizer, "extract_tokens", rb_tokenizer_extract_tokens, 1);
}

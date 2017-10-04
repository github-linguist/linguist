#include "ruby.h"
#include "yy.lex.h"

extern char *tokenizer_token;

int yywrap(void) {
	return 1;
}

static VALUE rb_tokenizer_extract_tokens(VALUE self, VALUE rb_data) {
	Check_Type(rb_data, T_STRING);

	long len = RSTRING_LEN(rb_data);
	if (len > 100000)
		len = 100000;

	YY_BUFFER_STATE buf = yy_scan_bytes(RSTRING_PTR(rb_data), len);

	VALUE ary = rb_ary_new();
	int r;
	while ((r = yylex())) {
		if (r == 1) {
			rb_ary_push(ary, rb_str_new2(tokenizer_token));
			free(tokenizer_token);
		} else if (r == 2) {
			VALUE s = rb_str_new2("SHEBANG#!");
			rb_str_cat2(s, tokenizer_token);
			rb_ary_push(ary, s);
			free(tokenizer_token);
		}
	}

	yy_delete_buffer(buf);

	return ary;
}

__attribute__((visibility("default"))) void Init_linguist() {
	VALUE rb_mLinguist = rb_define_module("Linguist");
	VALUE rb_cTokenizer = rb_define_class_under(rb_mLinguist, "Tokenizer", rb_cObject);

	rb_define_method(rb_cTokenizer, "extract_tokens", rb_tokenizer_extract_tokens, 1);
}

enum tokenizer_type {
  NO_ACTION,
  REGULAR_TOKEN,
  SHEBANG_TOKEN,
	SGML_TOKEN,
};

extern char *tokenizer_token;
extern enum tokenizer_type tokenizer_type;

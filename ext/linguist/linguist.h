enum tokenizer_type {
  NO_ACTION,
  REGULAR_TOKEN,
  SHEBANG_TOKEN,
  SGML_TOKEN,
};

struct tokenizer_extra {
  char *token;
  enum tokenizer_type type;
};

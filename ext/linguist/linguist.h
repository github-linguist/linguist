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

#ifdef __APPLE__
char *strndup(const char *s1, size_t n);
#elif defined(_WIN32) || defined(_WIN64)
char *strndup(const char *s1, size_t n);
#pragma warning (disable: 4244)
#endif // _WIN32 || _WIN64

#include <ruby.h>
#include <ruby/st.h>
#include <arpa/inet.h>
#include "judy.h"

#define MAX_TOKEN_SIZE 64

static const char DB_HEADER[] = "linguist1";
static const char DB_FOOTER[] = "\0db";

VALUE rb_cLinguistDB;
VALUE rb_cLanguage;

struct training_db {
	Judy *tokens;
	st_table *languages;

	uint32_t samples_total;
	uint32_t tokens_total;

	uint16_t lang_ids;

	int dump_fd;
};

struct training_lang {
	uint32_t sample_count;
	uint32_t token_count;
	uint8_t id[2];
	char name[1];
};

static void encode_lang_id(uint8_t enc[2], uint16_t n)
{
	static char encoding_table[] = {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '+', '/'
	};

	enc[0] = encoding_table[n % 64];
	enc[1] = encoding_table[(n / 64) % 64];
}

static int free_each_lang(const char *lang_name, struct training_lang *lang, st_data_t _unused)
{
	xfree(lang);
	return ST_CONTINUE;
}

static void rb_lng_trainingdb__free(void *_db)
{
	struct training_db *db = _db;

	st_foreach(db->languages, &free_each_lang, (st_data_t)0);
	st_free_table(db->languages);
	judy_close(db->tokens);
	xfree(db);
}

static VALUE rb_lng_trainingdb_new(VALUE self, VALUE rb_path)
{
	struct training_db *db = NULL;

	db = xmalloc(sizeof(struct training_db));
	db->tokens = judy_open(MAX_TOKEN_SIZE, 0);
	db->languages = st_init_strcasetable();

	db->lang_ids = 1;
	db->samples_total = 0;
	db->tokens_total = 0;

	return Data_Wrap_Struct(self, NULL, &rb_lng_trainingdb__free, db);
}

static VALUE rb_lng_trainingdb_samples_total(VALUE self)
{
	struct training_db *db;
	Data_Get_Struct(self, struct training_db, db);
	return rb_float_new((double)db->samples_total);
}

static VALUE rb_lng_trainingdb_tokens_total(VALUE self)
{
	struct training_db *db;
	Data_Get_Struct(self, struct training_db, db);
	return rb_float_new((double)db->tokens_total);
}

static struct training_lang *
lookup_language(struct training_db *db, const char *name)
{
	struct training_lang *lang = NULL;
	int name_len;

	if (st_lookup(db->languages, (st_data_t)name, (st_data_t *)&lang))
		return lang;

	name_len = strlen(name);

	lang = xmalloc(sizeof(struct training_lang) + name_len + 1);
	encode_lang_id(lang->id, db->lang_ids++);
	lang->sample_count = 0;
	lang->token_count = 0;

	memcpy(lang->name, name, name_len);
	lang->name[name_len] = '\0';

	st_add_direct(db->languages, (st_data_t)lang->name, (st_data_t)lang);
	return lang;
}

static VALUE rb_lng_trainingdb_train(VALUE self, VALUE rb_lang, VALUE rb_tokens)
{
	struct training_db *db;
	struct training_lang *lang = NULL;
	long i;
	uint8_t token_name[MAX_TOKEN_SIZE + 2];

	Data_Get_Struct(self, struct training_db, db);

	Check_Type(rb_lang, T_STRING);
	Check_Type(rb_tokens, T_ARRAY);

	lang = lookup_language(db, StringValueCStr(rb_lang));

	memcpy(token_name, lang->id, 2);

	for (i = 0; i < RARRAY_LEN(rb_tokens); ++i) {
		VALUE token = rb_ary_entry(rb_tokens, i);
		long len;

		Check_Type(token, T_STRING);
		len = RSTRING_LEN(token);

		if (len > MAX_TOKEN_SIZE)
			continue;

		memcpy(token_name + 2, RSTRING_PTR(token), len);

		*(judy_cell(db->tokens, token_name, len + 2)) += 1;

		lang->token_count++;
		db->tokens_total++;
	}

	lang->sample_count++;
	db->samples_total++;

	return Qnil;
}

static int each_lang_name(const char *lang_name, struct training_lang *_lang, VALUE rb_lang_names)
{
	rb_ary_push(rb_lang_names, rb_str_new2(lang_name));
	return ST_CONTINUE;
}

static VALUE rb_lng_trainingdb_lang_names(VALUE self)
{
	struct training_db *db;
	VALUE rb_lang_names;

	Data_Get_Struct(self, struct training_db, db);

	rb_lang_names = rb_ary_new2(db->languages->num_entries);
	st_foreach(db->languages, &each_lang_name, (st_data_t)rb_lang_names);
	return rb_lang_names;
}

static VALUE rb_lng_trainingdb_get_lang(VALUE self, VALUE rb_lang_name)
{
	struct training_db *db;
	struct training_lang *lang = NULL;
	VALUE rb_lang;

	Data_Get_Struct(self, struct training_db, db);

	Check_Type(rb_lang_name, T_STRING);

	if (!st_lookup(db->languages, (st_data_t)StringValueCStr(rb_lang_name), (st_data_t *)&lang))
		return Qnil;

	rb_lang = Data_Wrap_Struct(rb_cLanguage, NULL, NULL, lang);
	rb_iv_set(rb_lang, "@db", self);

	return rb_lang;
}

static void load_training_data(struct training_db *db, const uint8_t *data, size_t size)
{
	uint8_t token_name[MAX_TOKEN_SIZE + 2];
	uint32_t t;

	if (memcmp(data, DB_HEADER, sizeof(DB_HEADER)) != 0 ||
		memcmp(data + size - sizeof(DB_FOOTER), DB_FOOTER, sizeof(DB_FOOTER)) != 0) {
		rb_raise(rb_eRuntimeError, "Corrupted Linguist database file");
	}

	data += sizeof(DB_HEADER);

	while (*data != '\0') {
		struct training_lang *lang = NULL;

		const uint8_t *name = data;
		long name_len = strlen(name);

		data += name_len + 1;
		lang = lookup_language(db, name);

		lang->sample_count = ntohl(*((uint32_t *)data));
		data += 4;

		lang->token_count = ntohl(*((uint32_t *)data));
		data += 4;

		memcpy(token_name, lang->id, 2);

		for (t = 0; t < lang->token_count; ++t) {
			const char *token = data;
			long token_len = strlen(token);
			uint16_t token_val;

			data += token_len + 1;

			token_val = ntohs(*((uint16_t *)data));
			data += 2;

			if (token_len > MAX_TOKEN_SIZE)
				continue;

			memcpy(token_name + 2, token, token_len);

			*(judy_cell(db->tokens, token_name, token_len + 2)) = token_val;
		}

		db->tokens_total += lang->token_count;
		db->samples_total += lang->sample_count;
	}
}

static int dump_lang(const char *_lname, struct training_lang *lang, struct training_db *db)
{
	static char TERM[] = { 0 };

	int fd = db->dump_fd;
	JudySlot *slot;

	write(fd, lang->name, strlen(lang->name) + 1);

	{
		uint32_t sample_count = htonl(lang->sample_count);
		write(fd, &sample_count, sizeof(uint32_t));
	}

	{
		uint32_t token_count = htonl(lang->token_count);
		write(fd, &token_count, sizeof(uint32_t));
	}

	slot = judy_strt(db->tokens, lang->id, 2);

	while (slot != NULL) {
		uint8_t token_name[MAX_TOKEN_SIZE + 2];
		uint32_t token_len = judy_key(db->tokens, token_name, MAX_TOKEN_SIZE + 2);
		uint16_t token_val;

		if (token_name[0] != lang->id[0] || token_name[1] != lang->id[1])
			break;

		write(fd, token_name + 2, token_len - 2);
		write(fd, TERM, 1);

		token_val = *slot;
		token_val = htons(token_val);

		write(fd, &token_val, 2);

		slot = judy_nxt(db->tokens);
	}

	return ST_CONTINUE;
}

static void dump_training_data(struct training_db *db)
{
	write(db->dump_fd, DB_HEADER, sizeof(DB_HEADER));
	st_foreach(db->languages, &dump_lang, (st_data_t)db);
	write(db->dump_fd, DB_FOOTER, sizeof(DB_FOOTER));
}

static VALUE rb_lng_language_name(VALUE self)
{
	struct training_lang *lang = NULL;
	Data_Get_Struct(self, struct training_lang, lang);
	return rb_str_new2(lang->name);
}

static VALUE rb_lng_language_sample_count(VALUE self)
{
	struct training_lang *lang = NULL;
	Data_Get_Struct(self, struct training_lang, lang);
	return rb_float_new((double)lang->sample_count);
}

static VALUE rb_lng_language_token_count(VALUE self)
{
	struct training_lang *lang = NULL;
	Data_Get_Struct(self, struct training_lang, lang);
	return rb_float_new((double)lang->token_count);
}

static VALUE rb_lng_language_token_prob(VALUE self, VALUE rb_token)
{
	uint8_t token_name[MAX_TOKEN_SIZE + 2];
	long token_len;
	JudySlot *slot;

	struct training_db *db;
	struct training_lang *lang;

	VALUE rb_database = rb_iv_get(self, "@db");

	Data_Get_Struct(self, struct training_lang, lang);
	Data_Get_Struct(rb_database, struct training_db, db);

	Check_Type(rb_token, T_STRING);
	token_len = RSTRING_LEN(rb_token);

	if (token_len > MAX_TOKEN_SIZE)
		return rb_float_new(0.0);

	memcpy(token_name, lang->id, 2);
	memcpy(token_name + 2, RSTRING_PTR(rb_token), token_len);

	slot = judy_slot(db->tokens, token_name, token_len + 2);
	if (!slot)
		return rb_float_new(0.0);

	return rb_float_new((double)(*slot));
}

static VALUE rb_lng_language_tokens(VALUE self)
{
	JudySlot *slot;

	struct training_db *db;
	struct training_lang *lang;

	VALUE rb_database = rb_iv_get(self, "@db");

	Data_Get_Struct(self, struct training_lang, lang);
	Data_Get_Struct(rb_database, struct training_db, db);

	slot = judy_strt(db->tokens, lang->id, 2);

	while (slot != NULL) {
		uint8_t token_name[MAX_TOKEN_SIZE + 2];
		uint32_t token_len = judy_key(db->tokens, token_name, MAX_TOKEN_SIZE + 2);

		if (token_name[0] != lang->id[0] || token_name[1] != lang->id[1])
			break;

		rb_yield_values(2, rb_str_new(token_name + 2, token_len - 2), LONG2NUM(*slot));
		slot = judy_nxt(db->tokens);
	}

	return Qnil;
}

void Init_cclassifier()
{
	VALUE rb_mLinguist = rb_const_get(rb_cObject, rb_intern("Linguist"));
	VALUE rb_mClassifier = rb_const_get(rb_mLinguist, rb_intern("Classifier"));

	rb_cLinguistDB = rb_define_class_under(rb_mClassifier, "TrainingDB", rb_cObject);
	rb_define_singleton_method(rb_cLinguistDB, "new", rb_lng_trainingdb_new, 1);

	rb_define_method(rb_cLinguistDB, "train!", rb_lng_trainingdb_train, 2);
	rb_define_method(rb_cLinguistDB, "[]", rb_lng_trainingdb_get_lang, 1);
	rb_define_method(rb_cLinguistDB, "language_names", rb_lng_trainingdb_lang_names, 0);
	rb_define_method(rb_cLinguistDB, "samples_total", rb_lng_trainingdb_samples_total, 0);
	rb_define_method(rb_cLinguistDB, "tokens_total", rb_lng_trainingdb_tokens_total, 0);

	rb_cLanguage = rb_define_class_under(rb_cLinguistDB, "Language", rb_cObject);
	rb_define_method(rb_cLanguage, "name", rb_lng_language_name, 0);
	rb_define_method(rb_cLanguage, "sample_count", rb_lng_language_sample_count, 0);
	rb_define_method(rb_cLanguage, "token_count", rb_lng_language_token_count, 0);
	rb_define_method(rb_cLanguage, "token_probability", rb_lng_language_token_prob, 1);
	rb_define_method(rb_cLanguage, "each_token", rb_lng_language_tokens, 0);
}

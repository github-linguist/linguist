/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

const char *RUGGED_ERROR_NAMES[] = {
	"None",            /* GIT_ERROR_NONE */
	"NoMemError",      /* GIT_ERROR_NOMEMORY */
	"OSError",         /* GIT_ERROR_OS */
	"InvalidError",    /* GIT_ERROR_INVALID */
	"ReferenceError",  /* GIT_ERROR_REFERENCE */
	"ZlibError",       /* GIT_ERROR_ZLIB */
	"RepositoryError", /* GIT_ERROR_REPOSITORY */
	"ConfigError",     /* GIT_ERROR_CONFIG */
	"RegexError",      /* GIT_ERROR_REGEX */
	"OdbError",        /* GIT_ERROR_ODB */
	"IndexError",      /* GIT_ERROR_INDEX */
	"ObjectError",     /* GIT_ERROR_OBJECT */
	"NetworkError",    /* GIT_ERROR_NET */
	"TagError",        /* GIT_ERROR_TAG */
	"TreeError",       /* GIT_ERROR_TREE */
	"IndexerError",    /* GIT_ERROR_INDEXER */
	"SslError",        /* GIT_ERROR_SSL */
	"SubmoduleError",  /* GIT_ERROR_SUBMODULE */
	"ThreadError",     /* GIT_ERROR_THREAD */
	"StashError",      /* GIT_ERROR_STASH */
	"CheckoutError",   /* GIT_ERROR_CHECKOUT */
	"FetchheadError",  /* GIT_ERROR_FETCHHEAD */
	"MergeError",      /* GIT_ERROR_MERGE */
	"SshError",        /* GIT_ERROR_SSH */
	"FilterError",     /* GIT_ERROR_FILTER */
	"RevertError",     /* GIT_ERROR_REVERT */
	"CallbackError",   /* GIT_ERROR_CALLBACK */
	"CherrypickError", /* GIT_ERROR_CHERRYPICK */
	"DescribeError",   /* GIT_ERROR_DESCRIBE */
	"RebaseError",     /* GIT_ERROR_REBASE */
	"FilesystemError", /* GIT_ERROR_FILESYSTEM */
	"PathError",       /* GIT_ERROR_PATCH */
	"WorktreeError",   /* GIT_ERROR_WORKTREE */
	"SHA1Error",       /* GIT_ERROR_SHA1 */
	"HTTPError",       /* GIT_ERROR_HTTP */
};

#define RUGGED_ERROR_COUNT (int)((sizeof(RUGGED_ERROR_NAMES)/sizeof(RUGGED_ERROR_NAMES[0])))

VALUE rb_mRugged;
VALUE rb_eRuggedError;
VALUE rb_eRuggedErrors[RUGGED_ERROR_COUNT];

static VALUE rb_mShutdownHook;

/*
 *  call-seq:
 *     Rugged.libgit2_version -> version
 *
 *  Returns an array representing the current libgit2 version in use. Using
 *  the array makes it easier for the end-user to take conditional actions
 *  based on each respective version attribute: major, minor, rev.
 *
 *    Rugged.libgit2_version #=> [0, 17, 0]
 */
static VALUE rb_git_libgit2_version(VALUE self)
{
	int major;
	int minor;
	int rev;

	git_libgit2_version(&major, &minor, &rev);

	// We return an array of three elements to represent the version components
	return rb_ary_new3(3, INT2NUM(major), INT2NUM(minor), INT2NUM(rev));
}

/*
 *  call-seq:
 *     Rugged.libgit2_prerelease -> prerelease string
 *
 *  Returns a string with the prerelease string for libgit2. This will be empty
 *  for tagged releases.
 */
static VALUE rb_git_libgit2_prerelease(VALUE self)
{
	const char *prerelease;

	prerelease = git_libgit2_prerelease();
	return rb_str_new_utf8(prerelease ? prerelease : "");
}

/*
 *  call-seq:
 *     Rugged.features -> [feature, ...]
 *
 *  Returns an array representing the features that libgit2 was compiled
 *  with — this includes `:threads` (thread support), `:https` and `:ssh`.
 *
 *    Rugged.features #=> [:threads, :https]
 */
static VALUE rb_git_features(VALUE self)
{
	VALUE ret_arr = rb_ary_new();

	int caps = git_libgit2_features();

	if (caps & GIT_FEATURE_THREADS)
		rb_ary_push(ret_arr, CSTR2SYM("threads"));

	if (caps & GIT_FEATURE_HTTPS)
		rb_ary_push(ret_arr, CSTR2SYM("https"));

	if (caps & GIT_FEATURE_SSH)
		rb_ary_push(ret_arr, CSTR2SYM("ssh"));

	return ret_arr;
}

/*
 *  call-seq:
 *    Rugged.valid_full_oid?(oid) -> true or false
 *
 *  Checks to see if a string contains a full 40-character sha1.
 *
 *    Rugged.valid_full_oid?('d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f')
 *    #=> true
 */
static VALUE rb_git_valid_full_oid(VALUE self, VALUE hex)
{
	git_oid oid;
	int errorcode;

	Check_Type(hex, T_STRING);
	errorcode = git_oid_fromstr(&oid, StringValueCStr(hex));
	if (errorcode < 0) {
		return Qfalse;
	} else {
		return Qtrue;
	}
}

/*
 *  call-seq:
 *    Rugged.hex_to_raw(oid) -> raw_buffer
 *
 *  Turn a string of 40 hexadecimal characters into the buffer of
 *  20 bytes it represents.
 *
 *    Rugged.hex_to_raw('d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f')
 *    #=> "\330xk\374\227H^\215{\031\262\037\270\214\216\361\361\231\374?"
 */
static VALUE rb_git_hex_to_raw(VALUE self, VALUE hex)
{
	git_oid oid;

	Check_Type(hex, T_STRING);
	rugged_exception_check(git_oid_fromstr(&oid, StringValueCStr(hex)));

	return rb_str_new((const char *)oid.id, 20);
}

/*
 *  call-seq:
 *    Rugged.raw_to_hex(buffer) -> hex_oid
 *
 *  Turn a buffer of 20 bytes (representing a SHA1 OID) into its
 *  readable hexadecimal representation.
 *
 *    Rugged.raw_to_hex("\330xk\374\227H^\215{\031\262\037\270\214\216\361\361\231\374?")
 *    #=> "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f"
 */
static VALUE rb_git_raw_to_hex(VALUE self, VALUE raw)
{
	git_oid oid;
	char out[40];

	Check_Type(raw, T_STRING);

	if (RSTRING_LEN(raw) != GIT_OID_RAWSZ)
		rb_raise(rb_eTypeError, "Invalid buffer size for an OID");

	git_oid_fromraw(&oid, (const unsigned char *)RSTRING_PTR(raw));
	git_oid_fmt(out, &oid);

	return rb_usascii_str_new(out, 40);
}

/*
 *  call-seq:
 *    Rugged.prettify_message(message, strip_comments = '#') -> clean_message
 *
 *  Process a commit or tag message into standard form, by stripping trailing spaces and
 *  comments, and making sure that the message has a proper header line.
 */
static VALUE rb_git_prettify_message(int argc, VALUE *argv, VALUE self)
{
	char comment_char = '#';
	int strip_comments = 1;

	git_buf message = { NULL };
	VALUE rb_message, rb_strip;
	int error;
	VALUE result = Qnil;

	rb_scan_args(argc, argv, "11", &rb_message, &rb_strip);

	Check_Type(rb_message, T_STRING);

	switch (TYPE(rb_strip)) {
	case T_FALSE:
		strip_comments = 0;
		break;

	case T_STRING:
		if (RSTRING_LEN(rb_strip) > 0)
			comment_char = RSTRING_PTR(rb_strip)[0];
		break;

	case T_TRUE:
	case T_NIL:
	default:
		break;
	}

	error = git_message_prettify(&message,
				StringValueCStr(rb_message), strip_comments, comment_char);

	if (!error)
		result = rb_enc_str_new(message.ptr, message.size, rb_utf8_encoding());

	git_buf_dispose(&message);
	rugged_exception_check(error);

	return result;
}

static VALUE minimize_cb(RB_BLOCK_CALL_FUNC_ARGLIST(rb_oid, shorten))
{
	git_oid_shorten *shortener = (git_oid_shorten*) shorten;

	Check_Type(rb_oid, T_STRING);
	git_oid_shorten_add(shortener, RSTRING_PTR(rb_oid));
	return Qnil;
}

static VALUE minimize_yield(RB_BLOCK_CALL_FUNC_ARGLIST(rb_oid, args))
{
	VALUE *data = (VALUE*) args;
	rb_funcall(data[0], rb_intern("call"), 1,
		rb_str_substr(rb_oid, 0, FIX2INT(data[1])));
	return Qnil;
}

/*
 *  call-seq:
 *    Rugged.minimize_oid(oid_iterator, min_length = 7) { |short_oid| block }
 *    Rugged.minimize_oid(oid_iterator, min_length = 7) -> min_length
 *
 *  Iterate through +oid_iterator+, which should yield any number of SHA1 OIDs
 *  (represented as 40-character hexadecimal strings), and tries to minify them.
 *
 *  Minifying a set of a SHA1 strings means finding the shortest root substring
 *  for each string that uniquely identifies it.
 *
 *  If no +block+ is given, the function will return the minimal length as an
 *  integer value:
 *
 *    oids = [
 *      'd8786bfc974aaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
 *      'd8786bfc974bbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
 *      'd8786bfc974ccccccccccccccccccccccccccccc',
 *      '68d041ee999cb07c6496fbdd4f384095de6ca9e1',
 *    ]
 *
 *    Rugged.minimize_oids(oids) #=> 12
 *
 *  If a +block+ is given, it will be called with each OID from +iterator+
 *  in its minified form:
 *
 *    Rugged.minimize_oid(oids) { |oid| puts oid }
 *
 *  produces:
 *
 *    d8786bfc974a
 *    d8786bfc974b
 *    d8786bfc974c
 *    68d041ee999c
 *
 *  The optional +min_length+ argument allows you to specify a lower bound for
 *  the minified strings; returned strings won't be shorter than the given value,
 *  even if they would still be uniquely represented.
 *
 *    Rugged.minimize_oid(oids, 18) #=> 18
 */
static VALUE rb_git_minimize_oid(int argc, VALUE *argv, VALUE self)
{
	git_oid_shorten *shrt;
	int length, minlen = 7;
	VALUE rb_enum, rb_minlen, rb_block;

	rb_scan_args(argc, argv, "11&", &rb_enum, &rb_minlen, &rb_block);

	if (!NIL_P(rb_minlen)) {
		Check_Type(rb_minlen, T_FIXNUM);
		minlen = FIX2INT(rb_minlen);
	}

	if (!rb_respond_to(rb_enum, rb_intern("each")))
		rb_raise(rb_eTypeError, "Expecting an Enumerable instance");

	shrt = git_oid_shorten_new(minlen);

	rb_block_call(rb_enum, rb_intern("each"), 0, NULL, minimize_cb, (VALUE)shrt);
	length = git_oid_shorten_add(shrt, NULL);

	git_oid_shorten_free(shrt);
	rugged_exception_check(length);

	if (!NIL_P(rb_block)) {
		VALUE yield_data[2];

		yield_data[0] = rb_block;
		yield_data[1] = INT2FIX(length);

		rb_block_call(rb_enum, rb_intern("each"), 0, NULL, minimize_yield, (VALUE)yield_data);
		return Qnil;
	}

	return INT2FIX(length);
}

static void cleanup_cb(void *unused)
{
	(void)unused;
	git_libgit2_shutdown();
}

void rugged_exception_raise(void)
{
	VALUE err_klass, err_obj;
	const git_error *error;
	const char *err_message;

	error = giterr_last();

	if (error && error->klass > 0 && error->klass < RUGGED_ERROR_COUNT) {
		err_klass = rb_eRuggedErrors[error->klass];
		err_message = error->message;
	} else {
		err_klass = rb_eRuntimeError;
		err_message = "Rugged operation failed";
	}

	err_obj = rb_exc_new2(err_klass, err_message);
	giterr_clear();
	rb_exc_raise(err_obj);
}

VALUE rugged__block_yield_splat(VALUE args) {
	VALUE block = rb_ary_shift(args);
	int n = RARRAY_LENINT(args);

	if (n == 0) {
		return rb_funcall(block, rb_intern("call"), 0);
	} else {
		int i;
		VALUE *argv;
		argv = ALLOCA_N(VALUE, n);

		for (i=0; i < n; i++) {
			argv[i] = rb_ary_entry(args, i);
		}

		return rb_funcall2(block, rb_intern("call"), n, argv);
	}
}

/*
 *  call-seq:
 *    Rugged.__cache_usage__ -> [current, max]
 *
 *  Returns an array representing the current bytes in the internal
 *  libgit2 cache and the maximum size of the cache.
 */
static VALUE rb_git_cache_usage(VALUE self)
{
	int64_t used, max;
	git_libgit2_opts(GIT_OPT_GET_CACHED_MEMORY, &used, &max);
	return rb_ary_new3(2, LL2NUM(used), LL2NUM(max));
}

/*
 *  call-seq:
 *    Rugged.signature_from_buffer(buffer[, encoding_name]) -> signature
 *
 * Parse the signature from the given buffer. If an encoding is given, the
 * strings will be tagged with that encoding.
 *
 *    commit.author #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_signature_from_buffer(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_buffer, rb_encoding_name;
	const char *buffer, *encoding_name = NULL;

	rb_scan_args(argc, argv, "11", &rb_buffer, &rb_encoding_name);

	buffer = StringValueCStr(rb_buffer);
	if (!NIL_P(rb_encoding_name))
		encoding_name = StringValueCStr(rb_encoding_name);

	return rugged_signature_from_buffer(buffer, encoding_name);
}

VALUE rugged_strarray_to_rb_ary(git_strarray *str_array)
{
	VALUE rb_array = rb_ary_new2(str_array->count);
	size_t i;

	for (i = 0; i < str_array->count; ++i) {
		rb_ary_push(rb_array, rb_str_new_utf8(str_array->strings[i]));
	}

	return rb_array;
}

void rugged_rb_ary_to_strarray(VALUE rb_array, git_strarray *str_array)
{
	int i;

	str_array->strings = NULL;
	str_array->count = 0;

	if (NIL_P(rb_array))
		return;

	if (TYPE(rb_array) == T_STRING) {
		str_array->count = 1;
		str_array->strings = xmalloc(sizeof(char *));
		str_array->strings[0] = StringValueCStr(rb_array);
		return;
	}

	Check_Type(rb_array, T_ARRAY);

	for (i = 0; i < RARRAY_LEN(rb_array); ++i)
		Check_Type(rb_ary_entry(rb_array, i), T_STRING);

	str_array->count = RARRAY_LEN(rb_array);
	str_array->strings = xmalloc(str_array->count * sizeof(char *));

	for (i = 0; i < RARRAY_LEN(rb_array); ++i) {
		VALUE rb_string = rb_ary_entry(rb_array, i);
		str_array->strings[i] = StringValueCStr(rb_string);
	}
}

void rugged_parse_merge_file_options(git_merge_file_options *opts, VALUE rb_options)
{
	VALUE rb_value;

	Check_Type(rb_options, T_HASH);

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("ancestor_label"));
	if (!NIL_P(rb_value)) {
		Check_Type(rb_value, T_STRING);
		opts->ancestor_label = StringValueCStr(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("our_label"));
	if (!NIL_P(rb_value)) {
		Check_Type(rb_value, T_STRING);
		opts->our_label = StringValueCStr(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("their_label"));
	if (!NIL_P(rb_value)) {
		Check_Type(rb_value, T_STRING);
		opts->their_label = StringValueCStr(rb_value);
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("favor"));
	if (!NIL_P(rb_value)) {
		ID id_favor;

		Check_Type(rb_value, T_SYMBOL);
		id_favor = SYM2ID(rb_value);

		if (id_favor == rb_intern("normal")) {
			opts->favor = GIT_MERGE_FILE_FAVOR_NORMAL;
		} else if (id_favor == rb_intern("ours")) {
			opts->favor = GIT_MERGE_FILE_FAVOR_OURS;
		} else if (id_favor == rb_intern("theirs")) {
			opts->favor = GIT_MERGE_FILE_FAVOR_THEIRS;
		} else if (id_favor == rb_intern("union")) {
			opts->favor = GIT_MERGE_FILE_FAVOR_UNION;
		} else {
			rb_raise(rb_eTypeError,
				"Invalid favor mode. Expected `:normal`, `:ours`, `:theirs` or `:union`");
		}
	}

	rb_value = rb_hash_aref(rb_options, CSTR2SYM("style"));
	if (!NIL_P(rb_value)) {
		ID id_style;

		Check_Type(rb_value, T_SYMBOL);
		id_style = SYM2ID(rb_value);

		if (id_style == rb_intern("standard")) {
			opts->flags |= GIT_MERGE_FILE_STYLE_MERGE;
		} else if (id_style == rb_intern("diff3")) {
			opts->flags |= GIT_MERGE_FILE_STYLE_DIFF3;
		} else {
			rb_raise(rb_eTypeError,
				"Invalid style mode. Expected `:standard`, or `:diff3`");
		}
	} else {
		opts->flags |= GIT_MERGE_FILE_STYLE_MERGE;
	}

	if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("simplify")))) {
		opts->flags |= GIT_MERGE_FILE_SIMPLIFY_ALNUM;
	}
}

VALUE rb_merge_file_result_fromC(const git_merge_file_result *result)
{
	VALUE rb_result = rb_hash_new();

	rb_hash_aset(rb_result, CSTR2SYM("automergeable"), result->automergeable ? Qtrue : Qfalse);
	rb_hash_aset(rb_result, CSTR2SYM("path"),          result->path ? rb_str_new_utf8(result->path) : Qnil);
	rb_hash_aset(rb_result, CSTR2SYM("filemode"),      INT2FIX(result->mode));
	rb_hash_aset(rb_result, CSTR2SYM("data"),          rb_str_new(result->ptr, result->len));

	return rb_result;
}

static VALUE rb_git_path_is_dotgit_modules(VALUE self, VALUE rb_path)
{
	const char *path;
	int is_dotgit;

	Check_Type(rb_path, T_STRING);

	path = StringValueCStr(rb_path);

	is_dotgit = git_path_is_gitfile(path, strlen(path), GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_GENERIC);

	return is_dotgit ? Qtrue : Qfalse;
}

static VALUE rb_git_path_is_dotgit_ignore(VALUE self, VALUE rb_path)
{
	const char *path;
	int is_dotgit;

	Check_Type(rb_path, T_STRING);

	path = StringValueCStr(rb_path);

	is_dotgit = git_path_is_gitfile(path, strlen(path), GIT_PATH_GITFILE_GITIGNORE, GIT_PATH_FS_GENERIC);

	return is_dotgit ? Qtrue : Qfalse;
}

static VALUE rb_git_path_is_dotgit_attributes(VALUE self, VALUE rb_path)
{
	const char *path;
	int is_dotgit;

	Check_Type(rb_path, T_STRING);

	path = StringValueCStr(rb_path);

	is_dotgit = git_path_is_gitfile(path, strlen(path), GIT_PATH_GITFILE_GITATTRIBUTES, GIT_PATH_FS_GENERIC);

	return is_dotgit ? Qtrue : Qfalse;
}

void Init_rugged(void)
{
	rb_mRugged = rb_define_module("Rugged");

	/* Initialize the Error classes */
	{
		int i;

		rb_eRuggedError = rb_define_class_under(rb_mRugged, "Error", rb_eStandardError);

		rb_eRuggedErrors[0] = Qnil; /* 0 return value -- no exception */
		rb_eRuggedErrors[1] = rb_define_class_under(rb_mRugged, RUGGED_ERROR_NAMES[1], rb_eNoMemError);
		rb_eRuggedErrors[2] = rb_define_class_under(rb_mRugged, RUGGED_ERROR_NAMES[2], rb_eIOError);
		rb_eRuggedErrors[3] = rb_define_class_under(rb_mRugged, RUGGED_ERROR_NAMES[3], rb_eArgError);

		for (i = 4; i < RUGGED_ERROR_COUNT; ++i) {
			rb_eRuggedErrors[i] = rb_define_class_under(rb_mRugged, RUGGED_ERROR_NAMES[i], rb_eRuggedError);
		}
	}

	rb_define_module_function(rb_mRugged, "libgit2_version", rb_git_libgit2_version, 0);
	rb_define_module_function(rb_mRugged, "libgit2_prerelease", rb_git_libgit2_prerelease, 0);
	rb_define_module_function(rb_mRugged, "features", rb_git_features, 0);
	rb_define_module_function(rb_mRugged, "valid_full_oid?", rb_git_valid_full_oid, 1);
	rb_define_module_function(rb_mRugged, "hex_to_raw", rb_git_hex_to_raw, 1);
	rb_define_module_function(rb_mRugged, "raw_to_hex", rb_git_raw_to_hex, 1);
	rb_define_module_function(rb_mRugged, "minimize_oid", rb_git_minimize_oid, -1);
	rb_define_module_function(rb_mRugged, "prettify_message", rb_git_prettify_message, -1);
	rb_define_module_function(rb_mRugged, "__cache_usage__", rb_git_cache_usage, 0);
	rb_define_module_function(rb_mRugged, "signature_from_buffer", rb_git_signature_from_buffer, -1);
	rb_define_module_function(rb_mRugged, "dotgit_modules?", rb_git_path_is_dotgit_modules, 1);
	rb_define_module_function(rb_mRugged, "dotgit_ignore?", rb_git_path_is_dotgit_ignore, 1);
	rb_define_module_function(rb_mRugged, "dotgit_attributes?", rb_git_path_is_dotgit_attributes, 1);

	Init_rugged_reference();
	Init_rugged_reference_collection();

	Init_rugged_object();
	Init_rugged_commit();
	Init_rugged_tree();
	Init_rugged_tag();
	Init_rugged_tag_collection();
	Init_rugged_blob();

	Init_rugged_index();
	Init_rugged_repo();
	Init_rugged_revwalk();
	Init_rugged_branch();
	Init_rugged_branch_collection();
	Init_rugged_config();
	Init_rugged_remote();
	Init_rugged_remote_collection();
	Init_rugged_notes();
	Init_rugged_settings();
	Init_rugged_submodule();
	Init_rugged_submodule_collection();
	Init_rugged_diff();
	Init_rugged_patch();
	Init_rugged_diff_delta();
	Init_rugged_diff_hunk();
	Init_rugged_diff_line();
	Init_rugged_blame();
	Init_rugged_cred();
	Init_rugged_backend();
	Init_rugged_rebase();

	/*
	 * Sort the output with the same default time-order method from git.
	 * This is the default sorting for new walkers.
	 */
	rb_define_const(rb_mRugged, "SORT_NONE", INT2FIX(GIT_SORT_NONE));

	/*
	 * Sort the repository contents in topological order (parents before
	 * children); this sorting mode can be combined with time sorting to
	 * produce git's "time-order".
	 */
	rb_define_const(rb_mRugged, "SORT_TOPO", INT2FIX(GIT_SORT_TOPOLOGICAL));

	/*
	 * Sort the repository contents by commit time;
	 * this sorting mode can be combined with
	 * topological sorting.
	 */
	rb_define_const(rb_mRugged, "SORT_DATE", INT2FIX(GIT_SORT_TIME));

	/*
	 * Iterate through the repository contents in reverse
	 * order; this sorting mode can be combined with
	 * any of the above.
	 */
	rb_define_const(rb_mRugged, "SORT_REVERSE", INT2FIX(GIT_SORT_REVERSE));

	/* Set the allocator and initialize libgit2 */
	rugged_set_allocator();
	git_libgit2_init();

	/* Hook a global object to cleanup the library
	 * on shutdown */
	rb_mShutdownHook = Data_Wrap_Struct(rb_cObject, NULL, &cleanup_cb, NULL);
	rb_global_variable(&rb_mShutdownHook);
}

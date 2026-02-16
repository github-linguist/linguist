/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
VALUE rb_cRuggedConfig;

void rb_git_config__free(git_config *config)
{
	git_config_free(config);
}

VALUE rugged_config_new(VALUE klass, VALUE owner, git_config *cfg)
{
	VALUE rb_config = Data_Wrap_Struct(klass, NULL, &rb_git_config__free, cfg);
	rugged_set_owner(rb_config, owner);
	return rb_config;
}

/*
 *  call-seq:
 *    Config.new(path) -> new_config
 *
 *  Open the file specified in +path+ as a +Rugged::Config+ file.
 *  If +path+ cannot be found, or the file is an invalid Git config,
 *  an exception will be raised.
 */
static VALUE rb_git_config_new(VALUE klass, VALUE rb_path)
{
	git_config *config = NULL;

	if (TYPE(rb_path) == T_ARRAY) {
		int error, i;

		error = git_config_new(&config);
		rugged_exception_check(error);

		for (i = 0; i < RARRAY_LEN(rb_path) && !error; ++i) {
			VALUE f = rb_ary_entry(rb_path, i);
			Check_Type(f, T_STRING);
			error = git_config_add_file_ondisk(config, StringValueCStr(f), i + 1, NULL, 1);
		}

		if (error) {
			git_config_free(config);
			rugged_exception_check(error);
		}
	} else if (TYPE(rb_path) == T_STRING) {
		rugged_exception_check(
			git_config_open_ondisk(&config, StringValueCStr(rb_path))
		);
	} else {
		rb_raise(rb_eTypeError, "Expecting a filename or an array of filenames");
	}

	return rugged_config_new(klass, Qnil, config);
}

/*
 *  call-seq:
 *    cfg.get(key) -> value
 *    cfg[key] -> value
 *
 *  Get the value for the given config +key+. Values are always
 *  returned as +String+, or +nil+ if the given key doesn't exist
 *  in the Config file.
 *
 *    cfg['apply.whitespace'] #=> 'fix'
 *    cfg['diff.renames'] #=> 'true'
 */
static VALUE rb_git_config_get(VALUE self, VALUE rb_key)
{
	git_config *config;
	git_buf buf = { NULL };
	int error;
	VALUE rb_result;

	Data_Get_Struct(self, git_config, config);
	Check_Type(rb_key, T_STRING);

	error = git_config_get_string_buf(&buf, config, StringValueCStr(rb_key));
	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);
	rb_result = rb_str_new_utf8(buf.ptr);
	git_buf_dispose(&buf);

	return rb_result;
}

/*
 *  call-seq:
 *    cfg.store(key, value)
 *    cfg[key] = value
 *
 *  Store the given +value+ in the Config file, under the section
 *  and name specified by +key+. Value can be any of the following
 *  Ruby types: +String+, +true+, +false+ and +Fixnum+.
 *
 *  The config file will be automatically stored to disk.
 *
 *    cfg['apply.whitespace'] = 'fix'
 *    cfg['diff.renames'] = true
 *    cfg['gc.reflogexpre'] = 90
 */
static VALUE rb_git_config_store(VALUE self, VALUE rb_key, VALUE rb_val)
{
	git_config *config;
	const char *key;
	int error;

	Data_Get_Struct(self, git_config, config);
	Check_Type(rb_key, T_STRING);

	key = StringValueCStr(rb_key);

	switch (TYPE(rb_val)) {
	case T_STRING:
		error = git_config_set_string(config, key, StringValueCStr(rb_val));
		break;

	case T_TRUE:
	case T_FALSE:
		error = git_config_set_bool(config, key, (rb_val == Qtrue));
		break;

	case T_FIXNUM:
		error = git_config_set_int32(config, key, FIX2INT(rb_val));
		break;

	default:
		rb_raise(rb_eTypeError,
			"Invalid value; config files can only store string, bool or int keys");
	}

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    cfg.delete(key) -> true or false
 *
 *  Delete the given +key+ from the config file. Return +true+ if
 *  the deletion was successful, or +false+ if the key was not
 *  found in the Config file.
 *
 *  The config file is immediately updated on disk.
 */
static VALUE rb_git_config_delete(VALUE self, VALUE rb_key)
{
	git_config *config;
	int error;

	Data_Get_Struct(self, git_config, config);
	Check_Type(rb_key, T_STRING);

	error = git_config_delete_entry(config, StringValueCStr(rb_key));
	if (error == GIT_ENOTFOUND)
		return Qfalse;

	rugged_exception_check(error);
	return Qtrue;
}

static int cb_config__each_key(const git_config_entry *entry, void *payload)
{
	int *exception = (int *) payload;

	rb_protect(rb_yield, rb_str_new_utf8(entry->name), exception);

	return (*exception != 0) ? GIT_EUSER : GIT_OK;
}

static int cb_config__each_pair(const git_config_entry *entry, void *payload)
{
	int *exception = (int *) payload;
	VALUE value;

	value = entry->value ? rb_str_new_utf8(entry->value) : Qnil;
	rb_protect(rb_yield, rb_ary_new3(2, rb_str_new_utf8(entry->name), value), exception);

	return (*exception != 0) ? GIT_EUSER : GIT_OK;
}

static int cb_config__to_hash(const git_config_entry *entry, void *opaque)
{
	VALUE value;

	value = entry->value ? rb_str_new_utf8(entry->value) : Qnil;
	rb_hash_aset((VALUE)opaque,
		rb_str_new_utf8(entry->name),
		value
	);

	return GIT_OK;
}

/*
 *  call-seq:
 *    cfg.each_key { |key| block }
 *    cfg.each_key -> enumerator
 *
 *  Call the given block once for each key in the config file. If no block
 *  is given, an enumerator is returned.
 *
 *    cfg.each_key do |key|
 *      puts key
 *    end
 */
static VALUE rb_git_config_each_key(VALUE self)
{
	git_config *config;
	int error, exception;

	RETURN_ENUMERATOR(self, 0, 0);
	Data_Get_Struct(self, git_config, config);

	error = git_config_foreach(config, &cb_config__each_key, &exception);
	if (error == GIT_EUSER)
		rb_jump_tag(exception);

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    cfg.each_pair { |key, value| block }
 *    cfg.each_pair -> enumerator
 *    cfg.each { |key, value| block }
 *    cfg.each -> enumerator
 *
 *  Call the given block once for each key/value pair in the config file.
 *  If no block is given, an enumerator is returned.
 *
 *    cfg.each do |key, value|
 *      puts "#{key} => #{value}"
 *    end
 */
static VALUE rb_git_config_each_pair(VALUE self)
{
	git_config *config;
	int error, exception;

	RETURN_ENUMERATOR(self, 0, 0);
	Data_Get_Struct(self, git_config, config);

	error = git_config_foreach(config, &cb_config__each_pair, &exception);
	if (error == GIT_EUSER)
		rb_jump_tag(exception);

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    cfg.to_hash -> hash
 *
 *  Returns the config file represented as a Ruby hash, where
 *  each configuration entry appears as a key with its
 *  corresponding value.
 *
 *    cfg.to_hash #=> {"core.autolf" => "true", "core.bare" => "true"}
 */
static VALUE rb_git_config_to_hash(VALUE self)
{
	git_config *config;
	int error;
	VALUE hash;

	Data_Get_Struct(self, git_config, config);
	hash = rb_hash_new();

	error = git_config_foreach(config, &cb_config__to_hash, (void *)hash);
	rugged_exception_check(error);
	return hash;
}

/*
 *  call-seq:
 *    Config.global() -> new_config
 *    Config.open_global() -> new_config
 *
 *  Open the default global config file as a new +Rugged::Config+ object.
 *  An exception will be raised if the global config file doesn't
 *  exist.
 */
static VALUE rb_git_config_open_default(VALUE klass)
{
	git_config *cfg;
	int error;

	error = git_config_open_default(&cfg);
	rugged_exception_check(error);

	return rugged_config_new(klass, Qnil, cfg);
}

/*
 *  call-seq:
 *    config.snapshot -> snapshot
 *
 *  Create a snapshot of the configuration.
 *
 *  Provides a consistent, read-only view of the configuration for
 *  looking up complex values from a configuration.
 */
static VALUE rb_git_config_snapshot(VALUE self)
{
	git_config *config, *snapshot;

	Data_Get_Struct(self, git_config, config);

	rugged_exception_check(
		git_config_snapshot(&snapshot, config)
	);

	return rugged_config_new(rb_obj_class(self), Qnil, snapshot);
}

/*
 *  call-seq:
 *    config.transaction { |config| }
 *
 *  Perform configuration changes in a transaction.
 *
 *  Locks the configuration, executes the given block and stores
 *  any changes that were made to the configuration. If the block
 *  throws an exception, all changes are rolled back automatically.
 *
 *  During the execution of the block, configuration changes don't
 *  get stored to disk immediately, so reading from the configuration
 *  will continue to return the values that were stored in the configuration
 *  when the transaction was started.
 */
static VALUE rb_git_config_transaction(VALUE self)
{
	git_config *config;
	git_transaction *tx;
	VALUE rb_result;
	int error = 0, exception = 0;

	Data_Get_Struct(self, git_config, config);

	git_config_lock(&tx, config);

	rb_result = rb_protect(rb_yield, self, &exception);

	if (!exception)
		error = git_transaction_commit(tx);

	git_transaction_free(tx);

	if (exception)
		rb_jump_tag(exception);
	else if (error)
		rugged_exception_check(error);

	return rb_result;
}

static int each_config_value(const git_config_entry * entry, void *ctx)
{
	VALUE list = (VALUE)ctx;
	rb_ary_push(list, rb_str_new_utf8(entry->value));
	return 0;
}

/*
 *  call-seq:
 *    cfg.get_all(key) -> [value1, value2, ...]
 *
 *  Get a list of values for the given config +key+. Values are always
 *  returned as an +Array+ of +String+, or +nil+ if the given key doesn't exist
 *  in the Config file.
 *
 *    cfg['apply.whitespace'] #=> ['fix']
 *    cfg['diff.renames'] #=> ['true']
 *    cfg['remote.origin.fetch'] #=> ["+refs/heads/*:refs/remotes/origin/*", "+refs/heads/*:refs/lolol/origin/*"]
 */
static VALUE rb_git_config_get_all(VALUE self, VALUE key)
{
	git_config *config;
	VALUE list;
	int error;

	Data_Get_Struct(self, git_config, config);

	list = rb_ary_new();
	error = git_config_get_multivar_foreach(
		config, StringValueCStr(key), NULL, each_config_value, (void *)list);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);
	return list;
}

void Init_rugged_config(void)
{
	/*
	 * Config
	 */
	rb_cRuggedConfig = rb_define_class_under(rb_mRugged, "Config", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedConfig);

	rb_define_singleton_method(rb_cRuggedConfig, "new", rb_git_config_new, 1);

	rb_define_singleton_method(rb_cRuggedConfig, "global", rb_git_config_open_default, 0);
	rb_define_singleton_method(rb_cRuggedConfig, "open_global", rb_git_config_open_default, 0);

	rb_define_method(rb_cRuggedConfig, "delete", rb_git_config_delete, 1);

	rb_define_method(rb_cRuggedConfig, "store", rb_git_config_store, 2);
	rb_define_method(rb_cRuggedConfig, "[]=", rb_git_config_store, 2);

	rb_define_method(rb_cRuggedConfig, "get", rb_git_config_get, 1);
	rb_define_method(rb_cRuggedConfig, "[]", rb_git_config_get, 1);
	rb_define_method(rb_cRuggedConfig, "get_all", rb_git_config_get_all, 1);

	rb_define_method(rb_cRuggedConfig, "each_key", rb_git_config_each_key, 0);
	rb_define_method(rb_cRuggedConfig, "each_pair", rb_git_config_each_pair, 0);
	rb_define_method(rb_cRuggedConfig, "each", rb_git_config_each_pair, 0);
	rb_define_method(rb_cRuggedConfig, "to_hash", rb_git_config_to_hash, 0);

	rb_define_method(rb_cRuggedConfig, "snapshot", rb_git_config_snapshot, 0);
	rb_define_method(rb_cRuggedConfig, "transaction", rb_git_config_transaction, 0);
}

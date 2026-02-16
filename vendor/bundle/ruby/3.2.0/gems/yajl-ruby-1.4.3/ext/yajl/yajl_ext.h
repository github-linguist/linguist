/*
 * Copyright (c) 2008-2011 Brian Lopez - http://github.com/brianmario
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include "api/yajl_parse.h"
#include "api/yajl_gen.h"

/* tell rbx not to use it's caching compat layer
   by doing this we're making a promize to RBX that
   we'll never modify the pointers we get back from RSTRING_PTR */
#define RSTRING_NOT_MODIFIED

#include <ruby.h>

#ifdef HAVE_RUBY_ENCODING_H
#include <ruby/encoding.h>
static rb_encoding *utf8Encoding;
#endif

#define READ_BUFSIZE 8192
#define WRITE_BUFSIZE 8192

/* Older versions of Ruby (< 1.8.6) need these */
#ifndef RSTRING_PTR
#define RSTRING_PTR(s) (RSTRING(s)->ptr)
#endif
#ifndef RSTRING_LEN
#define RSTRING_LEN(s) (RSTRING(s)->len)
#endif
#ifndef RARRAY_PTR
#define RARRAY_PTR(s) (RARRAY(s)->ptr)
#endif
#ifndef RARRAY_LEN
#define RARRAY_LEN(s) (RARRAY(s)->len)
#endif

static VALUE cStandardError, cParseError, cEncodeError, mYajl, cParser, cProjector, cEncoder;
static ID intern_io_read, intern_call, intern_keys, intern_to_s,
            intern_to_json, intern_has_key, intern_to_sym, intern_as_json;
static ID sym_allow_comments, sym_check_utf8, sym_pretty, sym_indent, sym_terminator, sym_symbolize_keys, sym_symbolize_names, sym_html_safe, sym_entities;

#define GetParser(obj, sval) Data_Get_Struct(obj, yajl_parser_wrapper, sval);
#define GetEncoder(obj, sval) Data_Get_Struct(obj, yajl_encoder_wrapper, sval);

static void yajl_check_and_fire_callback(void * ctx);
static void yajl_set_static_value(void * ctx, VALUE val);
static void yajl_encode_part(void * wrapper, VALUE obj, VALUE io);
static void yajl_parse_chunk(const unsigned char * chunk, unsigned int len, yajl_handle parser);

static int yajl_found_null(void * ctx);
static int yajl_found_boolean(void * ctx, int boolean);
static int yajl_found_number(void * ctx, const char * numberVal, unsigned int numberLen);
static int yajl_found_string(void * ctx, const unsigned char * stringVal, unsigned int stringLen);
static int yajl_found_hash_key(void * ctx, const unsigned char * stringVal, unsigned int stringLen);
static int yajl_found_start_hash(void * ctx);
static int yajl_found_end_hash(void * ctx);
static int yajl_found_start_array(void * ctx);
static int yajl_found_end_array(void * ctx);

static yajl_callbacks callbacks = {
    yajl_found_null,
    yajl_found_boolean,
    NULL,
    NULL,
    yajl_found_number,
    yajl_found_string,
    yajl_found_start_hash,
    yajl_found_hash_key,
    yajl_found_end_hash,
    yajl_found_start_array,
    yajl_found_end_array
};

typedef struct {
    VALUE builderStack;
    VALUE parse_complete_callback;
    int nestedArrayLevel;
    int nestedHashLevel;
    int objectsFound;
    int symbolizeKeys;
    yajl_handle parser;
} yajl_parser_wrapper;

typedef struct {
    VALUE on_progress_callback;
    VALUE terminator;
    yajl_gen encoder;
    unsigned char *indentString;
} yajl_encoder_wrapper;

static VALUE rb_yajl_parser_new(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_parser_init(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_parser_parse(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_parser_parse_chunk(VALUE self, VALUE chunk);
static VALUE rb_yajl_parser_set_complete_cb(VALUE self, VALUE callback);
static void yajl_parser_wrapper_free(void * wrapper);
static void yajl_parser_wrapper_mark(void * wrapper);

static VALUE rb_yajl_encoder_new(int argc, VALUE * argv, VALUE klass);
static VALUE rb_yajl_encoder_init(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_encoder_encode(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_encoder_set_progress_cb(VALUE self, VALUE callback);
static void yajl_encoder_wrapper_free(void * wrapper);
static void yajl_encoder_wrapper_mark(void * wrapper);

static VALUE rb_yajl_json_ext_hash_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_array_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_fixnum_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_float_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_string_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_true_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_false_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_json_ext_nil_to_json(int argc, VALUE * argv, VALUE self);
static VALUE rb_yajl_encoder_enable_json_gem_ext(VALUE klass);

void Init_yajl();

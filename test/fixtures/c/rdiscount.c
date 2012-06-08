#include <stdio.h>
#include "ruby.h"
#include "mkdio.h"

static VALUE rb_cRDiscount;

static VALUE
rb_rdiscount_to_html(int argc, VALUE *argv, VALUE self)
{
    /* grab char pointer to markdown input text */
    char *res;
    int szres;
    VALUE encoding;
    VALUE text = rb_funcall(self, rb_intern("text"), 0);
    VALUE buf = rb_str_buf_new(1024);
    Check_Type(text, T_STRING);

    int flags = rb_rdiscount__get_flags(self);

    MMIOT *doc = mkd_string(RSTRING_PTR(text), RSTRING_LEN(text), flags);

    if ( mkd_compile(doc, flags) ) {
        szres = mkd_document(doc, &res);

        if ( szres != EOF ) {
            rb_str_cat(buf, res, szres);
            rb_str_cat(buf, "\n", 1);
        }
    }
    mkd_cleanup(doc);


    /* force the input encoding */
    if ( rb_respond_to(text, rb_intern("encoding")) ) {
      encoding = rb_funcall(text, rb_intern("encoding"), 0);
      rb_funcall(buf, rb_intern("force_encoding"), 1, encoding);
    }

    return buf;
}

static VALUE
rb_rdiscount_toc_content(int argc, VALUE *argv, VALUE self)
{
    char *res;
    int szres;

    int flags = rb_rdiscount__get_flags(self);

    /* grab char pointer to markdown input text */
    VALUE text = rb_funcall(self, rb_intern("text"), 0);
    Check_Type(text, T_STRING);

    /* allocate a ruby string buffer and wrap it in a stream */
    VALUE buf = rb_str_buf_new(4096);

    MMIOT *doc = mkd_string(RSTRING_PTR(text), RSTRING_LEN(text), flags);

    if ( mkd_compile(doc, flags) ) {
        szres = mkd_toc(doc, &res);

        if ( szres != EOF ) {
            rb_str_cat(buf, res, szres);
            rb_str_cat(buf, "\n", 1);
        }
    }
    mkd_cleanup(doc);

    return buf;
}

int rb_rdiscount__get_flags(VALUE ruby_obj)
{
  /* compile flags */
  int flags = MKD_TABSTOP | MKD_NOHEADER;

  /* smart */
  if ( rb_funcall(ruby_obj, rb_intern("smart"), 0) != Qtrue )
      flags = flags | MKD_NOPANTS;

  /* filter_html */
  if ( rb_funcall(ruby_obj, rb_intern("filter_html"), 0) == Qtrue )
      flags = flags | MKD_NOHTML;

  /* generate_toc */
  if ( rb_funcall(ruby_obj, rb_intern("generate_toc"), 0) == Qtrue)
    flags = flags | MKD_TOC;

  /* no_image */
  if ( rb_funcall(ruby_obj, rb_intern("no_image"), 0) == Qtrue)
    flags = flags | MKD_NOIMAGE;

  /* no_links */
  if ( rb_funcall(ruby_obj, rb_intern("no_links"), 0) == Qtrue)
    flags = flags | MKD_NOLINKS;

  /* no_tables */
  if ( rb_funcall(ruby_obj, rb_intern("no_tables"), 0) == Qtrue)
    flags = flags | MKD_NOTABLES;

  /* strict */
  if ( rb_funcall(ruby_obj, rb_intern("strict"), 0) == Qtrue)
    flags = flags | MKD_STRICT;

  /* autolink */
  if ( rb_funcall(ruby_obj, rb_intern("autolink"), 0) == Qtrue)
    flags = flags | MKD_AUTOLINK;

  /* safelink */
  if ( rb_funcall(ruby_obj, rb_intern("safelink"), 0) == Qtrue)
    flags = flags | MKD_SAFELINK;

  /* no_pseudo_protocols */
  if ( rb_funcall(ruby_obj, rb_intern("no_pseudo_protocols"), 0) == Qtrue)
    flags = flags | MKD_NO_EXT;


  return flags;
}


void Init_rdiscount()
{
    rb_cRDiscount = rb_define_class("RDiscount", rb_cObject);
    rb_define_method(rb_cRDiscount, "to_html", rb_rdiscount_to_html, -1);
    rb_define_method(rb_cRDiscount, "toc_content", rb_rdiscount_toc_content, -1);
}

/* vim: set ts=4 sw=4: */

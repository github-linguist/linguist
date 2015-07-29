/*
 * This software is copyright (C) by Nick Wellnhofer <wellnhofer@aevum.de>.
 *
 * This is free software; you can redistribute it and/or modify it under
 * the same terms as the Perl 5 programming language system itself.
 *
 * Terms of the Perl programming language system itself
 *
 * a) the GNU General Public License as published by the Free
 *    Software Foundation; either version 1, or (at your option) any
 *    later version, or
 * b) the "Artistic License"
 */

/*
 * Notes on memory management
 *
 * - A pointer to the Perl SV representing a node is stored in the
 *   user data slot of `struct cmark_node`, so there's a 1:1 mapping
 *   between Perl and C objects.
 * - Every node SV keeps a reference to the parent SV. This is done
 *   indirectly by looking up the parent SV and increasing its refcount.
 * - This makes sure that a document isn't freed if the last reference
 *   from Perl to the root node is dropped, as references to child nodes
 *   might still exist.
 * - As a consequence, as long as a node is referenced from Perl, all its
 *   ancestor nodes will also be associated with a Perl object.
 */

#define PERL_NO_GET_CONTEXT

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <stdlib.h>
#include <cmark.h>

#if CMARK_VERSION < 0x001000
    #error libcmark 0.16.0 is required.
#endif

/* Fix prefixes of render functions. */
#define cmark_node_render_html cmark_render_html
#define cmark_node_render_xml  cmark_render_xml
#define cmark_node_render_man  cmark_render_man

static SV*
S_create_or_incref_node_sv(pTHX_ cmark_node *node) {
    SV *new_obj = NULL;

    while (node) {
        SV *obj;
        HV *stash;

        /* Look for existing object. */
        obj = (SV*)cmark_node_get_user_data(node);

        if (obj) {
            /* Incref if found. */
            SvREFCNT_inc_simple_void_NN(obj);
            if (!new_obj) {
                new_obj = obj;
            }
            break;
        }

        /* Create a new SV. */
        obj = newSViv(PTR2IV(node));
        cmark_node_set_user_data(node, obj);
        if (!new_obj) {
            new_obj = obj;
        }

        /*
         * Unfortunately, Perl doesn't offer an API function to bless an SV
         * without a reference. The following code is mostly copied from
         * sv_bless.
         */
        SvOBJECT_on(obj);
#if (PERL_VERSION <= 16)
        PL_sv_objcount++;
#endif
        SvUPGRADE(obj, SVt_PVMG);
        stash = gv_stashpvn("CommonMark::Node", 16, GV_ADD);
        SvSTASH_set(obj, (HV*)SvREFCNT_inc(stash));

        /* Recurse into parent. */
        node = cmark_node_parent(node);
    }

    return new_obj;
}

static void
S_decref_node_sv(pTHX_ cmark_node *node) {
    SV *obj;

    if (!node) {
        return;
    }

    obj = (SV*)cmark_node_get_user_data(node);
    if (!obj) {
        /* Should never happen. */
        croak("Internal error: node SV not found");
    }

    SvREFCNT_dec_NN(obj);
}

/* Find or create an SV for a cmark_node. */
static SV*
S_node2sv(pTHX_ cmark_node *node) {
    SV *obj;

    if (!node) {
        return &PL_sv_undef;
    }

    obj = S_create_or_incref_node_sv(aTHX_ node);

    return newRV_noinc(obj);
}

/* Transfer refcount from a node to another. */
static void
S_transfer_refcount(pTHX_ cmark_node *from, cmark_node *to) {
    if (from != to) {
        S_create_or_incref_node_sv(aTHX_ to);
        S_decref_node_sv(aTHX_ from);
    }
}

/* Get C struct pointer from an SV argument. */
static void*
S_sv2c(pTHX_ SV *sv, const char *class_name, STRLEN len, CV *cv,
       const char *var_name) {
    if (!SvROK(sv) || !sv_derived_from_pvn(sv, class_name, len, 0)) {
        const char *sub_name = GvNAME(CvGV(cv));
        croak("%s: %s is not of type %s", sub_name, var_name, class_name);
    }
    return INT2PTR(void*, SvIV(SvRV(sv)));
}


MODULE = CommonMark  PACKAGE = CommonMark  PREFIX = cmark_

PROTOTYPES: DISABLE

BOOT:
    if (cmark_version != CMARK_VERSION) {
        warn("Compiled against libcmark %s, but runtime version is %s",
             CMARK_VERSION_STRING, cmark_version_string);
    }

char*
cmark_markdown_to_html(package, string)
    SV *package = NO_INIT
    SV *string
PREINIT:
    STRLEN len;
    const char *buffer;
CODE:
    (void)package;
    buffer = SvPVutf8(string, len);
    RETVAL = cmark_markdown_to_html(buffer, len);
OUTPUT:
    RETVAL

cmark_node*
cmark_parse_document(package, string)
    SV *package = NO_INIT
    SV *string
PREINIT:
    STRLEN len;
    const char *buffer;
CODE:
    (void)package;
    buffer = SvPVutf8(string, len);
    RETVAL = cmark_parse_document(buffer, len);
OUTPUT:
    RETVAL

cmark_node*
cmark_parse_file(package, file)
    SV *package = NO_INIT
    SV *file
PREINIT:
    PerlIO *perl_io;
    FILE *stream = NULL;
CODE:
    (void)package;
    perl_io = IoIFP(sv_2io(file));
    if (perl_io) {
        stream = PerlIO_findFILE(perl_io);
    }
    if (!stream) {
        croak("parse_file: file is not a file handle");
    }
    RETVAL = cmark_parse_file(stream);
OUTPUT:
    RETVAL

int
cmark_version(package)
    SV *package = NO_INIT
CODE:
    (void)package;
    RETVAL = cmark_version;
OUTPUT:
    RETVAL

const char*
cmark_version_string(package)
    SV *package = NO_INIT
CODE:
    (void)package;
    RETVAL = cmark_version_string;
OUTPUT:
    RETVAL

int
cmark_compile_time_version(package)
    SV *package = NO_INIT
CODE:
    (void)package;
    RETVAL = CMARK_VERSION;
OUTPUT:
    RETVAL

const char*
cmark_compile_time_version_string(package)
    SV *package = NO_INIT
CODE:
    (void)package;
    RETVAL = CMARK_VERSION_STRING;
OUTPUT:
    RETVAL


MODULE = CommonMark  PACKAGE = CommonMark::Node  PREFIX = cmark_node_

cmark_node*
new(package, type)
    SV *package = NO_INIT
    cmark_node_type type
CODE:
    (void)package;
    RETVAL = cmark_node_new(type);
OUTPUT:
    RETVAL

void
DESTROY(cmark_node *node)
CODE:
    cmark_node *parent = cmark_node_parent(node);
    if (parent) {
        cmark_node_set_user_data(node, NULL);
        S_decref_node_sv(aTHX_ parent);
    }
    else {
        cmark_node_free(node);
    }

cmark_iter*
iterator(cmark_node *node)
CODE:
    S_create_or_incref_node_sv(aTHX_ node);
    RETVAL = cmark_iter_new(node);
OUTPUT:
    RETVAL

cmark_node*
interface_get_node(cmark_node *node)
INTERFACE:
    cmark_node_next
    cmark_node_previous
    cmark_node_parent
    cmark_node_first_child
    cmark_node_last_child

int
interface_get_int(cmark_node *node)
INTERFACE:
    cmark_node_get_type
    cmark_node_get_header_level
    cmark_node_get_list_type
    cmark_node_get_list_delim
    cmark_node_get_list_start
    cmark_node_get_list_tight
    cmark_node_get_start_line
    cmark_node_get_start_column
    cmark_node_get_end_line
    cmark_node_get_end_column

NO_OUTPUT int
interface_set_int(cmark_node *node, int value)
INTERFACE:
    cmark_node_set_header_level
    cmark_node_set_list_type
    cmark_node_set_list_delim
    cmark_node_set_list_start
    cmark_node_set_list_tight
POSTCALL:
    if (!RETVAL) {
        croak("%s: invalid operation", GvNAME(CvGV(cv)));
    }

const char*
interface_get_utf8(cmark_node *node)
INTERFACE:
    cmark_node_get_type_string
    cmark_node_get_literal
    cmark_node_get_title
    cmark_node_get_url
    cmark_node_get_fence_info

NO_OUTPUT int
interface_set_utf8(cmark_node *node, const char *value)
INTERFACE:
    cmark_node_set_literal
    cmark_node_set_title
    cmark_node_set_url
    cmark_node_set_fence_info
POSTCALL:
    if (!RETVAL) {
        croak("%s: invalid operation", GvNAME(CvGV(cv)));
    }

void
cmark_node_unlink(cmark_node *node)
PREINIT:
    cmark_node *old_parent;
INIT:
    old_parent = cmark_node_parent(node);
POSTCALL:
    S_decref_node_sv(aTHX_ old_parent);

NO_OUTPUT int
interface_move_node(cmark_node *node, cmark_node *other)
PREINIT:
    cmark_node *old_parent;
    cmark_node *new_parent;
INIT:
    old_parent = cmark_node_parent(other);
INTERFACE:
    cmark_node_insert_before
    cmark_node_insert_after
    cmark_node_prepend_child
    cmark_node_append_child
POSTCALL:
    if (!RETVAL) {
        croak("%s: invalid operation", GvNAME(CvGV(cv)));
    }
    new_parent = cmark_node_parent(other);
    S_transfer_refcount(aTHX_ old_parent, new_parent);

char*
interface_render(cmark_node *root, long options = 0)
INTERFACE:
    cmark_node_render_html
    cmark_node_render_xml
    cmark_node_render_man


MODULE = CommonMark  PACKAGE = CommonMark::Iterator  PREFIX = cmark_iter_

void
DESTROY(cmark_iter *iter)
CODE:
    S_decref_node_sv(aTHX_ cmark_iter_get_node(iter));
    S_decref_node_sv(aTHX_ cmark_iter_get_root(iter));
    cmark_iter_free(iter);

void
cmark_iter_next(cmark_iter *iter)
PREINIT:
    I32 gimme;
    cmark_node *old_node;
    cmark_event_type ev_type;
PPCODE:
    gimme    = GIMME_V;
    old_node = cmark_iter_get_node(iter);
    ev_type  = cmark_iter_next(iter);

    if (ev_type != CMARK_EVENT_DONE) {
        cmark_node *node = cmark_iter_get_node(iter);

        ST(0) = sv_2mortal(newSViv((IV)ev_type));

        if (gimme == G_ARRAY) {
            SV *obj = S_create_or_incref_node_sv(aTHX_ node);

            /* A bit more efficient than S_transfer_refcount. */
            if (old_node != node) {
                S_decref_node_sv(aTHX_ old_node);
                SvREFCNT_inc_simple_void_NN(obj);
            }

            ST(1) = sv_2mortal(newRV_noinc(obj));
            XSRETURN(2);
        }
        else {
            S_transfer_refcount(aTHX_ old_node, node);
            XSRETURN(1);
        }
    }
    else {
        S_decref_node_sv(aTHX_ old_node);

        if (gimme == G_ARRAY) {
            XSRETURN_EMPTY;
        }
        else {
            ST(0) = sv_2mortal(newSViv((IV)ev_type));
            XSRETURN(1);
        }
    }

cmark_node*
cmark_iter_get_node(cmark_iter *iter)

cmark_event_type
cmark_iter_get_event_type(cmark_iter *iter)

void
cmark_iter_reset(iter, node, event_type)
    cmark_iter *iter
    cmark_node *node
    cmark_event_type event_type
PREINIT:
    cmark_node *old_node;
INIT:
    old_node = cmark_iter_get_node(iter);
    S_transfer_refcount(aTHX_ old_node, node);


MODULE = CommonMark  PACKAGE = CommonMark::Parser  PREFIX = cmark_parser_

cmark_parser*
cmark_parser_new(package)
    SV *package = NO_INIT
CODE:
    (void)package;
    RETVAL = cmark_parser_new();
OUTPUT:
    RETVAL

void
DESTROY(cmark_parser *parser)
CODE:
    cmark_parser_free(parser);

void
cmark_parser_feed(cmark_parser *parser, SV *string)
PREINIT:
    STRLEN len;
    const char *buffer;
CODE:
    buffer = SvPVutf8(string, len);
    cmark_parser_feed(parser, buffer, len);

cmark_node*
cmark_parser_finish(cmark_parser *parser)


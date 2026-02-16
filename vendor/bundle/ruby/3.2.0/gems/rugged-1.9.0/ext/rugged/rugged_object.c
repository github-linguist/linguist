/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedTagAnnotation;
extern VALUE rb_cRuggedTree;
extern VALUE rb_cRuggedCommit;
extern VALUE rb_cRuggedBlob;
extern VALUE rb_cRuggedRepo;

VALUE rb_cRuggedObject;

const rb_data_type_t rugged_object_type;

git_otype rugged_otype_get(VALUE self)
{
	git_otype type = GIT_OBJ_BAD;

	if (NIL_P(self))
		return GIT_OBJ_ANY;

	switch (TYPE(self)) {
	case T_STRING:
		type = git_object_string2type(StringValueCStr(self));
		break;

	case T_FIXNUM:
		type = FIX2INT(self);
		break;

	case T_SYMBOL: {
		ID t = SYM2ID(self);

		if (t == rb_intern("commit"))
			type = GIT_OBJ_COMMIT;
		else if (t == rb_intern("tree"))
			type = GIT_OBJ_TREE;
		else if (t == rb_intern("tag"))
			type = GIT_OBJ_TAG;
		else if (t == rb_intern("blob"))
			type = GIT_OBJ_BLOB;
	   }
	}

	if (!git_object_typeisloose(type))
		rb_raise(rb_eTypeError, "Invalid Git object type specifier");

	return type;
}

VALUE rugged_otype_new(git_otype t)
{
	switch (t) {
		case GIT_OBJ_COMMIT:
			return CSTR2SYM("commit");
		case GIT_OBJ_TAG:
			return CSTR2SYM("tag");
		case GIT_OBJ_TREE:
			return CSTR2SYM("tree");
		case GIT_OBJ_BLOB:
			return CSTR2SYM("blob");
		default:
			return Qnil;
	}
}

int rugged_oid_get(git_oid *oid, git_repository *repo, VALUE p)
{
	git_object *object;
	int error;

	if (rb_obj_is_kind_of(p, rb_cRuggedObject)) {
		TypedData_Get_Struct(p, git_object, &rugged_object_type, object);
		git_oid_cpy(oid, git_object_id(object));
	} else {
		Check_Type(p, T_STRING);

		/* Fast path: see if the 40-char string is an OID */
		if (RSTRING_LEN(p) == 40 &&
			git_oid_fromstr(oid, RSTRING_PTR(p)) == 0)
			return GIT_OK;

		if ((error = git_revparse_single(&object, repo, StringValueCStr(p))))
			return error;

		git_oid_cpy(oid, git_object_id(object));
		git_object_free(object);
	}

	return GIT_OK;
}

git_object *rugged_object_get(git_repository *repo, VALUE object_value, git_otype type)
{
	git_object *object = NULL;

	if (rb_obj_is_kind_of(object_value, rb_cRuggedObject)) {
		git_object *owned_obj = NULL;
		TypedData_Get_Struct(object_value, git_object, &rugged_object_type, owned_obj);
		git_object_dup(&object, owned_obj);
	} else {
		int error;

		Check_Type(object_value, T_STRING);

		/* Fast path: if we have a 40-char string, just perform the lookup directly */
		if (RSTRING_LEN(object_value) == 40) {
			git_oid oid;

			/* If it's not an OID, we can still try the revparse */
			if (git_oid_fromstr(&oid, RSTRING_PTR(object_value)) == 0) {
				error = git_object_lookup(&object, repo, &oid, type);
				rugged_exception_check(error);
				return object;
			}
		}

		/* Otherwise, assume the string is a revlist and try to parse it */
		error = git_revparse_single(&object, repo, StringValueCStr(object_value));
		rugged_exception_check(error);
	}

	assert(object);

	if (type != GIT_OBJ_ANY && git_object_type(object) != type)
		rb_raise(rb_eArgError, "Object is not of the required type");

	return object;
}

static void rb_git_object__free(void *data)
{
	git_object *object = (git_object *) data;
	git_object_free(object);
}

static size_t rb_git_object__size(const void *data)
{
	git_object *object = (git_object *) data;
	size_t size = 0;

	/*
	 * We cannot always be accurate cheaply, but we can give some numbers
	 * which are closer than zero by taking an average/guessed size.
	 */
	switch (git_object_type(object)) {
	case GIT_OBJ_BLOB:
	{
		git_blob *blob = (git_blob *) object;
		size = git_blob_rawsize(blob);
		break;
	}
	case GIT_OBJ_TREE:
	{
		git_tree *tree = (git_tree *) object;
		size = git_tree_entrycount(tree) * 64;
		break;
	}
	case GIT_OBJ_COMMIT:
	case GIT_OBJ_TAG:
		size = 256;
		break;
	default:
		break;
	}

	return size;
}

const rb_data_type_t rugged_object_type = {
	.wrap_struct_name = "Rugged::Object",
	.function = {
		.dmark = NULL,
		.dfree = rb_git_object__free,
		.dsize = rb_git_object__size,
	},
	.data = NULL,
	.flags = RUBY_TYPED_FREE_IMMEDIATELY,
};

VALUE rugged_object_new(VALUE owner, git_object *object)
{
	VALUE klass, rb_object;

	switch (git_object_type(object))
	{
		case GIT_OBJ_COMMIT:
			klass = rb_cRuggedCommit;
			break;

		case GIT_OBJ_TAG:
			klass = rb_cRuggedTagAnnotation;
			break;

		case GIT_OBJ_TREE:
			klass = rb_cRuggedTree;
			break;

		case GIT_OBJ_BLOB:
			klass = rb_cRuggedBlob;
			break;

		default:
			rb_raise(rb_eTypeError, "Invalid type for Rugged::Object");
			return Qnil; /* never reached */
	}

	rb_object = TypedData_Wrap_Struct(klass, &rugged_object_type, object);
	rugged_set_owner(rb_object, owner);
	return rb_object;
}


static git_otype class2otype(VALUE klass)
{
	if (RTEST(rb_class_inherited_p(klass, rb_cRuggedCommit)))
        return GIT_OBJ_COMMIT;

	if (RTEST(rb_class_inherited_p(klass, rb_cRuggedTagAnnotation)))
		return GIT_OBJ_TAG;

	if (RTEST(rb_class_inherited_p(klass, rb_cRuggedBlob)))
		return GIT_OBJ_BLOB;

	if (RTEST(rb_class_inherited_p(klass, rb_cRuggedTree)))
		return GIT_OBJ_TREE;

	return GIT_OBJ_BAD;
}

/*
 *  call-seq:
 *    Object.new(repo, oid) -> object
 *    Object.lookup(repo, oid) -> object
 *
 *  Find and return the git object inside +repo+ with the given +oid+.
 *
 *  +oid+ can either have be the complete, 40 character string or any
 *  unique prefix.
 */
VALUE rb_git_object_lookup(VALUE klass, VALUE rb_repo, VALUE rb_hex)
{
	git_object *object;
	git_otype type;
	git_oid oid;
	int error;
	int oid_length;

	git_repository *repo;

	type = class2otype(klass);

	if (type == GIT_OBJ_BAD)
		type = GIT_OBJ_ANY;

	Check_Type(rb_hex, T_STRING);
	oid_length = (int)RSTRING_LEN(rb_hex);

	rugged_check_repo(rb_repo);

	if (oid_length > GIT_OID_HEXSZ)
		rb_raise(rb_eTypeError, "The given OID is too long");

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_oid_fromstrn(&oid, RSTRING_PTR(rb_hex), oid_length);
	rugged_exception_check(error);

	if (oid_length < GIT_OID_HEXSZ)
		error = git_object_lookup_prefix(&object, repo, &oid, oid_length, type);
	else
		error = git_object_lookup(&object, repo, &oid, type);

	rugged_exception_check(error);

	return rugged_object_new(rb_repo, object);
}

VALUE rugged_object_rev_parse(VALUE rb_repo, VALUE rb_spec, int as_obj)
{
	git_object *object;
	const char *spec;
	int error;
	git_repository *repo;
	VALUE ret;

	Check_Type(rb_spec, T_STRING);
	spec = RSTRING_PTR(rb_spec);

	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_revparse_single(&object, repo, spec);
	rugged_exception_check(error);

	if (as_obj) {
		return rugged_object_new(rb_repo, object);
	}

	ret = rugged_create_oid(git_object_id(object));
	git_object_free(object);
	return ret;
}

/*
 *  call-seq: Object.rev_parse(repo, str) -> object
 *
 *  Find and return a single object inside +repo+ as specified by the
 *  git revision string +str+.
 *
 *  See http://git-scm.com/docs/git-rev-parse.html#_specifying_revisions or
 *  <code>man gitrevisions</code> for information on the accepted syntax.
 *
 *  Raises a Rugged::InvalidError if +str+ does not contain a valid revision string.
 */
VALUE rb_git_object_rev_parse(VALUE klass, VALUE rb_repo, VALUE rb_spec)
{
	return rugged_object_rev_parse(rb_repo, rb_spec, 1);
}

/*
 *  call-seq: Object.rev_parse_oid(repo, str) -> oid
 *
 *  Find and return the id of the object inside +repo+ as specified by the
 *  git revision string +str+.
 *
 *  See http://git-scm.com/docs/git-rev-parse.html#_specifying_revisions or
 *  <code>man gitrevisions</code> for information on the accepted syntax.
 *
 *  Raises a Rugged::InvalidError if +str+ does not contain a valid revision string.
 */
VALUE rb_git_object_rev_parse_oid(VALUE klass, VALUE rb_repo, VALUE rb_spec)
{
	return rugged_object_rev_parse(rb_repo, rb_spec, 0);
}

/*
 *  call-seq: object == other
 *
 *  Returns true only if +object+ and other are both instances or subclasses of
 *  Rugged::Object and have the same object id, false otherwise.
 */
static VALUE rb_git_object_equal(VALUE self, VALUE other)
{
	git_object *a, *b;

	if (!rb_obj_is_kind_of(other, rb_cRuggedObject))
		return Qfalse;

	TypedData_Get_Struct(self, git_object, &rugged_object_type, a);
	TypedData_Get_Struct(other, git_object, &rugged_object_type, b);

	return git_oid_cmp(git_object_id(a), git_object_id(b)) == 0 ? Qtrue : Qfalse;
}

/*
 *  call-seq: object.oid -> oid
 *
 *  Return the Object ID (a 40 character SHA1 hash) for +object+.
 */
static VALUE rb_git_object_oid_GET(VALUE self)
{
	git_object *object;
	TypedData_Get_Struct(self, git_object, &rugged_object_type, object);
	return rugged_create_oid(git_object_id(object));
}

/*
 *  call-seq: object.type -> type
 *
 *  Returns the object's type. Can be one of +:commit+, +:tag+, +:tree+ or +:blob+.
 */
static VALUE rb_git_object_type_GET(VALUE self)
{
	git_object *object;
	TypedData_Get_Struct(self, git_object, &rugged_object_type, object);

	return rugged_otype_new(git_object_type(object));
}

/*
 *  call-seq: object.read_raw -> raw_object
 *
 *  Returns the git object as a Rugged::OdbObject instance.
 */
static VALUE rb_git_object_read_raw(VALUE self)
{
	git_object *object;
	TypedData_Get_Struct(self, git_object, &rugged_object_type, object);

	return rugged_raw_read(git_object_owner(object), git_object_id(object));
}

void Init_rugged_object(void)
{
	rb_cRuggedObject = rb_define_class_under(rb_mRugged, "Object", rb_cObject);
	rb_define_singleton_method(rb_cRuggedObject, "lookup", rb_git_object_lookup, 2);
	rb_define_singleton_method(rb_cRuggedObject, "rev_parse", rb_git_object_rev_parse, 2);
	rb_define_singleton_method(rb_cRuggedObject, "rev_parse_oid", rb_git_object_rev_parse_oid, 2);
	rb_define_singleton_method(rb_cRuggedObject, "new", rb_git_object_lookup, 2);

	rb_define_method(rb_cRuggedObject, "read_raw", rb_git_object_read_raw, 0);
	rb_define_method(rb_cRuggedObject, "==", rb_git_object_equal, 1);
	rb_define_method(rb_cRuggedObject, "oid", rb_git_object_oid_GET, 0);
	rb_define_method(rb_cRuggedObject, "type", rb_git_object_type_GET, 0);
}

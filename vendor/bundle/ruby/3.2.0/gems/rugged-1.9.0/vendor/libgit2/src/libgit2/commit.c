/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "commit.h"

#include "git2/common.h"
#include "git2/object.h"
#include "git2/repository.h"
#include "git2/signature.h"
#include "git2/mailmap.h"
#include "git2/sys/commit.h"

#include "buf.h"
#include "odb.h"
#include "commit.h"
#include "signature.h"
#include "refs.h"
#include "object.h"
#include "array.h"
#include "oidarray.h"
#include "grafts.h"

void git_commit__free(void *_commit)
{
	git_commit *commit = _commit;

	git_array_clear(commit->parent_ids);

	git_signature_free(commit->author);
	git_signature_free(commit->committer);

	git__free(commit->raw_header);
	git__free(commit->raw_message);
	git__free(commit->message_encoding);
	git__free(commit->summary);
	git__free(commit->body);

	git__free(commit);
}

static int git_commit__create_buffer_internal(
	git_str *out,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	git_array_oid_t *parents)
{
	size_t i = 0;
	const git_oid *parent;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(tree);

	if (git_object__write_oid_header(out, "tree ", tree) < 0)
		goto on_error;

	for (i = 0; i < git_array_size(*parents); i++) {
		parent = git_array_get(*parents, i);
		if (git_object__write_oid_header(out, "parent ", parent) < 0)
			goto on_error;
	}

	git_signature__writebuf(out, "author ", author);
	git_signature__writebuf(out, "committer ", committer);

	if (message_encoding != NULL)
		git_str_printf(out, "encoding %s\n", message_encoding);

	git_str_putc(out, '\n');

	if (git_str_puts(out, message) < 0)
		goto on_error;

	return 0;

on_error:
	git_str_dispose(out);
	return -1;
}

static int validate_tree_and_parents(git_array_oid_t *parents, git_repository *repo, const git_oid *tree,
	git_commit_parent_callback parent_cb, void *parent_payload,
	const git_oid *current_id, bool validate)
{
	size_t i;
	int error;
	git_oid *parent_cpy;
	const git_oid *parent;

	if (validate && !git_object__is_valid(repo, tree, GIT_OBJECT_TREE))
		return -1;

	i = 0;
	while ((parent = parent_cb(i, parent_payload)) != NULL) {
		if (validate && !git_object__is_valid(repo, parent, GIT_OBJECT_COMMIT)) {
			error = -1;
			goto on_error;
		}

		parent_cpy = git_array_alloc(*parents);
		GIT_ERROR_CHECK_ALLOC(parent_cpy);

		git_oid_cpy(parent_cpy, parent);
		i++;
	}

	if (current_id && (parents->size == 0 || git_oid_cmp(current_id, git_array_get(*parents, 0)))) {
		git_error_set(GIT_ERROR_OBJECT, "failed to create commit: current tip is not the first parent");
		error = GIT_EMODIFIED;
		goto on_error;
	}

	return 0;

on_error:
	git_array_clear(*parents);
	return error;
}

static int git_commit__create_internal(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	git_commit_parent_callback parent_cb,
	void *parent_payload,
	bool validate)
{
	int error;
	git_odb *odb;
	git_reference *ref = NULL;
	git_str buf = GIT_STR_INIT;
	const git_oid *current_id = NULL;
	git_array_oid_t parents = GIT_ARRAY_INIT;

	if (update_ref) {
		error = git_reference_lookup_resolved(&ref, repo, update_ref, 10);
		if (error < 0 && error != GIT_ENOTFOUND)
			return error;
	}
	git_error_clear();

	if (ref)
		current_id = git_reference_target(ref);

	if ((error = validate_tree_and_parents(&parents, repo, tree, parent_cb, parent_payload, current_id, validate)) < 0)
		goto cleanup;

	error = git_commit__create_buffer_internal(&buf, author, committer,
		message_encoding, message, tree,
		&parents);

	if (error < 0)
		goto cleanup;

	if (git_repository_odb__weakptr(&odb, repo) < 0)
		goto cleanup;

	if (git_odb__freshen(odb, tree) < 0)
		goto cleanup;

	if (git_odb_write(id, odb, buf.ptr, buf.size, GIT_OBJECT_COMMIT) < 0)
		goto cleanup;


	if (update_ref != NULL) {
		error = git_reference__update_for_commit(
			repo, ref, update_ref, id, "commit");
		goto cleanup;
	}

cleanup:
	git_array_clear(parents);
	git_reference_free(ref);
	git_str_dispose(&buf);
	return error;
}

int git_commit_create_from_callback(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	git_commit_parent_callback parent_cb,
	void *parent_payload)
{
	return git_commit__create_internal(
		id, repo, update_ref, author, committer, message_encoding, message,
		tree, parent_cb, parent_payload, true);
}

typedef struct {
	size_t total;
	va_list args;
} commit_parent_varargs;

static const git_oid *commit_parent_from_varargs(size_t curr, void *payload)
{
	commit_parent_varargs *data = payload;
	const git_commit *commit;
	if (curr >= data->total)
		return NULL;
	commit = va_arg(data->args, const git_commit *);
	return commit ? git_commit_id(commit) : NULL;
}

int git_commit_create_v(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	...)
{
	int error = 0;
	commit_parent_varargs data;

	GIT_ASSERT_ARG(tree);
	GIT_ASSERT_ARG(git_tree_owner(tree) == repo);

	data.total = parent_count;
	va_start(data.args, parent_count);

	error = git_commit__create_internal(
		id, repo, update_ref, author, committer,
		message_encoding, message, git_tree_id(tree),
		commit_parent_from_varargs, &data, false);

	va_end(data.args);
	return error;
}

typedef struct {
	size_t total;
	const git_oid **parents;
} commit_parent_oids;

static const git_oid *commit_parent_from_ids(size_t curr, void *payload)
{
	commit_parent_oids *data = payload;
	return (curr < data->total) ? data->parents[curr] : NULL;
}

int git_commit_create_from_ids(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	size_t parent_count,
	const git_oid *parents[])
{
	commit_parent_oids data = { parent_count, parents };

	return git_commit__create_internal(
		id, repo, update_ref, author, committer,
		message_encoding, message, tree,
		commit_parent_from_ids, &data, true);
}

typedef struct {
	size_t total;
	const git_commit **parents;
	git_repository *repo;
} commit_parent_data;

static const git_oid *commit_parent_from_array(size_t curr, void *payload)
{
	commit_parent_data *data = payload;
	const git_commit *commit;
	if (curr >= data->total)
		return NULL;
	commit = data->parents[curr];
	if (git_commit_owner(commit) != data->repo)
		return NULL;
	return git_commit_id(commit);
}

int git_commit_create(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	const git_commit *parents[])
{
	commit_parent_data data = { parent_count, parents, repo };

	GIT_ASSERT_ARG(tree);
	GIT_ASSERT_ARG(git_tree_owner(tree) == repo);

	return git_commit__create_internal(
		id, repo, update_ref, author, committer,
		message_encoding, message, git_tree_id(tree),
		commit_parent_from_array, &data, false);
}

static const git_oid *commit_parent_for_amend(size_t curr, void *payload)
{
	const git_commit *commit_to_amend = payload;
	if (curr >= git_array_size(commit_to_amend->parent_ids))
		return NULL;
	return git_array_get(commit_to_amend->parent_ids, curr);
}

int git_commit_amend(
	git_oid *id,
	const git_commit *commit_to_amend,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree)
{
	git_repository *repo;
	git_oid tree_id;
	git_reference *ref;
	int error;

	GIT_ASSERT_ARG(id);
	GIT_ASSERT_ARG(commit_to_amend);

	repo = git_commit_owner(commit_to_amend);

	if (!author)
		author = git_commit_author(commit_to_amend);
	if (!committer)
		committer = git_commit_committer(commit_to_amend);
	if (!message_encoding)
		message_encoding = git_commit_message_encoding(commit_to_amend);
	if (!message)
		message = git_commit_message(commit_to_amend);

	if (!tree) {
		git_tree *old_tree;
		GIT_ERROR_CHECK_ERROR( git_commit_tree(&old_tree, commit_to_amend) );
		git_oid_cpy(&tree_id, git_tree_id(old_tree));
		git_tree_free(old_tree);
	} else {
		GIT_ASSERT_ARG(git_tree_owner(tree) == repo);
		git_oid_cpy(&tree_id, git_tree_id(tree));
	}

	if (update_ref) {
		if ((error = git_reference_lookup_resolved(&ref, repo, update_ref, 5)) < 0)
			return error;

		if (git_oid_cmp(git_commit_id(commit_to_amend), git_reference_target(ref))) {
			git_reference_free(ref);
			git_error_set(GIT_ERROR_REFERENCE, "commit to amend is not the tip of the given branch");
			return -1;
		}
	}

	error = git_commit__create_internal(
		id, repo, NULL, author, committer, message_encoding, message,
		&tree_id, commit_parent_for_amend, (void *)commit_to_amend, false);

	if (!error && update_ref) {
		error = git_reference__update_for_commit(
			repo, ref, NULL, id, "commit");
		git_reference_free(ref);
	}

	return error;
}

static int commit_parse(
	git_commit *commit,
	const char *data,
	size_t size,
	git_commit__parse_options *opts)
{
	const char *buffer_start = data, *buffer;
	const char *buffer_end = buffer_start + size;
	git_oid parent_id;
	size_t header_len;
	git_signature dummy_sig;
	int error;

	GIT_ASSERT_ARG(commit);
	GIT_ASSERT_ARG(data);
	GIT_ASSERT_ARG(opts);

	buffer = buffer_start;

	/* Allocate for one, which will allow not to realloc 90% of the time  */
	git_array_init_to_size(commit->parent_ids, 1);
	GIT_ERROR_CHECK_ARRAY(commit->parent_ids);

	/* The tree is always the first field */
	if (!(opts->flags & GIT_COMMIT_PARSE_QUICK)) {
		if (git_object__parse_oid_header(&commit->tree_id,
				&buffer, buffer_end, "tree ",
				opts->oid_type) < 0)
			goto bad_buffer;
	} else {
		size_t tree_len = strlen("tree ") + git_oid_hexsize(opts->oid_type) + 1;

		if (buffer + tree_len > buffer_end)
			goto bad_buffer;
		buffer += tree_len;
	}

	while (git_object__parse_oid_header(&parent_id,
			&buffer, buffer_end, "parent ",
			opts->oid_type) == 0) {
		git_oid *new_id = git_array_alloc(commit->parent_ids);
		GIT_ERROR_CHECK_ALLOC(new_id);

		git_oid_cpy(new_id, &parent_id);
	}

	if (!opts || !(opts->flags & GIT_COMMIT_PARSE_QUICK)) {
		commit->author = git__malloc(sizeof(git_signature));
		GIT_ERROR_CHECK_ALLOC(commit->author);

		if ((error = git_signature__parse(commit->author, &buffer, buffer_end, "author ", '\n')) < 0)
			return error;
	}

	/* Some tools create multiple author fields, ignore the extra ones */
	while (!git__prefixncmp(buffer, buffer_end - buffer, "author ")) {
		if ((error = git_signature__parse(&dummy_sig, &buffer, buffer_end, "author ", '\n')) < 0)
			return error;

		git__free(dummy_sig.name);
		git__free(dummy_sig.email);
	}

	/* Always parse the committer; we need the commit time */
	commit->committer = git__malloc(sizeof(git_signature));
	GIT_ERROR_CHECK_ALLOC(commit->committer);

	if ((error = git_signature__parse(commit->committer, &buffer, buffer_end, "committer ", '\n')) < 0)
		return error;

	if (opts && opts->flags & GIT_COMMIT_PARSE_QUICK)
		return 0;

	/* Parse add'l header entries */
	while (buffer < buffer_end) {
		const char *eoln = buffer;
		if (buffer[-1] == '\n' && buffer[0] == '\n')
			break;

		while (eoln < buffer_end && *eoln != '\n')
			++eoln;

		if (git__prefixncmp(buffer, buffer_end - buffer, "encoding ") == 0) {
			buffer += strlen("encoding ");

			commit->message_encoding = git__strndup(buffer, eoln - buffer);
			GIT_ERROR_CHECK_ALLOC(commit->message_encoding);
		}

		if (eoln < buffer_end && *eoln == '\n')
			++eoln;
		buffer = eoln;
	}

	header_len = buffer - buffer_start;
	commit->raw_header = git__strndup(buffer_start, header_len);
	GIT_ERROR_CHECK_ALLOC(commit->raw_header);

	/* point "buffer" to data after header, +1 for the final LF */
	buffer = buffer_start + header_len + 1;

	/* extract commit message */
	if (buffer <= buffer_end)
		commit->raw_message = git__strndup(buffer, buffer_end - buffer);
	else
		commit->raw_message = git__strdup("");
	GIT_ERROR_CHECK_ALLOC(commit->raw_message);

	return 0;

bad_buffer:
	git_error_set(GIT_ERROR_OBJECT, "failed to parse bad commit object");
	return GIT_EINVALID;
}

int git_commit__parse(
	void *commit,
	git_odb_object *odb_obj,
	git_oid_t oid_type)
{
	git_commit__parse_options parse_options = {0};
	parse_options.oid_type = oid_type;

	return git_commit__parse_ext(commit, odb_obj, &parse_options);
}

int git_commit__parse_raw(
	void *commit,
	const char *data,
	size_t size,
	git_oid_t oid_type)
{
	git_commit__parse_options parse_options = {0};
	parse_options.oid_type = oid_type;

	return commit_parse(commit, data, size, &parse_options);
}

static int assign_commit_parents_from_graft(git_commit *commit, git_commit_graft *graft) {
	size_t idx;
	git_oid *oid;

	git_array_clear(commit->parent_ids);
	git_array_init_to_size(commit->parent_ids, git_array_size(graft->parents));
	git_array_foreach(graft->parents, idx, oid) {
		git_oid *id = git_array_alloc(commit->parent_ids);
		GIT_ERROR_CHECK_ALLOC(id);

		git_oid_cpy(id, oid);
	}

	return 0;
}

int git_commit__parse_ext(
	git_commit *commit,
	git_odb_object *odb_obj,
	git_commit__parse_options *parse_opts)
{
	git_repository *repo = git_object_owner((git_object *)commit);
	git_commit_graft *graft;
	int error;

	if ((error = commit_parse(commit, git_odb_object_data(odb_obj),
				  git_odb_object_size(odb_obj), parse_opts)) < 0)
		return error;

	/* Perform necessary grafts */
	if (git_grafts_get(&graft, repo->grafts, git_odb_object_id(odb_obj)) != 0 &&
		git_grafts_get(&graft, repo->shallow_grafts, git_odb_object_id(odb_obj)) != 0)
		return 0;

	return assign_commit_parents_from_graft(commit, graft);
}

#define GIT_COMMIT_GETTER(_rvalue, _name, _return, _invalid) \
	_rvalue git_commit_##_name(const git_commit *commit) \
	{\
		GIT_ASSERT_ARG_WITH_RETVAL(commit, _invalid); \
		return _return; \
	}

GIT_COMMIT_GETTER(const git_signature *, author, commit->author, NULL)
GIT_COMMIT_GETTER(const git_signature *, committer, commit->committer, NULL)
GIT_COMMIT_GETTER(const char *, message_raw, commit->raw_message, NULL)
GIT_COMMIT_GETTER(const char *, message_encoding, commit->message_encoding, NULL)
GIT_COMMIT_GETTER(const char *, raw_header, commit->raw_header, NULL)
GIT_COMMIT_GETTER(git_time_t, time, commit->committer->when.time, INT64_MIN)
GIT_COMMIT_GETTER(int, time_offset, commit->committer->when.offset, -1)
GIT_COMMIT_GETTER(unsigned int, parentcount, (unsigned int)git_array_size(commit->parent_ids), 0)
GIT_COMMIT_GETTER(const git_oid *, tree_id, &commit->tree_id, NULL)

const char *git_commit_message(const git_commit *commit)
{
	const char *message;

	GIT_ASSERT_ARG_WITH_RETVAL(commit, NULL);

	message = commit->raw_message;

	/* trim leading newlines from raw message */
	while (*message && *message == '\n')
		++message;

	return message;
}

const char *git_commit_summary(git_commit *commit)
{
	git_str summary = GIT_STR_INIT;
	const char *msg, *space, *next;
	bool space_contains_newline = false;

	GIT_ASSERT_ARG_WITH_RETVAL(commit, NULL);

	if (!commit->summary) {
		for (msg = git_commit_message(commit), space = NULL; *msg; ++msg) {
			char next_character = msg[0];
			/* stop processing at the end of the first paragraph */
			if (next_character == '\n') {
				if (!msg[1])
					break;
				if (msg[1] == '\n')
					break;
				/* stop processing if next line contains only whitespace */
				next = msg + 1;
				while (*next && git__isspace_nonlf(*next)) {
					++next;
				}
				if (!*next || *next == '\n')
					break;
			}
			/* record the beginning of contiguous whitespace runs */
			if (git__isspace(next_character)) {
				if(space == NULL) {
					space = msg;
					space_contains_newline = false;
				}
				space_contains_newline |= next_character == '\n';
			}
			/* the next character is non-space */
			else {
				/* process any recorded whitespace */
				if (space) {
					if(space_contains_newline)
						git_str_putc(&summary, ' '); /* if the space contains a newline, collapse to ' ' */
					else
						git_str_put(&summary, space, (msg - space)); /* otherwise copy it */
					space = NULL;
				}
				/* copy the next character */
				git_str_putc(&summary, next_character);
			}
		}

		commit->summary = git_str_detach(&summary);
		if (!commit->summary)
			commit->summary = git__strdup("");
	}

	return commit->summary;
}

const char *git_commit_body(git_commit *commit)
{
	const char *msg, *end;

	GIT_ASSERT_ARG_WITH_RETVAL(commit, NULL);

	if (!commit->body) {
		/* search for end of summary */
		for (msg = git_commit_message(commit); *msg; ++msg)
			if (msg[0] == '\n' && (!msg[1] || msg[1] == '\n'))
				break;

		/* trim leading and trailing whitespace */
		for (; *msg; ++msg)
			if (!git__isspace(*msg))
				break;
		for (end = msg + strlen(msg) - 1; msg <= end; --end)
			if (!git__isspace(*end))
				break;

		if (*msg)
			commit->body = git__strndup(msg, end - msg + 1);
	}

	return commit->body;
}

int git_commit_tree(git_tree **tree_out, const git_commit *commit)
{
	GIT_ASSERT_ARG(commit);
	return git_tree_lookup(tree_out, commit->object.repo, &commit->tree_id);
}

const git_oid *git_commit_parent_id(
	const git_commit *commit, unsigned int n)
{
	GIT_ASSERT_ARG_WITH_RETVAL(commit, NULL);

	return git_array_get(commit->parent_ids, n);
}

int git_commit_parent(
	git_commit **parent, const git_commit *commit, unsigned int n)
{
	const git_oid *parent_id;
	GIT_ASSERT_ARG(commit);

	parent_id = git_commit_parent_id(commit, n);
	if (parent_id == NULL) {
		git_error_set(GIT_ERROR_INVALID, "parent %u does not exist", n);
		return GIT_ENOTFOUND;
	}

	return git_commit_lookup(parent, commit->object.repo, parent_id);
}

int git_commit_nth_gen_ancestor(
	git_commit **ancestor,
	const git_commit *commit,
	unsigned int n)
{
	git_commit *current, *parent = NULL;
	int error;

	GIT_ASSERT_ARG(ancestor);
	GIT_ASSERT_ARG(commit);

	if (git_commit_dup(&current, (git_commit *)commit) < 0)
		return -1;

	if (n == 0) {
		*ancestor = current;
		return 0;
	}

	while (n--) {
		error = git_commit_parent(&parent, current, 0);

		git_commit_free(current);

		if (error < 0)
			return error;

		current = parent;
	}

	*ancestor = parent;
	return 0;
}

int git_commit_header_field(
	git_buf *out,
	const git_commit *commit,
	const char *field)
{
	GIT_BUF_WRAP_PRIVATE(out, git_commit__header_field, commit, field);
}

int git_commit__header_field(
	git_str *out,
	const git_commit *commit,
	const char *field)
{
	const char *eol, *buf = commit->raw_header;

	git_str_clear(out);

	while ((eol = strchr(buf, '\n'))) {
		/* We can skip continuations here */
		if (buf[0] == ' ') {
			buf = eol + 1;
			continue;
		}

		/* Skip until we find the field we're after */
		if (git__prefixcmp(buf, field)) {
			buf = eol + 1;
			continue;
		}

		buf += strlen(field);
		/* Check that we're not matching a prefix but the field itself */
		if (buf[0] != ' ') {
			buf = eol + 1;
			continue;
		}

		buf++; /* skip the SP */

		git_str_put(out, buf, eol - buf);
		if (git_str_oom(out))
			goto oom;

		/* If the next line starts with SP, it's multi-line, we must continue */
		while (eol[1] == ' ') {
			git_str_putc(out, '\n');
			buf = eol + 2;
			eol = strchr(buf, '\n');
			if (!eol)
				goto malformed;

			git_str_put(out, buf, eol - buf);
		}

		if (git_str_oom(out))
			goto oom;

		return 0;
	}

	git_error_set(GIT_ERROR_OBJECT, "no such field '%s'", field);
	return GIT_ENOTFOUND;

malformed:
	git_error_set(GIT_ERROR_OBJECT, "malformed header");
	return -1;
oom:
	git_error_set_oom();
	return -1;
}

int git_commit_extract_signature(
	git_buf *signature_out,
	git_buf *signed_data_out,
	git_repository *repo,
	git_oid *commit_id,
	const char *field)
{
	git_str signature = GIT_STR_INIT, signed_data = GIT_STR_INIT;
	int error;

	if ((error = git_buf_tostr(&signature, signature_out)) < 0 ||
	    (error = git_buf_tostr(&signed_data, signed_data_out)) < 0 ||
	    (error = git_commit__extract_signature(&signature, &signed_data, repo, commit_id, field)) < 0 ||
	    (error = git_buf_fromstr(signature_out, &signature)) < 0 ||
	    (error = git_buf_fromstr(signed_data_out, &signed_data)) < 0)
		goto done;

done:
	git_str_dispose(&signature);
	git_str_dispose(&signed_data);
	return error;
}

int git_commit__extract_signature(
	git_str *signature,
	git_str *signed_data,
	git_repository *repo,
	git_oid *commit_id,
	const char *field)
{
	git_odb_object *obj;
	git_odb *odb;
	const char *buf;
	const char *h, *eol;
	int error;

	git_str_clear(signature);
	git_str_clear(signed_data);

	if (!field)
		field = "gpgsig";

	if ((error = git_repository_odb__weakptr(&odb, repo)) < 0)
		return error;

	if ((error = git_odb_read(&obj, odb, commit_id)) < 0)
		return error;

	if (obj->cached.type != GIT_OBJECT_COMMIT) {
		git_error_set(GIT_ERROR_INVALID, "the requested type does not match the type in the ODB");
		error = GIT_ENOTFOUND;
		goto cleanup;
	}

	buf = git_odb_object_data(obj);

	while ((h = strchr(buf, '\n')) && h[1] != '\0') {
		h++;
		if (git__prefixcmp(buf, field)) {
			if (git_str_put(signed_data, buf, h - buf) < 0)
				return -1;

			buf = h;
			continue;
		}

		h = buf;
		h += strlen(field);
		eol = strchr(h, '\n');
		if (h[0] != ' ') {
			buf = h;
			continue;
		}
		if (!eol)
			goto malformed;

		h++; /* skip the SP */

		git_str_put(signature, h, eol - h);
		if (git_str_oom(signature))
			goto oom;

		/* If the next line starts with SP, it's multi-line, we must continue */
		while (eol[1] == ' ') {
			git_str_putc(signature, '\n');
			h = eol + 2;
			eol = strchr(h, '\n');
			if (!eol)
				goto malformed;

			git_str_put(signature, h, eol - h);
		}

		if (git_str_oom(signature))
			goto oom;

		error = git_str_puts(signed_data, eol+1);
		git_odb_object_free(obj);
		return error;
	}

	git_error_set(GIT_ERROR_OBJECT, "this commit is not signed");
	error = GIT_ENOTFOUND;
	goto cleanup;

malformed:
	git_error_set(GIT_ERROR_OBJECT, "malformed header");
	error = -1;
	goto cleanup;
oom:
	git_error_set_oom();
	error = -1;
	goto cleanup;

cleanup:
	git_odb_object_free(obj);
	git_str_clear(signature);
	git_str_clear(signed_data);
	return error;
}

int git_commit_create_buffer(
	git_buf *out,
	git_repository *repo,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	const git_commit *parents[])
{
	GIT_BUF_WRAP_PRIVATE(out, git_commit__create_buffer, repo,
	                     author, committer, message_encoding, message,
	                     tree, parent_count, parents);
}

int git_commit__create_buffer(
	git_str *out,
	git_repository *repo,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	const git_commit *parents[])
{
	int error;
	commit_parent_data data = { parent_count, parents, repo };
	git_array_oid_t parents_arr = GIT_ARRAY_INIT;
	const git_oid *tree_id;

	GIT_ASSERT_ARG(tree);
	GIT_ASSERT_ARG(git_tree_owner(tree) == repo);

	tree_id = git_tree_id(tree);

	if ((error = validate_tree_and_parents(&parents_arr, repo, tree_id, commit_parent_from_array, &data, NULL, true)) < 0)
		return error;

	error = git_commit__create_buffer_internal(
		out, author, committer,
		message_encoding, message, tree_id,
		&parents_arr);

	git_array_clear(parents_arr);
	return error;
}

/**
 * Append to 'out' properly marking continuations when there's a newline in 'content'
 */
static int format_header_field(git_str *out, const char *field, const char *content)
{
	const char *lf;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(field);
	GIT_ASSERT_ARG(content);

	git_str_puts(out, field);
	git_str_putc(out, ' ');

	while ((lf = strchr(content, '\n')) != NULL) {
		git_str_put(out, content, lf - content);
		git_str_puts(out, "\n ");
		content = lf + 1;
	}

	git_str_puts(out, content);
	git_str_putc(out, '\n');

	return git_str_oom(out) ? -1 : 0;
}

static const git_oid *commit_parent_from_commit(size_t n, void *payload)
{
	const git_commit *commit = (const git_commit *) payload;

	return git_array_get(commit->parent_ids, n);

}

int git_commit_create_with_signature(
	git_oid *out,
	git_repository *repo,
	const char *commit_content,
	const char *signature,
	const char *signature_field)
{
	git_odb *odb;
	int error = 0;
	const char *field;
	const char *header_end;
	git_str commit = GIT_STR_INIT;
	git_commit *parsed;
	git_array_oid_t parents = GIT_ARRAY_INIT;
	git_commit__parse_options parse_opts = {0};

	parse_opts.oid_type = repo->oid_type;

	/* The first step is to verify that all the tree and parents exist */
	parsed = git__calloc(1, sizeof(git_commit));
	GIT_ERROR_CHECK_ALLOC(parsed);
	if (commit_parse(parsed, commit_content, strlen(commit_content), &parse_opts) < 0) {
		error = -1;
		goto cleanup;
	}

	if ((error = validate_tree_and_parents(&parents, repo, &parsed->tree_id, commit_parent_from_commit, parsed, NULL, true)) < 0)
		goto cleanup;

	git_array_clear(parents);

	/* Then we start appending by identifying the end of the commit header */
	header_end = strstr(commit_content, "\n\n");
	if (!header_end) {
		git_error_set(GIT_ERROR_INVALID, "malformed commit contents");
		error = -1;
		goto cleanup;
	}

	/* The header ends after the first LF */
	header_end++;
	git_str_put(&commit, commit_content, header_end - commit_content);

	if (signature != NULL) {
		field = signature_field ? signature_field : "gpgsig";

		if ((error = format_header_field(&commit, field, signature)) < 0)
			goto cleanup;
	}

	git_str_puts(&commit, header_end);

	if (git_str_oom(&commit))
		return -1;

	if ((error = git_repository_odb__weakptr(&odb, repo)) < 0)
		goto cleanup;

	if ((error = git_odb_write(out, odb, commit.ptr, commit.size, GIT_OBJECT_COMMIT)) < 0)
		goto cleanup;

cleanup:
	git_commit__free(parsed);
	git_str_dispose(&commit);
	return error;
}

int git_commit_create_from_stage(
	git_oid *out,
	git_repository *repo,
	const char *message,
	const git_commit_create_options *given_opts)
{
	git_commit_create_options opts = GIT_COMMIT_CREATE_OPTIONS_INIT;
	git_signature *default_signature = NULL;
	const git_signature *author, *committer;
	git_index *index = NULL;
	git_diff *diff = NULL;
	git_oid tree_id;
	git_tree *head_tree = NULL, *tree = NULL;
	git_commitarray parents = { 0 };
	int error = -1;

	GIT_ASSERT_ARG(out && repo);

	if (given_opts)
		memcpy(&opts, given_opts, sizeof(git_commit_create_options));

	author = opts.author;
	committer = opts.committer;

	if (!author || !committer) {
		if (git_signature_default(&default_signature, repo) < 0)
			goto done;

		if (!author)
			author = default_signature;

		if (!committer)
			committer = default_signature;
	}

	if (git_repository_index(&index, repo) < 0)
		goto done;

	if (!opts.allow_empty_commit) {
		error = git_repository_head_tree(&head_tree, repo);

		if (error && error != GIT_EUNBORNBRANCH)
			goto done;

		error = -1;

		if (git_diff_tree_to_index(&diff, repo, head_tree, index, NULL) < 0)
			goto done;

		if (git_diff_num_deltas(diff) == 0) {
			git_error_set(GIT_ERROR_REPOSITORY,
				"no changes are staged for commit");
			error = GIT_EUNCHANGED;
			goto done;
		}
	}

	if (git_index_write_tree(&tree_id, index) < 0 ||
	    git_tree_lookup(&tree, repo, &tree_id) < 0 ||
	    git_repository_commit_parents(&parents, repo) < 0)
		goto done;

	error = git_commit_create(out, repo, "HEAD", author, committer,
			opts.message_encoding, message,
			tree, parents.count,
			(const git_commit **)parents.commits);

done:
	git_commitarray_dispose(&parents);
	git_signature_free(default_signature);
	git_tree_free(tree);
	git_tree_free(head_tree);
	git_diff_free(diff);
	git_index_free(index);
	return error;
}

int git_commit_committer_with_mailmap(
	git_signature **out, const git_commit *commit, const git_mailmap *mailmap)
{
	return git_mailmap_resolve_signature(out, mailmap, commit->committer);
}

int git_commit_author_with_mailmap(
	git_signature **out, const git_commit *commit, const git_mailmap *mailmap)
{
	return git_mailmap_resolve_signature(out, mailmap, commit->author);
}

void git_commitarray_dispose(git_commitarray *array)
{
	size_t i;

	if (array == NULL)
		return;

	for (i = 0; i < array->count; i++)
		git_commit_free(array->commits[i]);

	git__free((git_commit **)array->commits);

	memset(array, 0, sizeof(*array));
}

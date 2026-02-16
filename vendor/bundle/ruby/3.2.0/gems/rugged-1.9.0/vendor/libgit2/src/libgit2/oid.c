/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oid.h"

#include "git2/oid.h"
#include "repository.h"
#include "runtime.h"
#include <string.h>
#include <limits.h>

const git_oid git_oid__empty_blob_sha1 =
	GIT_OID_INIT(GIT_OID_SHA1,
	  { 0xe6, 0x9d, 0xe2, 0x9b, 0xb2, 0xd1, 0xd6, 0x43, 0x4b, 0x8b,
	    0x29, 0xae, 0x77, 0x5a, 0xd8, 0xc2, 0xe4, 0x8c, 0x53, 0x91 });
const git_oid git_oid__empty_tree_sha1 =
	GIT_OID_INIT(GIT_OID_SHA1,
	  { 0x4b, 0x82, 0x5d, 0xc6, 0x42, 0xcb, 0x6e, 0xb9, 0xa0, 0x60,
	    0xe5, 0x4b, 0xf8, 0xd6, 0x92, 0x88, 0xfb, 0xee, 0x49, 0x04 });

static int oid_error_invalid(const char *msg)
{
	git_error_set(GIT_ERROR_INVALID, "unable to parse OID - %s", msg);
	return -1;
}

int git_oid__fromstrn(
	git_oid *out,
	const char *str,
	size_t length,
	git_oid_t type)
{
	size_t size, p;
	int v;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(str);

	if (!(size = git_oid_size(type)))
		return oid_error_invalid("unknown type");

	if (!length)
		return oid_error_invalid("too short");

	if (length > git_oid_hexsize(type))
		return oid_error_invalid("too long");

#ifdef GIT_EXPERIMENTAL_SHA256
	out->type = type;
#endif
	memset(out->id, 0, size);

	for (p = 0; p < length; p++) {
		v = git__fromhex(str[p]);
		if (v < 0)
			return oid_error_invalid("contains invalid characters");

		out->id[p / 2] |= (unsigned char)(v << (p % 2 ? 0 : 4));
	}

	return 0;
}

int git_oid__fromstrp(git_oid *out, const char *str, git_oid_t type)
{
	return git_oid__fromstrn(out, str, strlen(str), type);
}

int git_oid__fromstr(git_oid *out, const char *str, git_oid_t type)
{
	return git_oid__fromstrn(out, str, git_oid_hexsize(type), type);
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_oid_fromstrn(
	git_oid *out,
	const char *str,
	size_t length,
	git_oid_t type)
{
	return git_oid__fromstrn(out, str, length, type);
}

int git_oid_fromstrp(git_oid *out, const char *str, git_oid_t type)
{
	return git_oid_fromstrn(out, str, strlen(str), type);
}

int git_oid_fromstr(git_oid *out, const char *str, git_oid_t type)
{
	return git_oid_fromstrn(out, str, git_oid_hexsize(type), type);
}
#else
int git_oid_fromstrn(
	git_oid *out,
	const char *str,
	size_t length)
{
	return git_oid__fromstrn(out, str, length, GIT_OID_SHA1);
}

int git_oid_fromstrp(git_oid *out, const char *str)
{
	return git_oid__fromstrn(out, str, strlen(str), GIT_OID_SHA1);
}

int git_oid_fromstr(git_oid *out, const char *str)
{
	return git_oid__fromstrn(out, str, GIT_OID_SHA1_HEXSIZE, GIT_OID_SHA1);
}
#endif

int git_oid_nfmt(char *str, size_t n, const git_oid *oid)
{
	size_t hex_size;

	if (!oid) {
		memset(str, 0, n);
		return 0;
	}

	if (!(hex_size = git_oid_hexsize(git_oid_type(oid))))
		return oid_error_invalid("unknown type");

	if (n > hex_size) {
		memset(&str[hex_size], 0, n - hex_size);
		n = hex_size;
	}

	git_oid_fmt_substr(str, oid, 0, n);
	return 0;
}

int git_oid_fmt(char *str, const git_oid *oid)
{
	return git_oid_nfmt(str, git_oid_hexsize(git_oid_type(oid)), oid);
}

int git_oid_pathfmt(char *str, const git_oid *oid)
{
	size_t hex_size;

	if (!(hex_size = git_oid_hexsize(git_oid_type(oid))))
		return oid_error_invalid("unknown type");

	git_oid_fmt_substr(str, oid, 0, 2);
	str[2] = '/';
	git_oid_fmt_substr(&str[3], oid, 2, (hex_size - 2));
	return 0;
}

static git_tlsdata_key thread_str_key;

static void GIT_SYSTEM_CALL thread_str_free(void *s)
{
	char *str = (char *)s;
	git__free(str);
}

static void thread_str_global_shutdown(void)
{
	char *str = git_tlsdata_get(thread_str_key);
	git_tlsdata_set(thread_str_key, NULL);

	git__free(str);
	git_tlsdata_dispose(thread_str_key);
}

int git_oid_global_init(void)
{
	if (git_tlsdata_init(&thread_str_key, thread_str_free) != 0)
		return -1;

	return git_runtime_shutdown_register(thread_str_global_shutdown);
}

char *git_oid_tostr_s(const git_oid *oid)
{
	char *str;

	if ((str = git_tlsdata_get(thread_str_key)) == NULL) {
		if ((str = git__malloc(GIT_OID_MAX_HEXSIZE + 1)) == NULL)
			return NULL;

		git_tlsdata_set(thread_str_key, str);
	}

	git_oid_nfmt(str, git_oid_hexsize(git_oid_type(oid)) + 1, oid);
	return str;
}

char *git_oid_allocfmt(const git_oid *oid)
{
	size_t hex_size = git_oid_hexsize(git_oid_type(oid));
	char *str = git__malloc(hex_size + 1);

	if (!hex_size || !str)
		return NULL;

	if (git_oid_nfmt(str, hex_size + 1, oid) < 0) {
		git__free(str);
		return NULL;
	}

	return str;
}

char *git_oid_tostr(char *out, size_t n, const git_oid *oid)
{
	size_t hex_size;

	if (!out || n == 0)
		return "";

	hex_size = oid ? git_oid_hexsize(git_oid_type(oid)) : 0;

	if (n > hex_size + 1)
		n = hex_size + 1;

	git_oid_nfmt(out, n - 1, oid); /* allow room for terminating NUL */
	out[n - 1] = '\0';

	return out;
}

int git_oid__fromraw(git_oid *out, const unsigned char *raw, git_oid_t type)
{
	size_t size;

	if (!(size = git_oid_size(type)))
		return oid_error_invalid("unknown type");

#ifdef GIT_EXPERIMENTAL_SHA256
	out->type = type;
#endif
	memcpy(out->id, raw, size);
	return 0;
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_oid_fromraw(git_oid *out, const unsigned char *raw, git_oid_t type)
{
	return git_oid__fromraw(out, raw, type);
}
#else
int git_oid_fromraw(git_oid *out, const unsigned char *raw)
{
	return git_oid__fromraw(out, raw, GIT_OID_SHA1);
}
#endif

int git_oid_cpy(git_oid *out, const git_oid *src)
{
	size_t size;

	if (!(size = git_oid_size(git_oid_type(src))))
		return oid_error_invalid("unknown type");

#ifdef GIT_EXPERIMENTAL_SHA256
	out->type = src->type;
#endif

	return git_oid_raw_cpy(out->id, src->id, size);
}

int git_oid_cmp(const git_oid *a, const git_oid *b)
{
	return git_oid__cmp(a, b);
}

int git_oid_equal(const git_oid *a, const git_oid *b)
{
	return (git_oid__cmp(a, b) == 0);
}

int git_oid_ncmp(const git_oid *oid_a, const git_oid *oid_b, size_t len)
{
#ifdef GIT_EXPERIMENTAL_SHA256
	if (oid_a->type != oid_b->type)
		return oid_a->type - oid_b->type;
#endif

	return git_oid_raw_ncmp(oid_a->id, oid_b->id, len);
}

int git_oid_strcmp(const git_oid *oid_a, const char *str)
{
	const unsigned char *a;
	unsigned char strval;
	long size = (long)git_oid_size(git_oid_type(oid_a));
	int hexval;

	for (a = oid_a->id; *str && (a - oid_a->id) < size; ++a) {
		if ((hexval = git__fromhex(*str++)) < 0)
			return -1;
		strval = (unsigned char)(hexval << 4);
		if (*str) {
			if ((hexval = git__fromhex(*str++)) < 0)
				return -1;
			strval |= hexval;
		}
		if (*a != strval)
			return (*a - strval);
	}

	return 0;
}

int git_oid_streq(const git_oid *oid_a, const char *str)
{
	return git_oid_strcmp(oid_a, str) == 0 ? 0 : -1;
}

int git_oid_is_zero(const git_oid *oid_a)
{
	const unsigned char *a = oid_a->id;
	size_t size = git_oid_size(git_oid_type(oid_a)), i;

#ifdef GIT_EXPERIMENTAL_SHA256
	if (!oid_a->type)
		return 1;
	else if (!size)
		return 0;
#endif

	for (i = 0; i < size; ++i, ++a)
		if (*a != 0)
			return 0;
	return 1;
}

#ifndef GIT_DEPRECATE_HARD
int git_oid_iszero(const git_oid *oid_a)
{
	return git_oid_is_zero(oid_a);
}
#endif

typedef short node_index;

typedef union {
	const char *tail;
	node_index children[16];
} trie_node;

struct git_oid_shorten {
	trie_node *nodes;
	size_t node_count, size;
	int min_length, full;
};

static int resize_trie(git_oid_shorten *self, size_t new_size)
{
	self->nodes = git__reallocarray(self->nodes, new_size, sizeof(trie_node));
	GIT_ERROR_CHECK_ALLOC(self->nodes);

	if (new_size > self->size) {
		memset(&self->nodes[self->size], 0x0, (new_size - self->size) * sizeof(trie_node));
	}

	self->size = new_size;
	return 0;
}

static trie_node *push_leaf(git_oid_shorten *os, node_index idx, int push_at, const char *oid)
{
	trie_node *node, *leaf;
	node_index idx_leaf;

	if (os->node_count >= os->size) {
		if (resize_trie(os, os->size * 2) < 0)
			return NULL;
	}

	idx_leaf = (node_index)os->node_count++;

	if (os->node_count == SHRT_MAX) {
		os->full = 1;
        return NULL;
    }

	node = &os->nodes[idx];
	node->children[push_at] = -idx_leaf;

	leaf = &os->nodes[idx_leaf];
	leaf->tail = oid;

	return node;
}

git_oid_shorten *git_oid_shorten_new(size_t min_length)
{
	git_oid_shorten *os;

	GIT_ASSERT_ARG_WITH_RETVAL((size_t)((int)min_length) == min_length, NULL);

	os = git__calloc(1, sizeof(git_oid_shorten));
	if (os == NULL)
		return NULL;

	if (resize_trie(os, 16) < 0) {
		git__free(os);
		return NULL;
	}

	os->node_count = 1;
	os->min_length = (int)min_length;

	return os;
}

void git_oid_shorten_free(git_oid_shorten *os)
{
	if (os == NULL)
		return;

	git__free(os->nodes);
	git__free(os);
}


/*
 * What wizardry is this?
 *
 * This is just a memory-optimized trie: basically a very fancy
 * 16-ary tree, which is used to store the prefixes of the OID
 * strings.
 *
 * Read more: http://en.wikipedia.org/wiki/Trie
 *
 * Magic that happens in this method:
 *
 *	- Each node in the trie is an union, so it can work both as
 *	a normal node, or as a leaf.
 *
 *	- Each normal node points to 16 children (one for each possible
 *	character in the oid). This is *not* stored in an array of
 *	pointers, because in a 64-bit arch this would be sucking
 *	16*sizeof(void*) = 128 bytes of memory per node, which is
 *	insane. What we do is store Node Indexes, and use these indexes
 *	to look up each node in the om->index array. These indexes are
 *	signed shorts, so this limits the amount of unique OIDs that
 *	fit in the structure to about 20000 (assuming a more or less uniform
 *	distribution).
 *
 *	- All the nodes in om->index array are stored contiguously in
 *	memory, and each of them is 32 bytes, so we fit 2x nodes per
 *	cache line. Convenient for speed.
 *
 *	- To differentiate the leafs from the normal nodes, we store all
 *	the indexes towards a leaf as a negative index (indexes to normal
 *	nodes are positives). When we find that one of the children for
 *	a node has a negative value, that means it's going to be a leaf.
 *	This reduces the amount of indexes we have by two, but also reduces
 *	the size of each node by 1-4 bytes (the amount we would need to
 *	add a `is_leaf` field): this is good because it allows the nodes
 *	to fit cleanly in cache lines.
 *
 *	- Once we reach an empty children, instead of continuing to insert
 *	new nodes for each remaining character of the OID, we store a pointer
 *	to the tail in the leaf; if the leaf is reached again, we turn it
 *	into a normal node and use the tail to create a new leaf.
 *
 *	This is a pretty good balance between performance and memory usage.
 */
int git_oid_shorten_add(git_oid_shorten *os, const char *text_oid)
{
	int i;
	bool is_leaf;
	node_index idx;

	if (os->full) {
		git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
		return -1;
	}

	if (text_oid == NULL)
		return os->min_length;

	idx = 0;
	is_leaf = false;

	for (i = 0; i < GIT_OID_SHA1_HEXSIZE; ++i) {
		int c = git__fromhex(text_oid[i]);
		trie_node *node;

		if (c == -1) {
			git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - invalid hex value");
			return -1;
		}

		node = &os->nodes[idx];

		if (is_leaf) {
			const char *tail;

			tail = node->tail;
			node->tail = NULL;

			node = push_leaf(os, idx, git__fromhex(tail[0]), &tail[1]);
			if (node == NULL) {
				if (os->full)
					git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
				return -1;
			}
		}

		if (node->children[c] == 0) {
			if (push_leaf(os, idx, c, &text_oid[i + 1]) == NULL) {
				if (os->full)
					git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
				return -1;
			}
			break;
		}

		idx = node->children[c];
		is_leaf = false;

		if (idx < 0) {
			node->children[c] = idx = -idx;
			is_leaf = true;
		}
	}

	if (++i > os->min_length)
		os->min_length = i;

	return os->min_length;
}


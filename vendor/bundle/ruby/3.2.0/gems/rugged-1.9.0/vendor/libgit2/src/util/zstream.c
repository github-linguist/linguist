/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "zstream.h"

#include <zlib.h>

#include "str.h"

#define ZSTREAM_BUFFER_SIZE (1024 * 1024)
#define ZSTREAM_BUFFER_MIN_EXTRA 8

GIT_INLINE(int) zstream_seterr(git_zstream *zs)
{
	switch (zs->zerr) {
	case Z_OK:
	case Z_STREAM_END:
	case Z_BUF_ERROR: /* not fatal; we retry with a larger buffer */
		return 0;
	case Z_MEM_ERROR:
		git_error_set_oom();
		break;
	default:
		if (zs->z.msg)
			git_error_set_str(GIT_ERROR_ZLIB, zs->z.msg);
		else
			git_error_set(GIT_ERROR_ZLIB, "unknown compression error");
	}

	return -1;
}

int git_zstream_init(git_zstream *zstream, git_zstream_t type)
{
	zstream->type = type;

	if (zstream->type == GIT_ZSTREAM_INFLATE)
		zstream->zerr = inflateInit(&zstream->z);
	else
		zstream->zerr = deflateInit(&zstream->z, Z_DEFAULT_COMPRESSION);
	return zstream_seterr(zstream);
}

void git_zstream_free(git_zstream *zstream)
{
	if (zstream->type == GIT_ZSTREAM_INFLATE)
		inflateEnd(&zstream->z);
	else
		deflateEnd(&zstream->z);
}

void git_zstream_reset(git_zstream *zstream)
{
	if (zstream->type == GIT_ZSTREAM_INFLATE)
		inflateReset(&zstream->z);
	else
		deflateReset(&zstream->z);
	zstream->in = NULL;
	zstream->in_len = 0;
	zstream->zerr = Z_STREAM_END;
}

int git_zstream_set_input(git_zstream *zstream, const void *in, size_t in_len)
{
	zstream->in = in;
	zstream->in_len = in_len;
	zstream->zerr = Z_OK;
	return 0;
}

bool git_zstream_done(git_zstream *zstream)
{
	return (!zstream->in_len && zstream->zerr == Z_STREAM_END);
}

bool git_zstream_eos(git_zstream *zstream)
{
	return zstream->zerr == Z_STREAM_END;
}

size_t git_zstream_suggest_output_len(git_zstream *zstream)
{
	if (zstream->in_len > ZSTREAM_BUFFER_SIZE)
		return ZSTREAM_BUFFER_SIZE;
	else if (zstream->in_len > ZSTREAM_BUFFER_MIN_EXTRA)
		return zstream->in_len;
	else
		return ZSTREAM_BUFFER_MIN_EXTRA;
}

int git_zstream_get_output_chunk(
	void *out, size_t *out_len, git_zstream *zstream)
{
	size_t in_queued, in_used, out_queued;

	/* set up input data */
	zstream->z.next_in = (Bytef *)zstream->in;

	/* feed as much data to zlib as it can consume, at most UINT_MAX */
	if (zstream->in_len > UINT_MAX) {
		zstream->z.avail_in = UINT_MAX;
		zstream->flush = Z_NO_FLUSH;
	} else {
		zstream->z.avail_in = (uInt)zstream->in_len;
		zstream->flush = Z_FINISH;
	}
	in_queued = (size_t)zstream->z.avail_in;

	/* set up output data */
	zstream->z.next_out = out;
	zstream->z.avail_out = (uInt)*out_len;

	if ((size_t)zstream->z.avail_out != *out_len)
		zstream->z.avail_out = UINT_MAX;
	out_queued = (size_t)zstream->z.avail_out;

	/* compress next chunk */
	if (zstream->type == GIT_ZSTREAM_INFLATE)
		zstream->zerr = inflate(&zstream->z, zstream->flush);
	else
		zstream->zerr = deflate(&zstream->z, zstream->flush);

	if (zstream_seterr(zstream))
		return -1;

	in_used = (in_queued - zstream->z.avail_in);
	zstream->in_len -= in_used;
	zstream->in += in_used;

	*out_len = (out_queued - zstream->z.avail_out);

	return 0;
}

int git_zstream_get_output(void *out, size_t *out_len, git_zstream *zstream)
{
	size_t out_remain = *out_len;

	if (zstream->in_len && zstream->zerr == Z_STREAM_END) {
		git_error_set(GIT_ERROR_ZLIB, "zlib input had trailing garbage");
		return -1;
	}

	while (out_remain > 0 && zstream->zerr != Z_STREAM_END) {
		size_t out_written = out_remain;

		if (git_zstream_get_output_chunk(out, &out_written, zstream) < 0)
			return -1;

		out_remain -= out_written;
		out = ((char *)out) + out_written;
	}

	/* either we finished the input or we did not flush the data */
	GIT_ASSERT(zstream->in_len > 0 || zstream->flush == Z_FINISH);

	/* set out_size to number of bytes actually written to output */
	*out_len = *out_len - out_remain;

	return 0;
}

static int zstream_buf(git_str *out, const void *in, size_t in_len, git_zstream_t type)
{
	git_zstream zs = GIT_ZSTREAM_INIT;
	int error = 0;

	if ((error = git_zstream_init(&zs, type)) < 0)
		return error;

	if ((error = git_zstream_set_input(&zs, in, in_len)) < 0)
		goto done;

	while (!git_zstream_done(&zs)) {
		size_t step = git_zstream_suggest_output_len(&zs), written;

		if ((error = git_str_grow_by(out, step)) < 0)
			goto done;

		written = out->asize - out->size;

		if ((error = git_zstream_get_output(
				out->ptr + out->size, &written, &zs)) < 0)
			goto done;

		out->size += written;
	}

	/* NULL terminate for consistency if possible */
	if (out->size < out->asize)
		out->ptr[out->size] = '\0';

done:
	git_zstream_free(&zs);
	return error;
}

int git_zstream_deflatebuf(git_str *out, const void *in, size_t in_len)
{
	return zstream_buf(out, in, in_len, GIT_ZSTREAM_DEFLATE);
}

int git_zstream_inflatebuf(git_str *out, const void *in, size_t in_len)
{
	return zstream_buf(out, in, in_len, GIT_ZSTREAM_INFLATE);
}

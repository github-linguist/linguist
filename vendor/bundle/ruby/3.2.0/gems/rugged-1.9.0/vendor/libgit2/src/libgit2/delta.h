/*
 * diff-delta code taken from git.git. See diff-delta.c for details.
 *
 */
#ifndef INCLUDE_git_delta_h__
#define INCLUDE_git_delta_h__

#include "common.h"

#include "pack.h"

typedef struct git_delta_index git_delta_index;

/*
 * git_delta_index_init: compute index data from given buffer
 *
 * This returns a pointer to a struct delta_index that should be passed to
 * subsequent create_delta() calls, or to free_delta_index().  A NULL pointer
 * is returned on failure.  The given buffer must not be freed nor altered
 * before free_delta_index() is called.  The returned pointer must be freed
 * using free_delta_index().
 */
extern int git_delta_index_init(
	git_delta_index **out, const void *buf, size_t bufsize);

/*
 * Free the index created by git_delta_index_init()
 */
extern void git_delta_index_free(git_delta_index *index);

/*
 * Returns memory usage of delta index.
 */
extern size_t git_delta_index_size(git_delta_index *index);

/*
 * create_delta: create a delta from given index for the given buffer
 *
 * This function may be called multiple times with different buffers using
 * the same delta_index pointer.  If max_delta_size is non-zero and the
 * resulting delta is to be larger than max_delta_size then NULL is returned.
 * On success, a non-NULL pointer to the buffer with the delta data is
 * returned and *delta_size is updated with its size.  The returned buffer
 * must be freed by the caller.
 */
extern int git_delta_create_from_index(
	void **out,
	size_t *out_size,
	const struct git_delta_index *index,
	const void *buf,
	size_t bufsize,
	size_t max_delta_size);

/*
 * diff_delta: create a delta from source buffer to target buffer
 *
 * If max_delta_size is non-zero and the resulting delta is to be larger
 * than max_delta_size then GIT_EBUFS is returned.  On success, a non-NULL
 * pointer to the buffer with the delta data is returned and *delta_size is
 * updated with its size.  The returned buffer must be freed by the caller.
 */
GIT_INLINE(int) git_delta(
	void **out, size_t *out_len,
	const void *src_buf, size_t src_bufsize,
	const void *trg_buf, size_t trg_bufsize,
	size_t max_delta_size)
{
	git_delta_index *index;
	int error = 0;

	*out = NULL;
	*out_len = 0;

	if ((error = git_delta_index_init(&index, src_buf, src_bufsize)) < 0)
		return error;

	if (index) {
		error = git_delta_create_from_index(out, out_len,
			index, trg_buf, trg_bufsize, max_delta_size);

		git_delta_index_free(index);
	}

	return error;
}

/* the smallest possible delta size is 4 bytes */
#define GIT_DELTA_SIZE_MIN	4

/**
* Apply a git binary delta to recover the original content.
* The caller is responsible for freeing the returned buffer.
*
* @param out the output buffer
* @param out_len the length of the output buffer
* @param base the base to copy from during copy instructions.
* @param base_len number of bytes available at base.
* @param delta the delta to execute copy/insert instructions from.
* @param delta_len total number of bytes in the delta.
* @return 0 on success or an error code
*/
extern int git_delta_apply(
	void **out,
	size_t *out_len,
	const unsigned char *base,
	size_t base_len,
	const unsigned char *delta,
	size_t delta_len);

/**
* Read the header of a git binary delta.
*
* @param base_out pointer to store the base size field.
* @param result_out pointer to store the result size field.
* @param delta the delta to execute copy/insert instructions from.
* @param delta_len total number of bytes in the delta.
* @return 0 on success or an error code
*/
extern int git_delta_read_header(
	size_t *base_out,
	size_t *result_out,
	const unsigned char *delta,
	size_t delta_len);

/**
 * Read the header of a git binary delta
 *
 * This variant reads just enough from the packfile stream to read the
 * delta header.
 */
extern int git_delta_read_header_fromstream(
	size_t *base_out,
	size_t *result_out,
	git_packfile_stream *stream);

#endif

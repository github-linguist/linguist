#ifndef INCLUDE_blame_h__
#define INCLUDE_blame_h__

#include "common.h"

#include "git2/blame.h"
#include "vector.h"
#include "diff.h"
#include "array.h"
#include "git2/oid.h"

/*
 * One blob in a commit that is being suspected
 */
typedef struct git_blame__origin {
	int refcnt;
	struct git_blame__origin *previous;
	git_commit *commit;
	git_blob *blob;
	char path[GIT_FLEX_ARRAY];
} git_blame__origin;

/*
 * Each group of lines is described by a git_blame__entry; it can be split
 * as we pass blame to the parents.  They form a linked list in the
 * scoreboard structure, sorted by the target line number.
 */
typedef struct git_blame__entry {
	struct git_blame__entry *prev;
	struct git_blame__entry *next;

	/* the first line of this group in the final image;
	 * internally all line numbers are 0 based.
	 */
	size_t lno;

	/* how many lines this group has */
	size_t num_lines;

	/* the commit that introduced this group into the final image */
	git_blame__origin *suspect;

	/* true if the suspect is truly guilty; false while we have not
	 * checked if the group came from one of its parents.
	 */
	bool guilty;

	/* true if the entry has been scanned for copies in the current parent
	 */
	bool scanned;

	/* the line number of the first line of this group in the
	 * suspect's file; internally all line numbers are 0 based.
	 */
	size_t s_lno;

	/* how significant this entry is -- cached to avoid
	 * scanning the lines over and over.
	 */
	unsigned score;

	/* Whether this entry has been tracked to a boundary commit.
	 */
	bool is_boundary;
} git_blame__entry;

struct git_blame {
	char *path;
	git_repository *repository;
	git_mailmap *mailmap;
	git_blame_options options;

	git_vector hunks;
	git_array_t(git_blame_line) lines;
	git_vector paths;

	git_blob *final_blob;
	git_array_t(size_t) line_index;

	size_t current_diff_line;
	git_blame_hunk *current_hunk;

	/* Scoreboard fields */
	git_commit *final;
	git_blame__entry *ent;
	int num_lines;
	const char *final_buf;
	size_t final_buf_size;
};

git_blame *git_blame__alloc(
	git_repository *repo,
	git_blame_options opts,
	const char *path);

#endif

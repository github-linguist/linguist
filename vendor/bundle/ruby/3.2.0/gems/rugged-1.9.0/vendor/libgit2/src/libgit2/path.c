/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "path.h"

#include "repository.h"
#include "fs_path.h"
#include "utf8.h"

typedef struct {
	git_repository *repo;
	uint16_t file_mode;
	unsigned int flags;
} repository_path_validate_data;

static int32_t next_hfs_char(const char **in, size_t *len)
{
	while (*len) {
		uint32_t codepoint;
		int cp_len = git_utf8_iterate(&codepoint, *in, *len);
		if (cp_len < 0)
			return -1;

		(*in) += cp_len;
		(*len) -= cp_len;

		/* these code points are ignored completely */
		switch (codepoint) {
		case 0x200c: /* ZERO WIDTH NON-JOINER */
		case 0x200d: /* ZERO WIDTH JOINER */
		case 0x200e: /* LEFT-TO-RIGHT MARK */
		case 0x200f: /* RIGHT-TO-LEFT MARK */
		case 0x202a: /* LEFT-TO-RIGHT EMBEDDING */
		case 0x202b: /* RIGHT-TO-LEFT EMBEDDING */
		case 0x202c: /* POP DIRECTIONAL FORMATTING */
		case 0x202d: /* LEFT-TO-RIGHT OVERRIDE */
		case 0x202e: /* RIGHT-TO-LEFT OVERRIDE */
		case 0x206a: /* INHIBIT SYMMETRIC SWAPPING */
		case 0x206b: /* ACTIVATE SYMMETRIC SWAPPING */
		case 0x206c: /* INHIBIT ARABIC FORM SHAPING */
		case 0x206d: /* ACTIVATE ARABIC FORM SHAPING */
		case 0x206e: /* NATIONAL DIGIT SHAPES */
		case 0x206f: /* NOMINAL DIGIT SHAPES */
		case 0xfeff: /* ZERO WIDTH NO-BREAK SPACE */
			continue;
		}

		/* fold into lowercase -- this will only fold characters in
		 * the ASCII range, which is perfectly fine, because the
		 * git folder name can only be composed of ascii characters
		 */
		return git__tolower((int)codepoint);
	}
	return 0; /* NULL byte -- end of string */
}

static bool validate_dotgit_hfs_generic(
	const char *path,
	size_t len,
	const char *needle,
	size_t needle_len)
{
	size_t i;
	char c;

	if (next_hfs_char(&path, &len) != '.')
		return true;

	for (i = 0; i < needle_len; i++) {
		c = next_hfs_char(&path, &len);
		if (c != needle[i])
			return true;
	}

	if (next_hfs_char(&path, &len) != '\0')
		return true;

	return false;
}

static bool validate_dotgit_hfs(const char *path, size_t len)
{
	return validate_dotgit_hfs_generic(path, len, "git", CONST_STRLEN("git"));
}

GIT_INLINE(bool) validate_dotgit_ntfs(
	git_repository *repo,
	const char *path,
	size_t len)
{
	git_str *reserved = git_repository__reserved_names_win32;
	size_t reserved_len = git_repository__reserved_names_win32_len;
	size_t start = 0, i;

	if (repo)
		git_repository__reserved_names(&reserved, &reserved_len, repo, true);

	for (i = 0; i < reserved_len; i++) {
		git_str *r = &reserved[i];

		if (len >= r->size &&
			strncasecmp(path, r->ptr, r->size) == 0) {
			start = r->size;
			break;
		}
	}

	if (!start)
		return true;

	/*
	 * Reject paths that start with Windows-style directory separators
	 * (".git\") or NTFS alternate streams (".git:") and could be used
	 * to write to the ".git" directory on Windows platforms.
	 */
	if (path[start] == '\\' || path[start] == ':')
		return false;

	/* Reject paths like '.git ' or '.git.' */
	for (i = start; i < len; i++) {
		if (path[i] != ' ' && path[i] != '.')
			return true;
	}

	return false;
}

/*
 * Windows paths that end with spaces and/or dots are elided to the
 * path without them for backward compatibility.  That is to say
 * that opening file "foo ", "foo." or even "foo . . ." will all
 * map to a filename of "foo".  This function identifies spaces and
 * dots at the end of a filename, whether the proper end of the
 * filename (end of string) or a colon (which would indicate a
 * Windows alternate data stream.)
 */
GIT_INLINE(bool) ntfs_end_of_filename(const char *path)
{
	const char *c = path;

	for (;; c++) {
		if (*c == '\0' || *c == ':')
			return true;
		if (*c != ' ' && *c != '.')
			return false;
	}

	return true;
}

GIT_INLINE(bool) validate_dotgit_ntfs_generic(
	const char *name,
	size_t len,
	const char *dotgit_name,
	size_t dotgit_len,
	const char *shortname_pfix)
{
	int i, saw_tilde;

	if (name[0] == '.' && len >= dotgit_len &&
	    !strncasecmp(name + 1, dotgit_name, dotgit_len)) {
		return !ntfs_end_of_filename(name + dotgit_len + 1);
	}

	/* Detect the basic NTFS shortname with the first six chars */
	if (!strncasecmp(name, dotgit_name, 6) && name[6] == '~' &&
	    name[7] >= '1' && name[7] <= '4')
		return !ntfs_end_of_filename(name + 8);

	/* Catch fallback names */
	for (i = 0, saw_tilde = 0; i < 8; i++) {
		if (name[i] == '\0') {
			return true;
		} else if (saw_tilde) {
			if (name[i] < '0' || name[i] > '9')
				return true;
		} else if (name[i] == '~') {
			if (name[i+1] < '1' || name[i+1]  > '9')
				return true;
			saw_tilde = 1;
		} else if (i >= 6) {
			return true;
		} else if ((unsigned char)name[i] > 127) {
			return true;
		} else if (git__tolower(name[i]) != shortname_pfix[i]) {
			return true;
		}
	}

	return !ntfs_end_of_filename(name + i);
}

/*
 * Return the length of the common prefix between str and prefix, comparing them
 * case-insensitively (must be ASCII to match).
 */
GIT_INLINE(size_t) common_prefix_icase(const char *str, size_t len, const char *prefix)
{
	size_t count = 0;

	while (len > 0 && git__tolower(*str) == git__tolower(*prefix)) {
		count++;
		str++;
		prefix++;
		len--;
	}

	return count;
}

static bool validate_repo_component(
	const char *component,
	size_t len,
	void *payload)
{
	repository_path_validate_data *data = (repository_path_validate_data *)payload;

	if (data->flags & GIT_PATH_REJECT_DOT_GIT_HFS) {
		if (!validate_dotgit_hfs(component, len))
			return false;

		if (S_ISLNK(data->file_mode) &&
		    git_path_is_gitfile(component, len, GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_HFS))
			return false;
	}

	if (data->flags & GIT_PATH_REJECT_DOT_GIT_NTFS) {
		if (!validate_dotgit_ntfs(data->repo, component, len))
			return false;

		if (S_ISLNK(data->file_mode) &&
		    git_path_is_gitfile(component, len, GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_NTFS))
			return false;
	}

	/* don't bother rerunning the `.git` test if we ran the HFS or NTFS
	 * specific tests, they would have already rejected `.git`.
	 */
	if ((data->flags & GIT_PATH_REJECT_DOT_GIT_HFS) == 0 &&
	    (data->flags & GIT_PATH_REJECT_DOT_GIT_NTFS) == 0 &&
	    (data->flags & GIT_PATH_REJECT_DOT_GIT_LITERAL)) {
		if (len >= 4 &&
		    component[0] == '.' &&
		    (component[1] == 'g' || component[1] == 'G') &&
		    (component[2] == 'i' || component[2] == 'I') &&
		    (component[3] == 't' || component[3] == 'T')) {
			if (len == 4)
				return false;

			if (S_ISLNK(data->file_mode) &&
			    common_prefix_icase(component, len, ".gitmodules") == len)
				return false;
		}
	}

	return true;
}

GIT_INLINE(unsigned int) dotgit_flags(
	git_repository *repo,
	unsigned int flags)
{
	int protectHFS = 0, protectNTFS = 1;
	int error = 0;

	flags |= GIT_PATH_REJECT_DOT_GIT_LITERAL;

#ifdef __APPLE__
	protectHFS = 1;
#endif

	if (repo && !protectHFS)
		error = git_repository__configmap_lookup(&protectHFS, repo, GIT_CONFIGMAP_PROTECTHFS);
	if (!error && protectHFS)
		flags |= GIT_PATH_REJECT_DOT_GIT_HFS;

	if (repo)
		error = git_repository__configmap_lookup(&protectNTFS, repo, GIT_CONFIGMAP_PROTECTNTFS);
	if (!error && protectNTFS)
		flags |= GIT_PATH_REJECT_DOT_GIT_NTFS;

	return flags;
}

GIT_INLINE(unsigned int) length_flags(
	git_repository *repo,
	unsigned int flags)
{
#ifdef GIT_WIN32
	int allow = 0;

	if (repo &&
	    git_repository__configmap_lookup(&allow, repo, GIT_CONFIGMAP_LONGPATHS) < 0)
		allow = 0;

	if (allow)
		flags &= ~GIT_FS_PATH_REJECT_LONG_PATHS;

#else
	GIT_UNUSED(repo);
	flags &= ~GIT_FS_PATH_REJECT_LONG_PATHS;
#endif

	return flags;
}

bool git_path_str_is_valid(
	git_repository *repo,
	const git_str *path,
	uint16_t file_mode,
	unsigned int flags)
{
	repository_path_validate_data data = {0};

	/* Upgrade the ".git" checks based on platform */
	if ((flags & GIT_PATH_REJECT_DOT_GIT))
		flags = dotgit_flags(repo, flags);

	/* Update the length checks based on platform */
	if ((flags & GIT_FS_PATH_REJECT_LONG_PATHS))
		flags = length_flags(repo, flags);

	data.repo = repo;
	data.file_mode = file_mode;
	data.flags = flags;

	return git_fs_path_str_is_valid_ext(path, flags, NULL, validate_repo_component, NULL, &data);
}

static const struct {
	const char *file;
	const char *hash;
	size_t filelen;
} gitfiles[] = {
	{ "gitignore", "gi250a", CONST_STRLEN("gitignore") },
	{ "gitmodules", "gi7eba", CONST_STRLEN("gitmodules") },
	{ "gitattributes", "gi7d29", CONST_STRLEN("gitattributes") }
};

extern int git_path_is_gitfile(
	const char *path,
	size_t pathlen,
	git_path_gitfile gitfile,
	git_path_fs fs)
{
	const char *file, *hash;
	size_t filelen;

	if (!(gitfile >= GIT_PATH_GITFILE_GITIGNORE && gitfile < ARRAY_SIZE(gitfiles))) {
		git_error_set(GIT_ERROR_OS, "invalid gitfile for path validation");
		return -1;
	}

	file = gitfiles[gitfile].file;
	filelen = gitfiles[gitfile].filelen;
	hash = gitfiles[gitfile].hash;

	switch (fs) {
	case GIT_PATH_FS_GENERIC:
		return !validate_dotgit_ntfs_generic(path, pathlen, file, filelen, hash) ||
		       !validate_dotgit_hfs_generic(path, pathlen, file, filelen);
	case GIT_PATH_FS_NTFS:
		return !validate_dotgit_ntfs_generic(path, pathlen, file, filelen, hash);
	case GIT_PATH_FS_HFS:
		return !validate_dotgit_hfs_generic(path, pathlen, file, filelen);
	default:
		git_error_set(GIT_ERROR_OS, "invalid filesystem for path validation");
		return -1;
	}
}


/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "sysdir.h"

#include "runtime.h"
#include "str.h"
#include "fs_path.h"
#include <ctype.h>
#if GIT_WIN32
# include "fs_path.h"
# include "win32/path_w32.h"
# include "win32/utf-conv.h"
#else
# include <unistd.h>
# include <pwd.h>
#endif

#ifdef GIT_WIN32
# define REG_GITFORWINDOWS_KEY       L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Git_is1"
# define REG_GITFORWINDOWS_KEY_WOW64 L"SOFTWARE\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Git_is1"

static int expand_win32_path(git_win32_path dest, const wchar_t *src)
{
	DWORD len = ExpandEnvironmentStringsW(src, dest, GIT_WIN_PATH_UTF16);

	if (!len || len > GIT_WIN_PATH_UTF16)
		return -1;

	return 0;
}

static int win32_path_to_utf8(git_str *dest, const wchar_t *src)
{
	git_win32_utf8_path utf8_path;

	if (git_win32_path_to_utf8(utf8_path, src) < 0) {
		git_error_set(GIT_ERROR_OS, "unable to convert path to UTF-8");
		return -1;
	}

	/* Convert backslashes to forward slashes */
	git_fs_path_mkposix(utf8_path);

	return git_str_sets(dest, utf8_path);
}

static git_win32_path mock_registry;
static bool mock_registry_set;

extern int git_win32__set_registry_system_dir(const wchar_t *mock_sysdir)
{
	if (!mock_sysdir) {
		mock_registry[0] = L'\0';
		mock_registry_set = false;
	} else {
		size_t len = wcslen(mock_sysdir);

		if (len > GIT_WIN_PATH_MAX) {
			git_error_set(GIT_ERROR_INVALID, "mock path too long");
			return -1;
		}

		wcscpy(mock_registry, mock_sysdir);
		mock_registry_set = true;
	}

	return 0;
}

static int lookup_registry_key(
	git_win32_path out,
	const HKEY hive,
	const wchar_t* key,
	const wchar_t *value)
{
	HKEY hkey;
	DWORD type, size;
	int error = GIT_ENOTFOUND;

	/*
	 * Registry data may not be NUL terminated, provide room to do
	 * it ourselves.
	 */
	size = (DWORD)((sizeof(git_win32_path) - 1) * sizeof(wchar_t));

	if (RegOpenKeyExW(hive, key, 0, KEY_READ, &hkey) != 0)
		return GIT_ENOTFOUND;

	if (RegQueryValueExW(hkey, value, NULL, &type, (LPBYTE)out, &size) == 0 &&
	    type == REG_SZ &&
	    size > 0 &&
	    size < sizeof(git_win32_path)) {
		size_t wsize = size / sizeof(wchar_t);
		size_t len = wsize - 1;

		if (out[wsize - 1] != L'\0') {
			len = wsize;
			out[wsize] = L'\0';
		}

		if (out[len - 1] == L'\\')
			out[len - 1] = L'\0';

		if (_waccess(out, F_OK) == 0)
			error = 0;
	}

	RegCloseKey(hkey);
	return error;
}

static int find_sysdir_in_registry(git_win32_path out)
{
	if (mock_registry_set) {
		if (mock_registry[0] == L'\0')
			return GIT_ENOTFOUND;

		wcscpy(out, mock_registry);
		return 0;
	}

	if (lookup_registry_key(out, HKEY_CURRENT_USER, REG_GITFORWINDOWS_KEY, L"InstallLocation") == 0 ||
	    lookup_registry_key(out, HKEY_CURRENT_USER, REG_GITFORWINDOWS_KEY_WOW64, L"InstallLocation") == 0 ||
	    lookup_registry_key(out, HKEY_LOCAL_MACHINE, REG_GITFORWINDOWS_KEY, L"InstallLocation") == 0 ||
	    lookup_registry_key(out, HKEY_LOCAL_MACHINE, REG_GITFORWINDOWS_KEY_WOW64, L"InstallLocation") == 0)
		return 0;

    return GIT_ENOTFOUND;
}

static int find_sysdir_in_path(git_win32_path out)
{
	size_t out_len;

	if (git_win32_path_find_executable(out, L"git.exe") < 0 &&
	    git_win32_path_find_executable(out, L"git.cmd") < 0)
		return GIT_ENOTFOUND;

	out_len = wcslen(out);

	/* Trim the file name */
	if (out_len <= CONST_STRLEN(L"git.exe"))
		return GIT_ENOTFOUND;

	out_len -= CONST_STRLEN(L"git.exe");

	if (out_len && out[out_len - 1] == L'\\')
		out_len--;

	/*
	 * Git for Windows usually places the command in a 'bin' or
	 * 'cmd' directory, trim that.
	 */
	if (out_len >= CONST_STRLEN(L"\\bin") &&
	    wcsncmp(&out[out_len - CONST_STRLEN(L"\\bin")], L"\\bin", CONST_STRLEN(L"\\bin")) == 0)
		out_len -= CONST_STRLEN(L"\\bin");
	else if (out_len >= CONST_STRLEN(L"\\cmd") &&
	         wcsncmp(&out[out_len - CONST_STRLEN(L"\\cmd")], L"\\cmd", CONST_STRLEN(L"\\cmd")) == 0)
		out_len -= CONST_STRLEN(L"\\cmd");

	if (!out_len)
		return GIT_ENOTFOUND;

	out[out_len] = L'\0';
	return 0;
}

static int find_win32_dirs(
    git_str *out,
    const wchar_t* tmpl[])
{
	git_win32_path path16;
	git_str buf = GIT_STR_INIT;

	git_str_clear(out);

	for (; *tmpl != NULL; tmpl++) {
		if (!expand_win32_path(path16, *tmpl) &&
		    path16[0] != L'%' &&
		    !_waccess(path16, F_OK)) {
			win32_path_to_utf8(&buf, path16);

			if (buf.size)
				git_str_join(out, GIT_PATH_LIST_SEPARATOR, out->ptr, buf.ptr);
		}
	}

	git_str_dispose(&buf);

	return (git_str_oom(out) ? -1 : 0);
}

static int append_subdir(git_str *out, git_str *path, const char *subdir)
{
	static const char* architecture_roots[] = {
		"",
		"mingw64",
		"mingw32",
		NULL
	};
	const char **root;
	size_t orig_path_len = path->size;

	for (root = architecture_roots; *root; root++) {
		if ((*root[0] && git_str_joinpath(path, path->ptr, *root) < 0) ||
		    git_str_joinpath(path, path->ptr, subdir) < 0)
			return -1;

		if (git_fs_path_exists(path->ptr) &&
		    git_str_join(out, GIT_PATH_LIST_SEPARATOR, out->ptr, path->ptr) < 0)
			return -1;

		git_str_truncate(path, orig_path_len);
	}

	return 0;
}

int git_win32__find_system_dirs(git_str *out, const char *subdir)
{
	git_win32_path pathdir, regdir;
	git_str path8 = GIT_STR_INIT;
	bool has_pathdir, has_regdir;
	int error;

	has_pathdir = (find_sysdir_in_path(pathdir) == 0);
	has_regdir = (find_sysdir_in_registry(regdir) == 0);

	if (!has_pathdir && !has_regdir)
		return 0;

	/*
	 * Usually the git in the path is the same git in the registry,
	 * in this case there's no need to duplicate the paths.
	 */
	if (has_pathdir && has_regdir && wcscmp(pathdir, regdir) == 0)
		has_regdir = false;

	if (has_pathdir) {
		if ((error = win32_path_to_utf8(&path8, pathdir)) < 0 ||
		    (error = append_subdir(out, &path8, subdir)) < 0)
			goto done;
	}

	if (has_regdir) {
		if ((error = win32_path_to_utf8(&path8, regdir)) < 0 ||
		    (error = append_subdir(out, &path8, subdir)) < 0)
			goto done;
	}

done:
    git_str_dispose(&path8);
    return error;
}
#endif /* WIN32 */

static int git_sysdir_guess_programdata_dirs(git_str *out)
{
#ifdef GIT_WIN32
	static const wchar_t *programdata_tmpls[2] = {
		L"%PROGRAMDATA%\\Git",
		NULL,
	};

	return find_win32_dirs(out, programdata_tmpls);
#else
	git_str_clear(out);
	return 0;
#endif
}

static int git_sysdir_guess_system_dirs(git_str *out)
{
#ifdef GIT_WIN32
	return git_win32__find_system_dirs(out, "etc");
#else
	return git_str_sets(out, "/etc");
#endif
}

#ifndef GIT_WIN32
static int get_passwd_home(git_str *out, uid_t uid)
{
	struct passwd pwd, *pwdptr;
	char *buf = NULL;
	long buflen;
	int error;

	GIT_ASSERT_ARG(out);

	if ((buflen = sysconf(_SC_GETPW_R_SIZE_MAX)) == -1)
		buflen = 1024;

	do {
		buf = git__realloc(buf, buflen);
		error = getpwuid_r(uid, &pwd, buf, buflen, &pwdptr);
		buflen *= 2;
	} while (error == ERANGE && buflen <= 8192);

	if (error) {
		git_error_set(GIT_ERROR_OS, "failed to get passwd entry");
		goto out;
	}

	if (!pwdptr) {
		git_error_set(GIT_ERROR_OS, "no passwd entry found for user");
		goto out;
	}

	if ((error = git_str_puts(out, pwdptr->pw_dir)) < 0)
		goto out;

out:
	git__free(buf);
	return error;
}
#endif

static int git_sysdir_guess_home_dirs(git_str *out)
{
#ifdef GIT_WIN32
	static const wchar_t *global_tmpls[4] = {
		L"%HOME%\\",
		L"%HOMEDRIVE%%HOMEPATH%\\",
		L"%USERPROFILE%\\",
		NULL,
	};

	return find_win32_dirs(out, global_tmpls);
#else
	int error;
	uid_t uid, euid;
	const char *sandbox_id;

	uid = getuid();
	euid = geteuid();

	/**
	 * If APP_SANDBOX_CONTAINER_ID is set, we are running in a
	 * sandboxed environment on macOS.
	 */
	sandbox_id = getenv("APP_SANDBOX_CONTAINER_ID");

	/*
	 * In case we are running setuid, use the configuration
	 * of the effective user.
	 *
	 * If we are running in a sandboxed environment on macOS,
	 * we have to get the HOME dir from the password entry file.
	 */
	if (!sandbox_id && uid == euid)
	    error = git__getenv(out, "HOME");
	else
	    error = get_passwd_home(out, euid);

	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		error = 0;
	}

	return error;
#endif
}

static int git_sysdir_guess_global_dirs(git_str *out)
{
	return git_sysdir_guess_home_dirs(out);
}

static int git_sysdir_guess_xdg_dirs(git_str *out)
{
#ifdef GIT_WIN32
	static const wchar_t *global_tmpls[7] = {
		L"%XDG_CONFIG_HOME%\\git",
		L"%APPDATA%\\git",
		L"%LOCALAPPDATA%\\git",
		L"%HOME%\\.config\\git",
		L"%HOMEDRIVE%%HOMEPATH%\\.config\\git",
		L"%USERPROFILE%\\.config\\git",
		NULL,
	};

	return find_win32_dirs(out, global_tmpls);
#else
	git_str env = GIT_STR_INIT;
	int error;
	uid_t uid, euid;

	uid = getuid();
	euid = geteuid();

	/*
	 * In case we are running setuid, only look up passwd
	 * directory of the effective user.
	 */
	if (uid == euid) {
		if ((error = git__getenv(&env, "XDG_CONFIG_HOME")) == 0)
			error = git_str_joinpath(out, env.ptr, "git");

		if (error == GIT_ENOTFOUND && (error = git__getenv(&env, "HOME")) == 0)
			error = git_str_joinpath(out, env.ptr, ".config/git");
	} else {
		if ((error = get_passwd_home(&env, euid)) == 0)
			error = git_str_joinpath(out, env.ptr, ".config/git");
	}

	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		error = 0;
	}

	git_str_dispose(&env);
	return error;
#endif
}

static int git_sysdir_guess_template_dirs(git_str *out)
{
#ifdef GIT_WIN32
	return git_win32__find_system_dirs(out, "share/git-core/templates");
#else
	return git_str_sets(out, "/usr/share/git-core/templates");
#endif
}

struct git_sysdir__dir {
	git_str buf;
	int (*guess)(git_str *out);
};

static struct git_sysdir__dir git_sysdir__dirs[] = {
	{ GIT_STR_INIT, git_sysdir_guess_system_dirs },
	{ GIT_STR_INIT, git_sysdir_guess_global_dirs },
	{ GIT_STR_INIT, git_sysdir_guess_xdg_dirs },
	{ GIT_STR_INIT, git_sysdir_guess_programdata_dirs },
	{ GIT_STR_INIT, git_sysdir_guess_template_dirs },
	{ GIT_STR_INIT, git_sysdir_guess_home_dirs }
};

static void git_sysdir_global_shutdown(void)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(git_sysdir__dirs); ++i)
		git_str_dispose(&git_sysdir__dirs[i].buf);
}

int git_sysdir_global_init(void)
{
	size_t i;
	int error = 0;

	for (i = 0; !error && i < ARRAY_SIZE(git_sysdir__dirs); i++)
		error = git_sysdir__dirs[i].guess(&git_sysdir__dirs[i].buf);

	if (error)
		return error;

	return git_runtime_shutdown_register(git_sysdir_global_shutdown);
}

int git_sysdir_reset(void)
{
	size_t i;
	int error = 0;

	for (i = 0; !error && i < ARRAY_SIZE(git_sysdir__dirs); ++i) {
		git_str_dispose(&git_sysdir__dirs[i].buf);
		error = git_sysdir__dirs[i].guess(&git_sysdir__dirs[i].buf);
	}

	return error;
}

static int git_sysdir_check_selector(git_sysdir_t which)
{
	if (which < ARRAY_SIZE(git_sysdir__dirs))
		return 0;

	git_error_set(GIT_ERROR_INVALID, "config directory selector out of range");
	return -1;
}


int git_sysdir_get(const git_str **out, git_sysdir_t which)
{
	GIT_ASSERT_ARG(out);

	*out = NULL;

	GIT_ERROR_CHECK_ERROR(git_sysdir_check_selector(which));

	*out = &git_sysdir__dirs[which].buf;
	return 0;
}

#define PATH_MAGIC "$PATH"

int git_sysdir_set(git_sysdir_t which, const char *search_path)
{
	const char *expand_path = NULL;
	git_str merge = GIT_STR_INIT;

	GIT_ERROR_CHECK_ERROR(git_sysdir_check_selector(which));

	if (search_path != NULL)
		expand_path = strstr(search_path, PATH_MAGIC);

	/* reset the default if this path has been cleared */
	if (!search_path)
		git_sysdir__dirs[which].guess(&git_sysdir__dirs[which].buf);

	/* if $PATH is not referenced, then just set the path */
	if (!expand_path) {
		if (search_path)
			git_str_sets(&git_sysdir__dirs[which].buf, search_path);

		goto done;
	}

	/* otherwise set to join(before $PATH, old value, after $PATH) */
	if (expand_path > search_path)
		git_str_set(&merge, search_path, expand_path - search_path);

	if (git_str_len(&git_sysdir__dirs[which].buf))
		git_str_join(&merge, GIT_PATH_LIST_SEPARATOR,
			merge.ptr, git_sysdir__dirs[which].buf.ptr);

	expand_path += strlen(PATH_MAGIC);
	if (*expand_path)
		git_str_join(&merge, GIT_PATH_LIST_SEPARATOR, merge.ptr, expand_path);

	git_str_swap(&git_sysdir__dirs[which].buf, &merge);
	git_str_dispose(&merge);

done:
	if (git_str_oom(&git_sysdir__dirs[which].buf))
		return -1;

	return 0;
}

static int git_sysdir_find_in_dirlist(
	git_str *path,
	const char *name,
	git_sysdir_t which,
	const char *label)
{
	size_t len;
	const char *scan, *next = NULL;
	const git_str *syspath;

	GIT_ERROR_CHECK_ERROR(git_sysdir_get(&syspath, which));
	if (!syspath || !git_str_len(syspath))
		goto done;

	for (scan = git_str_cstr(syspath); scan; scan = next) {
		/* find unescaped separator or end of string */
		for (next = scan; *next; ++next) {
			if (*next == GIT_PATH_LIST_SEPARATOR &&
				(next <= scan || next[-1] != '\\'))
				break;
		}

		len = (size_t)(next - scan);
		next = (*next ? next + 1 : NULL);
		if (!len)
			continue;

		GIT_ERROR_CHECK_ERROR(git_str_set(path, scan, len));
		if (name)
			GIT_ERROR_CHECK_ERROR(git_str_joinpath(path, path->ptr, name));

		if (git_fs_path_exists(path->ptr))
			return 0;
	}

done:
	if (name)
		git_error_set(GIT_ERROR_OS, "the %s file '%s' doesn't exist", label, name);
	else
		git_error_set(GIT_ERROR_OS, "the %s directory doesn't exist", label);
	git_str_dispose(path);
	return GIT_ENOTFOUND;
}

int git_sysdir_find_system_file(git_str *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_SYSTEM, "system");
}

int git_sysdir_find_global_file(git_str *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_GLOBAL, "global");
}

int git_sysdir_find_xdg_file(git_str *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_XDG, "global/xdg");
}

int git_sysdir_find_programdata_file(git_str *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_PROGRAMDATA, "ProgramData");
}

int git_sysdir_find_template_dir(git_str *path)
{
	return git_sysdir_find_in_dirlist(
		path, NULL, GIT_SYSDIR_TEMPLATE, "template");
}

int git_sysdir_find_homedir(git_str *path)
{
	return git_sysdir_find_in_dirlist(
		path, NULL, GIT_SYSDIR_HOME, "home directory");
}

int git_sysdir_expand_global_file(git_str *path, const char *filename)
{
	int error;

	if ((error = git_sysdir_find_global_file(path, NULL)) == 0) {
		if (filename)
			error = git_str_joinpath(path, path->ptr, filename);
	}

	return error;
}

int git_sysdir_expand_homedir_file(git_str *path, const char *filename)
{
	int error;

	if ((error = git_sysdir_find_homedir(path)) == 0) {
		if (filename)
			error = git_str_joinpath(path, path->ptr, filename);
	}

	return error;
}

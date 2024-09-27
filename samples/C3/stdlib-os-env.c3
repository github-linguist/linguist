// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/env.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/os/env.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::os::env;
import std::io::path, libc, std::os;

/**
 * @param [in] name
 * @require name.len > 0
 * @return! SearchResult.MISSING
 **/
fn String! get_var(String name, Allocator allocator = allocator::heap())
{
	@pool(allocator)
	{
	$switch
	$case env::LIBC && !env::WIN32:
		ZString val = libc::getenv(name.zstr_tcopy());
		return val ? val.copy(allocator) : SearchResult.MISSING?;
	$case env::WIN32:
		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getenvironmentvariable
		const usz BUFSIZE = 1024;
		WString buff = (WString)tcalloc(BUFSIZE * 2 + 2);
		WString wstr = name.to_temp_wstring()!;
		usz len = win32::getEnvironmentVariableW(wstr, buff, BUFSIZE);
		if (len == 0) return SearchResult.MISSING?;
		if (len > BUFSIZE)
		{
			buff = (WString)tmalloc(len * 2 + 2);
			win32::getEnvironmentVariableW(wstr, buff, (Win32_DWORD)len);
		}
		return string::new_from_wstring(buff, allocator);
	$default:
		return "";
	$endswitch
	};
}

fn String! get_var_temp(String name)
{
	return get_var(name, allocator::temp());
}

/**
 * @param [in] name
 * @param [in] value
 * @require name.len > 0
 **/
fn bool set_var(String name, String value, bool overwrite = true)
{
	@pool()
	{
	$switch
	$case env::WIN32:
		WString wname = name.to_temp_wstring()!!;
		if (!overwrite)
		{
			Char16[8] buff;
			if (win32::getEnvironmentVariableW(wname, &buff, 8) > 0) return true;
		}
		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setenvironmentvariable
		return (win32::setEnvironmentVariableW(wname, value.to_temp_wstring()) ?? 1) == 0;
	$case env::LIBC && !env::WIN32:
		return libc::setenv(name.zstr_tcopy(), value.zstr_copy(), (int)overwrite) == 0;
	$default:
		return false;
	$endswitch
	};

}

/**
 * Returns the current user's home directory.
 **/
fn String! get_home_dir(Allocator using = allocator::heap())
{
	String home;
	$if !env::WIN32:
		home = "HOME";
	$else
		home = "USERPROFILE";
	$endif
	return get_var(home, using);
}

fn Path! get_config_dir(Allocator allocator = allocator::heap()) @deprecated("use new_get_config_dir()")
{
	return new_get_config_dir(allocator) @inline;
}

/**
 * Returns the current user's config directory.
 **/
fn Path! new_get_config_dir(Allocator allocator = allocator::heap())
{
	@pool(allocator)
	{
	$if env::WIN32:
		return path::new(get_var_temp("AppData"), allocator);
	$else
		$if env::DARWIN:
			String s = get_var_temp("HOME")!;
			const DIR = "Library/Application Support";
		$else
			String s = get_var_temp("XDG_CONFIG_HOME") ?? get_var_temp("HOME")!;
			const DIR = ".config";
		$endif
		return path::temp_new(s).new_append(DIR, allocator: allocator);
	$endif
	};
}


/**
 * @param [in] name
 * @require name.len > 0
 **/
fn bool clear_var(String name)
{
	@pool()
	{
	$switch
	$case env::WIN32:
		WString wname = name.to_temp_wstring()!!;
		return win32::setEnvironmentVariableW(wname, null) == 0;
	$case env::LIBC && !env::WIN32:
		return libc::unsetenv(name.zstr_tcopy()) == 0;
	$default:
		return false;
	$endswitch
	};
}

fn String! executable_path(Allocator allocator = allocator::heap()) @deprecated("use new_executable_path()")
{
	return new_executable_path(allocator) @inline;
}

fn String! new_executable_path(Allocator allocator = allocator::heap())
{
	$if env::DARWIN:
		return darwin::executable_path(allocator);
	$else
		return SearchResult.MISSING?;
	$endif
}
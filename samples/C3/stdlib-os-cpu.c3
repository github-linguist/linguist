// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/cpu.c3
// Permalink: https://github.com/c3lang/c3c/blob/81c93e3/lib/std/os/cpu.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

// https://www.cprogramming.com/snippets/source-code/find-the-number-of-cpu-cores-for-windows-mac-or-linux

module std::os @if(env::DARWIN);
import std::os::macos;

fn uint num_cpu()
{
	int[2] nm;
	usz len = 4;
	uint count;

	nm = { darwin::CTL_HW, darwin::HW_NCPU };
	darwin::sysctl(&nm, 2, &count, &len, null, 0);
	if (count < 1) count = 1;
	return count;
}

module std::os @if(env::LINUX);
import std::os::posix;

fn uint num_cpu()
{
	return posix::get_nprocs_conf();
}

module std::os @if(env::WIN32);
import std::os::win32;

fn uint num_cpu()
{
	Win32_SYSTEM_INFO info;
	win32::getSystemInfo(&info);
	return info.dwNumberOfProcessors;
}
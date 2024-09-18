// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/subprocess.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/os/subprocess.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::process @if(env::WIN32 || env::POSIX);
import std::io, libc, std::os;

// This code is based on https://github.com/sheredom/subprocess.h

fault SubProcessResult
{
	FAILED_TO_CREATE_PIPE,
	FAILED_TO_OPEN_STDIN,
	FAILED_TO_OPEN_STDOUT,
	FAILED_TO_OPEN_STDERR,
	FAILED_TO_START_PROCESS,
	FAILED_TO_INITIALIZE_ACTIONS,
	PROCESS_JOIN_FAILED,
	PROCESS_TERMINATION_FAILED,
	READ_FAILED,
}

struct SubProcess
{
	CFile stdin_file;
	CFile stdout_file;
	CFile stderr_file;

	Win32_HANDLE hProcess @if(env::WIN32);
	Win32_HANDLE hStdInput @if(env::WIN32);
	Win32_HANDLE hEventOutput @if(env::WIN32);
	Win32_HANDLE hEventError @if(env::WIN32);

	Pid_t child @if(!env::WIN32);
	int return_status @if(!env::WIN32);

	bool is_alive;
}

bitstruct SubProcessOptions : int
{
	// Combine stdout and stderr to the same file
	bool combined_stdout_stderr;
	// Child process should inhert env variables of parent process
	bool inherit_environment;
	// Enable async reading of stdout/stderr before completion
	bool read_async;
	// Spawn child process without window if supported
	bool no_window;
	// Search for program names in the PATH variable. Always enabled on Windows.
	// Note: this will **not** search for paths in any provided custom environment
	// and instead uses the PATH of the spawning process.
	bool search_user_path;
}

fn void! create_named_pipe_helper(void** rd, void **wr) @local @if(env::WIN32)
{
	Win32_SECURITY_ATTRIBUTES sa_attr = { Win32_SECURITY_ATTRIBUTES.sizeof, null, 1 };

	tlocal long index = 0;
	long unique = index++;
	@pool()
	{
		String s = string::tformat(`\\.\pipe\c3_subprocess.%08x.%08x.%d`, win32::getCurrentProcessId(), win32::getCurrentThreadId(), unique);
		Win32_LPCSTR str = (Win32_LPCSTR)s.ptr;
		*rd = win32::createNamedPipeA(
			str,
			win32::PIPE_ACCESS_INBOUND | win32::FILE_FLAG_OVERLAPPED,
			win32::PIPE_TYPE_BYTE | win32::PIPE_WAIT,
			1, 4096, 4096, 0, &sa_attr);
		if (win32::INVALID_HANDLE_VALUE == *rd) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
		*wr = win32::createFileA(
			str, win32::GENERIC_WRITE, 0, &sa_attr,
			win32::OPEN_EXISTING, win32::FILE_ATTRIBUTE_NORMAL, null);
		if (win32::INVALID_HANDLE_VALUE == *wr) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
	};
}

fn WString convert_command_line_win32(String[] command_line) @inline @if(env::WIN32) @local
{
	DString str = dstring::temp_with_capacity(512);
	foreach (i, s : command_line)
	{
		if (i != 0) str.append(' ');
		bool needs_escape = {|
			foreach (c : s)
			{
				switch (c)
				{
					case '\t':
					case ' ':
					case '\v':
						return true;
				}
			}
			return false;
		|};
		if (!needs_escape)
		{
			str.append(s);
			continue;
		}
		str.append('"');
		foreach (j, c : s)
		{
			switch (c)
			{
				case '\\':
					if (j != s.len - 1 && s[j + 1] == '"') str.append('\\');
				case '"':
					str.append('\\');
			}
			str.append(c);
		}
		str.append('"');
	}
	str.append('\0');
	return str.str_view().to_temp_wstring()!!;
}

/**
 * @require !environment || !options.inherit_environment
 **/
fn SubProcess! create(String[] command_line, SubProcessOptions options = {}, String[] environment = {}) @if(env::WIN32)
{
	void* rd, wr;
	Win32_DWORD flags = win32::CREATE_UNICODE_ENVIRONMENT;
	Win32_PROCESS_INFORMATION process_info;
	Win32_SECURITY_ATTRIBUTES sa_attr = { Win32_SECURITY_ATTRIBUTES.sizeof, null, 1 };
	Win32_STARTUPINFOW start_info = {
		.cb = Win32_STARTUPINFOW.sizeof,
		.dwFlags = win32::STARTF_USESTDHANDLES
	};
	if (options.no_window) flags |= win32::CREATE_NO_WINDOW;
	if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
	// TODO defer catch
	if (!win32::setHandleInformation(wr, win32::HANDLE_FLAG_INHERIT, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;

	CFile stdin;
	CFile stdout;
	CFile stderr;
	@pool()
	{
		WString used_environment = null;
		if (!options.inherit_environment)
		{
			DString env = dstring::temp_with_capacity(64);
			if (!environment.len)
			{
				env.append("\0");
			}
			foreach (String s : environment)
			{
				env.append(s);
				env.append("\0");
			}
			env.append("\0");
			used_environment = env.str_view().to_temp_wstring()!;
		}
		int fd = win32::_open_osfhandle((iptr)wr, 0);
		if (fd != -1)
		{
			stdin = win32::_fdopen(fd, "wb");
			if (!stdin) return SubProcessResult.FAILED_TO_OPEN_STDIN?;
		}
		start_info.hStdInput = rd;
		if (options.read_async)
		{
			create_named_pipe_helper(&rd, &wr)!;
		}
		else
		{
			if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
		}
		if (!win32::setHandleInformation(rd, win32::HANDLE_FLAG_INHERIT, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
		fd = win32::_open_osfhandle((iptr)rd, 0);
		if (fd != -1)
		{
			stdout = win32::_fdopen(fd, "rb");
			if (!stdout) return SubProcessResult.FAILED_TO_OPEN_STDOUT?;
		}

		start_info.hStdOutput = wr;

		{|
			if (options.combined_stdout_stderr)
			{
				stderr = stdout;
				start_info.hStdError = start_info.hStdOutput;
				return;
			}
			if (options.read_async)
			{
				create_named_pipe_helper(&rd, &wr)!;
			}
			else
			{
				if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;
			}
			if (!win32::setHandleInformation(rd, win32::HANDLE_FLAG_INHERIT, 0)) return SubProcessResult.FAILED_TO_CREATE_PIPE?;

			fd = win32::_open_osfhandle((iptr)rd, 0);
			if (fd != -1)
			{
				stderr = win32::_fdopen(fd, "rb");
				if (!stderr) return SubProcessResult.FAILED_TO_OPEN_STDERR?;
			}
			start_info.hStdError = wr;

		|}!;
		void *event_output;
		void *event_error;
		if (options.read_async)
		{
			event_output = win32::createEventA(&sa_attr, 1, 1, null);
			event_error = win32::createEventA(&sa_attr, 1, 1, null);
		}
		if (!win32::createProcessW(
			null,
			convert_command_line_win32(command_line),
			null,                   // process security attributes
			null,                   // primary thread security attributes
			1,                      // handles are inherited
			flags,                  // creation flags
			used_environment,  	    // environment
			null,                   // use parent dir
			&start_info,            // startup info ptr
			&process_info)) return SubProcessResult.FAILED_TO_START_PROCESS?;
	};
	// We don't need the handle of the primary thread in the called process.
	win32::closeHandle(process_info.hThread);
	if (start_info.hStdOutput)
	{
		win32::closeHandle(start_info.hStdOutput);
		if (start_info.hStdOutput != start_info.hStdError) win32::closeHandle(start_info.hStdError);
	}

	return {
		.hProcess = process_info.hProcess,
		.hStdInput = start_info.hStdInput,
		.stdin_file = stdin,
		.stdout_file = stdout,
		.stderr_file = stderr,
		.is_alive = true,
	};
}

/**
 * @require command_line.len > 0
 **/
fn ZString* tcopy_command_line(String[] command_line) @local @inline @if(env::POSIX)
{
	ZString* copy = mem::temp_alloc_array(ZString, command_line.len + 1);
	foreach (i, str : command_line)
	{
		copy[i] = str.zstr_tcopy();
	}
	copy[command_line.len] = null;
	return copy;
}

const ZString[1] EMPTY_ENVIRONMENT @if(env::POSIX) = { null };
fn ZString* tcopy_env(String[] environment) @local @inline @if(env::POSIX)
{
	if (!environment.len) return &EMPTY_ENVIRONMENT;
	ZString* copy = mem::temp_alloc_array(ZString, environment.len + 1);
	copy[environment.len] = null;
	foreach (i, str : environment)
	{
		copy[i] = str.zstr_tcopy();
	}
	return copy;
}

fn String! execute_stdout_to_buffer(char[] buffer, String[] command_line, SubProcessOptions options = {}, String[] environment = {})
{
	SubProcess process = process::create(command_line, options, environment)!;
	process.join()!;
	usz len = process.read_stdout(buffer.ptr, buffer.len)!;
	return (String)buffer[:len - 1];
}

/**
 * @require !environment || !options.inherit_environment
 **/
fn SubProcess! create(String[] command_line, SubProcessOptions options = {}, String[] environment = {}) @if(env::POSIX)
{
	CInt[2] stdinfd;
	CInt[2] stdoutfd;
	CInt[2] stderrfd;

	if (posix::pipe(&stdinfd)) return SubProcessResult.FAILED_TO_OPEN_STDIN?;
	if (posix::pipe(&stdoutfd)) return SubProcessResult.FAILED_TO_OPEN_STDOUT?;
	if (!options.combined_stdout_stderr && posix::pipe(&stderrfd)) return SubProcessResult.FAILED_TO_OPEN_STDERR?;

	Posix_spawn_file_actions_t actions;
	if (posix::spawn_file_actions_init(&actions)) return SubProcessResult.FAILED_TO_INITIALIZE_ACTIONS?;
	defer posix::spawn_file_actions_destroy(&actions);
	if (posix::spawn_file_actions_addclose(&actions, stdinfd[1])) return SubProcessResult.FAILED_TO_OPEN_STDIN?;
	if (posix::spawn_file_actions_adddup2(&actions, stdinfd[0], libc::STDIN_FD)) return SubProcessResult.FAILED_TO_OPEN_STDIN?;
	if (posix::spawn_file_actions_addclose(&actions, stdoutfd[0])) return SubProcessResult.FAILED_TO_OPEN_STDOUT?;
	if (posix::spawn_file_actions_adddup2(&actions, stdoutfd[1], libc::STDOUT_FD)) return SubProcessResult.FAILED_TO_OPEN_STDOUT?;
	if (options.combined_stdout_stderr)
	{
		if (posix::spawn_file_actions_adddup2(&actions, libc::STDOUT_FD, libc::STDERR_FD)) return SubProcessResult.FAILED_TO_OPEN_STDERR?;
	}
	else
	{
		if (posix::spawn_file_actions_addclose(&actions, stderrfd[0])) return SubProcessResult.FAILED_TO_OPEN_STDERR?;
		if (posix::spawn_file_actions_adddup2(&actions, stderrfd[1], libc::STDERR_FD)) return SubProcessResult.FAILED_TO_OPEN_STDERR?;
	}
	Pid_t child;
	@pool()
	{
		ZString* command_line_copy = tcopy_command_line(command_line);
		ZString* used_environment = options.inherit_environment ? posix::environ : tcopy_env(environment);
		if (options.search_user_path)
		{
			if (posix::spawnp(&child, command_line_copy[0], &actions, null, command_line_copy, used_environment)) return SubProcessResult.FAILED_TO_START_PROCESS?;
		}
		else
		{
			if (posix::spawnp(&child, command_line_copy[0], &actions, null, command_line_copy, used_environment)) return SubProcessResult.FAILED_TO_START_PROCESS?;
		}
	};
	libc::close(stdinfd[0]);
	CFile stdin = libc::fdopen(stdinfd[1], "wb");
	libc::close(stdoutfd[1]);
	CFile stdout = libc::fdopen(stdoutfd[0], "rb");
	CFile stderr = {|
		if (options.combined_stdout_stderr) return stdout;
		libc::close(stderrfd[1]);
		return libc::fdopen(stderrfd[0], "rb");
	|};
	return {
		.stdin_file = stdin,
		.stdout_file = stdout,
		.stderr_file = stderr,
		.child = child,
		.is_alive = true,
	};
}

fn CInt! SubProcess.join(&self) @if(env::POSIX)
{
	if (self.stdin_file)
	{
		libc::fclose(self.stdin_file);
		self.stdin_file = null;
	}
	CInt status;
	if (self.child && self.child != posix::waitpid(self.child, &status, 0)) return SubProcessResult.PROCESS_JOIN_FAILED?;

	self.child = 0;
	self.is_alive = false;

	return self.return_status = posix::wIFEXITED(status) ? posix::wEXITSTATUS(status) : libc::EXIT_FAILURE;
}

fn File SubProcess.stdout(&self)
{
	return file::from_handle(self.stdout_file);
}

fn CInt! SubProcess.join(&self) @if(env::WIN32)
{
	if (self.stdin_file)
	{
		libc::fclose(self.stdin_file);
		self.stdin_file = null;
	}
	if (self.hStdInput)
	{
		win32::closeHandle(self.hStdInput);
		self.hStdInput = null;
	}
	win32::waitForSingleObject(self.hProcess, win32::INFINITE);
	Win32_DWORD return_code @noinit;
	if (!win32::getExitCodeProcess(self.hProcess, &return_code)) return SubProcessResult.PROCESS_JOIN_FAILED?;
	self.is_alive = false;
	return return_code;
}

fn bool SubProcess.destroy(&self)
{
	if (self.stdin_file) libc::fclose(self.stdin_file);
	if (self.stdout_file)
	{
		libc::fclose(self.stdout_file);
		if (self.stdout_file != self.stderr_file) libc::fclose(self.stderr_file);
	}
	self.stdin_file = self.stdout_file = self.stderr_file = null;
	$if env::WIN32:
		if (self.hProcess) win32::closeHandle(self.hProcess);
		if (self.hStdInput) win32::closeHandle(self.hStdInput);
		if (self.hEventOutput) win32::closeHandle(self.hEventOutput);
		if (self.hEventError) win32::closeHandle(self.hEventError);
		self.hProcess = self.hStdInput = self.hEventOutput = self.hEventError = null;
	$endif;
	return true;
}

fn void! SubProcess.terminate(&self)
{
	$if env::WIN32:
		if (!win32::terminateProcess(self.hProcess, 99)) return SubProcessResult.PROCESS_TERMINATION_FAILED?;
	$else
		if (posix::kill(self.child, 9)) return SubProcessResult.PROCESS_TERMINATION_FAILED?;
	$endif
}

/**
 * @require size <= Win32_DWORD.max
 **/
fn usz! read_from_file_win32(CFile file, Win32_HANDLE event_handle, char* buffer, usz size) @if(env::WIN32) @local
{
	CInt fd = libc::fileno(file);
	Win32_DWORD bytes_read = 0;
	Win32_OVERLAPPED overlapped = { .hEvent = event_handle };
	Win32_HANDLE handle = (Win32_HANDLE)win32::_get_osfhandle(fd);
	if (!win32::readFile(handle, buffer, (Win32_DWORD)size, &bytes_read, &overlapped))
	{
		// Means we've got an async read!
		if (win32::getLastError() == win32::ERROR_IO_PENDING)
		{
			if (!win32::getOverlappedResult(handle, &overlapped, &bytes_read, 1))
			{
				switch (win32::getLastError())
				{
					case win32::ERROR_IO_INCOMPLETE:
					case win32::ERROR_HANDLE_EOF:
						break;
					default:
						return SubProcessResult.READ_FAILED?;
				}
			}
		}
	}
	return bytes_read;
}
fn usz! read_from_file_posix(CFile file, char* buffer, usz size) @if(env::POSIX) @local
{
	isz bytes_read = libc::read(libc::fileno(file), buffer, size);
	if (bytes_read < 0) return SubProcessResult.READ_FAILED?;
	return bytes_read;
}

fn usz! SubProcess.read_stdout(&self, char* buffer, usz size)
{
	$if env::WIN32:
		return read_from_file_win32(self.stdout_file, self.hEventOutput, buffer, size);
	$else
		return read_from_file_posix(self.stdout_file, buffer, size);
	$endif
}

fn usz! SubProcess.read_stderr(&self, char* buffer, usz size)
{
	$if env::WIN32:
		return read_from_file_win32(self.stderr_file, self.hEventError, buffer, size);
	$else
		return read_from_file_posix(self.stderr_file, buffer, size);
	$endif
}

fn bool! SubProcess.is_running(&self)
{
	if (!self.is_alive) return false;
	$if env::WIN32:
		bool is_alive = win32::waitForSingleObject(self.hProcess, 0) != win32::WAIT_OBJECT_0;
		if (!is_alive) self.is_alive = false;
		return is_alive;
	$else
		CInt status;
		bool is_alive = posix::waitpid(self.child, &status, posix::WNOHANG) == 0;
		if (is_alive) return true;
		self.is_alive = false;
		self.return_status = posix::wIFEXITED(status) ? posix::wEXITSTATUS(status) : libc::EXIT_FAILURE;
		self.child = 0;
		self.join()!;
		return false;
	$endif
}
// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/private/main_stub.c3
// Permalink: https://github.com/c3lang/c3c/blob/e66001c/lib/std/core/private/main_stub.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::main_stub;

macro usz _strlen(ptr) @private
{
	usz len = 0;
	while (ptr[len]) len++;
	return len;
}

macro int @main_to_err_main(#m, int, char**)
{
	if (catch #m()) return 1;
	return 0;
}
macro int @main_to_int_main(#m, int, char**) => #m();
macro int @main_to_void_main(#m, int, char**)
{
	#m();
	return 0;
}

macro String[] args_to_strings(int argc, char** argv) @private
{
	String[] list = mem::alloc_array(String, argc);
	for (int i = 0; i < argc; i++)
	{
		char* arg = argv[i];
		usz len = 0;
		list[i] = (String)arg[:_strlen(arg)];
	}
	return list;
}


macro int @main_to_err_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	if (catch #m(list)) return 1;
	return 0;
}

macro int @main_to_int_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	return #m(list);
}

macro int @main_to_void_main_args(#m, int argc, char** argv)
{
	String[] list = args_to_strings(argc, argv);
	defer free(list.ptr);
	#m(list);
	return 0;
}

module std::core::main_stub @if(env::WIN32);

extern fn Char16** _win_command_line_to_argv_w(ushort* cmd_line, int* argc_ptr) @extern("CommandLineToArgvW");

macro String[] win_command_line_to_strings(ushort* cmd_line) @private
{
	int argc;
	Char16** argv = _win_command_line_to_argv_w(cmd_line, &argc);
	return wargs_strings(argc, argv);
}

macro String[] wargs_strings(int argc, Char16** argv) @private
{
	String[] list = mem::alloc_array(String, argc);
	for (int i = 0; i < argc; i++)
	{
		Char16* arg = argv[i];
		Char16[] argstring = arg[:_strlen(arg)];
		list[i] = string::new_from_utf16(argstring) ?? "?".copy();
	}
	return list[:argc];
}

macro void release_wargs(String[] list) @private
{
	foreach (s : list) free(s.ptr);
	free(list.ptr);
}

macro int @win_to_err_main_noargs(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	if (catch #m()) return 1;
	return 0;
}
macro int @win_to_int_main_noargs(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd) => #m();
macro int @win_to_void_main_noargs(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	#m();
	return 0;
}

macro int @win_to_err_main_args(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	if (catch #m(args)) return 1;
	return 0;
}

macro int @win_to_int_main_args(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	return #m(args);
}

macro int @win_to_void_main_args(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	#m(args);
	return 0;
}

macro int @win_to_err_main(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	if (catch #m(handle, prev_handle, args, show_cmd)) return 1;
	return 0;
}

macro int @win_to_int_main(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	return #m(handle, prev_handle, args, show_cmd);
}

macro int @win_to_void_main(#m, void* handle, void* prev_handle, Char16* cmd_line, int show_cmd)
{
	String[] args = win_command_line_to_strings(cmd_line);
	defer release_wargs(args);
	#m(handle, prev_handle, args, show_cmd);
	return 0;
}

macro int @wmain_to_err_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	if (catch #m(args)) return 1;
	return 1;
}

macro int @wmain_to_int_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	return #m(args);
}

macro int @wmain_to_void_main_args(#m, int argc, Char16** argv)
{
	String[] args = wargs_strings(argc, argv);
	defer release_wargs(args);
	#m(args);
	return 0;
}

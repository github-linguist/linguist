--****
-- == Dynamic Linking to External Code

namespace dll

include std/error.e
include std/machine.e
include std/types.e

--****
-- === C Type Constants
-- These C type constants are used when defining external C functions in a shared
-- library file.

with define NO_CHANDLE

public constant
	C_CHAR    = #01000001,
	C_BYTE    = #01000001,
	C_UCHAR   = #02000001,
	C_UBYTE   = #02000001,
	C_SHORT   = #01000002,
	C_WORD   = #01000002,
	C_USHORT  = #02000002,
	C_INT     = #01000004,
	C_BOOL    = C_INT,
	C_UINT    = #02000004,
	C_LONG    = #01000008,
	C_ULONG   = #02000008,
	C_SIZE_T  = C_ULONG,
	C_POINTER = #03000001,
	C_LONGLONG  = #03000002,
	C_ULONGLONG  = #03000010
ifdef BITS32 then
public constant
	C_LONG_PTR = C_LONG
elsedef
public constant
	C_LONG_PTR = C_LONGLONG
end ifdef
public constant
	C_DWORD   = C_UINT,
	C_FLOAT   = #03000004,
	C_DOUBLE  = #03000008,
	C_DWORDLONG  = C_LONGLONG

--****
-- === External Euphoria Type Constants
-- These are used for arguments to and the return value from a Euphoria shared
-- library file (##.dll##, ##.so##, or ##.dylib##).

public constant
	E_INTEGER = #06000004,
	E_ATOM    = #07000004,
	E_SEQUENCE= #08000004,
	E_OBJECT  = #09000004

--****
-- === Constants

public constant NULL = 0

constant M_OPEN_DLL  = 50,
		 M_DEFINE_C  = 51,
		 M_DEFINE_VAR = 56

--****
-- === Routines

--**
-- opens a //Windows// dynamic link library (##.dll##) file, or a //Unix// shared library
-- (##.so##) file.
public function open_dll(sequence file_name)
	if length(file_name) > 0 and types:string(file_name) then
		return machine_func(M_OPEN_DLL, file_name)
	end if

	-- We have a list of filenames to try, try each one, when one succeeds
	-- abort the search and return it's value
	for idx = 1 to length(file_name) do
		atom fh = machine_func(M_OPEN_DLL, file_name[idx])
		if not fh = 0 then
			return fh
		end if
	end for

	return 0
end function

--**
-- gets the address of a symbol in a shared library or in RAM.
public function define_c_var(atom lib, sequence variable_name)
	return machine_func(M_DEFINE_VAR, {lib, variable_name})
end function

--**
-- defines the characteristics of either a C function, or a machine-code routine that you
-- wish to call as a procedure from your Euphoria program.
public function define_c_proc(object lib, object routine_name, sequence arg_types)
	if atom(routine_name) and not machine:safe_address(routine_name, 1, machine:A_EXECUTE) then
		error:crash("A C function is being defined from Non-executable memory.")
	end if
	return machine_func(M_DEFINE_C, {lib, routine_name, arg_types, 0})
end function

--**
-- defines the characteristics of either a C function, or a machine-code routine that returns
-- a value.
public function define_c_func(object lib, object routine_name, sequence arg_types, atom return_type)
	if atom(routine_name) and not machine:safe_address(routine_name, 1, machine:A_EXECUTE) then
		error:crash("A C function is being defined from Non-executable memory.")
	end if
	return machine_func(M_DEFINE_C, {lib, routine_name, arg_types, return_type})
end function

constant M_CALL_BACK = 52

--**
-- gets a machine address for an Euphoria procedure.
public function call_back(object id)
	return machine_func(M_CALL_BACK, id)
end function

ifdef EU4_0 then
	--**
	-- @nodoc@
	public function sizeof(integer x)
		switch x with fallthru do
			case C_CHAR, C_BYTE, C_UCHAR, C_UBYTE then
				return 1
			case C_SHORT, C_WORD, C_USHORT then
				return 2
			-- In 4.0 everything is x86-32
			case E_OBJECT, E_ATOM, E_SEQUENCE, E_INTEGER then
			case C_INT, C_LONG, C_ULONG then
			case C_SIZE_T, C_POINTER, C_FLOAT then
				return 4
			case C_DOUBLE, C_DWORDLONG, C_LONGLONG then
				return 8
		end switch
	end function
end ifdef

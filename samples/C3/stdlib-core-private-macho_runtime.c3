// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/private/macho_runtime.c3
// Permalink: https://github.com/c3lang/c3c/blob/147dee6/lib/std/core/private/macho_runtime.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::machoruntime @if(env::DARWIN) @private;

struct SegmentCommand64
{
	uint cmd;
	uint cmdsize;
	char[16] segname;
	ulong vmaddr;
	ulong vmsize;
	ulong fileoff;
	ulong filesize;
	uint maxprot;
	uint initprot;
	uint nsects;
	uint flags;
}

struct LoadCommand
{
	uint cmd;
	uint cmdsize;
}

struct Section64
{
	char[16] sectname;
	char[16] segname;
	ulong addr;
	ulong size;
	uint offset;
	uint align;
	uint reloff;
	uint nreloc;
	uint flags;
	uint reserved1;
	uint reserved2;
	uint reserved3;
}

struct MachHeader
{
	uint magic;
	uint cputype;
	uint cpusubtype;
	uint filetype;
	uint ncmds;
	uint sizeofcmds;
	uint flags;
}

struct MachHeader64
{
	inline MachHeader header;
	uint reserved;
}

const LC_SEGMENT_64 = 0x19;

fault MachoSearch
{
	NOT_FOUND
}
fn bool name_cmp(char* a, char[16]* b)
{
	for (usz i = 0; i < 16; i++)
	{
		if (a[i] != (*b)[i]) return false;
		if (a[i] == '\0') return true;
	}
	return false;
}

fn SegmentCommand64*! find_segment(MachHeader* header, char* segname)
{
	LoadCommand* command = (void*)header + MachHeader64.sizeof;
	for (uint i = 0; i < header.ncmds; i++)
	{
		if (command.cmd == LC_SEGMENT_64)
		{
			SegmentCommand64* segment = (SegmentCommand64*)command;
			if (name_cmp(segname, &segment.segname)) return segment;
		}
		command = (void*)command + command.cmdsize;
	}
	return MachoSearch.NOT_FOUND?;
}
fn Section64*! find_section(SegmentCommand64* command, char* sectname)
{
	Section64* section = (void*)command + SegmentCommand64.sizeof;
	for (uint i = 0; i < command.nsects; i++)
	{
		if (name_cmp(sectname, &section.sectname)) return section;
		section++;
	}
	return MachoSearch.NOT_FOUND?;
}

macro find_segment_section_body(MachHeader* header, char* segname, char* sectname, $Type)
{

	Section64*! section = find_section(find_segment(header, segname), sectname);
	if (catch section)
	{
		return $Type[] {};
	}
	$Type* ptr = (void*)header + section.offset;
	return ptr[:section.size / $Type.sizeof];
}

def DyldCallback = fn void (MachHeader* mh, isz vmaddr_slide);

extern fn void _dyld_register_func_for_add_image(DyldCallback);


struct DlInfo
{
	char* dli_fname;
	void* dli_fbase;
	char* dli_sname;
	void* dli_saddr;
}

extern fn void printf(char*, ...);
extern fn int dladdr(MachHeader* mh, DlInfo* dlinfo);
extern fn void* realloc(void* ptr, usz size);
extern fn void* malloc(usz size);
extern fn void free(void* ptr);

def CallbackFn = fn void();
struct Callback
{
	uint priority;
	CallbackFn xtor;
	Callback* next;
}
struct DynamicMethod
{
	void* fn_ptr;
	char* sel;
	union
	{
		DynamicMethod* next;
		TypeId* type;
	}
}

enum StartupState
{
	NOT_STARTED,
	INIT,
	RUN_CTORS,
	READ_DYLIB,
	RUN_DYLIB_CTORS,
	RUN_DTORS,
	SHUTDOWN
}

StartupState runtime_state = NOT_STARTED;

Callback* ctor_first;
Callback* dtor_first;

fn void runtime_startup() @public @export("__c3_runtime_startup")
{
	if (runtime_state != NOT_STARTED) return;
	runtime_state = INIT;
	_dyld_register_func_for_add_image(&dl_reg_callback);
	assert(runtime_state == INIT);
	runtime_state = RUN_CTORS;
	Callback* ctor = ctor_first;
	while (ctor)
	{
		ctor.xtor();
		ctor = ctor.next;
	}
	assert(runtime_state == RUN_CTORS);
	runtime_state = READ_DYLIB;
	ctor_first = null;
}

fn void runtime_finalize() @public @export("__c3_runtime_finalize")
{
	if (runtime_state != READ_DYLIB) return;
	runtime_state = RUN_DTORS;
	Callback* dtor = dtor_first;
	while (dtor)
	{
		dtor.xtor();
		dtor = dtor.next;
	}
	assert(runtime_state == RUN_DTORS);
	runtime_state = SHUTDOWN;
}

fn void append_xxlizer(Callback** ref, Callback* cb)
{
	while (Callback* current = *ref, current)
	{
		if (current.priority > cb.priority)
		{
			cb.next = current;
			break;
		}
		ref = &current.next;
	}
	*ref = cb;
}

struct TypeId
{
	char type;
	TypeId* parentof;
	DynamicMethod* dtable;
	usz sizeof;
	TypeId* inner;
	usz len;
	typeid[*] additional;
}

fn void dl_reg_callback(MachHeader* mh, isz vmaddr_slide)
{
	usz size = 0;
	assert(runtime_state == INIT || runtime_state == READ_DYLIB, "State was %s", runtime_state);
	foreach (&dm : find_segment_section_body(mh, "__DATA", "__c3_dynamic", DynamicMethod))
	{
		TypeId* type = dm.type;
		dm.next = type.dtable;
		type.dtable = dm;
		DynamicMethod* m = dm;
		while (m)
		{
			m = m.next;
		}
	}
	foreach (&cb : find_segment_section_body(mh, "__DATA", "__c3dtor", Callback))
	{
		append_xxlizer(&dtor_first, cb);
	}
	foreach (&cb : find_segment_section_body(mh, "__DATA", "__c3ctor", Callback))
	{
		append_xxlizer(&ctor_first, cb);
	}
	if (runtime_state != READ_DYLIB) return;
	runtime_state = RUN_DYLIB_CTORS;
	Callback* ctor = ctor_first;
	ctor_first = null;
	while (ctor)
	{
		ctor.xtor();
		ctor = ctor.next;
	}
	assert(runtime_state == RUN_DYLIB_CTORS);
	runtime_state = READ_DYLIB;
}

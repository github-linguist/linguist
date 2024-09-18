// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/macos/objc.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/os/macos/objc.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::macos::objc @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

distinct Class = void*;
distinct Method = void*;
distinct Ivar = void*;
distinct Selector = void*;

fault ObjcFailure
{
	CLASS_NOT_FOUND
}

macro ZString Class.name(Class cls) => macos_class_getName(cls);
macro Class Class.superclass(Class cls) => macos_class_getSuperclass(cls);
macro bool Class.responds_to(Class cls, Selector sel) => macos_class_respondsToSelector(cls, sel);
macro Method Class.method(Class cls, Selector name) => macos_class_getClassMethod(cls, name);

macro bool Selector.equals(Selector a, Selector b) => a == b;
macro bool Class.equals(Class a, Class b) => a == b;

macro Class! class_by_name(ZString c)
{
	Class cls = macos_objc_lookUpClass(c);
	return cls ?: ObjcFailure.CLASS_NOT_FOUND?;
}

macro Class[] class_get_list(Allocator allocator = allocator::heap())
{
	int num_classes = macos_objc_getClassList(null, 0);
	if (!num_classes) return {};
	Class[] entries = allocator.new_array(Class, num_classes);
	macos_objc_getClassList(entries.ptr, entries.len);
	return entries;
}

extern fn Class macos_objc_getClass(ZString name) @extern("objc_getClass") @builtin;
extern fn int macos_objc_getClassList(Class* buffer, int buffer_count) @extern("objc_getClassList") @builtin;
extern fn ZString macos_class_getName(Class cls) @extern("class_getName") @builtin;
extern fn Class macos_class_getSuperclass(Class cls) @extern("class_getSuperclass") @builtin;
extern fn Method macos_class_getClassMethod(Class cls, Selector name) @extern("class_getClassMethod") @builtin;
extern fn bool macos_class_respondsToSelector(Class cls, Selector name) @extern("class_respondsToSelector") @builtin;
extern fn Selector macos_sel_registerName(ZString str) @extern("sel_registerName") @builtin;
extern fn Class macos_objc_lookUpClass(ZString name) @extern("objc_lookUpClass") @builtin;



#this example works on Windows
ccall( (:GetDoubleClickTime, "User32"), stdcall,
	Uint, (), )

ccall( (:clock, "libc"), Int32, ())

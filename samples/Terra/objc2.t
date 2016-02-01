local f = assert(io.popen("uname", 'r'))
local s = assert(f:read('*a'))
f:close()

if s~="Darwin\n" then
  print("Warning, not running test b/c this isn't a mac")
else


local OC = require("lib/objc")
local IO = terralib.includec("stdio.h")

struct Rect {
	a : double,
	b : double,
	c : double,
	d : double
}

terra str(data : &uint8)
	return OC.NSString:stringWithUTF8String(data)
end

terra main()
	OC.NSAutoreleasePool:new()
	var app = OC.NSApplication:sharedApplication()
	var rec = Rect {0,0,200,200}
	var window = OC.NSWindow:alloc():initWithContentRect_styleMask_backing_defer(rec,1,2,false)
	window:makeKeyAndOrderFront(nil)
	--[[var img = OC.NSImage:alloc():initByReferencingFile(str("objc2.jpg"))
	var imgView = OC.NSImageView:alloc():initWithFrame(rec)
	imgView:setImage(img)
	window:setContentView(imgView)
	IO.printf("entering run loop\n")--]]
	app:run()
end

terralib.saveobj("objc2", {main = main}, {"-framework","Cocoa"})

--os.execute("./objc2")

end
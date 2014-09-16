(function(utils) {
	// Load C functions declared in utils.loadFuncs
	var shouldLoadCFuncs = true;
	// Expose the C functions to cycript's global scope
	var shouldExposeCFuncs = true;
	// Expose C constants to cycript's global scope
	var shouldExposeConsts = true;
	// Expose functions defined here to cycript's global scope
	var shouldExposeFuncs = true;
	// Which functions to expose
	var funcsToExpose = ["exec", "include", "sizeof", "logify", "apply", "str2voidPtr", "voidPtr2str", "double2voidPtr", "voidPtr2double", "isMemoryReadable", "isObject", "makeStruct"];
	
	// C functions that utils.loadFuncs loads
	var CFuncsDeclarations = [
		// <stdlib.h>
		"void *calloc(size_t num, size_t size)",
		// <string.h>
		"char *strcpy(char *restrict dst, const char *restrict src)",
		"char *strdup(const char *s1)",
		"void* memset(void* dest, int ch, size_t count)",
		// <stdio.h>
		"FILE *fopen(const char *, const char *)",
		"int fclose(FILE *)",
		"size_t fread(void *restrict, size_t, size_t, FILE *restrict)",
		"size_t fwrite(const void *restrict, size_t, size_t, FILE *restrict)",
		// <mach.h>
		"mach_port_t mach_task_self()",
		"kern_return_t task_for_pid(mach_port_name_t target_tport, int pid, mach_port_name_t *tn)",
		"kern_return_t mach_vm_protect(vm_map_t target_task, mach_vm_address_t address, mach_vm_size_t size, boolean_t set_maximum, vm_prot_t new_protection)",
		"kern_return_t mach_vm_write(vm_map_t target_task, mach_vm_address_t address, vm_offset_t data, mach_msg_type_number_t dataCnt)",
		"kern_return_t mach_vm_read(vm_map_t target_task, mach_vm_address_t address, mach_vm_size_t size, vm_offset_t *data, mach_msg_type_number_t *dataCnt)",
	];
	
	/*
		Replacement for eval that can handle @encode etc.
		
		Usage:
			cy# utils.exec("@encode(void *(int, char))")
			@encode(void*(int,char))
	*/
	utils.exec = function(str) {
		var mkdir = @encode(int (const char *, int))(dlsym(RTLD_DEFAULT, "mkdir"));
		var tempnam = @encode(char *(const char *, const char *))(dlsym(RTLD_DEFAULT, "tempnam"));
		var fopen = @encode(void *(const char *, const char *))(dlsym(RTLD_DEFAULT, "fopen"));
		var fclose = @encode(int (void *))(dlsym(RTLD_DEFAULT, "fclose"));
		var fwrite = @encode(int (const char *, int, int, void *))(dlsym(RTLD_DEFAULT, "fwrite"));
		var symlink = @encode(int (const char *, const char *))(dlsym(RTLD_DEFAULT, "symlink"));
		var unlink = @encode(int (const char *))(dlsym(RTLD_DEFAULT, "unlink"));
		var getenv = @encode(const char *(const char *))(dlsym(RTLD_DEFAULT, "getenv"));
		var setenv = @encode(int (const char *, const char *, int))(dlsym(RTLD_DEFAULT, "setenv"));
		
		var libdir = "/usr/lib/cycript0.9";
		var dir = libdir + "/tmp";

		mkdir(dir, 0777);
		
		// This is needed because tempnam seems to ignore the first argument on i386
		var old_tmpdir = getenv("TMPDIR");
		setenv("TMPDIR", dir, 1);

		// No freeing :(
		var f = tempnam(dir, "exec-");
		setenv("TMPDIR", old_tmpdir, 1);
		if(!f) {
			return false;
		}

		symlink(f, f + ".cy");
		
		str = "exports.result = " + str;

		var handle = fopen(f, "w");
		fwrite(str, str.length, 1, handle);
		fclose(handle);
		
		var r;
		var except = null;
		try {
			r = require(f.replace(libdir + "/", ""));
		} catch(e) {
			except = e;
		}

		unlink(f + ".cy");
		unlink(f);
		
		if(except !== null) {
			throw except;
		}

		return r.result;
	};
	
	/*
		Applies known typedefs
		Used in utils.include and utils.makeStruct
		
		Usage:
			cy# utils.applyTypedefs("mach_vm_address_t")
			"uint64_t"
	*/
	utils.applyTypedefs = function(str) {
		var typedefs = {
			"struct": "",
			"restrict": "",
			"FILE": "void",
			"size_t": "uint64_t",
			"uintptr_t": "unsigned long",
			"kern_return_t": "int",
			"mach_port_t": "unsigned int",
			"mach_port_name_t": "unsigned int",
			"vm_offset_t": "unsigned long",
			"vm_size_t": "unsigned long",
			"mach_vm_address_t": "uint64_t",
			"mach_vm_offset_t": "uint64_t",
			"mach_vm_size_t": "uint64_t",
			"vm_map_offset_t": "uint64_t",
			"vm_map_address_t": "uint64_t",
			"vm_map_size_t": "uint64_t",
			"mach_port_context_t": "uint64_t",
			"vm_map_t": "unsigned int",
			"boolean_t": "unsigned int",
			"vm_prot_t": "int",
			"mach_msg_type_number_t": "unsigned int",
			"cpu_type_t": "int",
			"cpu_subtype_t": "int",
			"cpu_threadtype_t": "int",
		};
		
		for(var k in typedefs) {
			str = str.replace(new RegExp("(\\s|\\*|,|\\(|^)" + k + "(\\s|\\*|,|\\)|$)", "g"), "$1" + typedefs[k] + "$2");
		}
		
		return str;
	};
	
	/*
		Parses a C function declaration and returns the function name and cycript type
		If load is true, tries to load it into cycript using utils.exec
		
		Usage:
			cy# var str = "void *calloc(size_t num, size_t size)";
			"void *calloc(size_t num, size_t size)"
			cy# utils.include(str)
			["calloc","@encode(void *(uint64_t num,  uint64_t size))(140735674376857)"]
			cy# var ret = utils.include(str, true)
			["calloc",0x7fff93e0e299]
			cy# ret[1].type
			@encode(void*(unsigned long long int,unsigned long long int))
			cy# ret[1](100, 1)
			0x100444100
	*/
	utils.include = function(str, load) {
		var re = /^\s*([^(]*(?:\s+|\*))(\w*)\s*\(([^)]*)\)\s*;?\s*$/;
		var match = re.exec(str);
		if(!match) {
			return -1;
		}
		var rType = utils.applyTypedefs(match[1]);
		var name = match[2];
		var args = match[3];

		var argsRe = /([^,]+)(?:,|$)/g;
		var argsTypes = [];
		while((match = argsRe.exec(args)) !== null) {
			var type = utils.applyTypedefs(match[1]);
			argsTypes.push(type);
		}
		
		var encodeString = "@encode(";
		encodeString += rType + "(";
		encodeString += argsTypes.join(", ") + "))";

		var fun = dlsym(RTLD_DEFAULT, name);
		if(fun !== null) {
			encodeString += "(" + fun + ")";
			if(load) {
				return [name, utils.exec(encodeString)];
			}
		} else if(load) {
			throw "Function couldn't be found with dlsym!";
		}

		return [name, encodeString];
	};
	
	/*
		Loads the function declaration in the defs array using utils.exec and exposes to cycript's global scope
		Is automatically called if shouldLoadCFuncs is true
	*/
	utils.funcs = {};
	utils.loadfuncs = function(expose) {
		for(var i = 0; i < CFuncsDeclarations.length; i++) {
			try {
				var o = utils.include(CFuncsDeclarations[i], true);
				utils.funcs[o[0]] = o[1];
				if(expose) {
					Cycript.all[o[0]] = o[1];
				}
			} catch(e) {
				system.print("Failed to load function: " + i);
				try {
					system.print(utils.include(CFuncsDeclarations[i]));
				} catch(e2) {
					
				}
			}
		}
	};
	
	/*
		Calculates the size of a type like the C operator sizeof
		
		Usage:
			cy# utils.sizeof(int)
			4
			cy# utils.sizeof(@encode(void *))
			8
			cy# utils.sizeof("mach_vm_address_t")
			8
	*/
	utils.sizeof = function(type) {
		if(typeof type === "string") {
			type = utils.applyTypedefs(type);
			type = utils.exec("@encode(" + type + ")");
		}
		
		// (const) char * has "infinite" preceision
		if(type.toString().slice(-1) === "*") {
			return utils.sizeof(@encode(void *));
		}
		
		// float and double
		if(type.toString() === @encode(float).toString()) {
			return 4;
		} else if (type.toString() === @encode(double).toString()) {
			return 8;
		}

		var typeInstance = type(0);
		
		if(typeInstance instanceof Object) {
			// Arrays
			if("length" in typeInstance) {
				return typeInstance.length * utils.sizeof(typeInstance.type);
			}
			
			// Structs
			if(typeInstance.toString() === "[object Struct]") {
				var typeStr = type.toString();
				var arrayTypeStr = "[2" + typeStr + "]";
				var arrayType = new Type(arrayTypeStr);
				
				var arrayInstance = new arrayType;
				
				return @encode(void *)(&(arrayInstance[1])) - @encode(void *)(&(arrayInstance[0]));
			}
		}
		
		for(var i = 0; i < 5; i++) {
			var maxSigned = Math.pow(2, 8 * Math.pow(2, i) - 1) - 1;
			if(i === 3) {
				// Floating point fix ;^)
				maxSigned /= 1000;
			}

			// can't use !== or sizeof(void *) === 0.5
			if(type(maxSigned) != maxSigned) {
				return Math.pow(2, i - 1);
			}
		}
	};
	
	/*
		Logs a specific message sent to an instance of a class like logify.pl in theos
		Requires Cydia Substrate (com.saurik.substrate.MS) and NSLog (org.cycript.NSLog) modules
		Returns the old message returned by MS.hookMessage (Note: this is not just the old message!)
		
		Usage:
			cy# var oldm = utils.logify(objc_getMetaClass(NSNumber), @selector(numberWithDouble:))
			...
			cy# var n = [NSNumber numberWithDouble:1.5]
			2014-07-28 02:26:39.805 cycript[71213:507] +[<NSNumber: 0x10032d0c4> numberWithDouble:1.5]
			2014-07-28 02:26:39.806 cycript[71213:507]  = 1.5
			@1.5
	*/
	utils.logify = function(cls, sel) {
		@import com.saurik.substrate.MS;
		@import org.cycript.NSLog;
		
		var oldm = {};
		
		MS.hookMessage(cls, sel, function() {
			var args = [].slice.call(arguments);
			
			var selFormat = sel.toString().replace(/:/g, ":%@ ").trim();
			var logFormat = "%@[<%@: 0x%@> " + selFormat + "]";
			
			var standardArgs = [logFormat, class_isMetaClass(cls)? "+": "-", cls.toString(), (&this).valueOf().toString(16)];
			var logArgs = standardArgs.concat(args);
			
			NSLog.apply(null, logArgs);
			
			var r = oldm->apply(this, arguments);
			
			if(r !== undefined) {
				NSLog(" = %@", r);
			}
			
			return r;
		}, oldm);
		
		return oldm;
	};
	
	/*
		Calls a C function by providing its name and arguments
		Doesn't support structs
		Return value is always a void pointer
		
		Usage:
			cy# utils.apply("printf", ["%s %.3s, %d -> %c, float: %f\n", "foo", "barrrr", 97, 97, 1.5])
			foo bar, 97 -> a, float: 1.500000
			0x22
	*/
	utils.apply = function(fun, args) {
		if(!(args instanceof Array)) {
			throw "Args needs to be an array!";
		}
		
		var argc = args.length;
		var voidPtr = @encode(void *);
		var argTypes = [];
		for(var i = 0; i < argc; i++) {
			var argType = voidPtr;
			
			var arg = args[i];
			if(typeof arg === "string") {
				argType = @encode(char *);
			}
			if(typeof arg === "number" && arg % 1 !== 0) {
				argType = @encode(double);
			}
			
			argTypes.push(argType);
		}
		
		var type = voidPtr.functionWith.apply(voidPtr, argTypes);
		
		if(typeof fun === "string") {
			fun = dlsym(RTLD_DEFAULT, fun);
		}
		
		if(!fun) {
			throw "Function not found!";
		}

		return type(fun).apply(null, args);
	};
	
	/*
		Converts a string (char *) to a void pointer (void *)
		You can't cast to strings to void pointers and vice versa in cycript. Blame saurik.
		
		Usage:
			cy# var voidPtr = utils.str2voidPtr("foobar")
			0x100331590
			cy# utils.voidPtr2str(voidPtr)
			"foobar"
	*/
	utils.str2voidPtr = function(str) {
		var strdup = @encode(void *(char *))(dlsym(RTLD_DEFAULT, "strdup"));
		return strdup(str);
	};
	
	/*
		The inverse function of str2voidPtr
	*/
	utils.voidPtr2str = function(voidPtr) {
		var strdup = @encode(char *(void *))(dlsym(RTLD_DEFAULT, "strdup"));
		return strdup(voidPtr);
	};
	
	/*
		Converts a double into a void pointer
		This can be used to view the binary representation of a floating point number
		
		Usage:
			cy# var n = utils.double2voidPtr(-1.5)
			0xbff8000000000000
			cy# utils.voidPtr2double(n)
			-1.5
	*/
	utils.double2voidPtr = function(n) {
		var doublePtr = new double;
		*doublePtr = n;
		
		var voidPtrPtr = @encode(void **)(doublePtr);
		
		return *voidPtrPtr;
	};
	
	/*
		The inverse function of double2voidPtr
	*/
	utils.voidPtr2double = function(voidPtr) {
		var voidPtrPtr = new @encode(void **);
		*voidPtrPtr = voidPtr;
		
		var doublePtr = @encode(double *)(voidPtrPtr);
		
		return *doublePtr;
	};
	
	/*
		Determines in a safe way if a memory location is readable
		
		Usage:
			cy# utils.isMemoryReadable(0)
			false
			cy# utils.isMemoryReadable(0x1337)
			false
			cy# utils.isMemoryReadable(NSObject)
			true
			cy# var a = malloc(100); utils.isMemoryReadable(a)
			true
	*/
	utils.isMemoryReadable = function(ptr) {
		if(typeof ptr === "string") {
			return true;
		}
		
		var fds = new @encode(int [2]);
		utils.apply("pipe", [fds]);
		var result = utils.apply("write", [fds[1], ptr, 1]) == 1;
		
		utils.apply("close", [fds[0]]);
		utils.apply("close", [fds[1]]);
		
		return result;
	};
	
	/*
		Determines in a safe way if the memory location contains an Objective-C object

		Usage:
			cy# utils.isObject(0)
			false
			cy# utils.isObject(0x1337)
			false
			cy# utils.isObject(NSObject)
			true
			cy# utils.isObject(objc_getMetaClass(NSObject))
			true
			cy# utils.isObject([new NSObject init])
			true
			cy# var a = malloc(100); utils.isObject(a)
			false
			cy# *@encode(void **)(a) = NSObject; utils.isObject(a)
			true
	*/
	utils.isObject = function(obj) {
		obj = @encode(void *)(obj);
		var lastObj = -1;
		
		function objc_isa_ptr(obj) {
			// See http://www.sealiesoftware.com/blog/archive/2013/09/24/objc_explain_Non-pointer_isa.html
			var objc_debug_isa_class_mask = 0x00000001fffffffa;
			obj = (obj & 1)? (obj & objc_debug_isa_class_mask): obj;
			
			if((obj & (utils.sizeof(@encode(void *)) - 1)) != 0) {
				return null;
			} else {
				return obj;
			}
		}
		
		function ptrValue(obj) {
			return obj? obj.valueOf(): null;
		}
		
		var foundMetaClass = false;
		
		for(obj = objc_isa_ptr(obj); utils.isMemoryReadable(obj); ) {
			obj = *@encode(void **)(obj);
			
			if(ptrValue(obj) == ptrValue(lastObj)) {
				foundMetaClass = true;
				break;
			}
			
			lastObj = obj;
		}
		
		if(!foundMetaClass) {
			return false;
		}
		
		if(lastObj === -1 || lastObj === null) {
			return false;
		}
		
		var obj_class = objc_isa_ptr(@encode(void **)(obj)[1]);
		
		if(!utils.isMemoryReadable(obj_class)) {
			return false;
		}
		
		var metaclass = objc_isa_ptr(@encode(void **)(obj_class)[0]);
		var superclass = objc_isa_ptr(@encode(void **)(obj_class)[1]);
		
		return ptrValue(obj) == ptrValue(metaclass) && superclass == null;
	};
	
	/*
		Creates a cycript struct type from a C struct definition
		
		Usage:
			cy# var foo = makeStruct("int a; short b; char c; uint64_t d; double e;", "foo");
			@encode(foo)
			cy# var f = new foo
			&{a:0,b:0,c:0,d:0,e:0}
			cy# f->a = 100; f
			&{a:100,b:0,c:0,d:0,e:0}
			cy# *@encode(int *)(f)
			100
	*/
	utils.makeStruct = function(str, name) {		
		var fieldRe = /(?:\s|\n)*([^;]+\s*(?:\s|\*))([^;]+)\s*;/g;
		
		if(!name) {
			name = "struct" + Math.floor(Math.random() * 100000);
		}
		var typeStr = "{" + name + "=";
		
		while((match = fieldRe.exec(str)) !== null) {
			var fieldType = utils.applyTypedefs(match[1]);
			var fieldName = match[2];
			var encodedType = utils.exec("@encode(" + fieldType + ")").toString();
			
			typeStr += '"' + fieldName + '"' + encodedType;
		}
		
		typeStr += "}";
		
		return new Type(typeStr);
	};
	
	// Various constants
	utils.constants = {
		VM_PROT_NONE:       0x0,
		VM_PROT_READ:       0x1,
		VM_PROT_WRITE:      0x2,
		VM_PROT_EXECUTE:    0x4,
		VM_PROT_NO_CHANGE:  0x8,
		VM_PROT_COPY:       0x10,
		VM_PROT_WANTS_COPY: 0x10,
		VM_PROT_IS_MASK:    0x40,
	};
	var c = utils.constants;
	c.VM_PROT_DEFAULT = c.VM_PROT_READ | c.VM_PROT_WRITE;
	c.VM_PROT_ALL =     c.VM_PROT_READ | c.VM_PROT_WRITE | c.VM_PROT_EXECUTE;
	
	if(shouldExposeConsts) {
		for(var k in c) {
			Cycript.all[k] = c[k];
		}
	}
	
	if(shouldExposeFuncs) {
		for(var i = 0; i < funcsToExpose.length; i++) {
			var name = funcsToExpose[i];
			Cycript.all[name] = utils[name];
		}
	}
	
	if(shouldLoadCFuncs) {
		utils.loadfuncs(shouldExposeCFuncs);
	}
})(exports);

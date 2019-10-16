package main

import "core:fmt"
import "core:mem"
import "core:os"
import "core:reflect"
import "intrinsics"

when os.OS == "windows" {
	import "core:thread"
}

/*
    The Odin programming language is fast, concise, readable, pragmatic and open sourced. It is designed with the intent of replacing C with the following goals:
     * simplicity
     * high performance
     * built for modern systems
     * joy of programming

    # Installing Odin
    Getting Started - https://odin-lang.org/docs/install/
        Instructions for downloading and install the Odin compiler and libraries.

    # Learning Odin
    Overview of Odin - https://odin-lang.org/docs/overview/
        An overview of the Odin programming language.
    Frequently Asked Questions (FAQ) - https://odin-lang.org/docs/faq/
        Answers to common questions about Odin.
*/

@(link_name="extra_general_stuff")
extra_general_stuff :: proc() {
	fmt.println("# extra_general_stuff");
	{ // `do` for inline statements rather than block
		foo :: proc() do fmt.println("Foo!");
		if   false do foo();
		for  false do foo();
		when false do foo();

		if false do foo();
		else     do foo();
	}

	{ // Removal of `++` and `--` (again)
		x: int;
		x += 1;
		x -= 1;
	}
	{ // Casting syntaxes
		i := i32(137);
		ptr := &i;

		_ = (^f32)(ptr);
		// ^f32(ptr) == ^(f32(ptr))
		_ = cast(^f32)ptr;

		_ = (^f32)(ptr)^;
		_ = (cast(^f32)ptr)^;
	}

	/*
	 * Remove *_val_of built-in procedures
	 * size_of, align_of, offset_of
	 * type_of, type_info_of, typeid_of
	 */

	{ // `expand_to_tuple` built-in procedure
		Foo :: struct {
			x: int,
			b: bool,
		};
		f := Foo{137, true};
		x, b := expand_to_tuple(f);
		fmt.println(f);
		fmt.println(x, b);
		fmt.println(expand_to_tuple(f));
	}

	{
		// .. open range
		// ..< half-closed range

		for in 0..2  {} // 0, 1, 2
		for in 0..<2 {} // 0, 1
	}

	{ // Multiple sized booleans

		x0: bool; // default
		x1: b8  = true;
		x2: b16 = false;
		x3: b32 = true;
		x4: b64 = false;

		fmt.printf("x0: %T = %v;\n", x0, x0);
		fmt.printf("x1: %T = %v;\n", x1, x1);
		fmt.printf("x2: %T = %v;\n", x2, x2);
		fmt.printf("x3: %T = %v;\n", x3, x3);
		fmt.printf("x4: %T = %v;\n", x4, x4);

		// Having specific sized booleans is very useful when dealing with foreign code
		// and to enforce specific alignment for a boolean, especially within a struct
	}

	{ // `distinct` types
		// Originally, all type declarations would create a distinct type unless #type_alias was present.
		// Now the behaviour has been reversed. All type declarations create a type alias unless `distinct` is present.
		// If the type expression is `struct`, `union`, `enum`, `proc`, or `bit_field`, the types will always been distinct.

		Int32 :: i32;
		#assert(Int32 == i32);

		My_Int32 :: distinct i32;
		#assert(My_Int32 != i32);

		My_Struct :: struct{x: int};
		#assert(My_Struct != struct{x: int});

		My_Struct2 :: My_Struct;
		#assert(My_Struct2 == My_Struct);
	}

	{
		X :: 123;
		when #defined(X) {
			fmt.println("X is defined");
		} else {
			fmt.println("X is not defined");
		}
		when #defined(Y) {
			fmt.println("Y is defined");
		} else {
			fmt.println("Y is not defined");
		}
	}

	{ // Labelled control blocks
		block: {
			if true {
				fmt.println("break block;");
				break block;
			}
		}

		{
			branch: if true {
				fmt.println("break branch;");
				break branch;
			}
		}

		{
			loop: for true {
				fmt.println("break loop;");
				break loop;
			}
		}

		{
			cases: switch {
			case:
				fmt.println("break cases;");
				break cases;
			}
		}

	}
}


union_type :: proc() {
	fmt.println("\n# union_type");
	{
		val: union{int, bool};
		val = 137;
		if i, ok := val.(int); ok {
			fmt.println(i);
		}
		val = true;
		fmt.println(val);

		val = nil;

		switch v in val {
		case int:  fmt.println("int",  v);
		case bool: fmt.println("bool", v);
		case:      fmt.println("nil");
		}
	}
	{
		// There is a duality between `any` and `union`
		// An `any` has a pointer to the data and allows for any type (open)
		// A `union` has as binary blob to store the data and allows only certain types (closed)
		// The following code is with `any` but has the same syntax
		val: any;
		val = 137;
		if i, ok := val.(int); ok {
			fmt.println(i);
		}
		val = true;
		fmt.println(val);

		val = nil;

		switch v in val {
		case int:  fmt.println("int",  v);
		case bool: fmt.println("bool", v);
		case:      fmt.println("nil");
		}
	}

	Vector3 :: distinct [3]f32;
	Quaternion :: distinct quaternion128;

	// More realistic examples
	{
		// NOTE(bill): For the above basic examples, you may not have any
		// particular use for it. However, my main use for them is not for these
		// simple cases. My main use is for hierarchical types. Many prefer
		// subtyping, embedding the base data into the derived types. Below is
		// an example of this for a basic game Entity.

		Entity :: struct {
			id:          u64,
			name:        string,
			position:    Vector3,
			orientation: Quaternion,

			derived: any,
		};

		Frog :: struct {
			using entity: Entity,
			jump_height:  f32,
		};

		Monster :: struct {
			using entity: Entity,
			is_robot:     bool,
			is_zombie:    bool,
		};

		// See `parametric_polymorphism` procedure for details
		new_entity :: proc($T: typeid) -> ^Entity {
			t := new(T);
			t.derived = t^;
			return t;
		}

		entity := new_entity(Monster);

		switch e in entity.derived {
		case Frog:
			fmt.println("Ribbit");
		case Monster:
			if e.is_robot  do fmt.println("Robotic");
			if e.is_zombie do fmt.println("Grrrr!");
			fmt.println("I'm a monster");
		}
	}

	{
		// NOTE(bill): A union can be used to achieve something similar. Instead
		// of embedding the base data into the derived types, the derived data
		// in embedded into the base type. Below is the same example of the
		// basic game Entity but using an union.

		Entity :: struct {
			id:          u64,
			name:        string,
			position:    Vector3,
			orientation: Quaternion,

			derived: union {Frog, Monster},
		};

		Frog :: struct {
			using entity: ^Entity,
			jump_height:  f32,
		};

		Monster :: struct {
			using entity: ^Entity,
			is_robot:     bool,
			is_zombie:    bool,
		};

		// See `parametric_polymorphism` procedure for details
		new_entity :: proc($T: typeid) -> ^Entity {
			t := new(Entity);
			t.derived = T{entity = t};
			return t;
		}

		entity := new_entity(Monster);

		switch e in entity.derived {
		case Frog:
			fmt.println("Ribbit");
		case Monster:
			if e.is_robot  do fmt.println("Robotic");
			if e.is_zombie do fmt.println("Grrrr!");
		}

		// NOTE(bill): As you can see, the usage code has not changed, only its
		// memory layout. Both approaches have their own advantages but they can
		// be used together to achieve different results. The subtyping approach
		// can allow for a greater control of the memory layout and memory
		// allocation, e.g. storing the derivatives together. However, this is
		// also its disadvantage. You must either preallocate arrays for each
		// derivative separation (which can be easily missed) or preallocate a
		// bunch of "raw" memory; determining the maximum size of the derived
		// types would require the aid of metaprogramming. Unions solve this
		// particular problem as the data is stored with the base data.
		// Therefore, it is possible to preallocate, e.g. [100]Entity.

		// It should be noted that the union approach can have the same memory
		// layout as the any and with the same type restrictions by using a
		// pointer type for the derivatives.

		/*
			Entity :: struct {
				...
				derived: union{^Frog, ^Monster},
			}

			Frog :: struct {
				using entity: Entity,
				...
			}
			Monster :: struct {
				using entity: Entity,
				...

			}
			new_entity :: proc(T: type) -> ^Entity {
				t := new(T);
				t.derived = t;
				return t;
			}
		*/
	}
}

parametric_polymorphism :: proc() {
	fmt.println("\n# parametric_polymorphism");

	print_value :: proc(value: $T) {
		fmt.printf("print_value: %T %v\n", value, value);
	}

	v1: int    = 1;
	v2: f32    = 2.1;
	v3: f64    = 3.14;
	v4: string = "message";

	print_value(v1);
	print_value(v2);
	print_value(v3);
	print_value(v4);

	fmt.println();

	add :: proc(p, q: $T) -> T {
		x: T = p + q;
		return x;
	}

	a := add(3, 4);
	fmt.printf("a: %T = %v\n", a, a);

	b := add(3.2, 4.3);
	fmt.printf("b: %T = %v\n", b, b);

	// This is how `new` is implemented
	alloc_type :: proc($T: typeid) -> ^T {
		t := cast(^T)alloc(size_of(T), align_of(T));
		t^ = T{}; // Use default initialization value
		return t;
	}

	copy_slice :: proc(dst, src: []$T) -> int {
		n := min(len(dst), len(src));
		if n > 0 {
			mem.copy(&dst[0], &src[0], n*size_of(T));
		}
		return n;
	}

	double_params :: proc(a: $A, b: $B) -> A {
		return a + A(b);
	}

	fmt.println(double_params(12, 1.345));



	{ // Polymorphic Types and Type Specialization
		Table_Slot :: struct(Key, Value: typeid) {
			occupied: bool,
			hash:     u32,
			key:      Key,
			value:    Value,
		};
		TABLE_SIZE_MIN :: 32;
		Table :: struct(Key, Value: typeid) {
			count:     int,
			allocator: mem.Allocator,
			slots:     []Table_Slot(Key, Value),
		};

		// Only allow types that are specializations of a (polymorphic) slice
		make_slice :: proc($T: typeid/[]$E, len: int) -> T {
			return make(T, len);
		}

		// Only allow types that are specializations of `Table`
		allocate :: proc(table: ^$T/Table, capacity: int) {
			c := context;
			if table.allocator.procedure != nil do c.allocator = table.allocator;
			context = c;

			table.slots = make_slice(type_of(table.slots), max(capacity, TABLE_SIZE_MIN));
		}

		expand :: proc(table: ^$T/Table) {
			c := context;
			if table.allocator.procedure != nil do c.allocator = table.allocator;
			context = c;

			old_slots := table.slots;
			defer delete(old_slots);

			cap := max(2*len(table.slots), TABLE_SIZE_MIN);
			allocate(table, cap);

			for s in old_slots do if s.occupied {
				put(table, s.key, s.value);
			}
		}

		// Polymorphic determination of a polymorphic struct
		// put :: proc(table: ^$T/Table, key: T.Key, value: T.Value) {
		put :: proc(table: ^Table($Key, $Value), key: Key, value: Value) {
			hash := get_hash(key); // Ad-hoc method which would fail in a different scope
			index := find_index(table, key, hash);
			if index < 0 {
				if f64(table.count) >= 0.75*f64(len(table.slots)) {
					expand(table);
				}
				assert(table.count <= len(table.slots));

				index = int(hash % u32(len(table.slots)));

				for table.slots[index].occupied {
					if index += 1; index >= len(table.slots) {
						index = 0;
					}
				}

				table.count += 1;
			}

			slot := &table.slots[index];
			slot.occupied = true;
			slot.hash     = hash;
			slot.key      = key;
			slot.value    = value;
		}


		// find :: proc(table: ^$T/Table, key: T.Key) -> (T.Value, bool) {
		find :: proc(table: ^Table($Key, $Value), key: Key) -> (Value, bool) {
			hash := get_hash(key);
			index := find_index(table, key, hash);
			if index < 0 {
				return Value{}, false;
			}
			return table.slots[index].value, true;
		}

		find_index :: proc(table: ^Table($Key, $Value), key: Key, hash: u32) -> int {
			if len(table.slots) <= 0 do return -1;

			index := int(hash % u32(len(table.slots)));
			for table.slots[index].occupied {
				if table.slots[index].hash == hash {
					if table.slots[index].key == key {
						return index;
					}
				}

				if index += 1; index >= len(table.slots) {
					index = 0;
				}
			}

			return -1;
		}

		get_hash :: proc(s: string) -> u32 { // fnv32a
			h: u32 = 0x811c9dc5;
			for i in 0..<len(s) {
				h = (h ~ u32(s[i])) * 0x01000193;
			}
			return h;
		}


		table: Table(string, int);

		for i in 0..36 do put(&table, "Hellope", i);
		for i in 0..42 do put(&table, "World!",  i);

		found, _ := find(&table, "Hellope");
		fmt.printf("`found` is %v\n", found);

		found, _ = find(&table, "World!");
		fmt.printf("`found` is %v\n", found);

		// I would not personally design a hash table like this in production
		// but this is a nice basic example
		// A better approach would either use a `u64` or equivalent for the key
		// and let the user specify the hashing function or make the user store
		// the hashing procedure with the table
	}

	{ // Parametric polymorphic union
		Error :: enum {
			Foo0,
			Foo1,
			Foo2,
			Foo3,
		};
		Para_Union :: union(T: typeid) {T, Error};
		r: Para_Union(int);
		fmt.println(typeid_of(type_of(r)));

		fmt.println(r);
		r = 123;
		fmt.println(r);
		r = Error.Foo0; // r = .Foo0; is allow too, see implicit selector expressions below
		fmt.println(r);
	}

	{ // Polymorphic names
		foo :: proc($N: $I, $T: typeid) -> (res: [N]T) {
			// `N` is the constant value passed
			// `I` is the type of N
			// `T` is the type passed
			fmt.printf("Generating an array of type %v from the value %v of type %v\n",
					   typeid_of(type_of(res)), N, typeid_of(I));
			for i in 0..<N {
				res[i] = T(i*i);
			}
			return;
		}

		T :: int;
		array := foo(4, T);
		for v, i in array {
			assert(v == T(i*i));
		}

		// Matrix multiplication
		mul :: proc(a: [$M][$N]$T, b: [N][$P]T) -> (c: [M][P]T) {
			for i in 0..<M {
				for j in 0..<P {
					for k in 0..<N {
						c[i][j] += a[i][k] * b[k][j];
					}
				}
			}
			return;
		}

		x := [2][3]f32{
			{1, 2, 3},
			{3, 2, 1},
		};
		y := [3][2]f32{
			{0, 8},
			{6, 2},
			{8, 4},
		};
		z := mul(x, y);
		assert(z == {{36, 24}, {20, 32}});
	}
}




prefix_table := [?]string{
	"White",
	"Red",
	"Green",
	"Blue",
	"Octarine",
	"Black",
};

threading_example :: proc() {
	when os.OS == "windows" {
		fmt.println("\n# threading_example");

		worker_proc :: proc(t: ^thread.Thread) -> int {
			for iteration in 1..5 {
				fmt.printf("Thread %d is on iteration %d\n", t.user_index, iteration);
				fmt.printf("`%s`: iteration %d\n", prefix_table[t.user_index], iteration);
				// win32.sleep(1);
			}
			return 0;
		}

		threads := make([dynamic]^thread.Thread, 0, len(prefix_table));
		defer delete(threads);

		for in prefix_table {
			if t := thread.create(worker_proc); t != nil {
				t.init_context = context;
				t.use_init_context = true;
				t.user_index = len(threads);
				append(&threads, t);
				thread.start(t);
			}
		}

		for len(threads) > 0 {
			for i := 0; i < len(threads); /**/ {
				if t := threads[i]; thread.is_done(t) {
					fmt.printf("Thread %d is done\n", t.user_index);
					thread.destroy(t);

					ordered_remove(&threads, i);
				} else {
					i += 1;
				}
			}
		}
	}
}

array_programming :: proc() {
	fmt.println("\n# array_programming");
	{
		a := [3]f32{1, 2, 3};
		b := [3]f32{5, 6, 7};
		c := a * b;
		d := a + b;
		e := 1 +  (c - d) / 2;
		fmt.printf("%.1f\n", e); // [0.5, 3.0, 6.5]
	}

	{
		a := [3]f32{1, 2, 3};
		b := swizzle(a, 2, 1, 0);
		assert(b == [3]f32{3, 2, 1});

		c := swizzle(a, 0, 0);
		assert(c == [2]f32{1, 1});
		assert(c == 1);
	}

	{
		Vector3 :: distinct [3]f32;
		a := Vector3{1, 2, 3};
		b := Vector3{5, 6, 7};
		c := (a * b)/2 + 1;
		d := c.x + c.y + c.z;
		fmt.printf("%.1f\n", d); // 22.0

		cross :: proc(a, b: Vector3) -> Vector3 {
			i := swizzle(a, 1, 2, 0) * swizzle(b, 2, 0, 1);
			j := swizzle(a, 2, 0, 1) * swizzle(b, 1, 2, 0);
			return i - j;
		}

		blah :: proc(a: Vector3) -> f32 {
			return a.x + a.y + a.z;
		}

		x := cross(a, b);
		fmt.println(x);
		fmt.println(blah(x));
	}
}

named_proc_return_parameters :: proc() {
	fmt.println("\n# named proc return parameters");

	foo0 :: proc() -> int {
		return 123;
	}
	foo1 :: proc() -> (a: int) {
		a = 123;
		return;
	}
	foo2 :: proc() -> (a, b: int) {
		// Named return values act like variables within the scope
		a = 321;
		b = 567;
		return b, a;
	}
	fmt.println("foo0 =", foo0()); // 123
	fmt.println("foo1 =", foo1()); // 123
	fmt.println("foo2 =", foo2()); // 567 321
}


using_enum :: proc() {
	fmt.println("\n# using enum");

	using Foo :: enum {A, B, C};

	f0 := A;
	f1 := B;
	f2 := C;
	fmt.println(f0, f1, f2);
	fmt.println(len(Foo));
}

map_type :: proc() {
	fmt.println("\n# map type");

	// enums of type u16, u32, i16 & i32 also work
	Enum_u8 :: enum u8 {
		A = 0,
		B = 1 << 8 - 1,
	};
	Enum_u64 :: enum u64 {
		A = 0,
		B = 1 << 64 - 1,
	};
	Enum_i8 :: enum i8 {
		A = 0,
		B = -(1 << 7),
	};
	Enum_i64 :: enum i64 {
		A = 0,
		B = -(1 << 63),
	};

	map_u8: map[Enum_u8]u8;
	map_u8[Enum_u8.A] = u8(Enum_u8.B);
	assert(map_u8[Enum_u8.A] == u8(Enum_u8.B));
	fmt.println(map_u8);

	map_u64: map[Enum_u64]u64;
	map_u64[Enum_u64.A] = u64(Enum_u64.B);
	assert(map_u64[Enum_u64.A] == u64(Enum_u64.B));
	fmt.println(map_u64);

	map_i8: map[Enum_i8]i8;
	map_i8[Enum_i8.A] = i8(Enum_i8.B);
	assert(map_i8[Enum_i8.A] == i8(Enum_i8.B));
	fmt.println(map_i8);

	map_i64: map[Enum_i64]i64;
	map_i64[Enum_i64.A] = i64(Enum_i64.B);
	assert(map_i64[Enum_i64.A] == i64(Enum_i64.B));
	fmt.println(map_i64);

	demo_struct :: struct {
		member: Enum_i64,
	};

	map_string: map[string]demo_struct;
	map_string["Hellope!"] = demo_struct{Enum_i64.B};
	assert(map_string["Hellope!"].member == Enum_i64.B);
	assert("Hellope?" notin map_string);
	fmt.println(map_string);
	fmt.println("Hellope! in map_string:", "Hellope!" in map_string);
	fmt.println("Hellope? in map_string:", "Hellope?" in map_string);

}

implicit_selector_expression :: proc() {
	fmt.println("\n# implicit selector expression");

	Foo :: enum {A, B, C};

	f: Foo;
	f = .A;

	BAR :: bit_set[Foo]{.B, .C};

	switch f {
	case .A:
		fmt.println("HERE");
	case .B:
		fmt.println("NEVER");
	case .C:
		fmt.println("FOREVER");
	}

	my_map := make(map[Foo]int);
	defer delete(my_map);

	my_map[.A] = 123;
	my_map[Foo.B] = 345;

	fmt.println(my_map[.A] + my_map[Foo.B] + my_map[.C]);
}

explicit_procedure_overloading :: proc() {
	fmt.println("\n# explicit procedure overloading");

	add_ints :: proc(a, b: int) -> int {
		x := a + b;
		fmt.println("add_ints", x);
		return x;
	}
	add_floats :: proc(a, b: f32) -> f32 {
		x := a + b;
		fmt.println("add_floats", x);
		return x;
	}
	add_numbers :: proc(a: int, b: f32, c: u8) -> int {
		x := int(a) + int(b) + int(c);
		fmt.println("add_numbers", x);
		return x;
	}

	add :: proc{add_ints, add_floats, add_numbers};

	add(int(1), int(2));
	add(f32(1), f32(2));
	add(int(1), f32(2), u8(3));

	add(1, 2);     // untyped ints coerce to int tighter than f32
	add(1.0, 2.0); // untyped floats coerce to f32 tighter than int
	add(1, 2, 3);  // three parameters

	// Ambiguous answers
	// add(1.0, 2);
	// add(1, 2.0);
}

complete_switch :: proc() {
	fmt.println("\n# complete_switch");
	{ // enum
		using Foo :: enum {
			A,
			B,
			C,
			D,
		};

		b := Foo.B;
		f := Foo.A;
		#complete switch f {
		case A: fmt.println("A");
		case B: fmt.println("B");
		case C: fmt.println("C");
		case D: fmt.println("D");
		case:   fmt.println("?");
		}

		_ = b;
	}
	{ // union
		Foo :: union {int, bool};
		f: Foo = 123;
		#complete switch in f {
		case int:  fmt.println("int");
		case bool: fmt.println("bool");
		case:
		}
	}
}

cstring_example :: proc() {
	fmt.println("\n# cstring_example");

	W :: "Hellope";
	X :: cstring(W);
	Y :: string(X);

	w := W;
	_ = w;
	x: cstring = X;
	y: string = Y;
	z := string(x);
	fmt.println(x, y, z);
	fmt.println(len(x), len(y), len(z));
	fmt.println(len(W), len(X), len(Y));
	// IMPORTANT NOTE for cstring variables
	// len(cstring) is O(N)
	// cast(string)cstring is O(N)
}

deprecated_attribute :: proc() {
	@(deprecated="Use foo_v2 instead")
	foo_v1 :: proc(x: int) {
		fmt.println("foo_v1");
	}
	foo_v2 :: proc(x: int) {
		fmt.println("foo_v2");
	}

	// NOTE: Uncomment to see the warning messages
	// foo_v1(1);
}

bit_set_type :: proc() {
	fmt.println("\n# bit_set_type");

	{
		using Day :: enum {
			Sunday,
			Monday,
			Tuesday,
			Wednesday,
			Thursday,
			Friday,
			Saturday,
		};

		Days :: distinct bit_set[Day];
		WEEKEND :: Days{Sunday, Saturday};

		d: Days;
		d = {Sunday, Monday};
		e := d | WEEKEND;
		e |= {Monday};
		fmt.println(d, e);

		ok := Saturday in e; // `in` is only allowed for `map` and `bit_set` types
		fmt.println(ok);
		if Saturday in e {
			fmt.println("Saturday in", e);
		}
		X :: Saturday in WEEKEND; // Constant evaluation
		fmt.println(X);
		fmt.println("Cardinality:", card(e));
	}
	{
		x: bit_set['A'..'Z'];
		#assert(size_of(x) == size_of(u32));
		y: bit_set[0..8; u16];
		fmt.println(typeid_of(type_of(x))); // bit_set[A..Z]
		fmt.println(typeid_of(type_of(y))); // bit_set[0..8; u16]

		incl(&x, 'F');
		assert('F' in x);
		excl(&x, 'F');
		assert('F' notin x);

		y |= {1, 4, 2};
		assert(2 in y);
	}
	{
		Letters :: bit_set['A'..'Z'];
		a := Letters{'A', 'B'};
		b := Letters{'A', 'B', 'C', 'D', 'F'};
		c := Letters{'A', 'B'};

		assert(a <= b); // 'a' is a subset of 'b'
		assert(b >= a); // 'b' is a superset of 'a'
		assert(a < b);  // 'a' is a strict subset of 'b'
		assert(b > a);  // 'b' is a strict superset of 'a'

		assert(!(a < c)); // 'a' is a not strict subset of 'c'
		assert(!(c > a)); // 'c' is a not strict superset of 'a'
	}
}

diverging_procedures :: proc() {
	fmt.println("\n# diverging_procedures");

	// Diverging procedures may never return
	foo :: proc() -> ! {
		fmt.println("I'm a diverging procedure");
	}

	foo();
}

deferred_procedure_associations :: proc() {
	fmt.println("\n# deferred_procedure_associations");

	@(deferred_out=closure)
	open :: proc(s: string) -> bool {
		fmt.println(s);
		return true;
	}

	closure :: proc(ok: bool) {
		fmt.println("Goodbye?", ok);
	}

	if open("Welcome") {
		fmt.println("Something in the middle, mate.");
	}
}

reflection :: proc() {
	fmt.println("\n# reflection");

	Foo :: struct {
		x: int    `tag1`,
		y: string `json:"y_field"`,
		z: bool, // no tag
	};

	id := typeid_of(Foo);
	names := reflect.struct_field_names(id);
	types := reflect.struct_field_types(id);
	tags  := reflect.struct_field_tags(id);

	assert(len(names) == len(types) && len(names) == len(tags));

	fmt.println("Foo :: struct {");
	for tag, i in tags {
		name, type := names[i], types[i];
		if tag != "" {
			fmt.printf("\t%s: %T `%s`,\n", name, type, tag);
		} else {
			fmt.printf("\t%s: %T,\n", name, type);
		}
	}
	fmt.println("}");


	for tag, i in tags {
		if val, ok := reflect.struct_tag_lookup(tag, "json"); ok {
			fmt.printf("json: %s -> %s\n", names[i], val);
		}
	}
}

quaternions :: proc() {
	fmt.println("\n# quaternions");

	{ // Quaternion operations
		q := 1 + 2i + 3j + 4k;
		r := quaternion(5, 6, 7, 8);
		t := q * r;
		fmt.printf("(%v) * (%v) = %v\n", q, r, t);
		v := q / r;
		fmt.printf("(%v) / (%v) = %v\n", q, r, v);
		u := q + r;
		fmt.printf("(%v) + (%v) = %v\n", q, r, u);
		s := q - r;
		fmt.printf("(%v) - (%v) = %v\n", q, r, s);
	}
	{ // The quaternion types
		q128: quaternion128; // 4xf32
		q256: quaternion256; // 4xf64
		q128 = quaternion(1, 0, 0, 0);
		q256 = 1; // quaternion(1, 0, 0, 0);
	}
	{ // Built-in procedures
		q := 1 + 2i + 3j + 4k;
		fmt.println("q =", q);
		fmt.println("real(q) =", real(q));
		fmt.println("imag(q) =", imag(q));
		fmt.println("jmag(q) =", jmag(q));
		fmt.println("kmag(q) =", kmag(q));
		fmt.println("conj(q) =", conj(q));
		fmt.println("abs(q)  =", abs(q));
	}
	{ // Conversion of a complex type to a quaternion type
		c := 1 + 2i;
		q := quaternion256(c);
		fmt.println(c);
		fmt.println(q);
	}
	{ // Memory layout of Quaternions
		q := 1 + 2i + 3j + 4k;
		a := transmute([4]f64)q;
		fmt.println("Quaternion memory layout: xyzw/(ijkr)");
		fmt.println(q); // 1.000+2.000i+3.000j+4.000k
		fmt.println(a); // [2.000, 3.000, 4.000, 1.000]
	}
}

inline_for_statement :: proc() {
	fmt.println("\n#inline for statements");

	// 'inline for' works the same as if the 'inline' prefix did not
	// exist but these ranged loops are explicitly unrolled which can
	// be very very useful for certain optimizations

	fmt.println("Ranges");
	inline for x, i in 1..<4 {
		fmt.println(x, i);
	}

	fmt.println("Strings");
	inline for r, i in "Hello, 世界" {
		fmt.println(r, i);
	}

	fmt.println("Arrays");
	inline for elem, idx in ([4]int{1, 4, 9, 16}) {
		fmt.println(elem, idx);
	}


	Foo_Enum :: enum {
		A = 1,
		B,
		C = 6,
		D,
	};
	fmt.println("Enum types");
	inline for elem, idx in Foo_Enum {
		fmt.println(elem, idx);
	}
}

where_clauses :: proc() {
	fmt.println("\n#procedure 'where' clauses");

	{ // Sanity checks
		simple_sanity_check :: proc(x: [2]int)
			where len(x) > 1,
			      type_of(x) == [2]int {
			fmt.println(x);
		}
	}
	{ // Parametric polymorphism checks
		cross_2d :: proc(a, b: $T/[2]$E) -> E
			where intrinsics.type_is_numeric(E) {
			return a.x*b.y - a.y*b.x;
		}
		cross_3d :: proc(a, b: $T/[3]$E) -> T
			where intrinsics.type_is_numeric(E) {
			x := a.y*b.z - a.z*b.y;
			y := a.z*b.x - a.x*b.z;
			z := a.x*b.y - a.y*b.z;
			return T{x, y, z};
		}

		a := [2]int{1, 2};
		b := [2]int{5, -3};
		fmt.println(cross_2d(a, b));

		x := [3]f32{1, 4, 9};
		y := [3]f32{-5, 0, 3};
		fmt.println(cross_3d(x, y));

		// Failure case
		// i := [2]bool{true, false};
		// j := [2]bool{false, true};
		// fmt.println(cross_2d(i, j));

	}

	{ // Procedure groups usage
		foo :: proc(x: [$N]int) -> bool
			where N > 2 {
			fmt.println(#procedure, "was called with the parameter", x);
			return true;
		}

		bar :: proc(x: [$N]int) -> bool
			where 0 < N,
			      N <= 2 {
			fmt.println(#procedure, "was called with the parameter", x);
			return false;
		}

		baz :: proc{foo, bar};

		x := [3]int{1, 2, 3};
		y := [2]int{4, 9};
		ok_x := baz(x);
		ok_y := baz(y);
		assert(ok_x == true);
		assert(ok_y == false);
	}

	{ // Record types
		Foo :: struct(T: typeid, N: int)
			where intrinsics.type_is_integer(T),
			      N > 2 {
			x: [N]T,
			y: [N-2]T,
		};

		T :: i32;
		N :: 5;
		f: Foo(T, N);
		#assert(size_of(f) == (N+N-2)*size_of(T));
	}
}

main :: proc() {
	when true {
		extra_general_stuff();
		union_type();
		parametric_polymorphism();
		threading_example();
		array_programming();
		named_proc_return_parameters();
		using_enum();
		map_type();
		implicit_selector_expression();
		explicit_procedure_overloading();
		complete_switch();
		cstring_example();
		deprecated_attribute();
		bit_set_type();
		diverging_procedures();
		deferred_procedure_associations();
		reflection();
		quaternions();
		inline_for_statement();
		where_clauses();
	}
}


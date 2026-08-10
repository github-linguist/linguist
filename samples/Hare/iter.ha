// SPDX-License-Identifier: MPL-2.0
// (c) Hare authors <https://harelang.org>

use ascii;
use strings;
use strconv;
use types;

// Tagged union of the [[formattable]] types and [[mods]]. Used for
// functions which accept format strings.
export type field = (...formattable | *mods);

// Tagged union of all types which are formattable.
export type formattable = (...types::numeric | uintptr | str | rune | bool |
	nullable *opaque | void);

// Negative modifier. Specifies for numerical arguments when to prepend a plus
// or minus sign or a blank space.
export type neg = enum {
	NONE,
	SPACE,
	PLUS,
};

// Alignment modifier. Specifies how to align an argument within a given width.
export type alignment = enum {
	RIGHT,
	CENTER,
	LEFT,
};

// Specifies how to format an argument.
export type mods = struct {
	alignment: alignment,
	pad: rune,
	neg: neg,
	width: size,
	prec: size,
	base: strconv::base,
	ffmt: strconv::ffmt,
	fflags: strconv::fflags,
};

type iterator = struct {
	iter: strings::iterator,
	args: []field,
	idx: size,
	checkunused: bool,
};

fn iter(fmt: str, args: []field) iterator = iterator {
	iter = strings::iter(fmt),
	args = args,
	idx = 0,
	checkunused = true,
};

fn next(it: *iterator) (str | (formattable, mods) | done) = {
	let r = match (strings::next(&it.iter)) {
	case done =>
		return done;
	case let r: rune =>
		yield r;
	};
	switch (r) {
	case '{' => void; // handled below
	case '}' =>
		match (strings::next(&it.iter)) {
		case done =>
			abort("Invalid format string (hanging '}')");
		case let r: rune =>
			assert(r == '}', "Invalid format string (hanging '}')");
		};
		return "}";
	case =>
		strings::prev(&it.iter);
		let start = it.iter;
		for (let r => strings::next(&it.iter)) {
			if (r == '{' || r == '}') {
				strings::prev(&it.iter);
				break;
			};
		};
		return strings::slice(&start, &it.iter);
	};

	r = getrune(it);
	if (r == '{') {
		return "{";
	};

	let idx = if (ascii::isdigit(r)) {
		strings::prev(&it.iter);
		it.checkunused = false;
		defer r = getrune(it);
		yield scan_sz(it);
	} else {
		defer it.idx += 1;
		yield it.idx;
	};
	assert(idx < len(it.args), "Not enough parameters given");
	let arg = it.args[idx] as formattable;
	let mod = mods { ... };

	switch (r) {
	case ':' =>
		scan_modifiers(it, &mod);
	case '%' =>
		r = getrune(it);
		let idx = if (ascii::isdigit(r)) {
			strings::prev(&it.iter);
			it.checkunused = false;
			defer r = getrune(it);
			yield scan_sz(it);
		} else {
			defer it.idx += 1;
			yield it.idx;
		};
		assert(idx < len(it.args), "Not enough parameters given");
		mod = *(it.args[idx] as *mods);
		assert(r == '}', "Invalid format string (didn't find '}' after modifier index)");
	case '}' => void;
	case => abort("Invalid format string");
	};

	return (arg, mod);
};

fn scan_modifiers(it: *iterator, mod: *mods) void = {
	mod.pad = ' ';
	for (true) switch (getrune(it)) {
	// alignment
	case '-' => mod.alignment = alignment::LEFT;
	case '=' => mod.alignment = alignment::CENTER;
	// padding
	case '_' => mod.pad = getrune(it);
	// negation
	case ' ' => mod.neg = neg::SPACE;
	case '+' => mod.neg = neg::PLUS;
	// base
	case 'x' => mod.base = strconv::base::HEX_LOWER;
	case 'X' => mod.base = strconv::base::HEX_UPPER;
	case 'o' => mod.base = strconv::base::OCT;
	case 'b' => mod.base = strconv::base::BIN;
	// ffmt
	case 'e' => mod.ffmt = strconv::ffmt::E;
	case 'f' => mod.ffmt = strconv::ffmt::F;
	case 'g' => mod.ffmt = strconv::ffmt::G;
	// fflags
	case 'F' =>
		switch (getrune(it)) {
		case 's' => mod.fflags |= strconv::fflags::SHOW_POS;
		case '.' => mod.fflags |= strconv::fflags::SHOW_POINT;
		case 'U' => mod.fflags |= strconv::fflags::UPPERCASE;
		case 'E' => mod.fflags |= strconv::fflags::UPPER_EXP;
		case 'S' => mod.fflags |= strconv::fflags::SHOW_POS_EXP;
		case '2' => mod.fflags |= strconv::fflags::SHOW_TWO_EXP_DIGITS;
		case => abort("Invalid float flag");
		};
	// precision
	case '.' => mod.prec = scan_sz(it);
	// width
	case '1', '2', '3', '4', '5', '6', '7', '8', '9' =>
		strings::prev(it);
		mod.width = scan_sz(it);
	case =>
		strings::prev(it);
		break;
	};
	assert(getrune(it) == '}', "Invalid format string (unterminated '{')");
};

fn scan_sz(it: *iterator) size = {
	let start = it.iter;
	assert(ascii::isdigit(getrune(it)));
	for (ascii::isdigit(getrune(it))) void;
	strings::prev(&it.iter);

	match (strconv::stoz(strings::slice(&start, &it.iter))) {
	case strconv::invalid =>
		abort("Invalid format string (invalid integer)");
	case strconv::overflow =>
		abort("Invalid format string (integer overflow)");
	case let z: size =>
		return z;
	};
};

fn getrune(it: *iterator) rune = match (strings::next(&it.iter)) {
case done =>
	abort("Invalid format string (unterminated '{')");
case let r: rune =>
	return r;
};

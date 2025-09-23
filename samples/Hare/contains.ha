// SPDX-License-Identifier: MPL-2.0
// (c) Hare authors <https://harelang.org>

// Returns true if a byte slice contains a byte or a sequence of bytes.
export fn contains(haystack: []u8, needles: (u8 | []u8)...) bool = {
	for (let i = 0z; i < len(needles); i += 1) {
		const matched = match (needles[i]) {
		case let b: u8 =>
			yield index_byte(haystack, b) is size;
		case let b: []u8 =>
			yield index_slice(haystack, b) is size;
		};
		if (matched) {
			return true;
		};
	};
	return false;
};

// Returns true if "in" has the given prefix, false otherwise
export fn hasprefix(in: []u8, prefix: []u8) bool = {
	return len(in) >= len(prefix) && equal(in[..len(prefix)], prefix);
};

@test fn hasprefix() void = {
	assert(hasprefix([], []));
	assert(hasprefix([0], []));
	assert(!hasprefix([], [0]));
	assert(hasprefix([1, 2, 3], [1, 2]));
	assert(!hasprefix([1, 2, 3], [1, 1]));
	assert(!hasprefix([1, 2, 3], [1, 2, 3, 4]));
};

// Returns true if "in" has the given suffix, false otherwise
export fn hassuffix(in: []u8, suffix: []u8) bool = {
	return len(in) >= len(suffix)
		&& equal(in[len(in) - len(suffix)..], suffix);
};

@test fn hassuffix() void = {
	assert(hassuffix([], []));
	assert(hassuffix([0], []));
	assert(!hassuffix([], [0]));
	assert(hassuffix([1, 2, 3], [2, 3]));
	assert(!hassuffix([1, 2, 3], [2, 2]));
	assert(hassuffix([1, 2, 3, 4], [2, 3, 4]));
};

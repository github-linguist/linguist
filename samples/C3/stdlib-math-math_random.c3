// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_random.c3
// Permalink: https://github.com/c3lang/c3c/blob/e1565cc/lib/std/math/math_random.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

/**
 * Randoms:
 * General usage -
 * 1. Create a Random (see std/math/random for various alternatives), or pick DefaultRandom
 * 2. Define it `DefaultRandom my_random;`
 * 3. Seed it: `random::seed(&my_random, some_seed);` or `rand::seed_entropy(&my_random);`
 * 4. Use it with the functions in random: `float random_float = random::next_float(&my_random);`
 *
 * For just a simple integer between 0 and n (not including n), you can use `rand(n)`.
 **/
module std::math::random;

/**
 * @require is_random(random)
 **/
macro void seed(random, seed)
{
	random.set_seed(@as_char_view(seed));
}

/**
 * Seed the random with some best effort entropy.
 *
 * @require is_random(random)
 **/
macro void seed_entropy(random)
{
	random.set_seed(&&entropy());
}

/**
 * Get the next value between 0 and max (not including max).
 *
 * @require is_random(random)
 **/
macro int next(random, int max)
{
	return (int)(next_double(random) * max);
}

def DefaultRandom = Sfc64Random;

/**
 * Get a default random value between 0 and max (not including max)
 **/
fn int rand(int max) @builtin
{
	tlocal Sfc64Random default_random;
	tlocal bool initialized = false;
	if (!initialized)
	{
		seed_entropy(&default_random);
		initialized = true;
	}
	return next(&default_random, max);
}

/**
 * Get 'true' or 'false'
 *
 * @require is_random(random)
 **/
macro bool next_bool(random)
{
	return (bool)(random.next_byte() & 1);
}

/**
 * Get a float between 0 and 1.0, not including 1.0.
 *
 * @require is_random(random)
 **/
macro float next_float(random)
{
	uint val = random.next_int() & (1 << 24 - 1);
	return val / (float)(1 << 24);
}

/**
 * Get a double between 0 and 1.0, not including 1.0.
 *
 * @require is_random(random)
 **/
macro double next_double(random)
{
	ulong val = random.next_long() & (1UL << 53 - 1);
	return val * 0x1.0p-53;
}

// True if the value is a Random.
macro bool is_random(random) => $assignable(random, Random);

macro uint128 @long_to_int128(#function) => (uint128)#function << 64 + #function;
macro ulong @int_to_long(#function) => (ulong)#function << 32 + #function;
macro uint @short_to_int(#function) => (uint)#function << 16 + #function;
macro ushort @char_to_short(#function) => (ushort)#function << 8 + #function;

macro @random_value_to_bytes(#function, char[] bytes)
{
	var $byte_size = $sizeof(#function());
	usz len = bytes.len;
	// Same size or smaller? Then just copy.
	while (len > 0)
	{
		var value = #function();
		if (len <= $byte_size)
		{
			bytes[..] = ((char*)&value)[:len];
			return;
		}
		bytes[:$byte_size] = ((char*)&value)[:$byte_size];
		len -= $byte_size;
		bytes = bytes[$byte_size..];
	}
	unreachable();
}

// This is the interface to implement for Random implementations, usually
// it is not invoked directly.
interface Random
{
	fn void set_seed(char[] input);
	fn char next_byte();
	fn ushort next_short();
	fn uint next_int();
	fn ulong next_long();
	fn uint128 next_int128();
	fn void next_bytes(char[] buffer);
}

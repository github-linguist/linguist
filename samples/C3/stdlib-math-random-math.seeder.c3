// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.seeder.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/math/random/math.seeder.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;
import std::hash::fnv32a, std::time;

const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_LCG64 @local = 0xd1342543de82ef95;

macro make_seed($Type, char[] input)
{
	$Type return_value;
	seeder(input, @as_char_view(return_value));
	return return_value;
}

// TODO: Make endian independent
/**
 * @param [in] input
 * @param [inout] out_buffer
 **/
fn void seeder(char[] input, char[] out_buffer)
{
	$if env::BIG_ENDIAN:
		$echo("Please note that the seeding function behaves differently on BE architectures.");
	$endif
	// Init words
	usz out_chars = out_buffer.len;
	@pool()
	{
		ulong[] words = mem::temp_alloc_array(ulong, (out_chars + 7) / 8);
		words[..] = ODD_PHI64;
		usz words_len_2 = words.len * 2;

		// Add word at a time
		for (usz i = 0; i < input.len / 8; i++)
		{
			usz j = i % words.len;
			words[j] -= bitcast(*(char[8]*)&input[i * 8], ulong) * MUL_LCG64;
			words[j] ^= words[j] >> 25;
		}

		// Add rest of the bytes
		usz remaining = input.len - input.len / 8 * 8;
		if (remaining)
		{
			ulong rest = MUL_MCG64;
			mem::copy(&rest, &input[^remaining], remaining);
			words[^1] -= rest * MUL_LCG64;
			words[^1] ^= words[^1] >> 25;
		}

		// Mix between words
		for (isz i = words_len_2 - 1; i >= 0; i--)
		{
			isz j = i % words.len;
			words[j] -= words[(i + 1) % words.len] * MUL_LCG64;
			words[j] ^= words[j] >> 25;
		}

		// Mix within words
		for (usz i = 0; i < words_len_2; i++)
		{
			usz j = i % words.len;
			words[j] *= MUL_MCG64;
			words[j] ^= words[j] >> 25;
		}
		out_buffer[..] = ((char*)words.ptr)[:out_chars];
	};
}

macro uint hash(value) @local
{
	return fnv32a::encode(&&bitcast(value, char[$sizeof(value)]));
}

fn char[8 * 4] entropy()
{
	void* addr = malloc(1);
	free(addr);
	static uint random_int;
	random_int += 0xedf19156;
	uint[8] entropy_data = {
		hash($$TIME),
		hash(addr),
		hash(&addr),
		hash(&entropy),
		random_int,
		hash(clock::now()),
		hash(&DString.new_init),
		hash(allocator::heap())
    };
    return bitcast(entropy_data, char[8 * 4]);
}
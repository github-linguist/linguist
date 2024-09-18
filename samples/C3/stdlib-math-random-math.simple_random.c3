// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.simple_random.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/math/random/math.simple_random.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;

distinct SimpleRandom (Random) = ulong;


fn void SimpleRandom.set_seed(&self, char[] seed) @dynamic
{
	char[8] full;
	foreach (i, c : seed)
	{
		full[i % 8] ^= c;
	}
	*self = (SimpleRandom)(bitcast(full, ulong) ^ SIMPLE_RANDOM_MULTIPLIER) & SIMPLE_RANDOM_MASK;
}

fn uint SimpleRandom.next_int(&self) @dynamic
{
	ulong nextseed = ((ulong)*self * SIMPLE_RANDOM_MULTIPLIER + SIMPLE_RANDOM_ADDEND) & SIMPLE_RANDOM_MASK;
	*self = (SimpleRandom)nextseed;
	return (uint)(nextseed >> (48 - 32));
}

/**
 * @require bytes.len > 0
 **/
fn void SimpleRandom.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 SimpleRandom.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong SimpleRandom.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort SimpleRandom.next_short(&self) @dynamic => (ushort)self.next_int();
fn char SimpleRandom.next_byte(&self) @dynamic => (char)self.next_int();

const long SIMPLE_RANDOM_MULTIPLIER @local = 0x5DEECE66D;
const long SIMPLE_RANDOM_ADDEND @local = 0xB;
const long SIMPLE_RANDOM_MASK @local = (1u64 << 48) - 1;

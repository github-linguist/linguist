// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.mcg.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/math/random/math.mcg.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;

const MUL_MCG128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_MCG32 @local = 0x93d765dd;
const MUL_MCG16 @local = 0x93d5; // TODO: Find good constant

// Mcg128_64

distinct Mcg128Random (Random) = uint128;

fn void Mcg128Random.set_seed(&self, char[] seed) @dynamic
{
	*self = (Mcg128Random)(random::make_seed(uint128, seed) | 1);
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg128Random.next_bytes(&self, char[] bytes) @dynamic
{
	@random_value_to_bytes(self.next_long, bytes);
}

fn ulong Mcg128Random.next_long(&self) @dynamic
{
	uint128* s = (uint128*)self;
	ulong result = (ulong)(*s >> 64);
	*s *= MUL_MCG128;
	return result;
}

fn uint128 Mcg128Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn uint Mcg128Random.next_int(&self) @dynamic => (uint)self.next_long();
fn ushort Mcg128Random.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Mcg128Random.next_byte(&self) @dynamic => (char)self.next_long();




// Mcg64RandomState

distinct Mcg64Random (Random) = ulong;

fn void Mcg64Random.set_seed(&self, char[] seed) @dynamic
{
	*self = (Mcg64Random)random::make_seed(ulong, seed) | 1;
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg64Random.next_bytes(&self, char[] bytes) @dynamic
{
	@random_value_to_bytes(self.next_int, bytes);
}

fn uint Mcg64Random.next_int(&self) @dynamic
{
	ulong* s = (ulong*)self;
	uint result = (uint)(*s >> 32);
	*s *= MUL_MCG64;
	return result;
}

fn uint128 Mcg64Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Mcg64Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Mcg64Random.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Mcg64Random.next_byte(&self) @dynamic => (char)self.next_int();


// Mcg32Random

distinct Mcg32Random (Random) = uint;

fn void Mcg32Random.set_seed(&self, char[] seed) @dynamic
{
	*self = (Mcg32Random)random::make_seed(uint, seed) | 1;
}

fn ushort Mcg32Random.next_short(&self) @dynamic
{
	uint* s = (uint*)self;
	ushort result = (ushort)(*s >> 16);
	*s *= MUL_MCG32;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg32Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Mcg32Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Mcg32Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Mcg32Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn char Mcg32Random.next_byte(&self) @dynamic => (char)self.next_short();

// -------------------------------- Mcg16RandomState --------------------------------

distinct Mcg16Random (Random) = ushort;

fn void Mcg16Random.set_seed(&self, char[] seed) @dynamic
{
	*self = (Mcg16Random)random::make_seed(ushort, seed) | 1;
}

fn char Mcg16Random.next_byte(&self) @dynamic
{
	ushort* s = (ushort*)self;
	char result = (char)(*s >> 8);
	*s *= MUL_MCG16;
	return result;
}

fn void Mcg16Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Mcg16Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Mcg16Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Mcg16Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn ushort Mcg16Random.next_short(&self) @dynamic => @char_to_short(self.next_byte());

// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.pcg.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/math/random/math.pcg.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;

const MUL_LCG128 @local = 0xdb36357734e34abb0050d0761fcdfc15;
const MUL_LCG64 @local = 0xd1342543de82ef95;
const MUL_LCG32 @local = 0x915f77f5;
const MUL_LCG16 @local = 0x915d; // TODO: Find good constant


// -------------------------------- Pcg128_64 --------------------------------


distinct Pcg128Random (Random) = uint128;

fn void  Pcg128Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Pcg128Random)random::make_seed(uint128, input);
}


fn ulong Pcg128Random.next_long(&self) @dynamic
{
	const ROT_SHIFT = 64 - 6;
	uint128* s = (uint128*)self;
	uint128 xor = *s ^ *s >> ((128 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (128 - 6));
	*s = *s * MUL_LCG128 + ODD_PHI128;
	return ((ulong)(xor >> ROT_SHIFT)).rotr(rot);
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg128Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_long, bytes);
fn uint128 Pcg128Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn uint Pcg128Random.next_int(&self) @dynamic => (uint)self.next_long();
fn ushort Pcg128Random.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Pcg128Random.next_byte(&self) @dynamic => (char)self.next_long();

// -------------------------------- Pcg64_32 --------------------------------

distinct Pcg64Random (Random) = ulong;

fn void Pcg64Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Pcg64Random)random::make_seed(ulong, input);
}

fn uint Pcg64Random.next_int(&self) @dynamic
{
	const ROT_SHIFT = 32 - 5;
	ulong* s = (ulong*)self;
	ulong xor = *s ^ *s >> ((64 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (64 - 5));
	*s = *s * MUL_LCG64 + ODD_PHI64;
	return ((uint)(xor >> ROT_SHIFT)).rotr(rot);
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg64Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Pcg64Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Pcg64Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Pcg64Random.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Pcg64Random.next_byte(&self) @dynamic => (char)self.next_int();


// -------------------------------- Pcg32_16 --------------------------------

distinct Pcg32Random (Random) = uint;

fn void Pcg32Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Pcg32Random)random::make_seed(uint, input);
}

fn ushort Pcg32Random.next_short(&self) @dynamic
{
	const ROT_SHIFT = 16 - 4;
	uint* s = (uint*)self;
	uint xor = *s ^ *s >> ((32 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (32 - 4));
	*s = *s * MUL_LCG32 + ODD_PHI32;
	return ((ushort)(xor >> ROT_SHIFT)).rotr(rot);
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg32Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Pcg32Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Pcg32Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Pcg32Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn char Pcg32Random.next_byte(&self) @dynamic => (char)self.next_short();

// -------------------------------- Pcg16_8 --------------------------------

distinct Pcg16Random (Random) = ushort;

fn void Pcg16Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Pcg16Random)random::make_seed(ushort, input);
}

fn char Pcg16Random.next_byte(&self) @dynamic
{
	const ROT_SHIFT = 8 - 3;
	ushort* s = (ushort*)self;
	ushort xor = *s ^ *s >> ((16 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (16 - 3));
	*s = *s * MUL_LCG16 + ODD_PHI16;
	return ((char)(xor >> ROT_SHIFT)).rotr(rot);
}

fn void Pcg16Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Pcg16Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Pcg16Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Pcg16Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn ushort Pcg16Random.next_short(&self) @dynamic => @char_to_short(self.next_byte());

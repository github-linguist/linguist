// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.msws.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/math/random/math.msws.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;

// Msws128
struct Msws128Random (Random)
{
	uint128 state0, state1;
	uint128 weyl0, weyl1;
}

fn void Msws128Random.set_seed(&self, char[] input) @dynamic
{
	*self = bitcast(random::make_seed(uint128[4], input), Msws128Random);
}


fn uint128 Msws128Random.next_int128(&self) @dynamic
{
	uint128 s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(64);
	self.weyl0 += ODD_PHI128;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	uint128 s1 = self.state1;
	self.state1 = self.state1.rotr(64);
	self.weyl1 -= ODD_PHI128;

	return s0 + s1;
}

/**
 * @require bytes.len > 0
 **/
fn void Msws128Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int128, bytes);
fn ulong Msws128Random.next_long(&self) @dynamic => (ulong)self.next_int128();
fn uint Msws128Random.next_int(&self) @dynamic => (uint)self.next_int128();
fn ushort Msws128Random.next_short(&self) @dynamic => (ushort)self.next_int128();
fn char Msws128Random.next_byte(&self) @dynamic => (char)self.next_int128();


// Msws64
struct Msws64Random (Random)
{
	ulong state0, state1;
	ulong weyl0, weyl1;
}

fn void  Msws64Random.set_seed(&self, char[] input) @dynamic
{
	*self = bitcast(random::make_seed(ulong[4], input), Msws64Random);
}

fn ulong Msws64Random.next_long(&self) @dynamic
{
	ulong s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(32);
	self.weyl0 += ODD_PHI64;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	ulong s1 = self.state1;
	self.state1 = self.state1.rotr(32);
	self.weyl1 -= ODD_PHI64;

	return s0 + s1;
}

/**
 * @require bytes.len > 0
 **/
fn void Msws64Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_long, bytes);
fn uint128 Msws64Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn uint Msws64Random.next_int(&self) @dynamic => (uint)self.next_long();
fn ushort Msws64Random.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Msws64Random.next_byte(&self) @dynamic => (char)self.next_long();

// Msws32
struct Msws32Random (Random)
{
	uint state0, state1;
	uint weyl0, weyl1;
}

fn void  Msws32Random.set_seed(&self, char[] input) @dynamic
{
	*self = bitcast(random::make_seed(uint[4], input), Msws32Random);
}


fn uint Msws32Random.next_int(&self) @dynamic
{
	uint s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(16);
	self.weyl0 += ODD_PHI32;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	uint s1 = self.state1;
	self.state1 = self.state1.rotr(16);
	self.weyl1 -= ODD_PHI32;

	return s0 + s1;
}

/**
 * @require bytes.len > 0
 **/
fn void Msws32Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Msws32Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Msws32Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Msws32Random.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Msws32Random.next_byte(&self) @dynamic => (char)self.next_int();

// Msws16

struct Msws16Random (Random)
{
	ushort state0, state1;
	ushort weyl0, weyl1;
}

fn void  Msws16Random.set_seed(&self, char[] input) @dynamic
{
	*self = bitcast(random::make_seed(ushort[4], input), Msws16Random);
}


fn ushort Msws16Random.next_short(&self) @dynamic
{
	ushort s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(8);
	self.weyl0 += ODD_PHI16;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	ushort s1 = self.state1;
	self.state1 = self.state1.rotr(8);
	self.weyl1 -= ODD_PHI16;

	return s0 + s1;
}

/**
 * @require bytes.len > 0
 **/
fn void Msws16Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Msws16Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Msws16Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Msws16Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn char Msws16Random.next_byte(&self) @dynamic => (char)self.next_short();

// Msws8
struct Msws8Random (Random)
{
	char state0, state1;
	char weyl0, weyl1;
}

fn void  Msws8Random.set_seed(&self, char[] input) @dynamic
{
	*self = bitcast(random::make_seed(char[4], input), Msws8Random);
}

fn char Msws8Random.next_byte(&self) @dynamic
{
	char s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(4);
	self.weyl0 += ODD_PHI8;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	char s1 = self.state1;
	self.state1 = self.state1.rotr(4);
	self.weyl1 -= ODD_PHI8;

	return s0 + s1;
}

fn void Msws8Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Msws8Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Msws8Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Msws8Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn ushort Msws8Random.next_short(&self) @dynamic => @char_to_short(self.next_byte());

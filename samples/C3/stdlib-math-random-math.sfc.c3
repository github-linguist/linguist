// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.sfc.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/math/random/math.sfc.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::random;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;


// Sfc128

distinct Sfc128Random (Random) = uint128[4];

fn void  Sfc128Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Sfc128Random)random::make_seed(uint128[4], input);
}

fn uint128 Sfc128Random.next_int128(&self) @dynamic // TODO: Find good constant
{
	uint128* s = (uint128[4]*)self;
	uint128 result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 11;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(40) + result;
	s[3] += ODD_PHI128;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc128Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int128, bytes);
fn ulong Sfc128Random.next_long(&self) @dynamic => (uint)self.next_int128();
fn uint Sfc128Random.next_int(&self) @dynamic => (uint)self.next_int128();
fn ushort Sfc128Random.next_short(&self) @dynamic => (ushort)self.next_int128();
fn char Sfc128Random.next_byte(&self) @dynamic => (char)self.next_int128();


// -------------------------------- Sfc64 --------------------------------

distinct Sfc64Random (Random) = ulong[4];

fn void  Sfc64Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Sfc64Random)random::make_seed(ulong[4], input);
}

fn ulong Sfc64Random.next_long(&self) @dynamic
{
	ulong* s = (ulong[4]*)self;
	ulong result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 11;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(40) + result;
	s[3] += ODD_PHI64;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc64Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_long, bytes);
fn uint128 Sfc64Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn uint Sfc64Random.next_int(&self) @dynamic => (uint)self.next_long();
fn ushort Sfc64Random.next_short(&self) @dynamic => (ushort)self.next_long();
fn char Sfc64Random.next_byte(&self) @dynamic => (char)self.next_long();

// -------------------------------- Sfc32 --------------------------------

distinct Sfc32Random (Random) = uint[4];

fn void  Sfc32Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Sfc32Random)random::make_seed(uint[4], input);
}

fn uint Sfc32Random.next_int(&sfc) @dynamic
{
	uint* s = (uint[4]*)sfc;
	uint result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 9;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(11) + result;
	s[3] += ODD_PHI32;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc32Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Sfc32Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Sfc32Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Sfc32Random.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Sfc32Random.next_byte(&self) @dynamic => (char)self.next_int();

// -------------------------------- Sfc16 --------------------------------

distinct Sfc16Random (Random) = ushort[4];

fn void  Sfc16Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Sfc16Random)random::make_seed(ushort[4], input);
}


fn ushort Sfc16Random.next_short(&seed) @dynamic
{
	ushort* s = (ushort[4]*)seed;
	ushort result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 2;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(12) + result;
	s[3] += ODD_PHI16;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc16Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Sfc16Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Sfc16Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Sfc16Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn char Sfc16Random.next_byte(&self) @dynamic => (char)self.next_short();

// -------------------------------- Sfc8 --------------------------------


distinct Sfc8Random (Random) = char[4];

fn void  Sfc8Random.set_seed(&self, char[] input) @dynamic
{
	*self = (Sfc8Random)random::make_seed(char[4], input);
}

fn char Sfc8Random.next_byte(&self) @dynamic // TODO: Find better constants
{
	char* s = (char[4]*)self;
	char result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 1;
	s[1] = s[2] + s[2] << 2;
	s[2] = s[2].rotr(3) + result;
	s[3] += ODD_PHI8;
	return result;
}

fn void Sfc8Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Sfc8Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Sfc8Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn uint Sfc8Random.next_int(&self) @dynamic => @short_to_int(self.next_short());
fn ushort Sfc8Random.next_short(&self) @dynamic => @char_to_short(self.next_byte());
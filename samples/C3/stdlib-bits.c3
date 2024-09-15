// Source: https://github.com/c3lang/c3c/blob/master/lib/std/bits.c3
// Permalink: https://github.com/c3lang/c3c/blob/6808a38/lib/std/bits.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::bits;

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro reverse(i) => $$bitreverse(i);

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro bswap(i) @builtin => $$bswap(i);

macro uint[<*>].popcount(self) => $$popcount(self);
macro uint[<*>].ctz(self) => $$ctz(self);
macro uint[<*>].clz(self) => $$clz(self);
macro uint[<*>] uint[<*>].fshl(hi, uint[<*>] lo, uint[<*>] shift) => $$fshl(hi, lo, shift);
macro uint[<*>] uint[<*>].fshr(hi, uint[<*>] lo, uint[<*>] shift) => $$fshr(hi, lo, shift);
macro uint[<*>] uint[<*>].rotl(self, uint[<*>] shift) => $$fshl(self, self, shift);
macro uint[<*>] uint[<*>].rotr(self, uint[<*>] shift) => $$fshr(self, self, shift);

macro int[<*>].popcount(self) => $$popcount(self);
macro int[<*>].ctz(self) => $$ctz(self);
macro int[<*>].clz(self) => $$clz(self);
macro int[<*>] int[<*>].fshl(hi, int[<*>] lo, int[<*>] shift) => $$fshl(hi, lo, shift);
macro int[<*>] int[<*>].fshr(hi, int[<*>] lo, int[<*>] shift) => $$fshr(hi, lo, shift);
macro int[<*>] int[<*>].rotl(self, int[<*>] shift) => $$fshl(self, self, shift);
macro int[<*>] int[<*>].rotr(self, int[<*>] shift) => $$fshr(self, self, shift);

macro ushort[<*>].popcount(self) => $$popcount(self);
macro ushort[<*>].ctz(self) => $$ctz(self);
macro ushort[<*>].clz(self) => $$clz(self);
macro ushort[<*>] ushort[<*>].fshl(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshl(hi, lo, shift);
macro ushort[<*>] ushort[<*>].fshr(hi, ushort[<*>] lo, ushort[<*>] shift) => $$fshr(hi, lo, shift);
macro ushort[<*>] ushort[<*>].rotl(self, ushort[<*>] shift) => $$fshl(self, self, shift);
macro ushort[<*>] ushort[<*>].rotr(self, ushort[<*>] shift) => $$fshr(self, self, shift);

macro short[<*>].popcount(self) => $$popcount(self);
macro short[<*>].ctz(self) => $$ctz(self);
macro short[<*>].clz(self) => $$clz(self);
macro short[<*>] short[<*>].fshl(hi, short[<*>] lo, short[<*>] shift) => $$fshl(hi, lo, shift);
macro short[<*>] short[<*>].fshr(hi, short[<*>] lo, short[<*>] shift) => $$fshr(hi, lo, shift);
macro short[<*>] short[<*>].rotl(self, short[<*>] shift) => $$fshl(self, self, shift);
macro short[<*>] short[<*>].rotr(self, short[<*>] shift) => $$fshr(self, self, shift);

macro char[<*>].popcount(self) => $$popcount(self);
macro char[<*>].ctz(self) => $$ctz(self);
macro char[<*>].clz(self) => $$clz(self);
macro char[<*>] char[<*>].fshl(hi, char[<*>] lo, char[<*>] shift) => $$fshl(hi, lo, shift);
macro char[<*>] char[<*>].fshr(hi, char[<*>] lo, char[<*>] shift) => $$fshr(hi, lo, shift);
macro char[<*>] char[<*>].rotl(self, char[<*>] shift) => $$fshl(self, self, shift);
macro char[<*>] char[<*>].rotr(self, char[<*>] shift) => $$fshr(self, self, shift);

macro ichar[<*>].popcount(self) => $$popcount(self);
macro ichar[<*>].ctz(self) => $$ctz(self);
macro ichar[<*>].clz(self) => $$clz(self);
macro ichar[<*>] ichar[<*>].fshl(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshl(hi, lo, shift);
macro ichar[<*>] ichar[<*>].fshr(hi, ichar[<*>] lo, ichar[<*>] shift) => $$fshr(hi, lo, shift);
macro ichar[<*>] ichar[<*>].rotl(self, ichar[<*>] shift) => $$fshl(self, self, shift);
macro ichar[<*>] ichar[<*>].rotr(self, ichar[<*>] shift) => $$fshr(self, self, shift);

macro ulong[<*>].popcount(self) => $$popcount(self);
macro ulong[<*>].ctz(self) => $$ctz(self);
macro ulong[<*>].clz(self) => $$clz(self);
macro ulong[<*>] ulong[<*>].fshl(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshl(hi, lo, shift);
macro ulong[<*>] ulong[<*>].fshr(hi, ulong[<*>] lo, ulong[<*>] shift) => $$fshr(hi, lo, shift);
macro ulong[<*>] ulong[<*>].rotl(self, ulong[<*>] shift) => $$fshl(self, self, shift);
macro ulong[<*>] ulong[<*>].rotr(self, ulong[<*>] shift) => $$fshr(self, self, shift);

macro long[<*>].popcount(self) => $$popcount(self);
macro long[<*>].ctz(self) => $$ctz(self);
macro long[<*>].clz(self) => $$clz(self);
macro long[<*>] long[<*>].fshl(hi, long[<*>] lo, long[<*>] shift) => $$fshl(hi, lo, shift);
macro long[<*>] long[<*>].fshr(hi, long[<*>] lo, long[<*>] shift) => $$fshr(hi, lo, shift);
macro long[<*>] long[<*>].rotl(self, long[<*>] shift) => $$fshl(self, self, shift);
macro long[<*>] long[<*>].rotr(self, long[<*>] shift) => $$fshr(self, self, shift);

macro uint128[<*>].popcount(self) => $$popcount(self);
macro uint128[<*>].ctz(self) => $$ctz(self);
macro uint128[<*>].clz(self) => $$clz(self);
macro uint128[<*>] uint128[<*>].fshl(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshl(hi, lo, shift);
macro uint128[<*>] uint128[<*>].fshr(hi, uint128[<*>] lo, uint128[<*>] shift) => $$fshr(hi, lo, shift);
macro uint128[<*>] uint128[<*>].rotl(self, uint128[<*>] shift) => $$fshl(self, self, shift);
macro uint128[<*>] uint128[<*>].rotr(self, uint128[<*>] shift) => $$fshr(self, self, shift);

macro int128[<*>].popcount(self) => $$popcount(self);
macro int128[<*>].ctz(self) => $$ctz(self);
macro int128[<*>].clz(self) => $$clz(self);
macro int128[<*>] int128[<*>].fshl(hi, int128[<*>] lo, int128[<*>] shift) => $$fshl(hi, lo, shift);
macro int128[<*>] int128[<*>].fshr(hi, int128[<*>] lo, int128[<*>] shift) => $$fshr(hi, lo, shift);
macro int128[<*>] int128[<*>].rotl(self, int128[<*>] shift) => $$fshl(self, self, shift);
macro int128[<*>] int128[<*>].rotr(self, int128[<*>] shift) => $$fshr(self, self, shift);

macro uint.popcount(self) => $$popcount(self);
macro uint.ctz(self) => $$ctz(self);
macro uint.clz(self) => $$clz(self);
macro uint uint.fshl(hi, uint lo, uint shift) => $$fshl(hi, lo, shift);
macro uint uint.fshr(hi, uint lo, uint shift) => $$fshr(hi, lo, shift);
macro uint uint.rotl(self, uint shift) => $$fshl(self, self, shift);
macro uint uint.rotr(self, uint shift) => $$fshr(self, self, shift);

macro int.popcount(self) => $$popcount(self);
macro int.ctz(self) => $$ctz(self);
macro int.clz(self) => $$clz(self);
macro int int.fshl(hi, int lo, int shift) => $$fshl(hi, lo, shift);
macro int int.fshr(hi, int lo, int shift) => $$fshr(hi, lo, shift);
macro int int.rotl(self, int shift) => $$fshl(self, self, shift);
macro int int.rotr(self, int shift) => $$fshr(self, self, shift);

macro ushort.popcount(self) => $$popcount(self);
macro ushort.ctz(self) => $$ctz(self);
macro ushort.clz(self) => $$clz(self);
macro ushort ushort.fshl(hi, ushort lo, ushort shift) => $$fshl(hi, lo, shift);
macro ushort ushort.fshr(hi, ushort lo, ushort shift) => $$fshr(hi, lo, shift);
macro ushort ushort.rotl(self, ushort shift) => $$fshl(self, self, shift);
macro ushort ushort.rotr(self, ushort shift) => $$fshr(self, self, shift);

macro short.popcount(self) => $$popcount(self);
macro short.ctz(self) => $$ctz(self);
macro short.clz(self) => $$clz(self);
macro short short.fshl(hi, short lo, short shift) => $$fshl(hi, lo, shift);
macro short short.fshr(hi, short lo, short shift) => $$fshr(hi, lo, shift);
macro short short.rotl(self, short shift) => $$fshl(self, self, shift);
macro short short.rotr(self, short shift) => $$fshr(self, self, shift);

macro char.popcount(self) => $$popcount(self);
macro char.ctz(self) => $$ctz(self);
macro char.clz(self) => $$clz(self);
macro char char.fshl(hi, char lo, char shift) => $$fshl(hi, lo, shift);
macro char char.fshr(hi, char lo, char shift) => $$fshr(hi, lo, shift);
macro char char.rotl(self, char shift) => $$fshl(self, self, shift);
macro char char.rotr(self, char shift) => $$fshr(self, self, shift);

macro ichar.popcount(self) => $$popcount(self);
macro ichar.ctz(self) => $$ctz(self);
macro ichar.clz(self) => $$clz(self);
macro ichar ichar.fshl(hi, ichar lo, ichar shift) => $$fshl(hi, lo, shift);
macro ichar ichar.fshr(hi, ichar lo, ichar shift) => $$fshr(hi, lo, shift);
macro ichar ichar.rotl(self, ichar shift) => $$fshl(self, self, shift);
macro ichar ichar.rotr(self, ichar shift) => $$fshr(self, self, shift);

macro ulong.popcount(self) => $$popcount(self);
macro ulong.ctz(self) => $$ctz(self);
macro ulong.clz(self) => $$clz(self);
macro ulong ulong.fshl(hi, ulong lo, ulong shift) => $$fshl(hi, lo, shift);
macro ulong ulong.fshr(hi, ulong lo, ulong shift) => $$fshr(hi, lo, shift);
macro ulong ulong.rotl(self, ulong shift) => $$fshl(self, self, shift);
macro ulong ulong.rotr(self, ulong shift) => $$fshr(self, self, shift);

macro long.popcount(self) => $$popcount(self);
macro long.ctz(self) => $$ctz(self);
macro long.clz(self) => $$clz(self);
macro long long.fshl(hi, long lo, long shift) => $$fshl(hi, lo, shift);
macro long long.fshr(hi, long lo, long shift) => $$fshr(hi, lo, shift);
macro long long.rotl(self, long shift) => $$fshl(self, self, shift);
macro long long.rotr(self, long shift) => $$fshr(self, self, shift);

macro uint128.popcount(self) => $$popcount(self);
macro uint128.ctz(self) => $$ctz(self);
macro uint128.clz(self) => $$clz(self);
macro uint128 uint128.fshl(hi, uint128 lo, uint128 shift) => $$fshl(hi, lo, shift);
macro uint128 uint128.fshr(hi, uint128 lo, uint128 shift) => $$fshr(hi, lo, shift);
macro uint128 uint128.rotl(self, uint128 shift) => $$fshl(self, self, shift);
macro uint128 uint128.rotr(self, uint128 shift) => $$fshr(self, self, shift);

macro int128.popcount(self) => $$popcount(self);
macro int128.ctz(self) => $$ctz(self);
macro int128.clz(self) => $$clz(self);
macro int128 int128.fshl(hi, int128 lo, int128 shift) => $$fshl(hi, lo, shift);
macro int128 int128.fshr(hi, int128 lo, int128 shift) => $$fshr(hi, lo, shift);
macro int128 int128.rotl(self, int128 shift) => $$fshl(self, self, shift);
macro int128 int128.rotr(self, int128 shift) => $$fshr(self, self, shift);

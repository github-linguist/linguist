(***********************************************************************)
(*                                                                     *)
(*                         ATS/contrib/atshwxi                         *)
(*                                                                     *)
(***********************************************************************)

(*
** Copyright (C) 2013 Hongwei Xi, ATS Trustful Software, Inc.
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and associated documentation files (the "Software"),
** to deal in the Software without restriction, including without limitation
** the rights to use, copy, modify, merge, publish, distribute, sublicense,
** and/or sell copies of the Software, and to permit persons to whom the
** Software is furnished to do so, subject to the following stated conditions:
** 
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
** 
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
** THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
** FROM OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
** IN THE SOFTWARE.
*)

// Source: https://github.com/githwxi/ATS-Postiats-contrib/blob/04a984d9c08c1831f7dda8a05ce356db01f81850/contrib/libats-/hwxi/intinf/DATS/intinf_vt.dats

(* ****** ****** *)
//
// Author: Hongwei Xi
// Authoremail: hwxi AT gmail DOT com
// Start Time: April, 2013
//
(* ****** ****** *)

#include
"share/atspre_define.hats"

(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload
GMP = "{$LIBGMP}/SATS/gmp.sats"

(* ****** ****** *)

vtypedef mpz = $GMP.mpz_vt0ype

(* ****** ****** *)
//
staload "./../SATS/intinf.sats"
staload "./../SATS/intinf_vt.sats"
//
(* ****** ****** *)

macdef i2u (x) = g1int2uint_int_uint (,(x))

(* ****** ****** *)

local

assume
intinf_vtype
  (i: int) = // HX: [i] is a fake
  [l:addr] (mpz @ l, mfree_gc_v (l) | ptr l)
// end of [intinf_vtype]

in (* in of [local] *)

implement{}
intinf_make_int
  (i) = (x) where
{
//
val x = ptr_alloc<mpz> ()
val () = $GMP.mpz_init_set_int (!(x.2), i)
//
} (* end of [intinf_make_int] *)

implement{}
intinf_make_uint
  (i) = (x) where
{
//
val x = ptr_alloc<mpz> ()
val () = $GMP.mpz_init_set_uint (!(x.2), i)
//
} (* end of [intinf_make_uint] *)

implement{}
intinf_make_lint
  (i) = (x) where
{
//
val x = ptr_alloc<mpz> ()
val () = $GMP.mpz_init_set_lint (!(x.2), i)
//
} (* end of [intinf_make_lint] *)

implement{}
intinf_make_ulint
  (i) = (x) where
{
//
val x = ptr_alloc<mpz> ()
val () = $GMP.mpz_init_set_ulint (!(x.2), i)
//
} (* end of [intinf_make_ulint] *)

(* ****** ****** *)

implement{}
intinf_free (x) = let
  val (pfat, pfgc | p) = x
  val () = $GMP.mpz_clear (!p) in ptr_free (pfgc, pfat | p)
end (* end of [intinf_free] *)

(* ****** ****** *)

implement{}
intinf_get_int (x) = $GMP.mpz_get_int (!(x.2))
implement{}
intinf_get_lint (x) = $GMP.mpz_get_lint (!(x.2))

(* ****** ****** *)

implement{}
intinf_get_strptr
  (x, base) = $GMP.mpz_get_str_null (base, !(x.2))
// end of [intinf_get_strptr]

(* ****** ****** *)

implement{}
fprint_intinf_base
  (out, x, base) = let
  val nsz = $GMP.mpz_out_str (out, base, !(x.2))
in
//
if (nsz = 0) then
  exit_errmsg (1, "libgmp/gmp: fprint_intinf_base")
// end of [if]
//
end (* fprint_intinf_base *)

(* ****** ****** *)

implement{
} neg_intinf0
  (x) = (x) where
{
//
val () = $GMP.mpz_neg (!(x.2))
//
} (* end of [neg_intinf0] *)

implement{
} neg_intinf1
  (x) = (y) where
{
//
val y = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(y.2))
val () = $GMP.mpz_neg (!(y.2), !(x.2))
//
} (* end of [neg_intinf1] *)

(* ****** ****** *)

implement{
} abs_intinf0
  (x) = (x) where
{
//
val () = $GMP.mpz_abs (!(x.2))
//
} (* end of [abs_intinf0] *)

implement{
} abs_intinf1
  (x) = (y) where
{
//
val y = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(y.2))
val () = $GMP.mpz_abs (!(y.2), !(x.2))
//
} (* end of [abs_intinf1] *)

(* ****** ****** *)

implement{}
succ_intinf0 (x) = add_intinf0_int (x, 1)
implement{}
succ_intinf1 (x) = add_intinf1_int (x, 1)

(* ****** ****** *)

implement{}
pred_intinf0 (x) = sub_intinf0_int (x, 1)
implement{}
pred_intinf1 (x) = sub_intinf1_int (x, 1)

(* ****** ****** *)

implement{}
add_intinf0_int
  (x, y) = (x) where
{
//
val () = $GMP.mpz_add2_int (!(x.2), y)
//
} (* end of [add_intinf0_int] *)

implement{}
add_intinf1_int
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_add3_int (!(z.2), !(x.2), y)
//
} (* end of [add_intinf1_int] *)

(* ****** ****** *)

implement{}
add_int_intinf0 (x, y) = add_intinf0_int (y, x)
implement{}
add_int_intinf1 (x, y) = add_intinf1_int (y, x)

(* ****** ****** *)

implement{}
add_intinf0_intinf1
  (x, y) = (x) where
{
//
val () = $GMP.mpz_add2_mpz (!(x.2), !(y.2))
//
} (* end of [add_intinf0_intinf1] *)

implement{}
add_intinf1_intinf0
  (x, y) = (y) where
{
//
val () = $GMP.mpz_add2_mpz (!(y.2), !(x.2))
//
} (* end of [add_intinf1_intinf0] *)

(* ****** ****** *)

implement{}
add_intinf1_intinf1
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_add3_mpz (!(z.2), !(x.2), !(y.2))
//
} (* end of [add_intinf1_intinf1] *)

(* ****** ****** *)

implement{}
sub_intinf0_int
  (x, y) = (x) where
{
//
val () = $GMP.mpz_sub2_int (!(x.2), y)
//
} (* end of [sub_intinf0_int] *)

implement{}
sub_intinf1_int
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_sub3_int (!(z.2), !(x.2), y)
//
} (* end of [sub_intinf1_int] *)

(* ****** ****** *)

implement{}
sub_int_intinf0 (x, y) = let
  val z = sub_intinf0_int (y, x) in neg_intinf0 (z)
end (* end of [sub_int_intinf0] *)

implement{}
sub_int_intinf1 (x, y) = let
  val z = sub_intinf1_int (y, x) in neg_intinf0 (z)
end (* end of [sub_int_intinf1] *)

(* ****** ****** *)

implement{}
sub_intinf0_intinf1
  (x, y) = (x) where
{
//
val () = $GMP.mpz_sub2_mpz (!(x.2), !(y.2))
//
} (* end of [sub_intinf0_intinf1] *)

implement{}
sub_intinf1_intinf0
  (x, y) = neg_intinf0 (sub_intinf0_intinf1 (y, x))
// end of [sub_intinf1_intinf0]

implement{}
sub_intinf1_intinf1
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_sub3_mpz (!(z.2), !(x.2), !(y.2))
//
} (* end of [sub_intinf1_intinf1] *)

(* ****** ****** *)

implement{}
mul_intinf0_int
  (x, y) = (x) where
{
//
val () = $GMP.mpz_mul2_int (!(x.2), y)
//
} (* end of [mul_intinf0_int] *)

implement{}
mul_intinf1_int
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_mul3_int (!(z.2), !(x.2), y)
//
} (* end of [mul_intinf1_int] *)

(* ****** ****** *)

implement{}
mul_int_intinf0 (x, y) = mul_intinf0_int (y, x)
implement{}
mul_int_intinf1 (x, y) = mul_intinf1_int (y, x)

(* ****** ****** *)

implement{}
mul_intinf0_intinf1
  (x, y) = (x) where
{
//
val () = $GMP.mpz_mul2_mpz (!(x.2), !(y.2))
//
} (* end of [mul_intinf0_intinf1] *)

implement{}
mul_intinf1_intinf0
  (x, y) = (y) where
{
//
val () = $GMP.mpz_mul2_mpz (!(y.2), !(x.2))
//
} (* end of [mul_intinf0_intinf1] *)

(* ****** ****** *)

implement{}
mul_intinf1_intinf1
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_mul3_mpz (!(z.2), !(x.2), !(y.2))
//
} (* end of [mul_intinf1_intinf1] *)

(* ****** ****** *)

implement{}
div_intinf0_int
  {i,j} (x, y) = let
in
//
if y >= 0 then let
  val () = $GMP.mpz_tdiv2_q_uint (!(x.2), i2u(y)) in x
end else let
  val () = $GMP.mpz_tdiv2_q_uint (!(x.2), i2u(~y)) in neg_intinf0 (x)
end // end of [if]
//
end (* end of [div_intinf0_int] *)

implement{}
div_intinf1_int
  {i,j} (x, y) = let
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
//
in
//
if y >= 0 then let
  val () = $GMP.mpz_tdiv3_q_uint (!(z.2), !(x.2), i2u(y)) in z
end else let
  val () = $GMP.mpz_tdiv3_q_uint (!(z.2), !(x.2), i2u(~y)) in neg_intinf0 (z)
end // end of [if]
//
end (* end of [div_intinf1_int] *)

(* ****** ****** *)

implement{}
div_intinf0_intinf1
  (x, y) = (x) where
{
//
val () = $GMP.mpz_tdiv2_q_mpz (!(x.2), !(y.2))
//
} (* end of [div_intinf0_intinf1] *)

(* ****** ****** *)

implement{}
div_intinf1_intinf1
  (x, y) = (z) where
{
//
val z = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(z.2))
val () = $GMP.mpz_tdiv3_q_mpz (!(z.2), !(x.2), !(y.2))
//
} (* end of [div_intinf1_intinf1] *)

(* ****** ****** *)

implement{}
ndiv_intinf0_int (x, y) = div_intinf0_int (x, y)
implement{}
ndiv_intinf1_int (x, y) = div_intinf1_int (x, y)

(* ****** ****** *)

implement{}
nmod_intinf0_int
  {i,j} (x, y) = let
//
val r =
  $GMP.mpz_fdiv_uint (!(x.2), i2u(y))
val () = intinf_free (x)
//
in
  $UN.cast{intBtw(0,j)}(r)
end (* end of [nmod_intinf0_int] *)

implement{}
nmod_intinf1_int
  {i,j} (x, y) = let
//
val r = $GMP.mpz_fdiv_uint (!(x.2), i2u(y))
//
in
  $UN.cast{intBtw(0,j)}(r)
end (* end of [nmod_intinf1_int] *)

(* ****** ****** *)
//
// comparison-functions
//
(* ****** ****** *)

implement{}
lt_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn < 0 then true else false): bool
//
in
  $UN.cast{bool(i < j)}(sgn)
end // end of [lt_intinf_int]

implement{}
lt_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn < 0 then true else false): bool
//
in
  $UN.cast{bool(i < j)}(sgn)
end // end of [lt_intinf_intinf]

(* ****** ****** *)

implement{}
lte_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn <= 0 then true else false): bool
//
in
  $UN.cast{bool(i <= j)}(sgn)
end // end of [lte_intinf_int]

implement{}
lte_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn <= 0 then true else false): bool
//
in
  $UN.cast{bool(i <= j)}(sgn)
end // end of [lte_intinf_intinf]

(* ****** ****** *)

implement{}
gt_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn > 0 then true else false): bool
//
in
  $UN.cast{bool(i > j)}(sgn)
end // end of [gt_intinf_int]

implement{}
gt_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn > 0 then true else false): bool
//
in
  $UN.cast{bool(i > j)}(sgn)
end // end of [gt_intinf_intinf]

(* ****** ****** *)

implement{}
gte_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn >= 0 then true else false): bool
//
in
  $UN.cast{bool(i >= j)}(sgn)
end // end of [gte_intinf_int]

implement{}
gte_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn >= 0 then true else false): bool
//
in
  $UN.cast{bool(i >= j)}(sgn)
end // end of [gte_intinf_intinf]

(* ****** ****** *)

implement{}
eq_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn = 0 then true else false): bool
//
in
  $UN.cast{bool(i == j)}(sgn)
end // end of [eq_intinf_int]

implement{}
eq_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn = 0 then true else false): bool
//
in
  $UN.cast{bool(i == j)}(sgn)
end // end of [eq_intinf_intinf]

(* ****** ****** *)

implement{}
neq_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val ans = (if sgn != 0 then true else false): bool
//
in
  $UN.cast{bool(i != j)}(sgn)
end // end of [neq_intinf_int]

implement{}
neq_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val ans = (if sgn != 0 then true else false): bool
//
in
  $UN.cast{bool(i != j)}(sgn)
end // end of [neq_intinf_intinf]

(* ****** ****** *)

implement{}
compare_intinf_int
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(x.2), y)
val sgn = (if sgn < 0 then ~1 else (if sgn > 0 then 1 else 0)): int
//
in
  $UN.cast{int(sgn(i-j))}(sgn)
end // end of [compare_intinf_int]

implement{}
compare_int_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_int (!(y.2), x)
val sgn = (if sgn > 0 then ~1 else (if sgn < 0 then 1 else 0)): int
//
in
  $UN.cast{int(sgn(i-j))}(sgn)
end // end of [compare_int_intinf]

implement{}
compare_intinf_intinf
  {i,j} (x, y) = let
//
val sgn = $GMP.mpz_cmp_mpz (!(x.2), !(y.2))
val sgn = (if sgn < 0 then ~1 else (if sgn > 0 then 1 else 0)): int
//
in
  $UN.cast{int(sgn(i-j))}(sgn)
end // end of [compare_intinf_intinf]

(* ****** ****** *)

implement{}
pow_intinf_int
  (base, exp) = r where
{
//
val r = ptr_alloc<mpz> ()
val () = $GMP.mpz_init (!(r.2))
val () = $GMP.mpz_pow_uint (!(r.2), !(base.2), i2u(exp))
//
} (* end of [pow_intinf_int] *)

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

implement{}
print_intinf (x) = fprint_intinf (stdout_ref, x)
implement{}
prerr_intinf (x) = fprint_intinf (stderr_ref, x)
implement{}
fprint_intinf (out, x) = fprint_intinf_base (out, x, 10(*base*))

(* ****** ****** *)

(* end of [intinf_vt.dats] *)

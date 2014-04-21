(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-2013 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* Author: Hongwei Xi *)
(* Authoremail: hwxi AT cs DOT bu DOT edu *)
(* Start time: December, 2012 *)

(* ****** ****** *)
//
// HX: shared by linset_listord (* ordered list *)
// HX: shared by linset_avltree (* AVL-tree-based *)
//
(* ****** ****** *)
//
// HX-2013-02:
// for sets of nonlinear elements
//
absvtype set_vtype (a:t@ype+) = ptr
//
(* ****** ****** *)

vtypedef set (a:t0p) = set_vtype (a)

(* ****** ****** *)

fun{a:t0p}
compare_elt_elt (x1: a, x2: a):<> int

(* ****** ****** *)

fun{} linset_nil{a:t0p} ():<> set(a)
fun{} linset_make_nil{a:t0p} ():<> set(a)

(* ****** ****** *)

fun{a:t0p} linset_sing (x: a):<!wrt> set(a)
fun{a:t0p} linset_make_sing (x: a):<!wrt> set(a)

(* ****** ****** *)

fun{a:t0p}
linset_make_list (xs: List(INV(a))):<!wrt> set(a)

(* ****** ****** *)

fun{}
linset_is_nil {a:t0p} (xs: !set(INV(a))):<> bool
fun{}
linset_isnot_nil {a:t0p} (xs: !set(INV(a))):<> bool

(* ****** ****** *)

fun{a:t0p} linset_size (!set(INV(a))): size_t

(* ****** ****** *)

fun{a:t0p}
linset_is_member (xs: !set(INV(a)), x0: a):<> bool
fun{a:t0p}
linset_isnot_member (xs: !set(INV(a)), x0: a):<> bool

(* ****** ****** *)

fun{a:t0p}
linset_copy (!set(INV(a))):<!wrt> set(a)
fun{a:t0p}
linset_free (xs: set(INV(a))):<!wrt> void

(* ****** ****** *)
//
fun{a:t0p}
linset_insert
  (xs: &set(INV(a)) >> _, x0: a):<!wrt> bool
//
(* ****** ****** *)
//
fun{a:t0p}
linset_takeout
(
  &set(INV(a)) >> _, a, res: &(a?) >> opt(a, b)
) :<!wrt> #[b:bool] bool(b) // endfun
fun{a:t0p}
linset_takeout_opt (&set(INV(a)) >> _, a):<!wrt> Option_vt(a)
//
(* ****** ****** *)
//
fun{a:t0p}
linset_remove
  (xs: &set(INV(a)) >> _, x0: a):<!wrt> bool
//
(* ****** ****** *)
//
// HX: choosing an element in an unspecified manner
//
fun{a:t0p}
linset_choose
(
  xs: !set(INV(a)), x: &a? >> opt (a, b)
) :<!wrt> #[b:bool] bool(b)
//
fun{a:t0p}
linset_choose_opt (xs: !set(INV(a))):<!wrt> Option_vt(a)
//
(* ****** ****** *)

fun{a:t0p}
linset_takeoutmax
(
  xs: &set(INV(a)) >> _, res: &a? >> opt(a, b)
) :<!wrt> #[b:bool] bool (b)
fun{a:t0p}
linset_takeoutmax_opt (xs: &set(INV(a)) >> _):<!wrt> Option_vt(a)

(* ****** ****** *)

fun{a:t0p}
linset_takeoutmin
(
  xs: &set(INV(a)) >> _, res: &a? >> opt(a, b)
) :<!wrt> #[b:bool] bool (b)
fun{a:t0p}
linset_takeoutmin_opt (xs: &set(INV(a)) >> _):<!wrt> Option_vt(a)

(* ****** ****** *)
//
fun{}
fprint_linset$sep (FILEref): void // ", "
//
fun{a:t0p}
fprint_linset (out: FILEref, xs: !set(INV(a))): void
//
overload fprint with fprint_linset
//
(* ****** ****** *)
//
fun{
a:t0p}{env:vt0p
} linset_foreach$fwork
  (x: a, env: &(env) >> _): void
//
fun{a:t0p}
linset_foreach (set: !set(INV(a))): void
fun{
a:t0p}{env:vt0p
} linset_foreach_env
  (set: !set(INV(a)), env: &(env) >> _): void
// end of [linset_foreach_env]
//
(* ****** ****** *)

fun{a:t0p}
linset_listize (xs: set(INV(a))): List0_vt (a)

(* ****** ****** *)

fun{a:t0p}
linset_listize1 (xs: !set(INV(a))): List0_vt (a)

(* ****** ****** *)

(* end of [linset.hats] *)

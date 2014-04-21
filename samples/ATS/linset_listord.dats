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
(* Start time: February, 2013 *)

(* ****** ****** *)
//
// HX-2013-08:
// a set is represented as a sorted list in descending order;
// note that descending order is chosen to faciliate set comparison
//
(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libats/SATS/linset_listord.sats"

(* ****** ****** *)

#include "./SHARE/linset.hats" // code reuse
#include "./SHARE/linset_node.hats" // code reuse

(* ****** ****** *)

assume
set_vtype (elt:t@ype) = List0_vt (elt)

(* ****** ****** *)

implement{}
linset_nil () = list_vt_nil ()
implement{}
linset_make_nil () = list_vt_nil ()

(* ****** ****** *)

implement
{a}(*tmp*)
linset_sing
  (x) = list_vt_cons{a}(x, list_vt_nil)
// end of [linset_sing]
implement{a}
linset_make_sing
  (x) = list_vt_cons{a}(x, list_vt_nil)
// end of [linset_make_sing]

(* ****** ****** *)

implement{}
linset_is_nil (xs) = list_vt_is_nil (xs)
implement{}
linset_isnot_nil (xs) = list_vt_is_cons (xs)

(* ****** ****** *)

implement{a}
linset_size (xs) =
  let val n = list_vt_length(xs) in i2sz(n) end
// end of [linset_size]

(* ****** ****** *)

implement{a}
linset_is_member
  (xs, x0) = let
//
fun aux
  {n:nat} .<n>.
(
  xs: !list_vt (a, n)
) :<> bool = let
in
//
case+ xs of
| list_vt_cons (x, xs) => let
    val sgn = compare_elt_elt<a> (x0, x) in
    if sgn > 0 then false else (if sgn < 0 then aux (xs) else true)
  end // end of [list_vt_cons]
| list_vt_nil ((*void*)) => false
//
end // end of [aux]
//
in
  aux (xs)
end // end of [linset_is_member]

(* ****** ****** *)

implement{a}
linset_copy (xs) = list_vt_copy<a> (xs)
implement{a}
linset_free (xs) = list_vt_free<a> (xs)

(* ****** ****** *)

implement{a}
linset_insert
  (xs, x0) = let
//
fun
mynode_cons
  {n:nat} .<>.
(
  nx: mynode1 (a), xs: list_vt (a, n)
) : list_vt (a, n+1) = let
//
val xs1 =
$UN.castvwtp0{List1_vt(a)}(nx)
val+@list_vt_cons (_, xs2) = xs1
prval () = $UN.cast2void (xs2); val () = (xs2 := xs)
//
in
  fold@ (xs1); xs1
end // end of [mynode_cons]
//
fun ins
  {n:nat} .<n>. // tail-recursive
(
  xs: &list_vt (a, n) >> list_vt (a, n1)
) : #[n1:nat | n <= n1; n1 <= n+1] bool =
(
case+ xs of
| @list_vt_cons
    (x, xs1) => let
    val sgn =
      compare_elt_elt<a> (x0, x)
    // end of [val]
  in
    if sgn > 0 then let
      prval () = fold@ (xs)
      val nx = mynode_make_elt<a> (x0)
      val ((*void*)) = xs := mynode_cons (nx, xs)
    in
      false
    end else if sgn < 0 then let
      val ans = ins (xs1)
      prval () = fold@ (xs)
    in
      ans
    end else let // [x0] is found
      prval () = fold@ (xs)
    in
      true (* [x0] in [xs] *)
    end (* end of [if] *)
  end // end of [list_vt_cons]
| list_vt_nil () => let
    val nx = mynode_make_elt<a> (x0)
    val ((*void*)) = xs := mynode_cons (nx, xs)
  in
    false
  end // end of [list_vt_nil]
) (* end of [ins] *)
//
in
  $effmask_all (ins (xs))
end // end of [linset_insert]

(* ****** ****** *)

(*
//
HX-2013-08:
[linset_remove] moved up
//
implement{a}
linset_remove
  (xs, x0) = let
//
fun rem
  {n:nat} .<n>. // tail-recursive
(
  xs: &list_vt (a, n) >> list_vt (a, n1)
) : #[n1:nat | n1 <= n; n <= n1+1] bool =
(
case+ xs of
| @list_vt_cons
    (x, xs1) => let
    val sgn =
      compare_elt_elt<a> (x0, x)
    // end of [val]
  in
    if sgn > 0 then let
      prval () = fold@ (xs)
    in
      false
    end else if sgn < 0 then let
      val ans = rem (xs1)
      prval () = fold@ (xs)
    in
      ans
    end else let // x0 = x
      val xs1_ = xs1
      val ((*void*)) = free@{a}{0}(xs)
      val () = xs := xs1_
    in
      true // [x0] in [xs]
    end (* end of [if] *)
  end // end of [list_vt_cons]
| list_vt_nil () => false
) (* end of [rem] *)
//
in
  $effmask_all (rem (xs))
end // end of [linset_remove]
*)

(* ****** ****** *)
(*
** By Brandon Barker
*)
implement
{a}(*tmp*)
linset_choose
  (xs, x0) = let
in
//
case+ xs of
| list_vt_cons
    (x, xs1) => let
    val () = x0 := x
    prval () = opt_some{a}(x0)
  in
    true
  end // end of [list_vt_cons]
| list_vt_nil () => let
    prval () = opt_none{a}(x0)
  in
    false
  end // end of [list_vt_nil]
//
end // end of [linset_choose]

(* ****** ****** *)

implement
{a}{env}
linset_foreach_env (xs, env) = let
//
implement
list_vt_foreach$fwork<a><env>
  (x, env) = linset_foreach$fwork<a><env> (x, env)
//
in
  list_vt_foreach_env<a><env> (xs, env)
end // end of [linset_foreach_env]

(* ****** ****** *)

implement{a}
linset_listize (xs) = xs

(* ****** ****** *)

implement{a}
linset_listize1 (xs) = list_vt_copy (xs)

(* ****** ****** *)
//
// HX: functions for processing mynodes
//
(* ****** ****** *)

implement{
} mynode_null{a} () =
  $UN.castvwtp0{mynode(a,null)}(the_null_ptr)
// end of [mynode_null]

(* ****** ****** *)

implement
{a}(*tmp*)
mynode_make_elt
  (x) = let
//
val nx = list_vt_cons{a}{0}(x, _ )
//
in
  $UN.castvwtp0{mynode1(a)}(nx)
end // end of [mynode_make_elt]

(* ****** ****** *)

implement{
} mynode_free
  {a}(nx) = () where {
val nx =
  $UN.castvwtp0{List1_vt(a)}(nx)
//
val+~list_vt_cons (_, nx2) = nx
//
prval ((*void*)) = $UN.cast2void (nx2)
//
} (* end of [mynode_free] *)

(* ****** ****** *)

implement
{a}(*tmp*)
mynode_get_elt
  (nx) = (x) where {
//
val nx1 =
  $UN.castvwtp1{List1_vt(a)}(nx)
//
val+list_vt_cons (x, _) = nx1
//
prval ((*void*)) = $UN.cast2void (nx1)
//
} (* end of [mynode_get_elt] *)

(* ****** ****** *)

implement
{a}(*tmp*)
mynode_set_elt
  {l} (nx, x0) =
{
//
val nx1 =
  $UN.castvwtp1{List1_vt(a)}(nx)
//
val+@list_vt_cons (x, _) = nx1
//
val () = x := x0
//
prval () = fold@ (nx1)
prval () = $UN.cast2void (nx1)
//
prval () = __assert (nx) where
{
  extern praxi __assert (nx: !mynode(a?, l) >> mynode (a, l)): void
} (* end of [prval] *)
//
} (* end of [mynode_set_elt] *)

(* ****** ****** *)

implement
{a}(*tmp*)
mynode_getfree_elt
  (nx) = (x) where {
//
val nx =
  $UN.castvwtp0{List1_vt(a)}(nx)
//
val+~list_vt_cons (x, nx2) = nx
//
prval ((*void*)) = $UN.cast2void (nx2)
//
} (* end of [mynode_getfree_elt] *)

(* ****** ****** *)

(*
fun{a:t0p}
linset_takeout_ngc
  (set: &set(INV(a)) >> _, x0: a):<!wrt> mynode0 (a)
// end of [linset_takeout_ngc]
*)
implement
{a}(*tmp*)
linset_takeout_ngc
  (set, x0) = let
//
fun takeout
(
  xs: &List0_vt (a) >> _
) : mynode0(a) = let
in
//
case+ xs of
| @list_vt_cons
    (x, xs1) => let
    prval pf_x = view@x
    prval pf_xs1 = view@xs1
    val sgn =
      compare_elt_elt<a> (x0, x)
    // end of [val]
  in
    if sgn > 0 then let
      prval () = fold@ (xs)
    in
      mynode_null{a}((*void*))
    end else if sgn < 0 then let
      val res = takeout (xs1)
      prval ((*void*)) = fold@ (xs)
    in
      res
    end else let // x0 = x
      val xs1_ = xs1
      val res = $UN.castvwtp0{mynode1(a)}((pf_x, pf_xs1 | xs))
      val () = xs := xs1_
    in
      res // [x0] in [xs]
    end (* end of [if] *)
  end // end of [list_vt_cons]
| list_vt_nil () => mynode_null{a}((*void*))
//
end (* end of [takeout] *)
//
in
  $effmask_all (takeout (set))
end // end of [linset_takeout_ngc]

(* ****** ****** *)

implement
{a}(*tmp*)
linset_takeoutmax_ngc
  (xs) = let
in
//
case+ xs of
| @list_vt_cons
    (x, xs1) => let
    prval pf_x = view@x
    prval pf_xs1 = view@xs1
    val xs_ = xs
    val () = xs := xs1
  in
    $UN.castvwtp0{mynode1(a)}((pf_x, pf_xs1 | xs_))
  end // end of [list_vt_cons]
| @list_vt_nil () => let
    prval () = fold@ (xs)
  in
    mynode_null{a}((*void*))
  end // end of [list_vt_nil]
//
end // end of [linset_takeoutmax_ngc]

(* ****** ****** *)

implement
{a}(*tmp*)
linset_takeoutmin_ngc
  (xs) = let
//
fun unsnoc
  {n:pos} .<n>.
(
  xs: &list_vt (a, n) >> list_vt (a, n-1)
) :<!wrt> mynode1 (a) = let
//
val+@list_vt_cons (x, xs1) = xs
//
prval pf_x = view@x and pf_xs1 = view@xs1
//
in
//
case+ xs1 of
| list_vt_cons _ =>
    let val res = unsnoc(xs1) in fold@xs; res end
  // end of [list_vt_cons]
| list_vt_nil () => let
    val xs_ = xs
    val () = xs := list_vt_nil{a}()
  in
    $UN.castvwtp0{mynode1(a)}((pf_x, pf_xs1 | xs_))
  end // end of [list_vt_nil]
//
end // end of [unsnoc]
//
in
//
case+ xs of
| list_vt_cons _ => unsnoc (xs)
| list_vt_nil () => mynode_null{a}((*void*))
//
end // end of [linset_takeoutmin_ngc]

(* ****** ****** *)

(* end of [linset_listord.dats] *)

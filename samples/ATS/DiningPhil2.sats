(* ****** ****** *)
//
// HX-2013-11
//
// Implementing a variant of
// the problem of Dining Philosophers
//
(* ****** ****** *)

#include
"share/atspre_define.hats"

(* ****** ****** *)

staload "{$LIBATSHWXI}/teaching/mythread/SATS/channel.sats"

(* ****** ****** *)

%{#
#define NPHIL 5
%} // end of [%{#]
#define NPHIL 5

(* ****** ****** *)

typedef nphil = natLt(NPHIL)

(* ****** ****** *)

fun phil_left (n: nphil): nphil
fun phil_right (n: nphil): nphil

(* ****** ****** *)
//
fun phil_loop (n: nphil): void
//
(* ****** ****** *)

fun cleaner_loop ((*void*)): void

(* ****** ****** *)

absvtype fork_vtype = ptr
vtypedef fork = fork_vtype

(* ****** ****** *)

fun fork_get_num (!fork): nphil

(* ****** ****** *)

fun phil_dine
  (n: nphil, lf: !fork, rf: !fork): void
// end of [phil_dine]

fun phil_think (n: nphil): void

(* ****** ****** *)

fun cleaner_wash (f: !fork): void
fun cleaner_return (f: fork): void

(* ****** ****** *)
//
fun fork_changet (n: nphil): channel(fork)
//
fun forktray_changet ((*void*)): channel(fork)
//
(* ****** ****** *)

(* end of [DiningPhil2.sats] *)

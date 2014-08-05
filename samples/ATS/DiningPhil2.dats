(* ****** ****** *)
//
// HX-2013-11
//
// Implementing a variant of
// the problem of Dining Philosophers
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "libc/SATS/stdlib.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

staload "{$LIBATSHWXI}/teaching/mythread/SATS/channel.sats"

(* ****** ****** *)

staload _ = "libats/DATS/deqarray.dats"
staload _ = "{$LIBATSHWXI}/teaching/mythread/DATS/channel.dats"

(* ****** ****** *)

staload "./DiningPhil2.sats"

(* ****** ****** *)

implement phil_left (n) = n
implement phil_right (n) = (n+1) \nmod NPHIL

(* ****** ****** *)
//
extern
fun randsleep (n: intGte(1)): void
//
implement
randsleep (n) =
  ignoret (sleep($UN.cast{uInt}(rand() mod n + 1)))
// end of [randsleep]
//
(* ****** ****** *)

implement
phil_think (n) =
{
val () = println! ("phil_think(", n, ") starts")
val () = randsleep (6)
val () = println! ("phil_think(", n, ") finishes")
}

(* ****** ****** *)

implement
phil_dine (n, lf, rf) =
{
val () = println! ("phil_dine(", n, ") starts")
val () = randsleep (3)
val () = println! ("phil_dine(", n, ") finishes")
}

(* ****** ****** *)

implement
phil_loop (n) = let
//
val () = phil_think (n)
//
val nl = phil_left (n)
val nr = phil_right (n)
//
val ch_lfork = fork_changet (nl)
val ch_rfork = fork_changet (nr)
//
val lf = channel_takeout (ch_lfork)
val () = println! ("phil_loop(", n, ") picks left fork")
//
val () = randsleep (2) // HX: try to actively induce deadlock
//
val rf = channel_takeout (ch_rfork)
val () = println! ("phil_loop(", n, ") picks right fork")
//
val () = phil_dine (n, lf, rf)
//
val ch_forktray = forktray_changet ()
val () = channel_insert (ch_forktray, lf)
val () = channel_insert (ch_forktray, rf)
//
in
  phil_loop (n)
end // end of [phil_loop]

(* ****** ****** *)

implement
cleaner_wash (f) =
{
val f = fork_get_num (f)
val () = println! ("cleaner_wash(", f, ") starts")
val () = randsleep (1)
val () = println! ("cleaner_wash(", f, ") finishes")
}

(* ****** ****** *)

implement
cleaner_return (f) =
{
  val n = fork_get_num (f)
  val ch = fork_changet (n)
  val () = channel_insert (ch, f)
}

(* ****** ****** *)

implement
cleaner_loop () = let
//
val ch = forktray_changet ()
val f0 = channel_takeout (ch)
//
val () = cleaner_wash (f0)
val () = cleaner_return (f0)
//
in
  cleaner_loop ()
end // end of [cleaner_loop]

(* ****** ****** *)

dynload "DiningPhil2.sats"
dynload "DiningPhil2_fork.dats"
dynload "DiningPhil2_thread.dats"

(* ****** ****** *)

local
//
staload
"{$LIBATSHWXI}/teaching/mythread/SATS/mythread.sats"
//
in (* in of [local] *)
//
val () = mythread_create_cloptr (llam () => phil_loop (0))
val () = mythread_create_cloptr (llam () => phil_loop (1))
val () = mythread_create_cloptr (llam () => phil_loop (2))
val () = mythread_create_cloptr (llam () => phil_loop (3))
val () = mythread_create_cloptr (llam () => phil_loop (4))
//
val () = mythread_create_cloptr (llam () => cleaner_loop ())
//
end // end of [local]

(* ****** ****** *)

implement
main0 () =
{
//
val () = println! ("DiningPhil2: starting")
val ((*void*)) = while (true) ignoret (sleep(1))
//
} (* end of [main0] *)

(* ****** ****** *)

(* end of [DiningPhil2.dats] *)

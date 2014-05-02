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

staload "{$LIBATSHWXI}/teaching/mythread/SATS/channel.sats"

(* ****** ****** *)

staload _ = "libats/DATS/deqarray.dats"
staload _ = "{$LIBATSHWXI}/teaching/mythread/DATS/channel.dats"

(* ****** ****** *)

staload "./DiningPhil2.sats"

(* ****** ****** *)

datavtype fork = FORK of (nphil)

(* ****** ****** *)

assume fork_vtype = fork

(* ****** ****** *)

implement
fork_get_num (f) = let val FORK(n) = f in n end

(* ****** ****** *)

local

val
the_forkarray = let
//
typedef t = channel(fork)
//
implement
array_tabulate$fopr<t>
  (n) = ch where
{
  val n = $UN.cast{nphil}(n)
  val ch = channel_create_exn<fork> (i2sz(2))
  val () = channel_insert (ch, FORK (n))
}
//
in
  arrayref_tabulate<t> (i2sz(NPHIL))
end // end of [val]

in (* in of [local] *)

implement fork_changet (n) = the_forkarray[n]

end // end of [local]

(* ****** ****** *)

local

val the_forktray =
  channel_create_exn<fork> (i2sz(NPHIL+1))

in (* in of [local] *)

implement forktray_changet () = the_forktray

end // end of [local]

(* ****** ****** *)

(* end of [DiningPhil2_fork.dats] *)

(* ****** ****** *)
//
// HX-2013-11
//
// Implementing a variant of
// the problem of Dining Philosophers
//
(* ****** ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
//
(* ****** ****** *)

staload "{$LIBATSHWXI}/teaching/mythread/SATS/mythread.sats"

(* ****** ****** *)

local
//
#include "{$LIBATSHWXI}/teaching/mythread/DATS/mythread.dats"
//
in (* in of [local] *)
//
// HX: it is intentionally left to be empty
//
end // end of [local]

(* ****** ****** *)

local
//
#include "{$LIBATSHWXI}/teaching/mythread/DATS/mythread_posix.dats"
//
in (* in of [local] *)
//
// HX: it is intentionally left to be empty
//
end // end of [local]

(* ****** ****** *)

(* end of [DiningPhil2_thread.dats] *)

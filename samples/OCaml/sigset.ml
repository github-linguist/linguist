(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open PosixTypes
open Ctypes
open Foreign

type t = sigset_t ptr

let t = ptr sigset_t

(* This function initializes the signal set set to exclude all of the defined
   signals. It always returns 0.  *)
let sigemptyset = foreign "sigemptyset" (ptr sigset_t @-> returning int)

let empty () =
  let setp = allocate_n ~count:1 sigset_t in begin
    ignore (sigemptyset setp);
    setp
  end

(* This function initializes the signal set set to include all of the defined
   signals. Again, the return value is 0. *)
let sigfillset = foreign "sigfillset" (ptr sigset_t @-> returning int)

let full () =
  let setp = allocate_n ~count:1 sigset_t in begin
    ignore (sigfillset setp);
    setp
  end

(* This function adds the signal signum to the signal set set. All sigaddset
   does is modify set; it does not block or unblock any signals.

   The return value is 0 on success and -1 on failure. The following errno
   error condition is defined for this function:

   EINVAL The signum argument doesn't specify a valid signal. 
*)
let sigaddset = foreign "sigaddset" ~check_errno:true
  (ptr sigset_t @-> int @-> returning int)

let add set signal = ignore (sigaddset set signal)

(* This function removes the signal signum from the signal set set. All
   sigdelset does is modify set; it does not block or unblock any signals.

   The return value and error conditions are the same as for
   sigaddset.  *)
let sigdelset = foreign "sigdelset" ~check_errno:true
  (ptr sigset_t @-> int @-> returning int)

let del set signal = ignore (sigdelset set signal)

(* The sigismember function tests whether the signal signum is a member of the
   signal set set. It returns 1 if the signal is in the set, 0 if not, and -1 if
   there is an error.

   The following errno error condition is defined for this function:

   EINVAL The signum argument doesn't specify a valid signal. 
*)
let sigismember = foreign "sigismember" ~check_errno:true
  (ptr sigset_t @-> int @-> returning int)

let mem set signal = sigismember set signal <> 0

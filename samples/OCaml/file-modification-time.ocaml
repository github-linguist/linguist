#load "unix.cma";;
open Unix;;
let mtime = (stat filename).st_mtime;; (* seconds since the epoch *)

utimes filename (stat filename).st_atime (time ());;
(* keep atime unchanged
   set mtime to current time *)

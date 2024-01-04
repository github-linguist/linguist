(* top.mod main top level program module for mc.

Copyright (C) 2015-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE top ;

FROM mcOptions IMPORT handleOptions ;
FROM mcComp IMPORT compile ;
FROM M2RTS IMPORT ExitOnHalt ;
FROM mcStream IMPORT removeFiles ;
FROM libc IMPORT atexit, perror ;


(*
   wrapRemoveFiles - call removeFiles and return 0.
*)

PROCEDURE wrapRemoveFiles () : INTEGER ;
BEGIN
   removeFiles ;
   RETURN 0
END wrapRemoveFiles ;


(*
   init - translate the source file after handling all the
          program arguments.
*)

PROCEDURE init ;
BEGIN
   IF atexit (wrapRemoveFiles) # 0
   THEN
      perror ("atexit failed")
   END ;
   ExitOnHalt (1) ;
   compile (handleOptions ())
END init ;


BEGIN
   init
END top.

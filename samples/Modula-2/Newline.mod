(*!m2iso*) (* Copyright (c) 2017 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE Newline;

(* Newline mode management *)

VAR defaultMode : Mode;


PROCEDURE SetMode ( mode : Mode );
(* Sets the default newline mode. *)

BEGIN
  defaultMode := mode
END SetMode;


PROCEDURE mode ( ) : Mode;
(* Returns the default newline mode. *)

BEGIN
  RETURN defaultMode;
END mode;


BEGIN
  defaultMode := LF
END Newline.

(* License Exception:
   This file has been relicensed to the Linguist project under MIT license
   terms for use with Linguist. For other uses the LGPL 2.1 license applies. *)

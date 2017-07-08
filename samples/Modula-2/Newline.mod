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
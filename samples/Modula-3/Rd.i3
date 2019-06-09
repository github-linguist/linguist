(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Nov  8 17:21:08 PST 1993 by mcjones        *)
(*      modified on Tue Jul  6 13:05:03 PDT 1993 by wobber         *)
(*      modified on Tue Jun 15 09:42:56 1993 by gnelson        *)
(*      modified on Wed Apr 22 16:41:35 PDT 1992 by kalsow     *)
(*      modified on Mon Dec 24 01:10:09 1990 by muller         *)


(* An "Rd.T" (or ``reader'') is a character input stream.  The basic
   operation on a reader is "GetChar", which returns the source
   character at the ``current position'' and advances the current
   position by one.  Some readers are ``seekable'', which means that
   they also allow setting the current position anywhere in the
   source.  For example, readers from random access files are
   seekable; readers from terminals and sequential files are not.
   \index{character input stream}
   \index{input stream}
   \index{stream!input}
   \index{reader}

   Some readers are ``intermittent'', which means that the source of
   the reader trickles in rather than being available to the
   implementation all at once.  For example, the input stream from an
   interactive terminal is intermittent.  An intermittent reader is
   never seekable.

   Abstractly, a reader "rd" consists of

| len(rd)           `the number of source characters`
| src(rd)           `a sequence of length "len(rd)+1"`
| cur(rd)           `an integer in the range "[0..len(rd)]"`
| avail(rd)         `an integer in the range "[cur(rd)..len(rd)+1]"`
| closed(rd)        `a boolean`
| seekable(rd)      `a boolean`
| intermittent(rd)  `a boolean`

   These values are not necessarily directly represented in the data
   fields of a reader object.  In particular, for an intermittent
   reader, "len(rd)" may be unknown to the implementation.  But in
   principle the values determine the state of the reader.

   The sequence "src(rd)" is zero-based: "src(rd)[i]" is valid for "i"
   from 0 to "len(rd"). The first "len(rd)" elements of "src" are the
   characters that are the source of the reader.  The final element is
   a special value "eof" used to represent end-of-file.  The value
   "eof" is not a character.

   The value of "cur(rd)" is the index in "src(rd)" of the next
   character to be returned by "GetChar", unless "cur(rd) = len(rd)",
   in which case a call to "GetChar" will raise the exception
   "EndOfFile".

   The value of "avail(rd)" is important for intermittent readers: the
   elements whose indexes in "src(rd)" are in the range
   "[cur(rd)..avail(rd)-1]" are available to the implementation and
   can be read by clients without blocking.  If the client tries to
   read further, the implementation will block waiting for the other
   characters.  If "rd" is not intermittent, then "avail(rd)" is equal
   to "len(rd)+1".  If "rd" is intermittent, then "avail(rd)" can
   increase asynchronously, although the procedures in this interface
   are atomic with respect to such increases.

   The definitions above encompass readers with infinite sources.  If
   "rd" is such a reader, then "len(rd)" and "len(rd)+1" are both
   infinity, and there is no final "eof" value.

   Every reader is a monitor; that is, it contains an internal lock
   that is acquired and held for each operation in this interface, so
   that concurrent operations will appear atomic.  For faster,
   unmonitored access, see the "UnsafeRd" interface.

   If you are implementing a long-lived reader class, such as a pipe 
   or TCP stream, the index of the reader may eventually overflow, 
   causing the program to crash with a bounds fault.  We recommend
   that you provide an operation to reset the reader index, which the
   client can call periodically. *)

INTERFACE Rd;

IMPORT AtomList;
FROM Thread IMPORT Alerted;

TYPE T <: ROOT;

EXCEPTION EndOfFile; Failure(AtomList.T); 

(* Since there are many classes of readers, there are many ways that a
   reader can break---for example, the connection to a terminal can be
   broken, the disk can signal a read error, etc.  All problems of
   this sort are reported by raising the exception "Failure".  The
   documentation of a reader class should specify what failures the
   class can raise and how they are encoded in the argument to
   "Failure".

   Illegal operations cause a checked runtime error. *)

PROCEDURE GetChar(rd: T): CHAR
  RAISES {EndOfFile, Failure, Alerted};
(* Return the next character from "rd". More precisely, this is
   equivalent to the following, in which "res" is a local variable of
   type "CHAR": *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| `Block until "avail(rd) > cur(rd)"`;
| IF cur(rd) = len(rd) THEN
|   RAISE EndOfFile
| ELSE
|   res := src(rd)[cur(rd)]; INC(cur(rd)); RETURN res
| END
*)

PROCEDURE GetWideChar(rd: T): WIDECHAR
  RAISES {EndOfFile, Failure, Alerted};
(* IF closed(rd) THEN `Cause checked runtime error` END;
   Return the next wide character from "rd".  Two 8-bit bytes are
   read from "rd" and concatenated in little-endian order to
   form a 16-bit character.  That is, the first byte read will be the
   low-order 8 bits of the result and the second byte will be the
   high-order 8 bits. *)

(* Many operations on a reader can wait indefinitely.  For example,
   "GetChar" can wait if the user is not typing. In general these waits
   are alertable, so each procedure that might wait includes
   "Thread.Alerted" in its "RAISES" clause. *)

PROCEDURE EOF(rd: T): BOOLEAN RAISES {Failure, Alerted};
(* Return "TRUE" iff "rd" is at end-of-file. More precisely, this is
   equivalent to: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| `Block until "avail(rd) > cur(rd)"`;
| RETURN cur(rd) = len(rd)
*)

(* Notice that on an intermittent reader, "EOF" can block. For example, if
   there are no characters buffered in a terminal reader, "EOF" must wait
   until the user types one before it can determine whether he typed the
   special key signalling end-of-file. If you are using "EOF" in an
   interactive input loop, the right sequence of operations is:
   \begin{enumerate}
   \item prompt the user;
   \item call "EOF", which probably waits on user input;
   \item presuming that "EOF" returned "FALSE", read the user's input.
   \end{enumerate} *)

PROCEDURE UnGetChar(rd: T) RAISES {};
(* ``Push back'' the last character read from "rd", so that the next
   call to "GetChar" will read it again. More precisely, this is
   equivalent to the following: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| IF cur(rd) > 0 THEN DEC(cur(rd)) END

   except there is a special rule: "UngetChar(rd)" is guaranteed to work only
   if "GetChar(rd)" or "EOF(rd)" was the last operation on "rd".  Thus
   "UngetChar" cannot be called twice in a row, or after "Seek".
   If this rule is violated, the implementation is allowed (but
   not required) to cause a checked runtime error. *)

CONST UnGetCapacity = 8; 
TYPE UnGetCount = [ 0 .. UnGetCapacity ]; 

PROCEDURE UnGetCharMulti(rd: T; n: UnGetCount:= 1): CARDINAL (* Number actually ungotten.*);
(* Like UnGetChar, but try to push back the last n characters.  Can accumulate at 
   least MIN(UnGetCapacity,Index(rd)) ungotten and not reread characters.  
   UnGetCharMulti reserves the right to exceed this on some calls.  Result may be less
   than n, if this would be exceeded.  
*) 

PROCEDURE CharsReady(rd: T): CARDINAL RAISES {Failure};
(* Return some number of characters that can be read without
   indefinite waiting. The ``end of file marker'' counts as one
   character for this purpose, so "CharsReady" will return 1, not 0,
   if "EOF(rd)" is true. More precisely, this is equivalent to the
   following: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| IF avail(rd) = cur(rd) THEN
|   RETURN 0
| ELSE 
|   RETURN `some number in the range "[1~..~avail(rd) - cur(rd)]"`
| END;
*)

(* Warning: "CharsReady" can return a result less than "avail(rd) -
   cur(rd)"; also, more characters might trickle in just as
   "CharsReady" returns. So the code to flush buffered input without
   blocking requires a loop:

| LOOP
|   n := Rd.CharsReady(rd);
|   IF n = 0 THEN EXIT END;
|   FOR i := 1 TO n DO EVAL Rd.GetChar(rd) END
| END;
*)

PROCEDURE GetSub(rd: T; VAR (*OUT*) str: ARRAY OF CHAR)
  : CARDINAL RAISES {Failure, Alerted};
(* Read from "rd" into "str" until "rd" is exhausted or "str" is
   filled. More precisely, this is equivalent to the following, in
   which "i" is a local variable: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| i := 0;
| WHILE i # NUMBER(str) AND NOT EOF(rd) DO
|   str[i] := GetChar(rd); INC(i)
| END;
| RETURN i
*)

PROCEDURE GetWideSub(rd: T; VAR (*OUT*) str: ARRAY OF WIDECHAR)
  : CARDINAL RAISES {Failure, Alerted};
(* Read from "rd" into "str" until "rd" is exhausted or "str" is
   filled. More precisely, this is equivalent to the following, in
   which "i" is a local variable: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| i := 0;
| WHILE i # NUMBER(str) AND NOT EOF(rd) DO
|   str[i] := GetWideChar(rd); INC(i)
| END;
| RETURN i
*)

PROCEDURE GetSubLine(rd: T; VAR (*OUT*) str: ARRAY OF CHAR)
  : CARDINAL RAISES {Failure, Alerted};
(* Read from "rd" into "str" until a newline is read, "rd" is
   exhausted, or "str" is filled. More precisely, this is equivalent
   to the following, in which "i" is a local variable: *)
(*
| IF closed(rd) AND NUMBER(str) > 0 THEN `Cause checked runtime error` END;
| i := 0;
| WHILE
|   i # NUMBER(str) AND
|   (i = 0 OR str[i-1] # '\n') AND
|   NOT EOF(rd) 
| DO
|   str[i] := GetChar(rd); INC(i)
| END;
| RETURN i
*)

(* Note that "GetLine" strips the terminating line break, while
   "GetSubLine" does not. *)

PROCEDURE GetWideSubLine(rd: T; VAR (*OUT*) str: ARRAY OF WIDECHAR)
  : CARDINAL RAISES {Failure, Alerted};
(* Read from "rd" into "str" until a newline is read, "rd" is
   exhausted, or "str" is filled. *)

PROCEDURE GetText(rd: T; len: CARDINAL): TEXT
  RAISES {Failure, Alerted};
(* Read from "rd" until it is exhausted or "len" characters have been
   read, and return the result as a "TEXT".  More precisely, this is
   equivalent to the following, in which "i" and "res" are local
   variables: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| res := ""; i := 0;
| WHILE i # len AND NOT EOF(rd) DO
|   res := res & Text.FromChar(GetChar(rd));
|   INC(i)
| END;
| RETURN res
*)

PROCEDURE GetWideText(rd: T; len: CARDINAL): TEXT
  RAISES {Failure, Alerted};
(* Read from "rd" until it is exhausted or "len" wide characters have been
   read, and return the result as a "TEXT".  More precisely, this is
   equivalent to the following, in which "i" and "res" are local
   variables: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| res := ""; i := 0;
| WHILE i # len AND NOT EOF(rd) DO
|   res := res & Text.FromWideChar(GetChar(rd));
|   INC(i)
| END;
| RETURN res
*)

PROCEDURE GetLine(rd: T): TEXT
  RAISES {EndOfFile, Failure, Alerted};
(* If "EOF(rd)" then raise "EndOfFile".  Otherwise, read characters
   until a line break is read or "rd" is exhausted, and return the
   result as a "TEXT"---but discard the line break if it is present.
   A line break is either {\tt \char'42\char'134n\char'42} or {\tt
   \char'42\char'134r\char'134n\char'42} More precisely, this is
   equivalent to the following, in which "ch" and "res" are local
   variables: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| IF EOF(rd) THEN RAISE EndOfFile END;
| res := ""; ch := '\000'; (* any char but newline *)
| WHILE ch # '\n' AND NOT EOF(rd) DO
|   ch := GetChar(rd);
|   IF ch = '\n' THEN
|     IF NOT Text.Empty(res) AND
|         Text.GetChar(res, Text.Length(res)-1) = '\r' THEN
|       res := Text.Sub(res, 0, Text.Length(res)-1)
|     END
|   ELSE
|     res := res & Text.FromChar(ch)
|   END
| RETURN res
*)

PROCEDURE GetWideLine(rd: T): TEXT
  RAISES {EndOfFile, Failure, Alerted};
(* If "EOF(rd)" then raise "EndOfFile".  Otherwise, read wide characters
   until a line break is read or "rd" is exhausted, and return the
   result as a "TEXT"---but discard the line break if it is present.
   A line break is either {\tt \char'42\char'134n\char'42} or {\tt
   \char'42\char'134r\char'134n\char'42}. *)

PROCEDURE Seek(rd: T; n: CARDINAL) RAISES {Failure, Alerted};
(* This is equivalent to: *)
(*
| IF closed(rd) OR NOT seekable(rd) THEN
|   `Cause checked runtime error`
| END;
| cur(rd) := MIN(n, len(rd))
*)

PROCEDURE Close(rd: T) RAISES {Failure, Alerted};
(* Release any resources associated with "rd" and set "closed(rd) :=
   TRUE".  The documentation of a procedure that creates a reader
   should specify what resources are released when the reader is
   closed.  This leaves "rd" closed even if it raises an exception,
   and is a no-op if "rd" is closed. *)

PROCEDURE Index(rd: T): CARDINAL RAISES {};
(* This is equivalent to: *)
(*
| IF closed(rd) THEN `Cause checked runtime error` END;
| RETURN cur(rd)
*)

PROCEDURE Length(rd: T): INTEGER RAISES {Failure, Alerted};
(* This is equivalent to: *)
(*
| IF closed(rd) THEN
|   `Cause checked runtime error`
| END;
| RETURN len(rd)

   If "len(rd)" is unknown to the implementation of an intermittent
   reader, "Length(rd)" returns -1.  *)

PROCEDURE Intermittent(rd: T): BOOLEAN RAISES {};
PROCEDURE Seekable(rd: T): BOOLEAN RAISES {};
PROCEDURE Closed(rd: T): BOOLEAN RAISES {};
(* Return "intermittent(rd)", "seekable(rd)", and "closed(rd)",
   respectively. These can be applied to closed readers. *)

END Rd.

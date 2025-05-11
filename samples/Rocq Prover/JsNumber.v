Set Implicit Arguments.
Require Export Shared.
Require Flocq.Appli.Fappli_IEEE Flocq.Appli.Fappli_IEEE_bits.


(**************************************************************)
(** ** Type for number (IEEE floats) *)

Definition number : Type :=
  Fappli_IEEE_bits.binary64.


(**************************************************************)
(** ** Particular values of numbers *)

(* LATER: find definitions in Flocq *)
Parameter nan : number.
Parameter zero : number.
Parameter neg_zero : number.
Definition one := Fappli_IEEE.binary_normalize 53 1024 eq_refl eq_refl Fappli_IEEE.mode_NE 1 0 false.
Parameter infinity : number.
Parameter neg_infinity : number.
Parameter max_value : number.
Parameter min_value : number.
Parameter pi : number.
Parameter e : number.
Parameter ln2 : number.

(**************************************************************)
(** ** Conversions on numbers *)

(* LATER: implement definitions *)
Parameter from_string : string -> number.
Parameter to_string : number -> string.

(**************************************************************)
(** ** Unary operations on numbers *)

(* LATER: find definitions in Flocq *)

Parameter neg : number -> number.
Parameter floor : number -> number.
Parameter absolute : number -> number.
Parameter sign : number -> number. (* returns arbitrary when x is zero or nan *)
Parameter lt_bool : number -> number -> bool.


(**************************************************************)
(** ** Binary operations on numbers *)

Definition add : number -> number -> number :=
  Fappli_IEEE_bits.b64_plus Fappli_IEEE.mode_NE.

Parameter sub : number -> number -> number. (* todo: bind *)

Parameter fmod : number -> number -> number. (* todo: bind *)

Definition mult : number -> number -> number :=
  Fappli_IEEE_bits.b64_mult Fappli_IEEE.mode_NE.

Definition div : number -> number -> number :=
  Fappli_IEEE_bits.b64_div Fappli_IEEE.mode_NE.

(* Todo: find comparison operator *)
Global Instance number_comparable : Comparable number.
Proof. Admitted.



(**************************************************************)
(** ** Conversions with Int32 *)

Parameter of_int : int -> number. (* LATER: this is quite complex. Should we make it precise? *)

Parameter to_int32 : number -> int. (* Remark: extracted code could, for efficiency reasons, use Ocaml Int32 *) 

Parameter to_uint32 : number -> int.

Parameter to_int16 : number -> int. (* currently not used *)

(* LATER: Check that the OCaml extraction is correct. *)


(**************************************************************)

(** Implements the operation that masks all but the 5 least significant bits
   of a non-negative number (obtained as the result of to_uint32 *)

Parameter modulo_32 : int -> int.

(** Implements int32 operation *)

Parameter int32_bitwise_not : int -> int.

Parameter int32_bitwise_and : int -> int -> int.
Parameter int32_bitwise_or : int -> int -> int.
Parameter int32_bitwise_xor : int -> int -> int.

Parameter int32_left_shift : int -> int -> int.
Parameter int32_right_shift : int -> int -> int.
Parameter uint32_right_shift : int -> int -> int.




(**************************************************************)
(** ** Int32 related conversion *)

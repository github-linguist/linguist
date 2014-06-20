(* ****** ****** *)
//
// HX-2014-01
// CoYoneda Lemma:
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

staload
"libats/ML/SATS/basis.sats"
staload
"libats/ML/SATS/list0.sats"

(* ****** ****** *)

staload _ = "libats/ML/DATS/list0.dats"

(* ****** ****** *)

sortdef ftype = type -> type

(* ****** ****** *)

infixr (->) ->>
typedef ->> (a:type, b:type) = a -<cloref1> b

(* ****** ****** *)

typedef
functor(F:ftype) =
  {a,b:type} (a ->> b) ->> F(a) ->> F(b)

(* ****** ****** *)

typedef
list0 (a:type) = list0 (a)
extern
val functor_list0 : functor (list0)

(* ****** ****** *)

implement
functor_list0{a,b}
  (f) = lam xs => list0_map<a><b> (xs, f)

(* ****** ****** *)

datatype
CoYoneda
 (F:ftype, r:type) = {a:type} CoYoneda of (a ->> r, F(a))
// end of [CoYoneda]

(* ****** ****** *)
//
extern
fun CoYoneda_phi
  : {F:ftype}functor(F) -> {r:type} (F (r) ->> CoYoneda (F, r))
extern
fun CoYoneda_psi
  : {F:ftype}functor(F) -> {r:type} (CoYoneda (F, r) ->> F (r))
//
(* ****** ****** *)

implement
CoYoneda_phi(ftor) = lam (fx) => CoYoneda (lam x => x, fx)
implement
CoYoneda_psi(ftor) = lam (CoYoneda(f, fx)) => ftor (f) (fx)

(* ****** ****** *)

datatype int0 = I of (int)
datatype bool = True | False // boxed boolean

(* ****** ****** *)
//
fun bool2string
  (x:bool): string =
(
  case+ x of True() => "True" | False() => "False"
)
//
implement
fprint_val<bool> (out, x) = fprint (out, bool2string(x))
//
(* ****** ****** *)

fun int2bool (i: int0): bool =
  let val+I(i) = i in if i > 0 then True else False end

(* ****** ****** *)

val myintlist0 = g0ofg1($list{int0}((I)1, (I)0, (I)1, (I)0, (I)0))
val myboolist0 = CoYoneda{list0,bool}{int0}(lam (i) => int2bool(i), myintlist0)
val myboolist0 = CoYoneda_psi{list0}(functor_list0){bool}(myboolist0)

(* ****** ****** *)

val ((*void*)) = fprintln! (stdout_ref, "myboolist0 = ", myboolist0)

(* ****** ****** *)

implement main0 () = ()

(* ****** ****** *)

(* end of [CoYonedaLemma.dats] *)

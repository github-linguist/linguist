(* ****** ****** *)
//
// HX-2014-01
// Yoneda Lemma:
// The hardest "trivial" theorem :)
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
staload
"libats/ML/SATS/option0.sats"

(* ****** ****** *)

staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/option0.dats"

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

typedef
option0 (a:type) = option0 (a)  
extern
val functor_option0 : functor (option0)
  
(* ****** ****** *)

implement
functor_option0{a,b}
  (f) = lam opt => option0_map<a><b> (opt, f)

(* ****** ****** *)

extern
val functor_homres
  : {c:type} functor (lam(r:type) => c ->> r)

(* ****** ****** *)

implement
functor_homres{c}{a,b} (f) = lam (r) => lam (x) => f (r(x))

(* ****** ****** *)
//
extern
fun Yoneda_phi : {F:ftype}functor(F) ->
  {a:type}F(a) ->> ({r:type}(a ->> r) ->> F(r))
extern
fun Yoneda_psi : {F:ftype}functor(F) ->
  {a:type}({r:type}(a ->> r) ->> F(r)) ->> F(a)
//
(* ****** ****** *)
//
implement
Yoneda_phi
  (ftor) = lam(fx) => lam (m) => ftor(m)(fx)
//
implement
Yoneda_psi (ftor) = lam(mf) => mf(lam x => x)
//
(* ****** ****** *)

(*

(* ****** ****** *)
//
// HX-2014-01-05:
// Another version based on Natural Transformation
//
(* ****** ****** *)

typedef
natrans(F:ftype, G:ftype) = {x:type} (F(x) ->> G(x))

(* ****** ****** *)
//
extern
fun Yoneda_phi_nat : {F:ftype}functor(F) ->
  {a:type} F(a) ->> natrans(lam (r:type) => (a ->> r), F)
extern
fun Yoneda_psi_nat : {F:ftype}functor(F) ->
  {a:type} natrans(lam (r:type) => (a ->> r), F) ->> F(a)
//
(* ****** ****** *)
//
implement
Yoneda_phi_nat
  (ftor) = lam(fx) => lam (m) => ftor(m)(fx)
//
implement
Yoneda_psi_nat (ftor) = lam(mf) => mf(lam x => x)
//
(* ****** ****** *)

*)

(* ****** ****** *)

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
//
val myboolist0 =
  $list_t{bool}(True, False, True, False, False)
val myboolist0 = g0ofg1_list (myboolist0)
//
(* ****** ****** *)
//
extern
val Yoneda_bool_list0 : {r:type} (bool ->> r) ->> list0(r)
//
implement
Yoneda_bool_list0 =
  Yoneda_phi(functor_list0){bool}(myboolist0)
//
(* ****** ****** *)
//
val myboolist1 =
  Yoneda_psi(functor_list0){bool}(Yoneda_bool_list0)
//
(* ****** ****** *)

val () = fprintln! (stdout_ref, "myboolist0 = ", myboolist0)
val () = fprintln! (stdout_ref, "myboolist1 = ", myboolist1)

(* ****** ****** *)

implement main0 () = ()

(* ****** ****** *)

(* end of [YonedaLemma.dats] *)

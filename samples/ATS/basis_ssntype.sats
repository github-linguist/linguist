(*
* The MIT License (MIT)
*
* Copyright (c) 2014 Hongwei Xi
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.)
*)

// Source: https://github.com/githwxi/ATS-Postiats-contrib/blob/201d635062d0ea64ff5ba5457a4ea0bb4d5ae202/contrib/libats-/hwxi/teaching/mysession-g/SATS/basis_ssntype.sats

(*
** Basis for g-session types
*)

(* ****** ****** *)
//
staload
"./basis_intset.sats"
//
(* ****** ****** *)
//
fun{}
channel_cap(): intGte(1)  
//
(* ****** ****** *)
//
abstype
session_msg
  (i:int, j:int, a:vt@ype)
//
(* ****** ****** *)

abstype ssession_nil
abstype ssession_cons(a:type, ssn:type)

(* ****** ****** *)
//
stadef msg = session_msg
//
stadef nil = ssession_nil
//
stadef :: = ssession_cons
stadef cons = ssession_cons
//
(* ****** ****** *)
//
abstype
session_append
  (ssn1: type, ssn2: type)
//
stadef append = session_append
//
(* ****** ****** *)
//
abstype
session_choose
(
  i:int, ssn1:type, ssn2:type
) (* session_choose *)
//
stadef choose = session_choose
//
(* ****** ****** *)
//
abstype
session_repeat
(
  i:int, ssn:type(*body*)
) (* session_repeat *)
//
stadef repeat = session_repeat
//
(* ****** ****** *)
//
typedef
session_sing
(
  i: int
, j: int
, a:vt@ype
) = cons(msg(i, j, a), nil)
//
(* ****** ****** *)
//
absvtype
channel1_vtype
  (G:iset, n:int, ssn:type) = ptr
//
vtypedef
channel1
  (G:iset, n:int, ssn:type) = channel1_vtype(G, n, ssn)
//
vtypedef
cchannel1
  (G:iset, n:int, ssn:type) = channel1_vtype(ncomp(n, G), n, ssn)
//
(* ****** ****** *)
//
fun{}
channel1_get_nrole
  {n:int}{ssn:type}{G:iset}
  (chan: !channel1(G, n, ssn)): int(n)
//
fun{}
channel1_get_group
  {n:int}{ssn:type}{G:iset}
  (chan: !channel1(G, n, ssn)): intset(n,G)
//
(* ****** ****** *)
//
fun
{a:vt0p}
channel1_close
  {n:int}{ssn:type}{G:iset}(chan: channel1(G, n, nil)): void
//
(* ****** ****** *)
//
fun{}
channel1_skipin
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // end-of-function
praxi
lemma_channel1_skipin
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // lemma_channel1_skipin
//
fun{}
channel1_skipex
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ~ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // end-of-function
praxi
lemma_channel1_skipex
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ~ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // lemma_channel1_skipex
//
(* ****** ****** *)
//
fun
{a:vt0p}
channel1_send
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j), a
) : void // end of [channel1_send]
//
fun
{a:vt0p}
channel1_recv
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ~ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j), &a? >> a
) : void // end of [channel1_recv]
//
fun
{a:vt0p}
channel1_recv_val
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ~ismbr(G, i); ismbr(G, j)}
  (!channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j)): (a)
//
(* ****** ****** *)

fun{}
channel1_append
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
(
  chan: !channel1(G, n, append(ssn1, ssn2)) >> channel1(G, n, ssn2)
, fserv: (!channel1(G, n, ssn1) >> channel1(G, n, nil)) -<lincloptr1> void
) : void // end of [channel1_append]

(* ****** ****** *)
//
datatype
choosetag
(
  a:type, b:type, c:type
) =
  | choosetag_l(a, b, a) of ()
  | choosetag_r(a, b, b) of ()
//
(* ****** ****** *)
//
fun{}
channel1_choose_l
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn1), i: int(i)
) : void // end of [channel1_choose_l]
//
fun{}
channel1_choose_r
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn2), i: int(i)
) : void // end of [channel1_choose_r]
//
fun{}
channel1_choose_tag
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ~isnil(G); ~ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn_chosen), i: int(i)
) : #[ssn_chosen:type] choosetag(ssn1, ssn2, ssn_chosen)
//
(* ****** ****** *)
//
fun{}
channel1_repeat_0
  {n:int}
  {ssn:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, repeat(i,ssn)) >> channel1(G, n, nil), i: int(i)
) : void // end of [channel1_repeat_nil]
//
fun{}
channel1_repeat_1
  {n:int}
  {ssn:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, repeat(i,ssn)) >> channel1(G, n, append(ssn,repeat(i,ssn))), i: int(i)
) : void // end of [channel1_repeat_more]
//
fun{}
channel1_repeat_tag
  {n:int}
  {ssn:type}
  {G:iset}
  {i:nat | i < n; ~isnil(G); ~ismbr(G, i)}
(
  !channel1(G, n, repeat(i,ssn)) >> channel1(G, n, ssn_chosen), i: int(i)
) : #[ssn_chosen:type] choosetag(nil, append(ssn,repeat(i,ssn)), ssn_chosen)
//
(* ****** ****** *)
//
(*
//
// HX-2015-03-06:
// This one does not work with sschoose!!!
//
fun{}
channel1_link
  {n:int}{ssn:type}
  {G1,G2:iset | isnil(G1*G2)}
  (channel1(G1, n, ssn), channel1(G2, n, ssn)): channel1(G1+G2, n, ssn)
*)
//
fun{}
channel1_link
  {n:int}{ssn:type}
  {G1,G2:iset | isful(G1+G2,n)}
  (channel1(G1, n, ssn), channel1(G2, n, ssn)): channel1(G1*G2, n, ssn)
//
(* ****** ****** *)
//
fun{}
channel1_link_elim
  {n:int}{ssn:type}{G:iset}(channel1(G, n, ssn), cchannel1(G, n, ssn)): void
//
(* ****** ****** *)
//
fun{}
cchannel1_create_exn
  {n:nat}{ssn:type}{G:iset}
(
  nrole: int(n), G: intset(n), fserv: channel1(G, n, ssn) -<lincloptr1> void
) : cchannel1(G, n, ssn) // end of [cchannel1_create_exn]
//
(* ****** ****** *)

(* end of [basis_ssntype.sats] *)

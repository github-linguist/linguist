module Hacl.HKDF

module ST = FStar.HyperStack.ST

open FStar.HyperStack.All

open FStar.Mul
open FStar.Ghost
open FStar.HyperStack
open FStar.HyperStack.ST
open FStar.Buffer
open FStar.UInt32


(* Definition of aliases for modules *)
module U8 = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

module S8 = Hacl.UInt8
module S32 = Hacl.UInt32
module S64 = Hacl.UInt64

module Buffer = FStar.Buffer
module Cast = Hacl.Cast

module Hash = Hacl.Hash.SHA2.L256
module HMAC = Hacl.HMAC


(* Definition of base types *)
let uint8_t   = FStar.UInt8.t
let uint32_t  = FStar.UInt32.t
let uint64_t  = FStar.UInt64.t

let suint8_t  = Hacl.UInt8.t
let suint32_t = Hacl.UInt32.t
let suint64_t = Hacl.UInt64.t

let suint32_p = Buffer.buffer suint32_t
let suint8_p  = Buffer.buffer suint8_t


(* Definitions of aliases for functions *)
let u8_to_s8 = Cast.uint8_to_sint8
let u32_to_s8 = Cast.uint32_to_sint8
let u32_to_s32 = Cast.uint32_to_sint32
let u32_to_s64 = Cast.uint32_to_sint64
let s32_to_s8  = Cast.sint32_to_sint8
let s32_to_s64 = Cast.sint32_to_sint64
let u64_to_s64 = Cast.uint64_to_sint64



//
// HKDF
//

(* Define parameters *)
inline_for_extraction let hashsize = HMAC.hashsize
inline_for_extraction let hashsize_32 = HMAC.hashsize_32
inline_for_extraction let blocksize = HMAC.blocksize
inline_for_extraction let blocksize_32 = HMAC.blocksize_32



(* Define HKDF Extraction function *)
val hkdf_extract :
  prk     :suint8_p{length prk = v hashsize} ->
  salt    :suint8_p ->
  saltlen :uint32_t{v saltlen = length salt} ->
  ikm     :suint8_p ->
  ikmlen  :uint32_t{v ikmlen = length ikm} ->
  Stack unit
        (requires (fun h0 -> live h0 prk /\ live h0 salt /\ live h0 ikm))
        (ensures  (fun h0 r h1 -> live h1 prk /\ modifies_1 prk h0 h1))

let hkdf_extract prk salt saltlen ikm ikmlen = HMAC.hmac prk salt saltlen ikm ikmlen



[@"c_inline"]
private val hkdf_expand_inner:
  state   :suint8_p ->
  prk     :suint8_p {v hashsize <= length prk} ->
  prklen  :uint32_t {v prklen = length prk} ->
  info    :suint8_p ->
  infolen :uint32_t {v infolen = length info} ->
  n       :uint32_t {v n <= pow2 8}->
  i       :uint32_t {v i <= v n} ->
  Stack unit
        (requires (fun h0 -> live h0 state /\ live h0 prk /\ live h0 info))
        (ensures  (fun h0 r h1 -> live h1 state /\ modifies_1 state h0 h1))

[@"c_inline"]
let rec hkdf_expand_inner state prk prklen info infolen n i =

  (* Push a new memory frame *)
  (**) push_frame();

  (* Recompute the sizes and position of the intermediary objects *)
  (* Note: here we favour readability over efficiency *)
  let size_T = U32.mul_mod n hashsize in
  let size_Ti  = hashsize in
  let size_Til = hashsize +^ infolen +^ 1ul in
  let pos_Ti = 0ul in
  let pos_Til = size_Ti in
  let pos_T = pos_Til +^ size_Til in

  (* Retreive the memory for local computations. state =  Ti | Til | T *)
  let ti = Buffer.sub state pos_Ti size_Ti in
  let til = Buffer.sub state pos_Til size_Til in
  let t = Buffer.sub state pos_T size_T in

  if (i =^ 1ul) then begin

    (* Concatenate T(i-1) | Info | i *)
    Buffer.blit info 0ul til 0ul infolen;
    Buffer.upd til (size_Til -^ 1ul) (u32_to_s8 i);

    (* Compute the mac of to get block Ti *)
    HMAC.hmac ti prk prklen til size_Til;

    (* Store the resulting block in T *)
    Buffer.blit ti 0ul t 0ul hashsize;

    (* Recursive call *)
    hkdf_expand_inner state prk prklen info infolen n (i +^ 1ul) end

  else if (i <=^ n) then begin

    (* Concatenate T(i-1) | Info | i *)
    Buffer.blit ti 0ul til 0ul hashsize;
    Buffer.blit info 0ul til hashsize infolen;
    Buffer.upd til (size_Til -^ 1ul) (u32_to_s8 i);

    (* Compute the mac of to get block Ti *)
    HMAC.hmac ti prk prklen til size_Til;

    (* Store the resulting block in T *)
    let pos = U32.mul_mod (i -^ 1ul) hashsize in
    Buffer.blit ti 0ul t pos hashsize;

    (* Recursive call *)
    hkdf_expand_inner state prk prklen info infolen n (i +^ 1ul) end
  else ();

  (* Pop the memory frame *)
  (**) pop_frame()



(* Define HKDF Expand function *)
val hkdf_expand :
  okm     :suint8_p ->
  prk     :suint8_p {v hashsize <= length prk} ->
  prklen  :uint32_t {v prklen <= length prk} ->
  info    :suint8_p ->
  infolen :uint32_t {v infolen <= length info} ->
  len     :uint32_t {v len <= length okm
                    /\ v len <= (255 * U32.v hashsize)
                    /\ (U32.v len / U32.v hashsize + 1) <= length okm} ->
  Stack unit
        (requires (fun h0 -> live h0 okm /\ live h0 prk /\ live h0 info))
        (ensures  (fun h0 r h1 -> live h1 okm /\ modifies_1 okm h0 h1))

let hkdf_expand okm prk prklen info infolen len =

  (* Push a new memory frame *)
  (**) push_frame ();

  (* Compute the number of blocks necessary to compute the output *)
  let n = U32.(div len hashsize) +^ 1ul in

  (* Describe the shape of memory used by the inner recursive function *)
  let size_T = U32.mul_mod n hashsize in
  let size_Ti  = hashsize in
  let size_Til = hashsize +^ infolen +^ 1ul in
  let pos_Ti = 0ul in
  let pos_Til = size_Ti in
  let pos_T = pos_Til +^ size_Til in

  (* Allocate memory for inner expension: state =  Ti | Til | T *)
  let state = Buffer.create (u8_to_s8 0uy) (size_Ti +^ size_Til +^ size_T) in

  (* Call the inner expension function *)
  hkdf_expand_inner state prk prklen info infolen n 0ul;

  (* Extract T from the state *)
  let _T = Buffer.sub state pos_T size_T in

  (* Redundant copy the desired part of T *)
  Buffer.blit _T 0ul okm 0ul len;

  (* Pop the memory frame *)
  (**) pop_frame()

(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let io_buffer_size = 65536                           (* IO_BUFFER_SIZE 4.0.0 *)

let pp = Format.fprintf
let invalid_encode () = invalid_arg "expected `Await encode"
let invalid_bounds j l =
  invalid_arg (Printf.sprintf "invalid bounds (index %d, length %d)" j l)

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_chr = Char.unsafe_chr
let unsafe_blit = String.unsafe_blit
let unsafe_array_get = Array.unsafe_get
let unsafe_byte s j = Char.code (String.unsafe_get s j)
let unsafe_set_byte s j byte = String.unsafe_set s j (Char.unsafe_chr byte)

(* Unicode characters *)

type uchar = int
let u_bom = 0xFEFF                                                   (* BOM. *)
let u_rep = 0xFFFD                                 (* replacement character. *)
let is_uchar cp =
  (0x0000 <= cp && cp <= 0xD7FF) || (0xE000 <= cp && cp <= 0x10FFFF)

let pp_cp ppf cp =
  if cp < 0 || cp > 0x10FFFF then pp ppf "U+Invalid(%X)" cp else
  if cp <= 0xFFFF then pp ppf "U+%04X" cp else
  pp ppf "U+%X" cp

let cp_to_string cp =                                    (* NOT thread safe. *)
  pp Format.str_formatter "%a" pp_cp cp; Format.flush_str_formatter ()

(* Unicode encoding schemes *)

type encoding = [ `UTF_8 | `UTF_16 | `UTF_16BE | `UTF_16LE ]
type decoder_encoding = [ encoding | `US_ASCII | `ISO_8859_1 ]

let encoding_of_string s = match String.uppercase s with      (* IANA names. *)
| "UTF-8" -> Some `UTF_8
| "UTF-16" -> Some `UTF_16
| "UTF-16LE" -> Some `UTF_16LE
| "UTF-16BE" -> Some `UTF_16BE
| "ANSI_X3.4-1968" | "ISO-IR-6" | "ANSI_X3.4-1986" | "ISO_646.IRV:1991"
| "ASCII" | "ISO646-US" | "US-ASCII" | "US" | "IBM367" | "CP367" | "CSASCII" ->
    Some `US_ASCII
| "ISO_8859-1:1987" | "ISO-IR-100" | "ISO_8859-1" | "ISO-8859-1"
| "LATIN1" | "L1" | "IBM819" | "CP819" | "CSISOLATIN1" ->
    Some `ISO_8859_1
| _ -> None

let encoding_to_string = function
| `UTF_8 -> "UTF-8" | `UTF_16 -> "UTF-16" | `UTF_16BE -> "UTF-16BE"
| `UTF_16LE -> "UTF-16LE" | `US_ASCII -> "US-ASCII"
| `ISO_8859_1 -> "ISO-8859-1"

(* Base character decoders. They assume enough data. *)

let malformed s j l = `Malformed (String.sub s j l)
let malformed_pair be hi s j l =    (* missing or half low surrogate at eoi. *)
  let bs1 = String.sub s j l in
  let bs0 = String.create 2 in
  let j0, j1 = if be then (0, 1) else (1, 0) in
  unsafe_set_byte bs0 j0 (hi lsr 8);
  unsafe_set_byte bs0 j1 (hi land 0xFF);
  `Malformed (bs0 ^ bs1)

let r_us_ascii s j =
  (* assert (0 <= j && j < String.length s); *)
  let b0 = unsafe_byte s j in
  if b0 <= 127 then `Uchar b0 else malformed s j 1

let r_iso_8859_1 s j =
  (* assert (0 <= j && j < String.length s); *)
  `Uchar (unsafe_byte s j)

let utf_8_len = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let r_utf_8 s j l =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  match l with
  | 1 -> `Uchar (unsafe_byte s j)
  | 2 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      if b1 lsr 6 != 0b10 then malformed s j l else
      `Uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
  | 3 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in
      let c = `Uchar (((b0 land 0x0F) lsl 12) lor
                      ((b1 land 0x3F) lsl 6) lor
          (b2 land 0x3F))
      in
      if b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then malformed s j l else c
      | 0xED -> if b1 < 0x80 || 0x9F < b1 then malformed s j l else c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else c
      end
  | 4 ->
      let b0 = unsafe_byte s j in let b1 = unsafe_byte s (j + 1) in
      let b2 = unsafe_byte s (j + 2) in let b3 = unsafe_byte s (j + 3) in
      let c = `Uchar (((b0 land 0x07) lsl 18) lor
                      ((b1 land 0x3F) lsl 12) lor
          ((b2 land 0x3F) lsl 6) lor
                      (b3 land 0x3F))
      in
      if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then malformed s j l else
      begin match b0 with
      | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then malformed s j l else c
      | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then malformed s j l else c
      | _ -> if b1 lsr 6 != 0b10 then malformed s j l else c
      end
  | _ -> assert false

let r_utf_16 s j0 j1 =                       (* May return a high surrogate. *)
  (* assert (0 <= j0 && 0 <= j1 && max j0 j1 < String.length s); *)
  let b0 = unsafe_byte s j0 in let b1 = unsafe_byte s j1 in
  let u = (b0 lsl 8) lor b1 in
  if u < 0xD800 || u > 0xDFFF then `Uchar u else
  if u > 0xDBFF then malformed s (min j0 j1) 2 else `Hi u

let r_utf_16_lo hi s j0 j1 =          (* Combines [hi] with a low surrogate. *)
  (* assert (0 <= j0 && 0 <= j1 && max j0 j1 < String.length s); *)
  let b0 = unsafe_byte s j0 in
  let b1 = unsafe_byte s j1 in
  let lo = (b0 lsl 8) lor b1 in
  if lo < 0xDC00 || lo > 0xDFFF
  then malformed_pair (j0 < j1 (* true => be *)) hi s (min j0 j1) 2
  else `Uchar ((((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000)

let r_encoding s j l =                  (* guess encoding with max. 3 bytes. *)
  (* assert (0 <= j && 0 <= l && j + l <= String.length s) *)
  let some i = if i < l then Some (unsafe_byte s (j + i)) else None in
  match (some 0), (some 1), (some 2) with
  | Some 0xEF, Some 0xBB, Some 0xBF                 -> `UTF_8 `BOM
  | Some 0xFE, Some 0xFF, _                         -> `UTF_16BE `BOM
  | Some 0xFF, Some 0xFE, _                         -> `UTF_16LE `BOM
  | Some 0x00, Some    p, _              when p > 0 -> `UTF_16BE (`ASCII p)
  | Some    p, Some 0x00, _              when p > 0 -> `UTF_16LE (`ASCII p)
  | Some    u,         _, _ when utf_8_len.(u) <> 0 -> `UTF_8 `Decode
  | Some    _, Some    _, _                         -> `UTF_16BE `Decode
  | Some    _, None     , None                      -> `UTF_8 `Decode
  | None     , None     , None                      -> `UTF_8 `End
  | None     , Some    _, _                         -> assert false
  | Some    _, None     , Some _                    -> assert false
  | None     , None     , Some _                    -> assert false

(* Decode *)

type src = [ `Channel of in_channel | `String of string | `Manual ]
type nln = [ `ASCII of uchar | `NLF of uchar | `Readline of uchar ]
type decode = [ `Await | `End | `Malformed of string | `Uchar of uchar]

let pp_decode ppf = function
| `Uchar u -> pp ppf "@[`Uchar %a@]" pp_cp u
| `End -> pp ppf "`End"
| `Await -> pp ppf "`Await"
| `Malformed bs ->
    let l = String.length bs in
    pp ppf "@[`Malformed (";
    if l > 0 then pp ppf "%02X" (Char.code (bs.[0]));
    for i = 1 to l - 1 do pp ppf " %02X" (Char.code (bs.[i])) done;
    pp ppf ")@]"

type decoder =
  { src : src;                                              (* input source. *)
    mutable encoding : decoder_encoding;                (* decoded encoding. *)
    nln : nln option;                     (* newline normalization (if any). *)
    nl : int;                            (* newline normalization character. *)
    mutable i : string;                              (* current input chunk. *)
    mutable i_pos : int;                          (* input current position. *)
    mutable i_max : int;                          (* input maximal position. *)
    t : string;        (* four bytes temporary buffer for overlapping reads. *)
    mutable t_len : int;                      (* current byte length of [t]. *)
    mutable t_need : int;                  (* number of bytes needed in [t]. *)
    mutable removed_bom : bool;     (* [true] if an initial BOM was removed. *)
    mutable last_cr : bool;                   (* [true] if last char was CR. *)
    mutable line : int;                                      (* line number. *)
    mutable col : int;                                     (* column number. *)
    mutable byte_count : int;                                 (* byte count. *)
    mutable count : int;                                      (* char count. *)
    mutable pp :        (* decoder post-processor for BOM, position and nln. *)
      decoder -> [ `Malformed of string | `Uchar of uchar ] -> decode;
    mutable k : decoder -> decode }                 (* decoder continuation. *)

(* On decodes that overlap two (or more) [d.i] buffers, we use [t_fill] to copy
   the input data to [d.t] and decode from there. If the [d.i] buffers are not
   too small this is faster than continuation based byte per byte writes.

   End of input (eoi) is signalled by [d.i_pos = 0] and [d.i_max = min_int]
   which implies that [i_rem d < 0] is [true]. *)

let i_rem d = d.i_max - d.i_pos + 1     (* remaining bytes to read in [d.i]. *)
let eoi d = d.i <- ""; d.i_pos <- 0; d.i_max <- min_int   (* set eoi in [d]. *)
let src d s j l =                                     (* set [d.i] with [s]. *)
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_bounds j l else
  if (l = 0) then eoi d else
  (d.i <- s; d.i_pos <- j; d.i_max <- j + l - 1)

let refill k d = match d.src with  (* get new input in [d.i] and [k]ontinue. *)
| `Manual -> d.k <- k; `Await
| `String _ -> eoi d; k d
| `Channel ic ->
    let rc = input ic d.i 0 (String.length d.i) in
    (src d d.i 0 rc; k d)

let t_need d need = d.t_len <- 0; d.t_need <- need
let rec t_fill k d =      (* get [d.t_need] bytes (or less if eoi) in [i.t]. *)
  let blit d l =
    unsafe_blit d.i d.i_pos d.t d.t_len (* write pos. *) l;
    d.i_pos <- d.i_pos + l; d.t_len <- d.t_len + l;
  in
  let rem = i_rem d in
  if rem < 0 (* eoi *) then k d else
  let need = d.t_need - d.t_len in
  if rem < need then (blit d rem; refill (t_fill k) d) else (blit d need; k d)

let ret k v byte_count d =                     (* return post-processed [v]. *)
  d.k <- k; d.byte_count <- d.byte_count + byte_count; d.pp d v

(* Decoders. *)

let rec decode_us_ascii d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_us_ascii d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; ret decode_us_ascii (r_us_ascii d.i j) 1 d

let rec decode_iso_8859_1 d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_iso_8859_1 d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 1; ret decode_iso_8859_1 (r_iso_8859_1 d.i j) 1 d

(* UTF-8 decoder *)

let rec t_decode_utf_8 d =                             (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_8 (malformed d.t 0 d.t_len) d.t_len d
  else ret decode_utf_8 (r_utf_8 d.t 0 d.t_len) d.t_len d

and decode_utf_8 d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_8 d) else
  let need = unsafe_array_get utf_8_len (unsafe_byte d.i d.i_pos) in
  if rem < need then (t_need d need; t_fill t_decode_utf_8 d) else
  let j = d.i_pos in
  if need = 0
  then (d.i_pos <- d.i_pos + 1; ret decode_utf_8 (malformed d.i j 1) 1 d)
  else (d.i_pos <- d.i_pos + need; ret decode_utf_8 (r_utf_8 d.i j need) need d)

(* UTF-16BE decoder *)

let rec t_decode_utf_16be_lo hi d =                    (* decode from [d.t]. *)
  let bcount = d.t_len + 2 (* hi count *) in
  if d.t_len < d.t_need
  then ret decode_utf_16be (malformed_pair true hi d.t 0 d.t_len) bcount d
  else ret decode_utf_16be (r_utf_16_lo hi d.t 0 1) bcount d

and t_decode_utf_16be d =                              (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16be (malformed d.t 0 d.t_len) d.t_len d
  else decode_utf_16be_lo (r_utf_16 d.t 0 1) d

and decode_utf_16be_lo v d = match v with
| `Uchar _ | `Malformed _ as v -> ret decode_utf_16be v 2 d
| `Hi hi ->
    let rem = i_rem d in
    if rem < 2 then (t_need d 2; t_fill (t_decode_utf_16be_lo hi) d) else
    let j = d.i_pos in
    d.i_pos <- d.i_pos + 2;
    ret decode_utf_16be (r_utf_16_lo hi d.i j (j + 1)) 4 d

and decode_utf_16be d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_16be d) else
  if rem < 2 then (t_need d 2; t_fill t_decode_utf_16be d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 2; decode_utf_16be_lo (r_utf_16 d.i j (j + 1)) d

(* UTF-16LE decoder, same as UTF-16BE with byte swapped. *)

let rec t_decode_utf_16le_lo hi d =                    (* decode from [d.t]. *)
  let bcount = d.t_len + 2 (* hi count *) in
  if d.t_len < d.t_need
  then ret decode_utf_16le (malformed_pair false hi d.t 0 d.t_len) bcount d
  else ret decode_utf_16le (r_utf_16_lo hi d.t 1 0) bcount d

and t_decode_utf_16le d =                              (* decode from [d.t]. *)
  if d.t_len < d.t_need
  then ret decode_utf_16le (malformed d.t 0 d.t_len) d.t_len d
  else decode_utf_16le_lo (r_utf_16 d.t 1 0) d

and decode_utf_16le_lo v d = match v with
| `Uchar _ | `Malformed _ as v -> ret decode_utf_16le v 2 d
| `Hi hi ->
    let rem = i_rem d in
    if rem < 2 then (t_need d 2; t_fill (t_decode_utf_16le_lo hi) d) else
    let j = d.i_pos in
    d.i_pos <- d.i_pos + 2;
    ret decode_utf_16le (r_utf_16_lo hi d.i (j + 1) j) 4 d

and decode_utf_16le d =
  let rem = i_rem d in
  if rem <= 0 then (if rem < 0 then `End else refill decode_utf_16le d) else
  if rem < 2 then (t_need d 2; t_fill t_decode_utf_16le d) else
  let j = d.i_pos in
  d.i_pos <- d.i_pos + 2; decode_utf_16le_lo (r_utf_16 d.i (j + 1) j) d

(* Encoding guessing. The guess is simple but starting the decoder
   after is tedious, uutf's decoders are not designed to put bytes
   back in the stream. *)

let guessed_utf_8 d =                   (* start decoder after `UTF_8 guess. *)
  let b3 d =                                 (* handles the third read byte. *)
    let b3 = unsafe_byte d.t 2 in
    match utf_8_len.(b3) with
    | 0 -> ret decode_utf_8 (malformed d.t 2 1) 1 d
    | n ->
        d.t_need <- n; d.t_len <- 1; unsafe_set_byte d.t 0 b3;
        t_fill t_decode_utf_8 d
  in
  let b2 d =                                     (* handle second read byte. *)
    let b2 = unsafe_byte d.t 1 in
    let b3 = if d.t_len > 2 then b3 else decode_utf_8 (* decodes `End *) in
    match utf_8_len.(b2) with
    | 0 -> ret b3 (malformed d.t 1 1) 1 d
    | 1 -> ret b3 (r_utf_8 d.t 1 1) 1 d
    | n ->                         (* copy d.t.(1-2) to d.t.(0-1) and decode *)
        d.t_need <- n;
        unsafe_set_byte d.t 0 b2;
        if (d.t_len < 3) then d.t_len <- 1 else
        (d.t_len <- 2; unsafe_set_byte d.t 1 (unsafe_byte d.t 2); );
        t_fill t_decode_utf_8 d
  in
  let b1 = unsafe_byte d.t 0 in                   (* handle first read byte. *)
  let b2 = if d.t_len > 1 then b2 else decode_utf_8 (* decodes `End *) in
  match utf_8_len.(b1) with
  | 0 -> ret b2 (malformed d.t 0 1) 1 d
  | 1 -> ret b2 (r_utf_8 d.t 0 1) 1 d
  | 2 ->
      if d.t_len < 2 then ret decode_utf_8 (malformed d.t 0 1) 1 d else
      if d.t_len < 3 then ret decode_utf_8 (r_utf_8 d.t 0 2) 2 d else
      ret b3 (r_utf_8 d.t 0 2) 2 d
  | 3 ->
      if d.t_len < 3
      then ret decode_utf_8 (malformed d.t 0 d.t_len) d.t_len d
      else ret decode_utf_8 (r_utf_8 d.t 0 3) 3 d
  | 4 ->
      if d.t_len < 3
      then ret decode_utf_8 (malformed d.t 0 d.t_len) d.t_len d
      else (d.t_need <- 4; t_fill t_decode_utf_8 d)
  | n -> assert false

let guessed_utf_16 d be v =     (* start decoder after `UTF_16{BE,LE} guess. *)
  let decode_utf_16, t_decode_utf_16, t_decode_utf_16_lo, j0, j1 =
    if be then decode_utf_16be, t_decode_utf_16be, t_decode_utf_16be_lo, 0, 1
    else decode_utf_16le, t_decode_utf_16le, t_decode_utf_16le_lo, 1, 0
  in
  let b3 k d =
    if d.t_len < 3 then decode_utf_16 d (* decodes `End *) else
    begin                             (* copy d.t.(2) to d.t.(0) and decode. *)
      d.t_need <- 2; d.t_len <- 1;
      unsafe_set_byte d.t 0 (unsafe_byte d.t 2);
      t_fill k d
    end
  in
  match v with
  | `BOM -> ret (b3 t_decode_utf_16) (`Uchar u_bom) 2 d
  | `ASCII u -> ret (b3 t_decode_utf_16) (`Uchar u) 2 d
  | `Decode ->
      match r_utf_16 d.t j0 j1 with
      | `Malformed _ | `Uchar _ as v -> ret (b3 t_decode_utf_16) v 2 d
      | `Hi hi ->
        if d.t_len < 3
        then ret decode_utf_16 (malformed_pair be hi "" 0 0) d.t_len d
        else (b3 (t_decode_utf_16_lo hi)) d

let guess_encoding d =                  (* guess encoding and start decoder. *)
  let setup d = match r_encoding d.t 0 d.t_len with
  | `UTF_8 r ->
      d.encoding <- `UTF_8; d.k <- decode_utf_8;
      begin match r with
      | `BOM -> ret decode_utf_8 (`Uchar u_bom) 3 d
      | `Decode -> guessed_utf_8 d
      | `End -> `End
      end
  | `UTF_16BE r ->
      d.encoding <- `UTF_16BE; d.k <- decode_utf_16be; guessed_utf_16 d true r
  | `UTF_16LE r ->
      d.encoding <- `UTF_16LE; d.k <- decode_utf_16le; guessed_utf_16 d false r

  in
  (t_need d 3; t_fill setup d)

(* Character post-processors. Used for BOM handling, newline
   normalization and position tracking. The [pp_remove_bom] is only
   used for the first character to remove a possible initial BOM and
   handle UTF-16 endianness recognition. *)

let nline d = d.col <- 0; d.line <- d.line + 1                   (* inlined. *)
let ncol d = d.col <- d.col + 1                                  (* inlined. *)
let ncount d = d.count <- d.count + 1                            (* inlined. *)
let cr d b = d.last_cr <- b                                      (* inlined. *)

let pp_remove_bom utf16 pp d = function(* removes init. BOM, handles UTF-16. *)
| `Uchar 0xFEFF (* BOM *) ->
    if utf16 then (d.encoding <- `UTF_16BE; d.k <- decode_utf_16be);
    d.removed_bom <- true; d.pp <- pp; d.k d
| `Uchar 0xFFFE (* BOM reversed from decode_utf_16be *) when utf16 ->
    d.encoding <- `UTF_16LE; d.k <- decode_utf_16le;
    d.removed_bom <- true; d.pp <- pp; d.k d
| `Malformed _ | `Uchar _ as v ->
    d.removed_bom <- false; d.pp <- pp; d.pp d v

let pp_nln_none d = function
| `Uchar 0x000A (* LF *) as v ->
    let last_cr = d.last_cr in
    cr d false; ncount d; if last_cr then v else (nline d; v)
| `Uchar 0x000D (* CR *) as v -> cr d true; ncount d; nline d; v
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) (* NEL | FF | LS | PS *) as v ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_readline d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) (* NEL | FF | LS | PS *) ->
    cr d false; ncount d; nline d; `Uchar d.nl
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_nlf d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar 0x0085 (* NEL *) -> cr d false; ncount d; nline d; `Uchar d.nl
| `Uchar (0x000C | 0x2028 | 0x2029) as v (* FF | LS | PS *) ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let pp_nln_ascii d = function
| `Uchar 0x000A (* LF *) ->
    let last_cr = d.last_cr in
    cr d false; if last_cr then d.k d else (ncount d; nline d; `Uchar d.nl)
| `Uchar 0x000D (* CR *) -> cr d true; ncount d; nline d; `Uchar d.nl
| `Uchar (0x0085 | 0x000C | 0x2028 | 0x2029) as v (* NEL | FF | LS | PS *) ->
    cr d false; ncount d; nline d; v
| `Uchar _ | `Malformed _ as v -> cr d false; ncount d; ncol d; v

let decode_fun = function
| `UTF_8 -> decode_utf_8
| `UTF_16 -> decode_utf_16be                         (* see [pp_remove_bom]. *)
| `UTF_16BE -> decode_utf_16be
| `UTF_16LE -> decode_utf_16le
| `US_ASCII -> decode_us_ascii
| `ISO_8859_1 -> decode_iso_8859_1

let decoder ?nln ?encoding src =
  let pp, nl = match nln with
  | None -> pp_nln_none, 0x000A (* not used. *)
  | Some (`ASCII nl) -> pp_nln_ascii, nl
  | Some (`NLF nl) -> pp_nln_nlf, nl
  | Some (`Readline nl) -> pp_nln_readline, nl
  in
  let encoding, k = match encoding with
  | None -> `UTF_8, guess_encoding
  | Some e -> (e :> decoder_encoding), decode_fun e
  in
  let i, i_pos, i_max = match src with
  | `Manual -> "", 1, 0                            (* implies src_rem d = 0. *)
  | `Channel _ -> String.create io_buffer_size, 1, 0                (* idem. *)
  | `String s -> s, 0, String.length s - 1
  in
  { src = (src :> src); encoding; nln = (nln :> nln option); nl;
    i; i_pos; i_max; t = String.create 4; t_len = 0; t_need = 0;
    removed_bom = false; last_cr = false; line = 1; col = 0;
    byte_count = 0; count = 0;
    pp = pp_remove_bom (encoding = `UTF_16) pp; k }

let decode d = d.k d
let decoder_line d = d.line
let decoder_col d = d.col
let decoder_byte_count d = d.byte_count
let decoder_count d = d.count
let decoder_removed_bom d = d.removed_bom
let decoder_src d = d.src
let decoder_nln d = d.nln
let decoder_encoding d = d.encoding
let set_decoder_encoding d e =
  d.encoding <- (e :> decoder_encoding); d.k <- decode_fun e

(* Encode *)

type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
type encode = [ `Await | `End | `Uchar of uchar ]
type encoder =
  { dst : dst;                                        (* output destination. *)
    encoding : encoding;                                (* encoded encoding. *)
    mutable o : string;                             (* current output chunk. *)
    mutable o_pos : int;                   (* next output position to write. *)
    mutable o_max : int;                (* maximal output position to write. *)
    t : string;                 (* four bytes buffer for overlapping writes. *)
    mutable t_pos : int;                    (* next position to read in [t]. *)
    mutable t_max : int;                 (* maximal position to read in [t]. *)
    mutable k :                                     (* encoder continuation. *)
      encoder -> encode -> [ `Ok | `Partial ] }

(* On encodes that overlap two (or more) [e.o] buffers, we encode the
   character to the temporary buffer [o.t] and continue with
   [tmp_flush] to write this data on the different [e.o] buffers. If
   the [e.o] buffers are not too small this is faster than
   continuation based byte per byte writes. *)

let o_rem e = e.o_max - e.o_pos + 1    (* remaining bytes to write in [e.o]. *)
let dst e s j l =                                     (* set [e.o] with [s]. *)
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_bounds j l;
  e.o <- s; e.o_pos <- j; e.o_max <- j + l - 1

let partial k e = function `Await -> k e | `Uchar _ | `End -> invalid_encode ()
let flush k e = match e.dst with(* get free storage in [d.o] and [k]ontinue. *)
| `Manual -> e.k <- partial k; `Partial
| `Buffer b -> Buffer.add_substring b e.o 0 e.o_pos; e.o_pos <- 0; k e
| `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

let t_range e max = e.t_pos <- 0; e.t_max <- max
let rec t_flush k e =               (* flush [d.t] up to [d.t_max] in [d.i]. *)
  let blit e l =
    unsafe_blit e.t e.t_pos e.o e.o_pos l;
    e.o_pos <- e.o_pos + l; e.t_pos <- e.t_pos + l
  in
  let rem = o_rem e in
  let len = e.t_max - e.t_pos + 1 in
  if rem < len then (blit e rem; flush (t_flush k) e) else (blit e len; k e)

(* Encoders. *)

let rec encode_utf_8 e v =
  let k e = e.k <- encode_utf_8; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u as v ->
      let rem = o_rem e in
      if u <= 0x007F then
      if rem < 1 then flush (fun e -> encode_utf_8 e v) e else
      (unsafe_set_byte e.o e.o_pos u; e.o_pos <- e.o_pos + 1; k e)
      else if u <= 0x07FF then
      begin
        let s, j, k =
          if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
        in
        unsafe_set_byte s j (0xC0 lor (u lsr 6));
        unsafe_set_byte s (j + 1) (0x80 lor (u land 0x3F));
        k e
      end
      else if u <= 0xFFFF then
      begin
        let s, j, k =
          if rem < 3 then (t_range e 2; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 3; e.o, j, k)
        in
        unsafe_set_byte s j (0xE0 lor (u lsr 12));
        unsafe_set_byte s (j + 1) (0x80 lor ((u lsr 6) land 0x3F));
        unsafe_set_byte s (j + 2) (0x80 lor (u land 0x3F));
        k e
      end
      else
      begin
        let s, j, k =
          if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
        in
        unsafe_set_byte s j (0xF0 lor (u lsr 18));
        unsafe_set_byte s (j + 1) (0x80 lor ((u lsr 12) land 0x3F));
        unsafe_set_byte s (j + 2) (0x80 lor ((u lsr 6) land 0x3F));
        unsafe_set_byte s (j + 3) (0x80 lor (u land 0x3F));
        k e
      end

let rec encode_utf_16be e v =
  let k e = e.k <- encode_utf_16be; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u ->
      let rem = o_rem e in
      if u < 0x10000 then
      begin
        let s, j, k =
          if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
        in
        unsafe_set_byte s j (u lsr 8);
        unsafe_set_byte s (j + 1) (u land 0xFF);
        k e
      end else begin
        let s, j, k =
          if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
          let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
        in
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        unsafe_set_byte s j (hi lsr 8);
        unsafe_set_byte s (j + 1) (hi land 0xFF);
        unsafe_set_byte s (j + 2) (lo lsr 8);
        unsafe_set_byte s (j + 3) (lo land 0xFF);
        k e
      end

let rec encode_utf_16le e v =         (* encode_uft_16be with bytes swapped. *)
  let k e = e.k <- encode_utf_16le; `Ok in
  match v with
  | `Await -> k e
  | `End -> flush k e
  | `Uchar u ->
    let rem = o_rem e in
    if u < 0x10000 then
    begin
      let s, j, k =
        if rem < 2 then (t_range e 1; e.t, 0, t_flush k) else
        let j = e.o_pos in (e.o_pos <- e.o_pos + 2; e.o, j, k)
      in
      unsafe_set_byte s j (u land 0xFF);
      unsafe_set_byte s (j + 1) (u lsr 8);
      k e
    end
    else
    begin
      let s, j, k =
        if rem < 4 then (t_range e 3; e.t, 0, t_flush k) else
        let j = e.o_pos in (e.o_pos <- e.o_pos + 4; e.o, j, k)
      in
      let u' = u - 0x10000 in
      let hi = (0xD800 lor (u' lsr 10)) in
      let lo = (0xDC00 lor (u' land 0x3FF)) in
      unsafe_set_byte s j (hi land 0xFF);
      unsafe_set_byte s (j + 1) (hi lsr 8);
      unsafe_set_byte s (j + 2) (lo land 0xFF);
      unsafe_set_byte s (j + 3) (lo lsr 8);
      k e
    end

let encode_fun = function
| `UTF_8 -> encode_utf_8
| `UTF_16 -> encode_utf_16be
| `UTF_16BE -> encode_utf_16be
| `UTF_16LE -> encode_utf_16le

let encoder encoding dst =
  let o, o_pos, o_max = match dst with
  | `Manual -> "", 1, 0                              (* implies o_rem e = 0. *)
  | `Buffer _
  | `Channel _ -> String.create io_buffer_size, 0, io_buffer_size - 1
  in
  { dst = (dst :> dst); encoding = (encoding :> encoding); o; o_pos; o_max;
    t = String.create 4; t_pos = 1; t_max = 0; k = encode_fun encoding}

let encode e v = e.k e (v :> encode)
let encoder_encoding e = e.encoding
let encoder_dst e = e.dst

(* Manual sources and destinations. *)

module Manual = struct
  let src = src
  let dst = dst
  let dst_rem = o_rem
end

(* Strings folders and Buffer encoders *)

module String = struct
  let encoding_guess s = match r_encoding s 0 (max (String.length s) 3) with
  | `UTF_8 d -> `UTF_8, (d = `BOM)
  | `UTF_16BE d -> `UTF_16BE, (d = `BOM)
  | `UTF_16LE d -> `UTF_16LE, (d = `BOM)

  type 'a folder =
    'a -> int -> [ `Uchar of uchar | `Malformed of string ] -> 'a

  let fold_utf_8 f acc s =
    let rec loop acc f s i l =
      if i = l then acc else
      let need = unsafe_array_get utf_8_len (unsafe_byte s i) in
      if need = 0 then loop (f acc i (malformed s i 1)) f s (i + 1) l else
      let rem = l - i in
      if rem < need then f acc i (malformed s i rem) else
      loop (f acc i (r_utf_8 s i need)) f s (i + need) l
    in
    loop acc f s 0 (String.length s)

  let fold_utf_16be f acc s =
    let rec loop acc f s i l =
      if i = l then acc else
      let rem = l - i in
      if rem < 2 then f acc i (malformed s i 1) else
      match r_utf_16 s i (i + 1) with
      | `Uchar _ | `Malformed _ as v -> loop (f acc i v) f s (i + 2) l
      | `Hi hi ->
          if rem < 4 then f acc i (malformed s i rem)  else
          loop (f acc i (r_utf_16_lo hi s (i + 2) (i + 3))) f s (i + 4) l
    in
    loop acc f s 0 (String.length s)

  let fold_utf_16le f acc s =             (* [fold_utf_16be], bytes swapped. *)
    let rec loop acc f s i l =
      if i = l then acc else
      let rem = l - i in
      if rem < 2 then f acc i (malformed s i 1) else
      match r_utf_16 s (i + 1) i with
      | `Uchar _ | `Malformed _ as v -> loop (f acc i v) f s (i + 2) l
      | `Hi hi ->
          if rem < 4 then f acc i (malformed s i rem)  else
          loop (f acc i (r_utf_16_lo hi s (i + 3) (i + 2))) f s (i + 4) l
    in
    loop acc f s 0 (String.length s)
end

module Buffer = struct
  let add_utf_8 b u =
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u <= 0x007F then
    (w u)
    else if u <= 0x07FF then
    (w (0xC0 lor (u lsr 6));
     w (0x80 lor (u land 0x3F)))
    else if u <= 0xFFFF then
    (w (0xE0 lor (u lsr 12));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))
    else
    (w (0xF0 lor (u lsr 18));
     w (0x80 lor ((u lsr 12) land 0x3F));
     w (0x80 lor ((u lsr 6) land 0x3F));
     w (0x80 lor (u land 0x3F)))

  let add_utf_16be b u =
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u < 0x10000 then (w (u lsr 8); w (u land 0xFF)) else
    let u' = u - 0x10000 in
    let hi = (0xD800 lor (u' lsr 10)) in
    let lo = (0xDC00 lor (u' land 0x3FF)) in
    w (hi lsr 8); w (hi land 0xFF);
    w (lo lsr 8); w (lo land 0xFF)

  let add_utf_16le b u =                            (* swapped add_utf_16be. *)
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u < 0x10000 then (w (u land 0xFF); w (u lsr 8)) else
    let u' = u - 0x10000 in
    let hi = (0xD800 lor (u' lsr 10)) in
    let lo = (0xDC00 lor (u' land 0x3FF)) in
    w (hi land 0xFF); w (hi lsr 8);
    w (lo land 0xFF); w (lo lsr 8)
end

(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

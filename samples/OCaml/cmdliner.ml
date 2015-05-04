(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Invalid_arg strings *)

let err_argv = "argv array must have at least one element"
let err_not_opt = "Option argument without name"
let err_not_pos = "Positional argument with a name"
let err_help s = "term error, help requested for unknown command " ^ s
let err_empty_list = "empty list"

(* A few useful definitions. *)

let rev_compare n n' = compare n' n
let str = Printf.sprintf
let pr = Format.fprintf
let pr_str = Format.pp_print_string
let pr_char = Format.pp_print_char
let str_of_pp pp v = pp Format.str_formatter v; Format.flush_str_formatter ()
let quote s = str "`%s'" s
let alts_str ?(quoted = true) alts =
  let quote = if quoted then quote else (fun s -> s) in
  match alts with
  | [] -> invalid_arg err_empty_list
  | [a] -> (quote a)
  | [a; b] -> str "either %s or %s" (quote a) (quote b)
  | alts ->
      let rev_alts = List.rev alts in
      str "one of %s or %s"
        (String.concat ", " (List.rev_map quote (List.tl rev_alts)))
        (quote (List.hd rev_alts))

let pr_white_str spaces ppf s =  (* spaces and new lines with Format's funs *)
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pr_text = pr_white_str true
let pr_lines = pr_white_str false
let pr_to_temp_file pr v = try
  let exec = Filename.basename Sys.argv.(0) in
  let file, oc = Filename.open_temp_file exec "out" in
  let ppf = Format.formatter_of_out_channel oc in
  pr ppf v; Format.pp_print_flush ppf (); close_out oc;
  at_exit (fun () -> try Sys.remove file with Sys_error e -> ());
  Some file
with Sys_error _ -> None

(* Levenshtein distance, for making spelling suggestions in case of error. *)

let levenshtein_distance s t =
  (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
  let minimum a b c = min a (min b c) in
  let m = String.length s in
  let n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in
  for i = 0 to m do d.(i).(0) <- i done;
  for j = 0 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- minimum
            (d.(i-1).(j) + 1)   (* a deletion *)
            (d.(i).(j-1) + 1)   (* an insertion *)
            (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;
  d.(m).(n)

let suggest s candidates =
  let add (min, acc) name =
    let d = levenshtein_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let dist, suggs = List.fold_left add (max_int, []) candidates in
  if dist < 3 (* suggest only if not too far *) then suggs else []

(* Tries. This implementation also maps any non ambiguous prefix of a
   key to its value. *)

module Trie : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : 'a t -> string -> 'a -> 'a t
  val find : 'a t -> string -> [ `Ok of 'a | `Ambiguous | `Not_found ]
  val ambiguities : 'a t -> string -> string list
  val of_list : (string * 'a) list -> 'a t
end = struct
  module Cmap = Map.Make (Char)                           (* character maps. *)
  type 'a value =                         (* type for holding a bound value. *)
    | Pre of 'a                    (* value is bound by the prefix of a key. *)
    | Key of 'a                          (* value is bound by an entire key. *)
    | Amb                     (* no value bound because of ambiguous prefix. *)
    | Nil                            (* not bound (only for the empty trie). *)

  type 'a t = { v : 'a value; succs : 'a t Cmap.t }
  let empty = { v = Nil; succs = Cmap.empty }
  let is_empty t = t = empty

  (* N.B. If we replace a non-ambiguous key, it becomes ambiguous but it's
     not important for our use. Also the following is not tail recursive but
     the stack is bounded by key length. *)
  let add t k d =
    let rec aux t k len i d pre_d =
      if i = len then { v = Key d; succs = t.succs } else
      let v = match t.v with
      | Amb | Pre _ -> Amb | Key _ as v -> v | Nil -> pre_d
      in
      let succs =
        let t' = try Cmap.find k.[i] t.succs with Not_found -> empty in
        Cmap.add k.[i] (aux t' k len (i + 1) d pre_d) t.succs
      in
      { v; succs }
    in
    aux t k (String.length k) 0 d (Pre d (* allocate less *))

  let find_node t k =
    let rec aux t k len i =
      if i = len then t else
      aux (Cmap.find k.[i] t.succs) k len (i + 1)
    in
    aux t k (String.length k) 0

  let find t k =
    try match (find_node t k).v with
    | Key v | Pre v -> `Ok v | Amb -> `Ambiguous | Nil -> `Not_found
    with Not_found -> `Not_found

  let ambiguities t p =                        (* ambiguities of [p] in [t]. *)
    try
      let t = find_node t p in
      match t.v with
      | Key _ | Pre _ | Nil -> []
      | Amb ->
          let add_char s c = s ^ (String.make 1 c) in
          let rem_char s = String.sub s 0 ((String.length s) - 1) in
          let to_list m = Cmap.fold (fun k t acc -> (k,t) :: acc) m [] in
          let rec aux acc p = function
          | ((c, t) :: succs) :: rest ->
              let p' = add_char p c in
              let acc' = match t.v with
              | Pre _ | Amb -> acc
              | Key _ -> (p' :: acc)
              | Nil -> assert false
              in
              aux acc' p' ((to_list t.succs) :: succs :: rest)
          | [] :: [] -> acc
          | [] :: rest -> aux acc (rem_char p) rest
          | [] -> assert false
          in
          aux [] p (to_list t.succs :: [])
    with Not_found -> []

  let of_list l = List.fold_left (fun t (s, v) -> add t s v) empty l
end

(* The following types keep untyped information about arguments and
   terms. This data is used to parse the command line, report errors
   and format man page information. *)

type absence =        (* what happens if the argument is absent from the cl. *)
  | Error                                           (* an error is reported. *)
  | Val of string Lazy.t         (* if <> "", takes the given default value. *)

type opt_kind =                              (* kinds of optional arguments. *)
  | Flag                                      (* just a flag, without value. *)
  | Opt                                                (* value is required. *)
  | Opt_vopt of string     (* option value is optional, takes given default. *)

type pos_kind =                            (* kinds of positional arguments. *)
  | All                                         (* all positional arguments. *)
  | Nth of bool * int                                  (* specific position. *)
  | Left of bool * int                (* all args on the left of a position. *)
  | Right of bool * int              (* all args on the right of a position. *)

type arg_info =                (* information about a command line argument. *)
    { id : int;                               (* unique id for the argument. *)
      absent : absence;                              (* behaviour if absent. *)
      doc : string;                                                 (* help. *)
      docv : string;              (* variable name for the argument in help. *)
      docs : string;                  (* title of help section where listed. *)
      p_kind : pos_kind;                             (* positional arg kind. *)
      o_kind : opt_kind;                               (* optional arg kind. *)
      o_names : string list;                        (* names (for opt args). *)
      o_all : bool; }                          (* repeatable (for opt args). *)

let arg_id =        (* thread-safe UIDs, Oo.id (object end) was used before. *)
  let c = ref 0 in
  fun () ->
    let id = !c in
    incr c; if id > !c then assert false (* too many ids *) else id

let is_opt a = a.o_names <> []
let is_pos a = a.o_names = []

module Amap = Map.Make                                     (* arg info maps. *)
    (struct type t = arg_info let compare a a' = compare a.id a'.id end)

type arg =        (* unconverted argument data as found on the command line. *)
  | O of (int * string * (string option)) list (* (pos, name, value) of opt. *)
  | P of string list

type cmdline = arg Amap.t      (* command line, maps arg_infos to arg value. *)

type man_block = [                                 (* block of manpage text. *)
  | `S of string | `P of string | `I of string * string | `Noblank ]

type term_info =
  { name : string;                                    (* name of the term. *)
    version : string option;                   (* version (for --version). *)
    tdoc : string;                        (* one line description of term. *)
    tdocs : string;       (* title of man section where listed (commands). *)
    sdocs : string;    (* standard options, title of section where listed. *)
    man : man_block list; }                              (* man page text. *)

type eval_info =                (* information about the evaluation context. *)
  { term : term_info * arg_info list;             (* term being evaluated. *)
    main : term_info * arg_info list;                        (* main term. *)
    choices : (term_info * arg_info list) list}       (* all term choices. *)

let eval_kind ei =                       (* evaluation with multiple terms ? *)
  if ei.choices = [] then `Simple else
  if (fst ei.term) == (fst ei.main) then `M_main else `M_choice

module Manpage = struct
  type title = string * int * string * string * string
  type block = man_block
  type t = title * block list

  let p_indent = 7                                  (* paragraph indentation. *)
  let l_indent = 4                                      (* label indentation. *)
  let escape subst esc buf s =
    let subst s =
      let len = String.length s in
      if not (len > 1 && s.[1] = ',') then (subst s) else
      if len = 2 then "" else
      esc s.[0] (String.sub s 2 (len - 2))
    in
    Buffer.clear buf; Buffer.add_substitute buf subst s;
    let s = Buffer.contents buf in (* twice for $(i,$(mname)). *)
    Buffer.clear buf; Buffer.add_substitute buf subst s;
    Buffer.contents buf

  let pr_tokens ?(groff = false) ppf s =
    let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
    let len = String.length s in
    let i = ref 0 in
    try while (true) do
        while (!i < len && is_space s.[!i]) do incr i done;
        let start = !i in
        if start = len then raise Exit;
        while (!i < len && not (is_space s.[!i]) && not (s.[!i] = '-')) do
          incr i
        done;
        pr_str ppf (String.sub s start (!i - start));
        if !i = len then raise Exit;
        if s.[!i] = '-' then
          (incr i; if groff then pr_str ppf "\\-" else pr_char ppf '-');
        if (!i < len && is_space s.[!i]) then
          (if groff then pr_char ppf ' ' else Format.pp_print_space ppf ())
      done with Exit -> ()

  (* Plain text output *)

  let plain_esc c s = match c with 'g' -> "" (* groff specific *) | _ ->  s
  let pr_indent ppf c = for i = 1 to c do pr_char ppf ' ' done
  let pr_plain_blocks subst ppf ts =
    let buf = Buffer.create 1024 in
    let escape t = escape subst plain_esc buf t in
    let pr_tokens ppf t = pr_tokens ppf (escape t) in
    let rec aux = function
    | [] -> ()
    | t :: ts ->
        begin match t with
        | `Noblank -> ()
        | `P s -> pr ppf "%a@[%a@]@," pr_indent p_indent pr_tokens s
        | `S s -> pr ppf "@[%a@]" pr_tokens s
        | `I (label, s) ->
            let label = escape label in
            let ll = String.length label in
            pr ppf "@[%a@[%a@]" pr_indent p_indent pr_tokens label;
            if s = "" then () else
            if ll < l_indent then
              pr ppf "%a@[%a@]@]@," pr_indent (l_indent - ll) pr_tokens s
            else
            pr ppf "@\n%a@[%a@]@]@,"
              pr_indent (p_indent + l_indent) pr_tokens s
        end;
        begin match ts with
        | `Noblank :: ts -> aux ts
        | ts -> Format.pp_print_cut ppf (); aux ts
        end
    in
    aux ts

  let pr_plain_page subst ppf (_, text) =
    pr ppf "@[<v>%a@]" (pr_plain_blocks subst) text

  (* Groff output *)

  let groff_esc c s = match c with
  | 'i' -> (str "\\fI%s\\fR" s)
  | 'b' -> (str "\\fB%s\\fR" s)
  | 'p' -> "" (* plain text specific *)
  | _ -> s

  let pr_groff_blocks subst ppf text =
    let buf = Buffer.create 1024 in
    let escape t = escape subst groff_esc buf t in
    let pr_tokens ppf t = pr_tokens ~groff:true ppf (escape t) in
    let pr_block = function
    | `P s -> pr ppf "@\n.P@\n%a" pr_tokens s
    | `S s -> pr ppf "@\n.SH %a" pr_tokens s
    | `Noblank -> pr ppf "@\n.sp -1"
    | `I (l, s) -> pr ppf "@\n.TP 4@\n%a@\n%a" pr_tokens l pr_tokens s
    in
    List.iter pr_block text

  let pr_groff_page subst ppf ((n, s, a1, a2, a3), t) =
    pr ppf ".\\\" Pipe this output to groff -man -Tutf8 | less@\n\
            .\\\"@\n\
            .TH \"%s\" %d \"%s\" \"%s\" \"%s\"@\n\
            .\\\" Disable hyphenantion and ragged-right@\n\
            .nh@\n\
      .ad l\
      %a@?"
      n s a1 a2 a3 (pr_groff_blocks subst) t

  (* Printing to a pager *)

  let find_cmd cmds =
    let test, null = match Sys.os_type with
    | "Win32" -> "where", " NUL"
    | _ -> "type", "/dev/null"
    in
    let cmd c = Sys.command (str "%s %s 1>%s 2>%s" test c null null) = 0 in
    try Some (List.find cmd cmds) with Not_found -> None

  let pr_to_pager print ppf v =
    let pager =
      let cmds = ["less"; "more"] in
      let cmds = try (Sys.getenv "PAGER") :: cmds with Not_found -> cmds in
      let cmds = try (Sys.getenv "MANPAGER") :: cmds with Not_found -> cmds in
      find_cmd cmds
    in
    match pager with
    | None -> print `Plain ppf v
    | Some pager ->
        let cmd = match (find_cmd ["groff"; "nroff"]) with
        | None ->
            begin match pr_to_temp_file (print `Plain) v with
            | None -> None
            | Some f -> Some (str "%s < %s" pager f)
            end
        | Some c ->
            begin match pr_to_temp_file (print `Groff) v with
            | None -> None
            | Some f ->
                (* TODO use -Tutf8, but annoyingly maps U+002D to U+2212. *)
                let xroff = if c = "groff" then c ^ " -Tascii -P-c" else c in
                Some (str "%s -man < %s | %s" xroff f pager)
            end
        in
        match cmd with
        | None -> print `Plain ppf v
        | Some cmd -> if (Sys.command cmd) <> 0 then print `Plain ppf v

  let rec print ?(subst = fun x -> x) fmt ppf page = match fmt with
  | `Pager -> pr_to_pager (print ~subst) ppf page
  | `Plain -> pr_plain_page subst ppf page
  | `Groff -> pr_groff_page subst ppf page
end

module Help = struct
  let invocation ?(sep = ' ') ei = match eval_kind ei with
  | `Simple | `M_main -> (fst ei.main).name
  | `M_choice -> str "%s%c%s" (fst ei.main).name sep (fst ei.term).name

  let title ei =
    let prog = String.capitalize (fst ei.main).name in
    let name = String.uppercase (invocation ~sep:'-' ei) in
    let left_footer = prog ^ match (fst ei.main).version with
      | None -> "" | Some v -> str " %s" v
    in
    let center_header = str "%s Manual" prog in
    name, 1, "", left_footer, center_header

  let name_section ei =
    let tdoc d = if d = "" then "" else (str " - %s" d) in
    [`S "NAME"; `P (str "%s%s" (invocation ~sep:'-' ei)
                      (tdoc (fst ei.term).tdoc)); ]

  let synopsis ei = match eval_kind ei with
  | `M_main -> str "$(b,%s) $(i,COMMAND) ..." (invocation ei)
  | `Simple | `M_choice ->
      let rev_cmp (p, _) (p', _) = match p', p with        (* best effort. *)
      | p, All -> -1 | All, p -> 1
      | Left _, Right _ -> -1 | Right _, Left _ -> 1
      | Left (false, k), Nth (false, k')
      | Nth (false, k), Nth (false, k')
      | Nth (false, k), Right (false, k') -> if k <= k' then -1 else 1
      | Nth (false, k), Left (false, k')
      | Right (false, k), Nth (false, k') -> if k >= k' then 1 else -1
      | Left (true, k), Nth (true, k')
      | Nth (true, k), Nth (true, k')
      | Nth (true, k), Right (true, k') -> if k >= k' then -1 else 1
      | Nth (true, k), Left (true, k')
      | Right (true, k), Nth (true, k') -> if k <= k' then 1 else -1
      | p, p' -> compare p p'
      in
      let rec format_pos acc = function
      | a :: al ->
          if is_opt a then format_pos acc al else
          let v = if a.docv = "" then "$(i,ARG)" else str "$(i,%s)" a.docv in
          let v = if a.absent = Error then str "%s" v else str "[%s]" v in
          let v = v ^ match a.p_kind with Nth _ -> "" | _ -> "..." in
          format_pos ((a.p_kind, v) :: acc) al
      | [] -> acc
      in
      let args = List.sort rev_cmp (format_pos [] (snd ei.term)) in
      let args = String.concat " " (List.rev_map snd args) in
      str "$(b,%s) [$(i,OPTION)]... %s" (invocation ei) args

  let get_synopsis_section ei =
    let rec extract_synopsis syn = function
    | `S _ :: _ as man -> List.rev syn, man
    |  block :: rest -> extract_synopsis (block :: syn) rest
    | [] -> List.rev syn, []
    in
    match (fst ei.term).man with
    | `S "SYNOPSIS" as s :: rest -> extract_synopsis [s] rest (* user-defined *)
    | man -> [ `S "SYNOPSIS"; `P (synopsis ei); ], man           (* automatic *)

  let make_arg_label a =
    if is_pos a then str "$(i,%s)" a.docv else
    let fmt_name var = match a.o_kind with
    | Flag -> fun n -> str "$(b,%s)" n
    | Opt ->
        fun n ->
          if String.length n > 2 then str "$(b,%s)=$(i,%s)" n var else
          str "$(b,%s) $(i,%s)" n var
    | Opt_vopt _ ->
        fun n ->
          if String.length n > 2 then str "$(b,%s)[=$(i,%s)]" n var else
          str "$(b,%s) [$(i,%s)]" n var
    in
    let var = if a.docv = "" then "VAL" else a.docv in
    let names = List.sort compare a.o_names in
    let s = String.concat ", " (List.rev_map (fmt_name var) names) in
    s

  let make_arg_items ei =
    let buf = Buffer.create 200 in
    let subst_docv docv d =
      let subst = function "docv" -> str "$(i,%s)" docv | s -> str "$(%s)" s in
      Buffer.clear buf; Buffer.add_substitute buf subst d; Buffer.contents buf
    in
    let rev_cmp a' a =
      let c = compare a.docs a'.docs in
      if c <> 0 then c else
      match is_opt a, is_opt a' with
      | true, true ->
          let key names =
            let k = String.lowercase (List.hd (List.sort rev_compare names)) in
            if k.[1] = '-' then String.sub k 1 (String.length k - 1) else k
          in
          compare (key a.o_names) (key a'.o_names)
      | false, false ->
          compare (String.lowercase a.docv) (String.lowercase a'.docv)
      | true, false -> -1
      | false, true -> 1
    in
    let format a =
      let absent = match a.absent with
      | Error -> ""
      | Val v -> match Lazy.force v with "" -> "" | v -> str "absent=%s" v
      in
      let optvopt = match a.o_kind with
      | Opt_vopt v -> str "default=%s" v
      | _ -> ""
      in
      let argvdoc = match absent, optvopt with
      | "", "" -> ""
      | s, "" | "", s -> str " (%s)" s
      | s, s' -> str " (%s, %s)" s s'
      in
      (a.docs, `I (make_arg_label a ^ argvdoc, (subst_docv a.docv a.doc)))
    in
    let is_arg_item a = not (is_pos a && (a.docv = "" || a.doc = "")) in
    let l = List.sort rev_cmp (List.filter is_arg_item (snd ei.term)) in
    List.rev_map format l

  let make_cmd_items ei = match eval_kind ei with
  | `Simple | `M_choice -> []
  | `M_main ->
      let add_cmd acc (ti, _) =
        (ti.tdocs, `I ((str "$(b,%s)" ti.name), ti.tdoc)) :: acc
      in
      List.sort rev_compare (List.fold_left add_cmd [] ei.choices)

  let text ei =                  (* man that code is particulary unreadable. *)
    let rec merge_items acc to_insert mark il = function
    | `S s as sec :: ts ->
        let acc = List.rev_append to_insert acc in
        let acc = if mark then sec :: `Orphan_mark :: acc else sec :: acc in
        let to_insert, il = List.partition (fun (n, _) -> n = s) il in
        let to_insert = List.rev_map (fun (_, i) -> i) to_insert in
        let to_insert = (to_insert :> [ `Orphan_mark | Manpage.block] list) in
        merge_items acc to_insert (s = "DESCRIPTION") il ts
    | t :: ts ->
        let t = (t :> [`Orphan_mark | Manpage.block]) in
        merge_items (t :: acc) to_insert mark il ts
    | [] ->
        let acc = List.rev_append to_insert acc in
        (if mark then `Orphan_mark :: acc else acc), il
    in
    let rec merge_orphans acc orphans = function
    | `Orphan_mark :: ts ->
        let rec merge acc s = function
        | [] -> (`S s) :: acc
        | (s', i) :: ss ->
            let i = (i :> Manpage.block) in
            if s = s' then merge (i :: acc) s ss else
            merge (i :: (`S s) :: acc) s' ss
        in
        let acc = match orphans with
        | [] -> acc | (s, _) :: _ -> merge acc s orphans
        in
        merge_orphans acc [] ts
    | (#Manpage.block as e) :: ts -> merge_orphans (e :: acc) orphans ts
    | [] -> acc
    in
    let cmds = make_cmd_items ei in
    let args = make_arg_items ei in
    let cmp (s, _) (s', _) = compare s s' in
    let items = List.rev (List.stable_sort cmp (List.rev_append cmds args)) in
    let synopsis, man = get_synopsis_section ei in
    let rev_text, orphans =
      merge_items [`Orphan_mark] [] false items man
    in
    synopsis @ merge_orphans [] orphans rev_text

  let ei_subst ei = function
  | "tname" -> (fst ei.term).name
  | "mname" -> (fst ei.main).name
  | s -> s

  let man ei =
    title ei, (name_section ei) @ (text ei)

  let print fmt ppf ei = Manpage.print ~subst:(ei_subst ei) fmt ppf (man ei)
  let pr_synopsis ppf ei =
    pr ppf "@[%s@]"
      (Manpage.escape (ei_subst ei)
         Manpage.plain_esc (Buffer.create 100) (synopsis ei))

  let pr_version ppf ei = match (fst ei.main).version with
  | None -> assert false
  | Some v -> pr ppf "@[%a@]@." pr_text v
end

(* Errors for the command line user *)

module Err = struct
  let invalid kind s exp = str "invalid %s %s, %s" kind (quote s) exp
  let invalid_val = invalid "value"
  let no kind s = str "no %s %s" (quote s) kind
  let not_dir s = str "%s is not a directory" (quote s)
  let is_dir s = str "%s is a directory" (quote s)
  let element kind s exp = str "invalid element in %s (`%s'): %s" kind s exp
  let sep_miss sep s = invalid_val s (str "missing a `%c' separator" sep)
  let unknown kind ?(hints = []) v =
    let did_you_mean s = str ", did you mean %s ?" s in
    let hints = match hints with [] -> "." | hs -> did_you_mean (alts_str hs) in
    str "unknown %s %s%s" kind (quote v) hints

  let ambiguous kind s ambs =
    str "%s %s ambiguous and could be %s" kind (quote s) (alts_str ambs)

  let pos_excess excess =
    str "too many arguments, don't know what to do with %s"
      (String.concat ", " (List.map quote excess))

  let flag_value f v =
    str "option %s is a flag, it cannot take the argument %s"
      (quote f) (quote v)

  let opt_value_missing f = str "option %s needs an argument" (quote f)
  let opt_parse_value f e = str "option %s: %s" (quote f) e
  let opt_repeated f f' =
    if f = f' then str "option %s cannot be repeated" (quote f) else
    str "options %s and %s cannot be present at the same time" (quote f)
      (quote f')

  let pos_parse_value a e =
    if a.docv = "" then e else match a.p_kind with
    | Nth _ -> str "%s argument: %s" a.docv e
    | _ -> str "%s... arguments: %s" a.docv e

  let arg_missing a =
    if is_opt a then
      let rec long_name = function
      | n :: l -> if (String.length n) > 2 || l = [] then n else long_name l
      | [] -> assert false
      in
      str "required option %s is missing" (long_name a.o_names)
    else
    if a.docv = "" then str "a required argument is missing" else
    str "required argument %s is missing" a.docv

  (* Error printers *)

  let print ppf ei e = pr ppf "%s: @[%a@]@." (fst ei.main).name pr_text e
  let pr_backtrace err ei e bt =
    let bt =
      let len = String.length bt in
      if len > 0 then String.sub bt 0 (len - 1) (* remove final '\n' *) else bt
    in
    pr err
      "%s: @[internal error, uncaught exception:@\n%a@]@."
      (fst ei.main).name pr_lines (str "%s\n%s" (Printexc.to_string e) bt)

  let pr_try_help ppf ei =
    let exec = Help.invocation ei in
    let main = (fst ei.main).name in
    if exec = main then
      pr ppf "@[<2>Try `%s --help' for more information.@]" exec
    else
    pr ppf "@[<2>Try `%s --help' or `%s --help' for more information.@]"
      exec main

  let pr_usage ppf ei e =
    pr ppf "@[<v>%s: @[%a@]@,@[Usage: @[%a@]@]@,%a@]@."
      (fst ei.main).name pr_text e Help.pr_synopsis ei pr_try_help ei
end

(* Command lines. A command line stores pre-parsed information about
   the command line's arguments in a more structured way. Given the
   [arg_info] values mentionned in a term and Sys.argv (whithout exec
   name) we parse the command line into a map of [arg_info] values to
   [arg] values. This map is used by the term's closures to retrieve
   and convert command line arguments (see the Arg module). *)

module Cmdline :sig
  exception Error of string
  val choose_term : term_info -> (term_info * 'a) list -> string list ->
    term_info * string list
  val create : ?peek_opts:bool -> arg_info list -> string list -> cmdline
  val opt_arg : cmdline -> arg_info -> (int * string * (string option)) list
  val pos_arg : cmdline -> arg_info -> string list
end = struct
  exception Error of string

  let opt_arg cl a = match try Amap.find a cl with Not_found -> assert false
  with O l -> l | _ -> assert false

  let pos_arg cl a = match try Amap.find a cl with Not_found -> assert false
  with P l -> l | _ -> assert false

  let choose_term ti choices = function
  | [] -> ti, []
  | maybe :: args' as args ->
      if String.length maybe > 1 && maybe.[0] = '-' then ti, args else
      let index =
        let add acc (choice, _) = Trie.add acc choice.name choice in
        List.fold_left add Trie.empty choices
      in
      match Trie.find index maybe with
      | `Ok choice -> choice, args'
      | `Not_found ->
        let all = Trie.ambiguities index "" in
        let hints = suggest maybe all in
        raise (Error (Err.unknown "command" ~hints maybe))
      | `Ambiguous ->
          let ambs = List.sort compare (Trie.ambiguities index maybe) in
          raise (Error (Err.ambiguous "command" maybe ambs))

  let arg_info_indexes al =
    (* from [al] returns a trie mapping the names of optional arguments to
       their arg_info, a list with all arg_info for positional arguments and
       a cmdline mapping each arg_info to an empty [arg]. *)
    let rec aux opti posi cl = function
    | a :: l ->
        if is_pos a then aux opti (a :: posi) (Amap.add a (P []) cl) l else
        let add t name = Trie.add t name a in
        aux (List.fold_left add opti a.o_names) posi (Amap.add a (O []) cl) l
    | [] -> opti, posi, cl
    in
    aux Trie.empty [] Amap.empty al

  let parse_opt_arg s =          (* (name,value) of opt arg, assert len > 1. *)
    let l = String.length s in
    if s.[1] <> '-' then
      if l = 2 then s, None else
      String.sub s 0 2, Some (String.sub s 2 (l - 2))
    else try
      let i = String.index s '=' in
      String.sub s 0 i, Some (String.sub s (i + 1) (l - i - 1))
    with Not_found -> s, None

  let parse_args ~peek_opts opti cl args =
    (* returns an updated [cl] cmdline according to the options found in [args]
       with the trie index [opti]. Positional arguments are returned in order
       in a list. *)
    let rec aux k opti cl pargs = function
    | [] -> cl, (List.rev pargs)
    | "--" :: args -> cl, (List.rev_append pargs args)
    | s :: args ->
        let is_opt s = String.length s > 1 && s.[0] = '-' in
        let is_short_opt s = String.length s = 2 && s.[0] = '-' in
        if not (is_opt s) then aux (k+1) opti cl (s :: pargs) args else
        let name, value = parse_opt_arg s in
        match Trie.find opti name with
        | `Ok a ->
            let value, args = match value, a.o_kind with
            | Some v, Flag when is_short_opt name -> None, ("-" ^ v) :: args
            | Some v, _ -> value, args
            | None, Flag -> value, args
            | None, _ ->
                match args with
                | v :: rest -> if is_opt v then None, args else Some v, rest
                | [] -> None, args
            in
            let arg = O ((k, name, value) :: opt_arg cl a) in
            aux (k+1) opti (Amap.add a arg cl) pargs args
        | `Not_found when peek_opts -> aux (k+1) opti cl pargs args (* skip *)
        | `Not_found ->
            let hints =
              if String.length s <= 2 then [] else
              let short_opt, long_opt =
                if s.[1] <> '-'
                then s, Printf.sprintf "-%s" s
                else String.sub s 1 (String.length s - 1), s
              in
              let short_opt, _ = parse_opt_arg short_opt in
              let long_opt, _ = parse_opt_arg long_opt in
              let all = Trie.ambiguities opti "-" in
              match List.mem short_opt all, suggest long_opt all with
              | false, [] -> []
              | false, l -> l
              | true, [] -> [short_opt]
              | true, l -> if List.mem short_opt l then l else short_opt :: l
            in
            raise (Error (Err.unknown "option" ~hints name))
        | `Ambiguous ->
            let ambs = List.sort compare (Trie.ambiguities opti name) in
            raise (Error (Err.ambiguous "option" name ambs))
    in
    aux 0 opti cl [] args

  let process_pos_args posi cl pargs =
    (* returns an updated [cl] cmdline in which each positional arg mentionned
       in the list index posi, is given a value according the list
       of positional arguments values [pargs]. *)
    if pargs = [] then cl else
    let rec take n acc l =
      if n = 0 then List.rev acc else
      take (n - 1) (List.hd l :: acc) (List.tl l)
    in
    let rec aux pargs last cl max_spec = function
    | a :: al ->
        let arg, max_spec = match a.p_kind with
        | All -> P pargs, last
        | Nth (rev, k) ->
            let k = if rev then last - k else k in
            let max_spec = max k max_spec in
            if k < 0 || k > last then P [], max_spec else
            P ([List.nth pargs k]), max_spec
        | Left (rev, k) ->
            let k = if rev then last - k else k in
            let max_spec = max k max_spec in
            if k <= 0 || k > last then P [], max_spec else
            P (take k [] pargs), max_spec
        | Right (rev, k) ->
            let k = if rev then last - k else k in
            if k < 0 || k >= last then P [], last else
            P (List.rev (take (last - k) [] (List.rev pargs))), last
        in
        aux pargs last (Amap.add a arg cl) max_spec al
    | [] -> cl, max_spec
    in
    let last = List.length pargs - 1 in
    let cl, max_spec = aux pargs last cl (-1) posi in
    if last <= max_spec then cl else
    let excess = List.rev (take (last - max_spec) [] (List.rev pargs)) in
    raise (Error (Err.pos_excess excess))

  let create ?(peek_opts = false) al args =
    let opti, posi, cl = arg_info_indexes al in
    let cl, pargs = parse_args ~peek_opts opti cl args in
    if peek_opts then cl (* skip positional arguments *) else
    process_pos_args posi cl pargs
end

module Arg = struct
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a converter = 'a parser * 'a printer
  type 'a arg_converter = (eval_info -> cmdline -> 'a)
  type 'a t = arg_info list * 'a arg_converter
  type info = arg_info

  let ( & ) f x = f x
  let parse_error e = raise (Cmdline.Error e)
  let some ?(none = "") (parse, print) =
    (fun s -> match parse s with `Ok v -> `Ok (Some v) | `Error _ as e -> e),
    (fun ppf v -> match v with None -> pr_str ppf none| Some v -> print ppf v)

  let info ?docs ?(docv = "") ?(doc = "") names =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let docs = match docs with
    | None -> if names = [] then "ARGUMENTS" else "OPTIONS"
    | Some s -> s
    in
    { id = arg_id (); absent = Val (Lazy.from_val "");
      doc = doc; docv = docv; docs = docs;
      p_kind = All; o_kind = Flag; o_names = List.rev_map dash names;
      o_all = false; }

  let flag a =
    if is_pos a then invalid_arg err_not_opt else
    let convert _ cl = match Cmdline.opt_arg cl a with
    | [] -> false
    | [_, _, None] -> true
    | [_, f, Some v] -> parse_error (Err.flag_value f v)
    | (_, f, _) :: (_ ,g, _) :: _  -> parse_error (Err.opt_repeated f g)
    in
    [a], convert

  let flag_all a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with o_all = true } in
    let convert _ cl = match Cmdline.opt_arg cl a with
    | [] -> []
    | l ->
        let truth (_, f, v) = match v with
        | None -> true | Some v -> parse_error (Err.flag_value f v)
  in
  List.rev_map truth l
    in
    [a], convert

  let vflag v l =
    let convert _ cl =
      let rec aux fv = function
      | (v, a) :: rest ->
          begin match Cmdline.opt_arg cl a with
          | [] -> aux fv rest
          | [_, f, None] ->
              begin match fv with
              | None -> aux (Some (f, v)) rest
              | Some (g, _) -> parse_error (Err.opt_repeated g f)
              end
          | [_, f, Some v] -> parse_error (Err.flag_value f v)
          | (_, f, _) :: (_, g, _) :: _ -> parse_error (Err.opt_repeated g f)
          end
      | [] -> match fv with None -> v | Some (_, v) -> v
      in
      aux None l
    in
    let flag (_, a) = if is_pos a then invalid_arg err_not_opt else a in
    List.rev_map flag l, convert

  let vflag_all v l =
    let convert _ cl =
      let rec aux acc = function
      | (fv, a) :: rest ->
          begin match Cmdline.opt_arg cl a with
          | [] -> aux acc rest
          | l ->
              let fval (k, f, v) = match v with
              | None -> (k, fv) | Some v -> parse_error (Err.flag_value f v)
              in
              aux (List.rev_append (List.rev_map fval l) acc) rest
          end
      | [] ->
          if acc = [] then v else List.rev_map snd (List.sort rev_compare acc)
      in
      aux [] l
    in
    let flag (_, a) =
      if is_pos a then invalid_arg err_not_opt else { a with o_all = true }
    in
    List.rev_map flag l, convert

  let parse_opt_value parse f v = match parse v with
  | `Ok v -> v | `Error e -> parse_error (Err.opt_parse_value f e)

  let opt ?vopt (parse, print) v a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with absent = Val (lazy (str_of_pp print v));
                     o_kind = match vopt with
                     | None -> Opt | Some dv -> Opt_vopt (str_of_pp print dv) }
    in
    let convert _ cl = match Cmdline.opt_arg cl a with
    | [] -> v
    | [_, f, Some v] -> parse_opt_value parse f v
    | [_, f, None] ->
        begin match vopt with
        | None -> parse_error (Err.opt_value_missing f)
        | Some optv -> optv
        end
    | (_, f, _) :: (_, g, _) :: _ -> parse_error (Err.opt_repeated g f)
    in
    [a], convert

  let opt_all ?vopt (parse, print) v a =
    if is_pos a then invalid_arg err_not_opt else
    let a = { a with absent = Val (Lazy.from_val ""); o_all = true;
                     o_kind = match vopt with
                     | None -> Opt | Some dv -> Opt_vopt (str_of_pp print dv) }
    in
    let convert _ cl = match Cmdline.opt_arg cl a with
    | [] -> v
    | l ->
        let parse (k, f, v) = match v with
        | Some v -> (k, parse_opt_value parse f v)
        | None -> match vopt with
        | None -> parse_error (Err.opt_value_missing f)
        | Some dv -> (k, dv)
        in
        List.rev_map snd (List.sort rev_compare (List.rev_map parse l))
    in
    [a], convert

  (* Positional arguments *)

  let parse_pos_value parse a v = match parse v with
  | `Ok v -> v | `Error e -> parse_error (Err.pos_parse_value a e)

  let pos ?(rev = false) k (parse, print) v a =
    if is_opt a then invalid_arg err_not_pos else
    let a = { a with p_kind = Nth (rev, k);
                     absent = Val (Lazy.from_val (str_of_pp print v)) }
    in
    let convert _ cl = match Cmdline.pos_arg cl a with
    | [] -> v
    | [v] -> parse_pos_value parse a v
    | _ -> assert false
    in
    [a], convert

  let pos_list kind (parse, _) v a =
    if is_opt a then invalid_arg err_not_pos else
    let a = { a with p_kind = kind } in
    let convert _ cl = match Cmdline.pos_arg cl a with
    | [] -> v
    | l -> List.rev (List.rev_map (parse_pos_value parse a) l)
    in
    [a], convert

  let pos_all c v a = pos_list All c v a
  let pos_left ?(rev = false) k = pos_list (Left (rev, k))
  let pos_right ?(rev = false) k = pos_list (Right (rev, k))

  (* Arguments as terms *)

  let absent_error al = List.rev_map (fun a -> { a with absent = Error }) al
  let value a = a
  let required (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | Some v -> v
    | None -> parse_error (Err.arg_missing (List.hd al))
    in
    al, convert

  let non_empty (al, convert) =
    let al = absent_error al in
    let convert ei cl = match convert ei cl with
    | [] -> parse_error (Err.arg_missing (List.hd al))
    | l -> l
    in
    al, convert

  let last (al, convert) =
    let convert ei cl = match convert ei cl with
    | [] -> parse_error (Err.arg_missing (List.hd al))
    | l -> List.hd (List.rev l)
    in
    al, convert

  (* Predefined converters. *)

  let bool =
    (fun s -> try `Ok (bool_of_string s) with Invalid_argument _ ->
        `Error (Err.invalid_val s (alts_str ["true"; "false"]))),
    Format.pp_print_bool

  let char =
    (fun s -> if String.length s = 1 then `Ok s.[0] else
      `Error (Err.invalid_val s "expected a character")),
    pr_char

  let parse_with t_of_str exp s =
    try `Ok (t_of_str s) with Failure _ -> `Error (Err.invalid_val s exp)

  let int =
    parse_with int_of_string "expected an integer", Format.pp_print_int

  let int32 =
    parse_with Int32.of_string "expected a 32-bit integer",
    (fun ppf -> pr ppf "%ld")

  let int64 =
    parse_with Int64.of_string "expected a 64-bit integer",
    (fun ppf -> pr ppf "%Ld")

  let nativeint =
    parse_with Nativeint.of_string "expected a processor-native integer",
    (fun ppf -> pr ppf "%nd")

  let float =
    parse_with float_of_string "expected a floating point number",
    Format.pp_print_float

  let string = (fun s -> `Ok s), pr_str
  let enum sl =
    if sl = [] then invalid_arg err_empty_list else
    let sl_inv = List.rev_map (fun (s,v) -> (v,s)) sl in
    let print ppf v = pr_str ppf (List.assoc v sl_inv) in
    let t = Trie.of_list sl in
    let parse s = match Trie.find t s with
    | `Ok _ as r -> r
    | `Ambiguous ->
        let ambs = List.sort compare (Trie.ambiguities t s) in
        `Error (Err.ambiguous "enum value" s ambs)
    | `Not_found ->
        let alts = List.rev (List.rev_map (fun (s, _) -> s) sl) in
        `Error (Err.invalid_val s ("expected " ^ (alts_str alts)))
    in
    parse, print

  let file =
    (fun s -> if Sys.file_exists s then `Ok s else
      `Error (Err.no "file or directory" s)),
    pr_str

  let dir =
    (fun s ->
       if Sys.file_exists s then
         if Sys.is_directory s then `Ok s else `Error (Err.not_dir s)
       else
       `Error (Err.no "directory" s)),
    pr_str

  let non_dir_file =
    (fun s ->
       if Sys.file_exists s then
         if not (Sys.is_directory s) then `Ok s else `Error (Err.is_dir s)
       else
       `Error (Err.no "file" s)),
    pr_str

  let split_and_parse sep parse s =
    let parse sub = match parse sub with
    | `Error e -> failwith e | `Ok v -> v in
    let rec split accum j =
      let i = try String.rindex_from s j sep with Not_found -> -1 in
      if (i = -1) then
        let p = String.sub s 0 (j + 1) in
        if p <> "" then parse p :: accum else accum
      else
      let p = String.sub s (i + 1) (j - i) in
      let accum' = if p <> "" then parse p :: accum else accum in
      split accum' (i - 1)
    in
    split [] (String.length s - 1)

  let list ?(sep = ',') (parse, pr_e) =
    let parse s = try `Ok (split_and_parse sep parse s) with
    | Failure e -> `Error (Err.element "list" s e)
    in
    let rec print ppf = function
    | v :: l -> pr_e ppf v; if (l <> []) then (pr_char ppf sep; print ppf l)
    | [] -> ()
    in
    parse, print

  let array ?(sep = ',') (parse, pr_e) =
    let parse s = try `Ok (Array.of_list (split_and_parse sep parse s)) with
    | Failure e -> `Error (Err.element "array" s e)
    in
    let print ppf v =
      let max = Array.length v - 1 in
      for i = 0 to max do pr_e ppf v.(i); if i <> max then pr_char ppf sep done
    in
    parse, print

  let split_left sep s =
    try
      let i = String.index s sep in
      let len = String.length s in
      Some ((String.sub s 0 i), (String.sub s (i + 1) (len - i - 1)))
    with Not_found -> None

  let pair ?(sep = ',') (pa0, pr0) (pa1, pr1) =
    let parser s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some (v0, v1) ->
        match pa0 v0, pa1 v1 with
        | `Ok v0, `Ok v1 -> `Ok (v0, v1)
        | `Error e, _ | _, `Error e -> `Error (Err.element "pair" s e)
    in
    let printer ppf (v0, v1) = pr ppf "%a%c%a" pr0 v0 sep pr1 v1 in
    parser, printer

  let t2 = pair
  let t3 ?(sep = ',') (pa0, pr0) (pa1, pr1) (pa2, pr2) =
    let parse s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some (v0, s) ->
        match split_left sep s with
        | None -> `Error (Err.sep_miss sep s)
        | Some (v1, v2) ->
            match pa0 v0, pa1 v1, pa2 v2 with
            | `Ok v0, `Ok v1, `Ok v2 -> `Ok (v0, v1, v2)
            | `Error e, _, _ | _, `Error e, _ | _, _, `Error e ->
                `Error (Err.element "triple" s e)
    in
    let print ppf (v0, v1, v2) =
      pr ppf "%a%c%a%c%a" pr0 v0 sep pr1 v1 sep pr2 v2
    in
    parse, print

  let t4 ?(sep = ',') (pa0, pr0) (pa1, pr1) (pa2, pr2) (pa3, pr3) =
    let parse s = match split_left sep s with
    | None -> `Error (Err.sep_miss sep s)
    | Some(v0, s) ->
        match split_left sep s with
        | None -> `Error (Err.sep_miss sep s)
        | Some (v1, s) ->
            match split_left sep s with
            | None -> `Error (Err.sep_miss sep s)
            | Some (v2, v3) ->
                match pa0 v0, pa1 v1, pa2 v2, pa3 v3 with
                | `Ok v1, `Ok v2, `Ok v3, `Ok v4 -> `Ok (v1, v2, v3, v4)
                | `Error e, _, _, _ | _, `Error e, _, _ | _, _, `Error e, _
                | _, _, _, `Error e -> `Error (Err.element "quadruple" s e)
    in
    let print ppf (v0, v1, v2, v3) =
      pr ppf "%a%c%a%c%a%c%a" pr0 v0 sep pr1 v1 sep pr2 v2 sep pr3 v3
    in
    parse, print

  (* Documentation formatting helpers *)

  let doc_quote = quote
  let doc_alts = alts_str
  let doc_alts_enum ?quoted enum = alts_str ?quoted (List.map fst enum)
end

module Term = struct
  type info = term_info
  type +'a t = arg_info list * (eval_info -> cmdline -> 'a)
  type 'a result = [
    | `Ok of 'a | `Error of [`Parse | `Term | `Exn ] | `Version | `Help ]

  exception Term of
      [ `Help of [`Pager | `Plain | `Groff] * string option
      | `Error of bool * string ]

  let info  ?(sdocs = "OPTIONS") ?(man = []) ?(docs = "COMMANDS") ?(doc = "")
      ?version name =
    { name = name; version = version; tdoc = doc; tdocs = docs; sdocs = sdocs;
      man = man }

  let name ti = ti.name
  let pure v = [], (fun _ _ -> v)
  let app (al, f) (al', v) =
    List.rev_append al al',
    fun ei cl -> (f ei cl) (v ei cl)

  let ( $ ) = app

  type 'a ret =
    [ `Help of [`Pager | `Plain | `Groff] * string option
    | `Error of (bool * string)
    | `Ok of 'a ]

  let ret (al, v) =
    al, fun ei cl -> match v ei cl with
    | `Ok v -> v
    | `Error (u,e) -> raise (Term (`Error (u,e)))
    | `Help h -> raise (Term (`Help h))

  let main_name = [], (fun ei _ -> (fst ei.main).name)
  let choice_names =
    [], fun ei _ -> List.rev_map (fun e -> (fst e).name) ei.choices

  let man_format =
    let fmts = ["pager", `Pager; "groff", `Groff; "plain", `Plain] in
    let doc = "Show output in format $(docv) (pager, plain or groff)."in
    Arg.(value & opt (enum fmts) `Pager & info ["man-format"] ~docv:"FMT" ~doc)

  (* Evaluation *)

  let remove_exec argv =
    try List.tl (Array.to_list argv) with Failure _ -> invalid_arg err_argv

  let add_std_opts ei =
    let docs = (fst ei.term).sdocs in
    let args, v_lookup =
      if (fst ei.main).version = None then [], None else
      let (a, lookup) =
        Arg.flag (Arg.info ["version"] ~docs ~doc:"Show version information.")
      in
      a, Some lookup
    in
    let args, h_lookup =
      let (a, lookup) =
        let fmt = Arg.enum ["pager",`Pager; "groff",`Groff; "plain",`Plain] in
        let doc = "Show this help in format $(docv) (pager, plain or groff)."in
        let a = Arg.info ["help"] ~docv:"FMT" ~docs ~doc in
        Arg.opt ~vopt:(Some `Pager) (Arg.some fmt) None a
      in
      List.rev_append a args, lookup
    in
    h_lookup, v_lookup,
    { ei with term = (fst ei.term), List.rev_append args (snd ei.term) }

  let eval_term help err ei f args =
    let help_arg, vers_arg, ei = add_std_opts ei in
    try
      let cl = Cmdline.create (snd ei.term) args in
      match help_arg ei cl, vers_arg with
      | Some fmt, _ -> Help.print fmt help ei; `Help
      | None, Some v_arg when v_arg ei cl -> Help.pr_version help ei; `Version
      | _ -> `Ok (f ei cl)
    with
    | Cmdline.Error e -> Err.pr_usage err ei e; `Error `Parse
    | Term (`Error (usage, e)) ->
        if usage then Err.pr_usage err ei e else Err.print err ei e;
        `Error `Term
    | Term (`Help (fmt, cmd)) ->
        let ei = match cmd with
        | Some cmd ->
            let cmd =
              try List.find (fun (i, _) -> i.name = cmd) ei.choices
              with Not_found -> invalid_arg (err_help cmd)
            in
            {ei with term = cmd }
        | None -> { ei with term = ei.main }
        in
        let _, _, ei = add_std_opts ei in
        Help.print fmt help ei; `Help

  let eval ?(help = Format.std_formatter) ?(err = Format.err_formatter)
      ?(catch = true) ?(argv = Sys.argv) ((al, f), ti)  =
    let term = ti, al in
    let ei = { term = term; main = term; choices = [] } in
    try eval_term help err ei f (remove_exec argv) with
    | e when catch ->
        Err.pr_backtrace err ei e (Printexc.get_backtrace ()); `Error `Exn

  let eval_choice ?(help = Format.std_formatter) ?(err = Format.err_formatter)
      ?(catch = true) ?(argv = Sys.argv) (((al, f) as t), ti) choices =
    let ei_choices = List.rev_map (fun ((al, _), ti) -> ti, al) choices in
    let main = (ti, al) in
    let ei = { term = main; main = main; choices = ei_choices } in
    try
      let chosen, args = Cmdline.choose_term ti ei_choices (remove_exec argv) in
      let find_chosen (_, ti) = ti = chosen in
      let (al, f), _ = List.find find_chosen ((t, ti) :: choices) in
      let ei = { ei with term = (chosen, al) } in
      eval_term help err ei f args
    with
    | Cmdline.Error e ->                    (* may be raised by choose_term. *)
        Err.pr_usage err ei e; `Error `Parse
    | e when catch ->
        Err.pr_backtrace err ei e (Printexc.get_backtrace ()); `Error `Exn

  let eval_peek_opts ?(version_opt = false) ?(argv = Sys.argv) (al, f) =
    let args = remove_exec argv in
    let version = if version_opt then Some "dummy" else None in
    let term = info ?version "dummy", al in
    let ei = { term = term; main = term; choices = [] } in
    let help_arg, vers_arg, ei = add_std_opts ei in
    try
      let cl = Cmdline.create ~peek_opts:true (snd ei.term) args in
      match help_arg ei cl, vers_arg with
      | Some fmt, _ ->
          (try (Some (f ei cl), `Help) with e -> None, `Help)
      | None, Some v_arg when v_arg ei cl ->
          (try (Some (f ei cl), `Version) with e -> None, `Version)
      | _ ->
          let v = f ei cl in
          Some v, `Ok v
    with
    | Cmdline.Error _ -> None, (`Error `Parse)
    | Term _ -> None, (`Error `Term)
    | e -> None, (`Error `Exn)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli
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

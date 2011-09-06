(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(*
    @author Louis Gesbert
**)

type 'a t = ('a -> unit) -> unit

module Ops = struct
  let (@>) f k = f k
  let (|>) x k = k x
end
open Ops

module List = struct
  let rec map f l k = match l with
    | [] -> [] |> k
    | hd::tl -> f hd @> fun hd -> map f tl @> fun tl -> hd::tl |> k

  let rec fold f acc l k = match l with
    | [] -> acc |> k
    | hd::tl -> f acc hd @> fun acc -> fold f acc tl @> k
end

module Option = struct
  let rec map f opt k = match opt with
    | None -> None |> k
    | Some x -> f x @> fun x -> Some x |> k
end

module Lazy = struct
  type 'a t = {
    push : (unit -> unit) -> unit;
    mutable value : 'a option;
    mutable waiters : ('a -> unit) list;
    cps : ('a -> unit) -> unit;
  }

  let make push cps = {
    push = push;
    value = None;
    waiters = [];
    cps = cps;
  }

  let force l k =
    match l.value with
    | Some x -> x |> k
    | None when l.waiters != [] ->
        l.waiters <- k::l.waiters
    | None ->
        l.waiters <- k::l.waiters;
        l.cps
        @> function x ->
          Base.List.iter (fun k -> l.push (fun () -> k x)) l.waiters;
          l.value <- Some x;
          l.waiters <- []

  let get_state cps = cps.value

  let lazy_from_val x = {
    push = (fun _ -> ());
    value = Some x;
    waiters = [];
    cps = fun k -> k x;
  }
end

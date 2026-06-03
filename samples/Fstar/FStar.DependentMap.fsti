(*
   Copyright 2008-2018 Microsoft Research

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

module FStar.DependentMap

/// This module provides an abstract type of maps whose co-domain
/// depends on the value of each key. i.e., it is an encapsulation
/// of [x:key -> value x], where [key] supports decidable equality.
///
/// The main constructors of the type are:
///   * [create]: To create the whole map from a function
///   * [upd]: To update a map at a point
///   * [restrict]: To restrict the domain of a map
///   * [concat]: To concatenate maps by taking the union of their key spaces
///   * [rename]: To rename the keys of a map
///   * [map]: To map a function over the values of a map
///
/// The main eliminators are:
///   * [sel]: To query the map for its value at a point
///
/// The interface is specified in a style that describes the action of
/// each eliminator over each of the constructors
///
/// The map also supports an extensional equality principle.

(** Abstract type of dependent maps, with universe polymorphic values
    and keys in universe 0 with decidable equality *)
val t (key: eqtype) (value: (key -> Type u#v)) : Type u#v

(** Creating a new map from a function *)
val create (#key: eqtype) (#value: (key -> Tot Type)) (f: (k: key -> Tot (value k)))
    : Tot (t key value)

(** Querying the map for its value at a given key *)
val sel (#key: eqtype) (#value: (key -> Tot Type)) (m: t key value) (k: key) : Tot (value k)

(** Relating [create] to [sel] *)
val sel_create (#key: eqtype) (#value: (key -> Tot Type)) (f: (k: key -> Tot (value k))) (k: key)
    : Lemma (ensures (sel #key #value (create f) k == f k)) [SMTPat (sel #key #value (create f) k)]

(** Updating a map at a point *)
val upd (#key: eqtype) (#value: (key -> Tot Type)) (m: t key value) (k: key) (v: value k)
    : Tot (t key value)

(** The action of selecting a key [k] a map with an updated value [v]
    at [k]

    This is one of the classic McCarthy select/update axioms in the
    setting of a dependent map.
    *)
val sel_upd_same (#key: eqtype) (#value: (key -> Tot Type)) (m: t key value) (k: key) (v: value k)
    : Lemma (ensures (sel (upd m k v) k == v)) [SMTPat (sel (upd m k v) k)]

(** The action of selecting a key [k] a map with an updated value [v]
    at a different key [k']

    This is one of the classic McCarthy select/update axioms in the
    setting of a dependent map.
    *)
val sel_upd_other
      (#key: eqtype)
      (#value: (key -> Tot Type))
      (m: t key value)
      (k: key)
      (v: value k)
      (k': key)
    : Lemma (requires (k' <> k))
      (ensures (sel (upd m k v) k' == sel m k'))
      [SMTPat (sel (upd m k v) k')]

(** Extensional propositional equality on maps *)
val equal (#key: eqtype) (#value: (key -> Tot Type)) (m1 m2: t key value) : prop

(** Introducing extensional equality by lifting equality on the map, pointwise *)
val equal_intro (#key: eqtype) (#value: (key -> Tot Type)) (m1 m2: t key value)
    : Lemma (requires (forall k. sel m1 k == sel m2 k))
      (ensures (equal m1 m2))
      [SMTPat (equal m1 m2)]

(** [equal] is reflexive *)
val equal_refl (#key: eqtype) (#value: (key -> Tot Type)) (m: t key value)
    : Lemma (ensures (equal m m)) [SMTPat (equal m m)]

(** [equal] can be eliminated into standard propositional equality
    (==), also proving that it is an equivalence relation *)
val equal_elim (#key: eqtype) (#value: (key -> Tot Type)) (m1 m2: t key value)
    : Lemma (requires (equal m1 m2)) (ensures (m1 == m2)) [SMTPat (equal m1 m2)]

(**** Restricting the domain of a map *)

(** Restricts the domain of the map to those keys satisfying [p] *)
val restrict (#key: eqtype) (#value: (key -> Tot Type)) (p: (key -> Tot Type0)) (m: t key value)
    : Tot (t (k: key{p k}) value)

(** The action of [sel] on [restrict] : the contents of the map isn't changed *)
val sel_restrict
      (#key: eqtype)
      (#value: (key -> Tot Type))
      (p: (key -> Tot Type0))
      (m: t key value)
      (k: key{p k})
    : Lemma (ensures (sel (restrict p m) k == sel m k))

(**** Concatenating maps *)

/// Concatenating [t k1 v1] and [t k2 v2] produces a map
/// [t (either k1 k2) (concat_value v1 v2)]
///
/// I.e., the key space varies contravariantly, to take the union of
/// the component key spaces. The co-domain is the dependent product
/// of the co-domains of the original map

(** The key space of a concatenated map is the product of the key spaces *)
let concat_value
      (#key1: eqtype)
      (value1: (key1 -> Tot Type))
      (#key2: eqtype)
      (value2: (key2 -> Tot Type))
      (k: either key1 key2)
    : Tot Type =
  match k with
  | Inl k1 -> value1 k1
  | Inr k2 -> value2 k2

(** Concatenating maps *)
val concat
      (#key1: eqtype)
      (#value1: (key1 -> Tot (Type u#v)))
      (#key2: eqtype)
      (#value2: (key2 -> Tot (Type u#v)))
      (m1: t key1 value1)
      (m2: t key2 value2)
    : Tot (t (either key1 key2) (concat_value value1 value2))

(** The action of [sel] on [concat], for a key on the left picks a
    value from the left map *)
val sel_concat_l
      (#key1: eqtype)
      (#value1: (key1 -> Tot (Type u#v)))
      (#key2: eqtype)
      (#value2: (key2 -> Tot (Type u#v)))
      (m1: t key1 value1)
      (m2: t key2 value2)
      (k1: key1)
    : Lemma (ensures (sel (concat m1 m2) (Inl k1) == sel m1 k1))

(** The action of [sel] on [concat], for a key on the right picks a
    value from the right map *)
val sel_concat_r
      (#key1: eqtype)
      (#value1: (key1 -> Tot Type))
      (#key2: eqtype)
      (#value2: (key2 -> Tot Type))
      (m1: t key1 value1)
      (m2: t key2 value2)
      (k2: key2)
    : Lemma (ensures (sel (concat m1 m2) (Inr k2) == sel m2 k2))

(**** Renamings *)

/// Given a map from [key2] to [key1], we can revise a map from [t
/// key1 v] to a map [t key2 v], by composing the maps.

(** The type of the co-domain of the renamed map also involves
    transformation along the renaming function *)
let rename_value
      (#key1: eqtype)
      (value1: (key1 -> Tot Type))
      (#key2: eqtype)
      (ren: (key2 -> Tot key1))
      (k: key2)
    : Tot Type = value1 (ren k)

(** Renaming the keys of a map *)
val rename
      (#key1: eqtype)
      (#value1: (key1 -> Tot Type))
      (m: t key1 value1)
      (#key2: eqtype)
      (ren: (key2 -> Tot key1))
    : Tot (t key2 (rename_value value1 ren))

(** The action of [sel] on [rename] *)
val sel_rename
      (#key1: eqtype)
      (#value1: (key1 -> Tot Type))
      (m: t key1 value1)
      (#key2: eqtype)
      (ren: (key2 -> Tot key1))
      (k2: key2)
    : Lemma (ensures (sel (rename m ren) k2 == sel m (ren k2)))

(**** Mapping a function over a dependent map *)

(** [map f m] applies f to each value in [m]'s co-domain *)
val map
      (#key: eqtype)
      (#value1 #value2: (key -> Tot Type))
      (f: (k: key -> value1 k -> Tot (value2 k)))
      (m: t key value1)
    : Tot (t key value2)

(** The action of [sel] on [map] *)
val sel_map
      (#key: eqtype)
      (#value1 #value2: (key -> Tot Type))
      (f: (k: key -> value1 k -> Tot (value2 k)))
      (m: t key value1)
      (k: key)
    : Lemma (ensures (sel (map f m) k == f k (sel m k)))
      [SMTPat (sel #key #value2 (map #key #value1 #value2 f m) k)]

(** [map] explained in terms of its action on [upd] *)
val map_upd
      (#key: eqtype)
      (#value1 #value2: (key -> Tot Type))
      (f: (k: key -> value1 k -> Tot (value2 k)))
      (m: t key value1)
      (k: key)
      (v: value1 k)
    : Lemma (ensures (map f (upd m k v) == upd (map f m) k (f k v)))
      [
        //AR: wanted to write an SMTPatOr, but gives some error
        SMTPat (map #key #value1 #value2 f (upd #key #value1 m k v))
      ]


/// We seem to miss lemmas that relate map to the other constructors,
/// including create, restrict etc.

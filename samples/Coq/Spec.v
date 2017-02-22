(** Specifications. *)
Require Import Coq.Lists.List.
Require Import Coq.Strings.Ascii.
Require Import FunctionNinjas.All.
Require Import ListString.All.
Require Import Computation.

Import ListNotations.
Local Open Scope char.

(** A run is an execution of the program with explicit answers for the
    system calls. *)
Module Run.
  (** We define a run by induction on the structure of a computation. *)
  Inductive t : C.t -> Type :=
  | Ret : t C.Ret
  | Call : forall (command : Command.t) (answer : Command.answer command)
    {handler : Command.answer command -> C.t}, t (handler answer) ->
    t (C.Call command handler).

  (** The trace of a run. *)
  Fixpoint trace {x : C.t} (run : t x)
    : list {command : Command.t & Command.answer command} :=
    match run with
    | Ret => []
    | Call command answer _ run => existT _ command answer :: trace run
    end.
End Run.

Module Temporal.
  Module All.
    Inductive t (P : Command.t -> Prop) : C.t -> Prop :=
    | Ret : t P C.Ret
    | Call : forall (c : Command.t) (h : Command.answer c -> C.t),
      P c -> (forall a, t P (h a)) ->
      t P (C.Call c h).
  End All.

  Module One.
    Inductive t (P : Command.t -> Prop) : C.t -> Prop :=
    | CallThis : forall (c : Command.t) (h : Command.answer c -> C.t),
      P c ->
      t P (C.Call c h)
    | CallOther : forall (c : Command.t) (h : Command.answer c -> C.t),
      (forall a, t P (h a)) ->
      t P (C.Call c h).
  End One.

  Module Then.
    Inductive t (P1 P2 : Command.t -> Prop) : C.t -> Prop :=
    | Ret : t P1 P2 C.Ret
    | Call : forall (c : Command.t) (h : Command.answer c -> C.t),
      (forall a, t P1 P2 (h a)) ->
      t P1 P2 (C.Call c h)
    | CallThen : forall (c : Command.t) (h : Command.answer c -> C.t),
      P1 c -> (forall a, One.t P2 (h a)) ->
      t P1 P2 (C.Call c h).
  End Then.
End Temporal.

Module CardBeforeMoney.
End CardBeforeMoney.

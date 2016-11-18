Require Import Omega Relations Multiset SetoidList.

(** This file is deprecated, use [Permutation.v] instead.

    Indeed, this file defines a notion of permutation based on
    multisets (there exists a permutation between two lists iff every
    elements have the same multiplicity in the two lists) which
    requires a more complex apparatus (the equipment of the domain
    with a decidable equality) than [Permutation] in [Permutation.v].

    The relation between the two relations are in lemma
    [permutation_Permutation].

    File [Permutation] concerns Leibniz equality : it shows in particular
    that [List.Permutation] and [permutation] are equivalent in this context.
*)

Set Implicit Arguments.

Local Notation "[ ]" := nil.
Local Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..).

Section Permut.

(** * From lists to multisets *)

Variable A : Type.
Variable eqA : relation A.
Hypothesis eqA_equiv : Equivalence eqA.
Hypothesis eqA_dec : forall x y:A, {eqA x y} + {~ eqA x y}.

Let emptyBag := EmptyBag A.
Let singletonBag := SingletonBag _ eqA_dec.

(** contents of a list *)

Fixpoint list_contents (l:list A) : multiset A :=
  match l with
  | [] => emptyBag
  | a :: l => munion (singletonBag a) (list_contents l)
  end.

Lemma list_contents_app :
  forall l m:list A,
    meq (list_contents (l ++ m)) (munion (list_contents l) (list_contents m)).
Proof.
  simple induction l; simpl; auto with datatypes.
  intros.
  apply meq_trans with
    (munion (singletonBag a) (munion (list_contents l0) (list_contents m)));
    auto with datatypes.
Qed.

(** * [permutation]: definition and basic properties *)

Definition permutation (l m:list A) := meq (list_contents l) (list_contents m).

Lemma permut_refl : forall l:list A, permutation l l.
Proof.
  unfold permutation; auto with datatypes.
Qed.

Lemma permut_sym :
  forall l1 l2 : list A, permutation l1 l2 -> permutation l2 l1.
Proof.
  unfold permutation, meq; intros; symmetry; trivial.
Qed.

Lemma permut_trans :
  forall l m n:list A, permutation l m -> permutation m n -> permutation l n.
Proof.
  unfold permutation; intros.
  apply meq_trans with (list_contents m); auto with datatypes.
Qed.

Lemma permut_cons_eq :
  forall l m:list A,
    permutation l m -> forall a a', eqA a a' -> permutation (a :: l) (a' :: m).
Proof.
  unfold permutation; simpl; intros.
  apply meq_trans with (munion (singletonBag a') (list_contents l)).
  apply meq_left, meq_singleton; auto.
  auto with datatypes.
Qed.

Lemma permut_cons :
  forall l m:list A,
    permutation l m -> forall a:A, permutation (a :: l) (a :: m).
Proof.
  unfold permutation; simpl; auto with datatypes.
Qed.

Lemma permut_app :
  forall l l' m m':list A,
    permutation l l' -> permutation m m' -> permutation (l ++ m) (l' ++ m').
Proof.
  unfold permutation; intros.
  apply meq_trans with (munion (list_contents l) (list_contents m));
    auto using permut_cons, list_contents_app with datatypes.
  apply meq_trans with (munion (list_contents l') (list_contents m'));
    auto using permut_cons, list_contents_app with datatypes.
  apply meq_trans with (munion (list_contents l') (list_contents m));
    auto using permut_cons, list_contents_app with datatypes.
Qed.

Lemma permut_add_inside_eq :
  forall a a' l1 l2 l3 l4, eqA a a' ->
    permutation (l1 ++ l2) (l3 ++ l4) ->
    permutation (l1 ++ a :: l2) (l3 ++ a' :: l4).
Proof.
  unfold permutation, meq in *; intros.
  specialize H0 with a0.
  repeat rewrite list_contents_app in *; simpl in *.
  destruct (eqA_dec a a0) as [Ha|Ha]; rewrite H in Ha;
    decide (eqA_dec a' a0) with Ha; simpl; auto with arith.
  do 2 rewrite <- plus_n_Sm; f_equal; auto.
Qed.

Lemma permut_add_inside :
  forall a l1 l2 l3 l4,
    permutation (l1 ++ l2) (l3 ++ l4) ->
    permutation (l1 ++ a :: l2) (l3 ++ a :: l4).
Proof.
  unfold permutation, meq in *; intros.
  generalize (H a0); clear H.
  do 4 rewrite list_contents_app.
  simpl.
  destruct (eqA_dec a a0); simpl; auto with arith.
  do 2 rewrite <- plus_n_Sm; f_equal; auto.
Qed.

Lemma permut_add_cons_inside_eq :
  forall a a' l l1 l2, eqA a a' ->
    permutation l (l1 ++ l2) ->
    permutation (a :: l) (l1 ++ a' :: l2).
Proof.
  intros;
  replace (a :: l) with ([] ++ a :: l); trivial;
    apply permut_add_inside_eq; trivial.
Qed.

Lemma permut_add_cons_inside :
  forall a l l1 l2,
    permutation l (l1 ++ l2) ->
    permutation (a :: l) (l1 ++ a :: l2).
Proof.
  intros;
    replace (a :: l) with ([] ++ a :: l); trivial;
        apply permut_add_inside; trivial.
Qed.

Lemma permut_middle :
  forall (l m:list A) (a:A), permutation (a :: l ++ m) (l ++ a :: m).
Proof.
  intros; apply permut_add_cons_inside; auto using permut_sym, permut_refl.
Qed.

Lemma permut_sym_app :
  forall l1 l2, permutation (l1 ++ l2) (l2 ++ l1).
Proof.
  intros l1 l2;
    unfold permutation, meq;
        intro a; do 2 rewrite list_contents_app; simpl;
          auto with arith.
Qed.

Lemma permut_rev :
  forall l, permutation l (rev l).
Proof.
  induction l.
  simpl; trivial using permut_refl.
  simpl.
  apply permut_add_cons_inside.
  rewrite <- app_nil_end. trivial.
Qed.

(** * Some inversion results. *)
Lemma permut_conv_inv :
  forall e l1 l2, permutation (e :: l1) (e :: l2) -> permutation l1 l2.
Proof.
  intros e l1 l2; unfold permutation, meq; simpl; intros H a;
    generalize (H a); apply plus_reg_l.
Qed.

Lemma permut_app_inv1 :
  forall l l1 l2, permutation (l1 ++ l) (l2 ++ l) -> permutation l1 l2.
Proof.
  intros l l1 l2; unfold permutation, meq; simpl;
    intros H a; generalize (H a); clear H.
  do 2 rewrite list_contents_app.
  simpl.
  intros; apply plus_reg_l with (multiplicity (list_contents l) a).
  rewrite plus_comm; rewrite H; rewrite plus_comm.
  trivial.
Qed.

(** we can use [multiplicity] to define [InA] and [NoDupA]. *)

Fact if_eqA_then : forall a a' (B:Type)(b b':B),
 eqA a a' -> (if eqA_dec a a' then b else b') = b.
Proof.
  intros. destruct eqA_dec as [_|NEQ]; auto.
  contradict NEQ; auto.
Qed.

Lemma permut_app_inv2 :
  forall l l1 l2, permutation (l ++ l1) (l ++ l2) -> permutation l1 l2.
Proof.
  intros l l1 l2; unfold permutation, meq; simpl;
    intros H a; generalize (H a); clear H.
  do 2 rewrite list_contents_app.
  simpl.
  intros; apply plus_reg_l with (multiplicity (list_contents l) a).
  trivial.
Qed.

Lemma permut_remove_hd_eq :
  forall l l1 l2 a b, eqA a b ->
    permutation (a :: l) (l1 ++ b :: l2) -> permutation l (l1 ++ l2).
Proof.
  unfold permutation, meq; simpl; intros l l1 l2 a b Heq H a0.
  specialize H with a0.
  rewrite list_contents_app in *; simpl in *.
  apply plus_reg_l with (if eqA_dec a a0 then 1 else 0).
  rewrite H; clear H.
  symmetry; rewrite plus_comm, <- ! plus_assoc; f_equal.
  rewrite plus_comm.
  destruct (eqA_dec a a0) as [Ha|Ha]; rewrite Heq in Ha;
    decide (eqA_dec b a0) with Ha; reflexivity.
Qed.

Lemma permut_remove_hd :
  forall l l1 l2 a,
    permutation (a :: l) (l1 ++ a :: l2) -> permutation l (l1 ++ l2).
Proof.
  eauto using permut_remove_hd_eq, Equivalence_Reflexive.
Qed.

Fact if_eqA_else : forall a a' (B:Type)(b b':B),
 ~eqA a a' -> (if eqA_dec a a' then b else b') = b'.
Proof.
  intros. decide (eqA_dec a a') with H; auto.
Qed.

Fact if_eqA_refl : forall a (B:Type)(b b':B),
 (if eqA_dec a a then b else b') = b.
Proof.
  intros; apply (decide_left (eqA_dec a a)); auto with *.
Qed.

(** PL: Inutilisable dans un rewrite sans un change prealable. *)

Global Instance if_eqA (B:Type)(b b':B) :
 Proper (eqA==>eqA==>@eq _) (fun x y => if eqA_dec x y then b else b').
Proof.
 intros x x' Hxx' y y' Hyy'.
 intros; destruct (eqA_dec x y) as [H|H];
  destruct (eqA_dec x' y') as [H'|H']; auto.
 contradict H'; transitivity x; auto with *; transitivity y; auto with *.
 contradict H; transitivity x'; auto with *; transitivity y'; auto with *.
Qed.

Fact if_eqA_rewrite_l : forall a1 a1' a2 (B:Type)(b b':B),
 eqA a1 a1' -> (if eqA_dec a1 a2 then b else b') =
               (if eqA_dec a1' a2 then b else b').
Proof.
 intros; destruct (eqA_dec a1 a2) as [A1|A1];
  destruct (eqA_dec a1' a2) as [A1'|A1']; auto.
 contradict A1'; transitivity a1; eauto with *.
 contradict A1; transitivity a1'; eauto with *.
Qed.

Fact if_eqA_rewrite_r : forall a1 a2 a2' (B:Type)(b b':B),
 eqA a2 a2' -> (if eqA_dec a1 a2 then b else b') =
               (if eqA_dec a1 a2' then b else b').
Proof.
 intros; destruct (eqA_dec a1 a2) as [A2|A2];
  destruct (eqA_dec a1 a2') as [A2'|A2']; auto.
 contradict A2'; transitivity a2; eauto with *.
 contradict A2; transitivity a2'; eauto with *.
Qed.


Global Instance multiplicity_eqA (l:list A) :
 Proper (eqA==>@eq _) (multiplicity (list_contents l)).
Proof.
  intros x x' Hxx'.
  induction l as [|y l Hl]; simpl; auto.
  rewrite (@if_eqA_rewrite_r y x x'); auto.
Qed.

Lemma multiplicity_InA :
  forall l a, InA eqA a l <-> 0 < multiplicity (list_contents l) a.
Proof.
  induction l.
  simpl.
  split; inversion 1.
  simpl.
  intros a'; split; intros H. inversion_clear H.
  apply (decide_left (eqA_dec a a')); auto with *.
  destruct (eqA_dec a a'); auto with *. simpl; rewrite <- IHl; auto.
  destruct (eqA_dec a a'); auto with *. right. rewrite IHl; auto.
Qed.

Lemma multiplicity_InA_O :
  forall l a, ~ InA eqA a l -> multiplicity (list_contents l) a = 0.
Proof.
  intros l a; rewrite multiplicity_InA;
    destruct (multiplicity (list_contents l) a); auto with arith.
  destruct 1; auto with arith.
Qed.

Lemma multiplicity_InA_S :
  forall l a, InA eqA a l -> multiplicity (list_contents l) a >= 1.
Proof.
  intros l a; rewrite multiplicity_InA; auto with arith.
Qed.

Lemma multiplicity_NoDupA : forall l,
  NoDupA eqA l <-> (forall a, multiplicity (list_contents l) a <= 1).
Proof.
  induction l.
  simpl.
  split; auto with arith.
  split; simpl.
  inversion_clear 1.
  rewrite IHl in H1.
  intros; destruct (eqA_dec a a0) as [EQ|NEQ]; simpl; auto with *.
  rewrite <- EQ.
  rewrite multiplicity_InA_O; auto.
  intros; constructor.
  rewrite multiplicity_InA.
  specialize (H a).
  rewrite if_eqA_refl in H.
  clear IHl; omega.
  rewrite IHl; intros.
  specialize (H a0). omega.
Qed.

(** Permutation is compatible with InA. *)
Lemma permut_InA_InA :
  forall l1 l2 e, permutation l1 l2 -> InA eqA e l1 -> InA eqA e l2.
Proof.
  intros l1 l2 e.
  do 2 rewrite multiplicity_InA.
  unfold permutation, meq.
  intros H;rewrite H; auto.
Qed.

Lemma permut_cons_InA :
  forall l1 l2 e, permutation (e :: l1) l2 -> InA eqA e l2.
Proof.
  intros; apply (permut_InA_InA (e:=e) H); auto with *.
Qed.

(** Permutation of an empty list. *)
Lemma permut_nil :
  forall l, permutation l [] -> l = [].
Proof.
  intro l; destruct l as [ | e l ]; trivial.
  assert (InA eqA e (e::l)) by (auto with *).
  intro Abs; generalize (permut_InA_InA Abs H).
  inversion 1.
Qed.

(** Permutation for short lists. *)

Lemma permut_length_1:
  forall a b, permutation [a] [b] -> eqA a b.
Proof.
  intros a b; unfold permutation, meq.
  intro P; specialize (P b); simpl in *.
  rewrite if_eqA_refl in *.
  destruct (eqA_dec a b); simpl; auto; discriminate.
Qed.

Lemma permut_length_2 :
  forall a1 b1 a2 b2, permutation [a1; b1] [a2; b2] ->
    (eqA a1 a2) /\ (eqA b1 b2) \/ (eqA a1 b2) /\ (eqA a2 b1).
Proof.
  intros a1 b1 a2 b2 P.
  assert (H:=permut_cons_InA P).
  inversion_clear H.
  left; split; auto.
  apply permut_length_1.
  red; red; intros.
  specialize (P a). simpl in *.
  rewrite (@if_eqA_rewrite_l a1 a2 a) in P by auto. omega.
  right.
  inversion_clear H0; [|inversion H].
  split; auto.
  apply permut_length_1.
  red; red; intros.
  specialize (P a); simpl in *.
  rewrite (@if_eqA_rewrite_l a1 b2 a) in P by auto. omega.
Qed.

(** Permutation is compatible with length. *)
Lemma permut_length :
  forall l1 l2, permutation l1 l2 -> length l1 = length l2.
Proof.
  induction l1; intros l2 H.
  rewrite (permut_nil (permut_sym H)); auto.
  assert (H0:=permut_cons_InA H).
  destruct (InA_split H0) as (h2,(b,(t2,(H1,H2)))).
  subst l2.
  rewrite app_length.
  simpl; rewrite <- plus_n_Sm; f_equal.
  rewrite <- app_length.
  apply IHl1.
  apply permut_remove_hd with b.
  apply permut_trans with (a::l1); auto.
  revert H1; unfold permutation, meq; simpl.
  intros; f_equal; auto.
  rewrite (@if_eqA_rewrite_l a b a0); auto.
Qed.

Lemma NoDupA_equivlistA_permut :
  forall l l', NoDupA eqA l -> NoDupA eqA l' ->
    equivlistA eqA l l' -> permutation l l'.
Proof.
  intros.
  red; unfold meq; intros.
  rewrite multiplicity_NoDupA in H, H0.
  generalize (H a) (H0 a) (H1 a); clear H H0 H1.
  do 2 rewrite multiplicity_InA.
  destruct 3; omega.
Qed.

End Permut.

Section Permut_map.

Variables A B : Type.

Variable eqA : relation A.
Hypothesis eqA_dec : forall x y:A, {eqA x y} + {~ eqA x y}.
Hypothesis eqA_equiv : Equivalence eqA.

Variable eqB : B->B->Prop.
Hypothesis eqB_dec : forall x y:B, { eqB x y }+{ ~eqB x y }.
Hypothesis eqB_trans : Transitive eqB.

(** Permutation is compatible with map. *)

Lemma permut_map :
  forall f,
    (Proper (eqA==>eqB) f) ->
    forall l1 l2, permutation _ eqA_dec l1 l2 ->
      permutation _ eqB_dec (map f l1) (map f l2).
Proof.
  intros f; induction l1.
  intros l2 P; rewrite (permut_nil eqA_equiv (permut_sym P)); apply permut_refl.
  intros l2 P.
  simpl.
  assert (H0:=permut_cons_InA eqA_equiv P).
  destruct (InA_split H0) as (h2,(b,(t2,(H1,H2)))).
  subst l2.
  rewrite map_app.
  simpl.
  apply permut_trans with (f b :: map f l1).
  revert H1; unfold permutation, meq; simpl.
  intros; f_equal; auto.
  destruct (eqB_dec (f b) a0) as [H2|H2];
    destruct (eqB_dec (f a) a0) as [H3|H3]; auto.
  destruct H3; transitivity (f b); auto with *.
  destruct H2; transitivity (f a); auto with *.
  apply permut_add_cons_inside.
  rewrite <- map_app.
  apply IHl1; auto.
  apply permut_remove_hd with b; trivial.
  apply permut_trans with (a::l1); auto.
  revert H1; unfold permutation, meq; simpl.
  intros; f_equal; auto.
  rewrite (@if_eqA_rewrite_l _ _ eqA_equiv eqA_dec a b a0); auto.
Qed.

End Permut_map.

Require Import Permutation.

Section Permut_permut.

Variable A : Type.

Variable eqA : relation A.
Hypothesis eqA_dec : forall x y:A, {eqA x y} + {~ eqA x y}.
Hypothesis eqA_equiv : Equivalence eqA.

Lemma Permutation_impl_permutation : forall l l',
  Permutation l l' -> permutation _ eqA_dec l l'.
Proof.
  induction 1.
    apply permut_refl.
    apply permut_cons; auto using Equivalence_Reflexive.
    change (x :: y :: l) with ([x] ++ y :: l);
      apply permut_add_cons_inside; simpl;
      apply permut_cons_eq; auto using Equivalence_Reflexive, permut_refl.
    apply permut_trans with l'; trivial.
Qed.

Lemma permut_eqA : forall l l', Forall2 eqA l l' -> permutation _ eqA_dec l l'.
Proof.
  induction 1.
    apply permut_refl.
    apply permut_cons_eq; trivial.
Qed.

Lemma permutation_Permutation : forall l l',
  permutation _ eqA_dec l l' <->
  exists l'', Permutation l l'' /\ Forall2 eqA l'' l'.
Proof.
  split; intro H.
  (* -> *)
  induction l in l', H |- *.
    exists []; apply permut_sym, permut_nil in H as ->; auto using Forall2.
    pose proof H as H'.
    apply permut_cons_InA, InA_split in H
      as (l1 & y & l2 & Heq & ->); trivial.
    apply permut_remove_hd_eq, IHl in H'
      as (l'' & IHP & IHA); clear IHl; trivial.
    apply Forall2_app_inv_r in IHA as (l1'' & l2'' & Hl1 & Hl2 & ->).
    exists (l1'' ++ a :: l2''); split.
      apply Permutation_cons_app; trivial.
      apply Forall2_app, Forall2_cons; trivial.
  (* <- *)
  destruct H as (l'' & H & Heq).
  apply permut_trans with l''.
    apply Permutation_impl_permutation; trivial.
    apply permut_eqA; trivial.
Qed.

End Permut_permut.

(* begin hide *)
(** For compatibilty *)
Notation permut_right := permut_cons (only parsing).
Notation permut_tran := permut_trans (only parsing).
(* end hide *)

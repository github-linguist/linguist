(** Sketch of the proof of {p:nat|p<=n} = {p:nat|p<=m} -> n=m

  - preliminary results on the irrelevance of boundedness proofs
  - introduce the notion of finite cardinal |A|
  - prove that |{p:nat|p<=n}| = n
  - prove that |A| = n /\ |A| = m -> n = m if equality is decidable on A
  - prove that equality is decidable on A
  - conclude
*)

(** * Preliminary results on [nat] and [le] *)

(** Proving axiom K on [nat] *)

Require Import Eqdep_dec.
Require Import Arith.

Theorem eq_rect_eq_nat :
  forall (p:nat) (Q:nat->Type) (x:Q p) (h:p=p), x = eq_rect p Q x p h.
Proof.
intros.
apply K_dec_set with (p := h).
apply eq_nat_dec.
reflexivity.
Qed.

(** Proving unicity of proofs of [(n<=m)%nat] *)

Scheme le_ind' := Induction for le Sort Prop.

Theorem le_uniqueness_proof : forall (n m : nat) (p q : n <= m), p = q.
Proof.
induction p using le_ind'; intro q.
 replace (le_n n) with
  (eq_rect _ (fun n0 => n <= n0) (le_n n) _ (refl_equal n)).
 2:reflexivity.
  generalize (refl_equal n).
    pattern n at 2 4 6 10, q; case q; [intro | intros m l e].
     rewrite <- eq_rect_eq_nat; trivial.
     contradiction (le_Sn_n m); rewrite <- e; assumption.
 replace (le_S n m p) with
  (eq_rect _ (fun n0 => n <= n0) (le_S n m p) _ (refl_equal (S m))).
 2:reflexivity.
  generalize (refl_equal (S m)).
    pattern (S m) at 1 3 4 6, q; case q; [intro Heq | intros m0 l HeqS].
     contradiction (le_Sn_n m); rewrite Heq; assumption.
     injection HeqS; intro Heq; generalize l HeqS.
      rewrite <- Heq; intros; rewrite <- eq_rect_eq_nat.
      rewrite (IHp l0); reflexivity.
Qed.

(** Proving irrelevance of boundedness proofs while building
    elements of interval *)

Lemma dep_pair_intro :
  forall (n x y:nat) (Hx : x<=n) (Hy : y<=n), x=y ->
    exist (fun x => x <= n) x Hx = exist (fun x => x <= n) y Hy.
Proof.
intros n x y Hx Hy Heq.
generalize Hy.
rewrite <- Heq.
intros.
rewrite (le_uniqueness_proof x n Hx Hy0).
reflexivity.
Qed.

(** * Proving that {p:nat|p<=n} = {p:nat|p<=m} -> n=m *)

(** Definition of having finite cardinality [n+1] for a set [A] *)

Definition card (A:Set) n :=
  exists f,
    (forall x:A, f x <= n) /\
    (forall x y:A, f x = f y -> x = y) /\
    (forall m, m <= n -> exists x:A, f x = m).

Require Import Arith.

(** Showing that the interval [0;n] has cardinality [n+1] *)

Theorem card_interval : forall n, card {x:nat|x<=n} n.
Proof.
intro n.
exists (fun x:{x:nat|x<=n} => proj1_sig x).
split.
(* bounded *)
intro x; apply (proj2_sig x).
split.
(* injectivity *)
intros (p,Hp) (q,Hq).
simpl.
intro Hpq.
apply dep_pair_intro; assumption.
(* surjectivity *)
intros m Hmn.
exists (exist (fun x : nat => x <= n) m Hmn).
reflexivity.
Qed.

(** Showing that equality on the interval [0;n] is decidable *)

Lemma interval_dec :
  forall n (x y : {m:nat|m<=n}), {x=y}+{x<>y}.
Proof.
intros n (p,Hp).
induction p; intros ([|q],Hq).
left.
  apply dep_pair_intro.
  reflexivity.
right.
  intro H; discriminate H.
right.
  intro H; discriminate H.
assert (Hp' : p <= n).
  apply le_Sn_le; assumption.
assert (Hq' : q <= n).
  apply le_Sn_le; assumption.
destruct (IHp Hp' (exist (fun m => m <= n) q Hq'))
  as [Heq|Hneq].
left.
  injection Heq; intro Heq'.
  apply dep_pair_intro.
  apply eq_S.
  assumption.
right.
  intro HeqS.
  injection HeqS; intro Heq.
  apply Hneq.
  apply dep_pair_intro.
  assumption.
Qed.

(** Showing that the cardinality relation is functional on decidable sets *)

Lemma card_inj_aux :
  forall (A:Type) f g n,
    (forall x:A, f x <= 0) ->
    (forall x y:A, f x = f y -> x = y) ->
    (forall m, m <= S n -> exists x:A, g x = m)
     -> False.
Proof.
intros A f g n Hfbound Hfinj Hgsurj.
destruct (Hgsurj (S n) (le_n _)) as (x,Hx).
destruct (Hgsurj n (le_S _ _ (le_n _))) as (x',Hx').
assert (Hfx : 0 = f x).
apply le_n_O_eq.
apply Hfbound.
assert (Hfx' : 0 = f x').
apply le_n_O_eq.
apply Hfbound.
assert (x=x').
apply Hfinj.
rewrite <- Hfx.
rewrite <- Hfx'.
reflexivity.
rewrite H in Hx.
rewrite Hx' in Hx.
apply (n_Sn _ Hx).
Qed.

(** For [dec_restrict], we use a lemma on the negation of equality
that requires proof-irrelevance. It should be possible to avoid this
lemma by generalizing over a first-order definition of [x<>y], say
[neq] such that [{x=y}+{neq x y}] and [~(x=y /\ neq x y)]; for such
[neq], unicity of proofs could be proven *)

  Require Import Classical.
  Lemma neq_dep_intro :
   forall (A:Set) (z x y:A) (p:x<>z) (q:y<>z), x=y ->
      exist (fun x => x <> z) x p = exist (fun x => x <> z) y q.
  Proof.
  intros A z x y p q Heq.
   generalize q; clear q; rewrite <- Heq; intro q.
   rewrite (proof_irrelevance _ p q); reflexivity.
  Qed.

Lemma dec_restrict :
  forall (A:Set),
    (forall x y :A, {x=y}+{x<>y}) ->
     forall z (x y :{a:A|a<>z}), {x=y}+{x<>y}.
Proof.
intros A Hdec z (x,Hx) (y,Hy).
destruct (Hdec x y) as [Heq|Hneq].
left; apply neq_dep_intro; assumption.
right; intro Heq; injection Heq; exact Hneq.
Qed.

Lemma pred_inj : forall n m,
  0 <> n -> 0 <> m -> pred m = pred n -> m = n.
Proof.
destruct n.
intros m H; destruct H; reflexivity.
destruct m.
intros _ H; destruct H; reflexivity.
simpl; intros _ _ H.
rewrite H.
reflexivity.
Qed.

Lemma le_neq_lt : forall n m, n <= m -> n<>m -> n < m.
Proof.
intros n m Hle Hneq.
destruct (le_lt_eq_dec n m Hle).
assumption.
contradiction.
Qed.

Lemma inj_restrict :
  forall (A:Set) (f:A->nat) x y z,
    (forall x y : A, f x = f y -> x = y)
    -> x <> z -> f y < f z -> f z <= f x
    -> pred (f x) = f y
    -> False.

(* Search error sans le type de f !! *)
Proof.
intros A f x y z Hfinj Hneqx Hfy Hfx Heq.
assert (f z <> f x).
  apply sym_not_eq.
  intro Heqf.
  apply Hneqx.
  apply Hfinj.
  assumption.
assert (f x = S (f y)).
  assert (0 < f x).
    apply le_lt_trans with (f z).
    apply le_O_n.
    apply le_neq_lt; assumption.
  apply pred_inj.
  apply O_S.
  apply lt_O_neq; assumption.
  exact Heq.
assert (f z <= f y).
destruct (le_lt_or_eq _ _ Hfx).
  apply lt_n_Sm_le.
  rewrite <- H0.
  assumption.
  contradiction Hneqx.
  symmetry.
  apply Hfinj.
  assumption.
contradiction (lt_not_le (f y) (f z)).
Qed.

Theorem card_inj : forall m n (A:Set),
  (forall x y :A, {x=y}+{x<>y}) ->
  card A m -> card A n -> m = n.
Proof.
induction m; destruct n;
intros A Hdec
 (f,(Hfbound,(Hfinj,Hfsurj)))
 (g,(Hgbound,(Hginj,Hgsurj))).
(* 0/0 *)
reflexivity.
(* 0/Sm *)
destruct (card_inj_aux _ _ _ _ Hfbound Hfinj Hgsurj).
(* Sn/0 *)
destruct (card_inj_aux _ _ _ _ Hgbound Hginj Hfsurj).
(* Sn/Sm *)
destruct (Hgsurj (S n) (le_n _)) as (xSn,HSnx).
rewrite IHm with (n:=n) (A := {x:A|x<>xSn}).
reflexivity.
(* decidability of eq on {x:A|x<>xSm} *)
apply dec_restrict.
assumption.
(* cardinality of {x:A|x<>xSn} is m *)
pose (f' := fun x' : {x:A|x<>xSn} =>
    let (x,Hneq) := x' in
    if le_lt_dec (f xSn) (f x)
    then pred (f x)
    else f x).
exists f'.
split.
(* f' is bounded *)
unfold f'.
intros (x,_).
destruct (le_lt_dec (f xSn) (f x)) as [Hle|Hge].
change m with (pred (S m)).
apply le_pred.
apply Hfbound.
apply le_S_n.
apply le_trans with (f xSn).
exact Hge.
apply Hfbound.
split.
(* f' is injective *)
unfold f'.
intros (x,Hneqx) (y,Hneqy) Heqf'.
destruct (le_lt_dec (f xSn) (f x)) as [Hlefx|Hgefx];
destruct (le_lt_dec (f xSn) (f y)) as [Hlefy|Hgefy].
(* f xSn <= f x et f xSn <= f y *)
assert (Heq : x = y).
  apply Hfinj.
  assert (f xSn <> f y).
    apply sym_not_eq.
    intro Heqf.
    apply Hneqy.
    apply Hfinj.
    assumption.
  assert (0 < f y).
    apply le_lt_trans with (f xSn).
    apply le_O_n.
    apply le_neq_lt; assumption.
  assert (f xSn <> f x).
    apply sym_not_eq.
    intro Heqf.
    apply Hneqx.
    apply Hfinj.
    assumption.
  assert (0 < f x).
    apply le_lt_trans with (f xSn).
    apply le_O_n.
    apply le_neq_lt; assumption.
  apply pred_inj.
  apply lt_O_neq; assumption.
  apply lt_O_neq; assumption.
  assumption.
apply neq_dep_intro; assumption.
(* f y < f xSn <= f x *)
destruct (inj_restrict A f x y xSn); assumption.
(* f x < f xSn <= f y *)
symmetry in Heqf'.
destruct (inj_restrict A f y x xSn); assumption.
(* f x < f xSn et f y < f xSn *)
assert (Heq : x=y).
  apply Hfinj; assumption.
apply neq_dep_intro; assumption.
(* f' is surjective *)
intros p Hlep.
destruct (le_lt_dec (f xSn) p) as [Hle|Hlt].
(* case f xSn <= p *)
destruct (Hfsurj (S p) (le_n_S _ _ Hlep)) as (x,Hx).
assert (Hneq : x <> xSn).
  intro Heqx.
  rewrite Heqx in Hx.
  rewrite Hx in Hle.
  apply le_Sn_n with p; assumption.
exists (exist (fun a => a<>xSn) x Hneq).
unfold f'.
destruct (le_lt_dec (f xSn) (f x)) as [Hle'|Hlt'].
rewrite Hx; reflexivity.
rewrite Hx in Hlt'.
contradiction (le_not_lt (f xSn) p).
apply lt_trans with (S p).
apply lt_n_Sn.
assumption.
(* case p < f xSn *)
destruct (Hfsurj p (le_S _ _ Hlep)) as (x,Hx).
assert (Hneq : x <> xSn).
  intro Heqx.
  rewrite Heqx in Hx.
  rewrite Hx in Hlt.
  apply (lt_irrefl p).
  assumption.
exists (exist (fun a => a<>xSn) x Hneq).
unfold f'.
destruct (le_lt_dec (f xSn) (f x)) as [Hle'|Hlt'].
  rewrite Hx in Hle'.
  contradiction (lt_irrefl p).
  apply lt_le_trans with (f xSn); assumption.
  assumption.
(* cardinality of {x:A|x<>xSn} is n *)
pose (g' := fun x' : {x:A|x<>xSn} =>
   let (x,Hneq) := x' in
   if Hdec x xSn then 0 else g x).
exists g'.
split.
(* g is bounded *)
unfold g'.
intros (x,_).
destruct (Hdec x xSn) as [_|Hneq].
apply le_O_n.
assert (Hle_gx:=Hgbound x).
destruct (le_lt_or_eq _ _ Hle_gx).
apply lt_n_Sm_le.
assumption.
contradiction Hneq.
apply Hginj.
rewrite HSnx.
assumption.
split.
(* g is injective *)
unfold g'.
intros (x,Hneqx) (y,Hneqy) Heqg'.
destruct (Hdec x xSn) as [Heqx|_].
contradiction Hneqx.
destruct (Hdec y xSn) as [Heqy|_].
contradiction Hneqy.
assert (Heq : x=y).
  apply Hginj; assumption.
apply neq_dep_intro; assumption.
(* g is surjective *)
intros p Hlep.
destruct (Hgsurj p (le_S _ _ Hlep)) as (x,Hx).
assert (Hneq : x<>xSn).
  intro Heq.
  rewrite Heq in Hx.
  rewrite Hx in HSnx.
  rewrite HSnx in Hlep.
  contradiction (le_Sn_n _ Hlep).
exists (exist (fun a => a<>xSn) x Hneq).
simpl.
destruct (Hdec x xSn) as [Heqx|_].
contradiction Hneq.
assumption.
Qed.

(** Conclusion *)

Theorem interval_discr :
  forall n m, {p:nat|p<=n} = {p:nat|p<=m} -> n=m.
Proof.
intros n m Heq.
apply card_inj with (A := {p:nat|p<=n}).
apply interval_dec.
apply card_interval.
rewrite Heq.
apply card_interval.
Qed.

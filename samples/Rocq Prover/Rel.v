Require Export Logic.
Import Playground1.

Definition relation (X:Type) := X -> X -> Prop.

Definition partial_function {X:Type} (R: relation X) :=
	forall x y1 y2 : X, R x y1 -> R x y2 -> y1 = y2.

Theorem next_nat_partial_function :
	partial_function next_nat.
Proof.
	unfold partial_function.
	intros x y1 y2 P Q.
	inversion P.
	inversion Q.
	reflexivity.
Qed.

Theorem le_not_a_partial_function :
	~ (partial_function le).
Proof.
	unfold not.
	unfold partial_function.
	intros H.
	assert (O = S O) as Nonsense.
		Case "Proof of assertion.".
		apply H with O.
		apply le_n.

		apply le_S.
		apply le_n.

	inversion Nonsense.
Qed.

Theorem total_relation_not_partial_function :
	~ (partial_function total_relation).
Proof.
	unfold not.
	unfold partial_function.
	intros H.
	assert (O = S O) as Nonsense.
	apply H with O.
	apply total_relation1.

	apply total_relation1.

	inversion Nonsense.
Qed.

Theorem empty_relation_not_partial_funcion :
	partial_function empty_relation.
Proof.
	unfold partial_function.
	intros x y1 y2.
	intros H.
	inversion H.
Qed.

Definition reflexive {X:Type} (R: relation X) :=
	forall a : X, R a a.

Theorem le_reflexive :
	reflexive le.
Proof.
	unfold reflexive.
	intros n. apply le_n.
Qed.

Definition transitive {X:Type} (R: relation X) :=
	forall a b c : X, (R a b) -> (R b c) -> (R a c).

Theorem le_trans:
	transitive le.
Proof.
	intros n m o Hnm Hmo.
	induction Hmo.
	Case "le_n". apply Hnm.
	Case "le_S". apply le_S. apply IHHmo.
Qed.

Theorem lt_trans:
	transitive lt.
Proof.
	unfold lt. unfold transitive.
	intros n m o Hnm Hmo.
	apply le_S in Hnm.
	apply le_trans with (a := (S n)) (b := (S m)) (c := o).
	apply Hnm.
	apply Hmo.
Qed.

Theorem lt_trans' :
	transitive lt.
Proof.
	unfold lt. unfold transitive.
	intros n m o Hnm Hmo.
	induction Hmo as [| m' Hm'o].
	apply le_S.
	apply Hnm.

	apply le_S.
	apply IHHm'o.
Qed.

Theorem le_Sn_le: forall n m, S n <= m -> n <= m.
Proof.
	intros n m H. apply le_trans with (S n).
	apply le_S. apply le_n.
	apply H. Qed.

Theorem le_S_n : forall n m,
	(S n <= S m) -> (n <= m).
Proof.
intros n m H.
apply Sn_le_Sm__n_le_m.
apply H.
Qed.

Theorem le_Sn_n : forall n,
	~ (S n <= n).
Proof.
induction n.
intros H.
inversion H.

unfold not in IHn.
intros H.
apply le_S_n in H.
apply IHn.
apply H.
Qed.

(*
 TODO
Theorem lt_trans'' :
	transitive lt.
Proof.
	unfold lt. unfold transitive.
	intros n m o Hnm Hmo.
	induction o as [| o'].
	*)

Definition symmetric {X: Type} (R: relation X) :=
	forall a b : X, (R a b) -> (R b a).

Definition antisymmetric {X : Type} (R: relation X) :=
	forall a b : X, (R a b) -> (R b a) -> a = b.

Theorem le_antisymmetric :
	antisymmetric le.
Proof.
intros a b.
generalize dependent a.
induction b.
intros a.
intros H.
intros H1.
inversion H.
reflexivity.

intros a H1 H2.
destruct a.
inversion H2.

apply Sn_le_Sm__n_le_m in H1.
apply Sn_le_Sm__n_le_m in H2.
apply IHb in H1.
rewrite H1 in |- *.
reflexivity.

apply H2.
Qed.

(*
 TODO
Theorem le_step : forall n m p,
	n < m ->
	n <= S p ->
	n <= p.
Proof.
*)

Definition equivalence {X:Type} (R: relation X) :=
	(reflexive R) /\ (symmetric R) /\ (transitive R).

Definition order {X:Type} (R: relation X) :=
	(reflexive R) /\ (antisymmetric R) /\ (transitive R).

Definition preorder {X:Type} (R: relation X) :=
	(reflexive R) /\ (transitive R).

Theorem le_order :
	order le.
Proof.
	unfold order. split.
	Case "refl". apply le_reflexive.
	split.
		Case "antisym". apply le_antisymmetric.
		Case "transitive". apply le_trans. Qed.

Inductive clos_refl_trans {A:Type} (R: relation A) : relation A :=
	| rt_step : forall x y, R x y -> clos_refl_trans R x y
	| rt_refl : forall x, clos_refl_trans R x x
	| rt_trans : forall x y z,
		clos_refl_trans R x y -> clos_refl_trans R y z -> clos_refl_trans R x z.

Theorem next_nat_closure_is_le : forall n m,
	(n <= m) <-> ((clos_refl_trans next_nat) n m).
Proof.
intros n m.
split.
intro H.
induction H.
apply rt_refl.

apply rt_trans with m.
apply IHle.

apply rt_step.
apply nn.

intro H.
induction H.
inversion H.
apply le_S.
apply le_n.

apply le_n.

apply le_trans with y.
apply IHclos_refl_trans1.

apply IHclos_refl_trans2.
Qed.																    

Inductive refl_step_closure {X : Type} (R: relation X)
											: X -> X -> Prop :=
	| rsc_refl : forall (x : X), refl_step_closure R x x
	| rsc_step : forall (x y z : X), R x y ->
								refl_step_closure R y z ->
								refl_step_closure R x z.

Tactic Notation "rt_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "rt_step" | Case_aux c "rt_refl" | Case_aux c "rt_trans" ].

Tactic Notation "rsc_cases" tactic(first) ident(c) :=
	  first;
		  [ Case_aux c "rsc_refl" | Case_aux c "rsc_step" ].

Theorem rsc_R : forall (X:Type) (R:relation X) (x y:X),
				R x y -> refl_step_closure R x y.
Proof.
intros X R x y r.
apply rsc_step with y.
 apply r.
  
  apply rsc_refl.
Qed.

Theorem rsc_trans :
	forall (X : Type) (R : relation X) (x y z : X),
		refl_step_closure R x y ->
			refl_step_closure R y z ->
				refl_step_closure R x z.
Proof.
intros X.
intros R x y z.
intros H.
induction H.
intros H1.
apply H1.

intros H1.
apply IHrefl_step_closure in H1.
apply rsc_step with y.
apply H.

apply H1.
Qed.

Theorem rtc_rsc_coincide:
	forall (X:Type) (R: relation X) (x y : X),
		clos_refl_trans R x y <-> refl_step_closure R x y.
Proof.
intros X R x y.
split.
intros H.
induction H.
apply rsc_step with y.
apply H.

apply rsc_refl.

apply rsc_refl.

apply rsc_trans with y.
apply IHclos_refl_trans1.

apply IHclos_refl_trans2.

intros H1.
induction H1.
apply rt_refl.

apply rt_trans with y.
apply rt_step.
apply H.

apply IHrefl_step_closure.
Qed.

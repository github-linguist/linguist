Inductive day : Type :=
| monday : day
| tuesday : day
| wednesday : day
| thursday : day
| friday : day
| saturday : day
| sunday : day.

Definition next_weekday (d:day) : day :=
  match d with
  | monday => tuesday
  | tuesday => wednesday
  | wednesday => thursday
  | thursday => friday
  | friday => monday
  | saturday => monday
  | sunday => monday
  end.

Example test_next_weekday:
(next_weekday (next_weekday saturday)) = tuesday.

Proof. simpl. reflexivity. Qed.

Inductive bool : Type :=
	| true : bool
	| false : bool.

Definition negb (b:bool) : bool :=
													 match b with
																			 | true => false
																			 | false => true
													 end.

Definition andb (b1:bool) (b2:bool) : bool :=
		match b1 with
		 | true => b2
		 | false => false
	  end.

Definition orb (b1:bool) (b2:bool) : bool :=
		match b1 with
		  | true => true
		  | false => b2
		end.

Example test_orb1: (orb true false) = true.
Proof. simpl. reflexivity. Qed.

Example test_orb2: (orb false false) = false.
Proof. simpl. reflexivity. Qed.

Example test_orb3: (orb false true) = true.
Proof. simpl. reflexivity. Qed.

Example test_orb4: (orb true true) = true.
Proof. simpl. reflexivity. Qed.

Definition nandb (b1: bool) (b2:bool) : bool :=
	match b1 with
		| true => match b2 with
										| false => true
										| true => false
							end
		| false => true
	end.

Example test_nandb1: (nandb true false) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb2: (nandb false false) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb3: (nandb false true) = true.
Proof. simpl. reflexivity. Qed.
Example test_nandb4: (nandb true true) = false.
Proof. simpl. reflexivity. Qed.

Definition andb3 (b1: bool) (b2:bool) (b3:bool) : bool :=
	match b1 with
    | false => false
		| true => match b2 with
								| false => false
								| true => b3
							end
	end.

Example test_andb31: (andb3 true true true) = true.
Proof. simpl. reflexivity. Qed.
Example test_andb32: (andb3 false true true) = false.
Proof. simpl. reflexivity. Qed.
Example test_andb33: (andb3 true false true) = false.
Proof. simpl. reflexivity. Qed.
Example test_andb34: (andb3 true true false) = false.
Proof. simpl. reflexivity. Qed.

Module Playground1.

Inductive nat : Type :=
	| O : nat
	| S : nat -> nat.

Definition pred (n : nat) : nat :=
	match n with
		| O => O
		| S n' => n'
	end.

Definition minustwo (n : nat) : nat :=
	match n with
		| O => O
		| S O => O
		| S (S n') => n'
	end.

Fixpoint evenb (n : nat) : bool :=
	match n with
		| O => true
		| S O => false
		| S (S n') => evenb n'
	end.

Definition oddb (n : nat) : bool := negb (evenb n).

Example test_oddb1: (oddb (S O)) = true.
Proof. reflexivity. Qed.
Example test_oddb2: (oddb (S (S (S (S O))))) = false.
Proof. reflexivity. Qed.

Fixpoint plus (n : nat) (m : nat) : nat :=
	match n with
		| O => m
		| S n' => S (plus n' m)
	end.

Fixpoint mult (n m : nat) : nat :=
	match n with
		| O => O
		| S n' => plus m (mult n' m)
	end.

Fixpoint minus (n m : nat) : nat :=
	match n, m with
		| O, _ => n
		| S n', O => S n'
		| S n', S m' => minus n' m'
	end.

Fixpoint exp (base power : nat) : nat :=
	match power with
		| O => S O
		| S p => mult base (exp base p)
	end.

Fixpoint factorial (n : nat) : nat :=
	match n with
		| O => S O
		| S n' => mult n (factorial n')
	end.

Example test_factorial1: (factorial (S (S (S O)))) = (S (S (S (S (S (S O)))))).
Proof. simpl. reflexivity. Qed.

Notation "x + y" := (plus x y) (at level 50, left associativity) : nat_scope.
Notation "x - y" := (minus x y) (at level 50, left associativity) : nat_scope.
Notation "x * y" := (mult x y) (at level 40, left associativity) : nat_scope.

Fixpoint beq_nat (n m : nat) : bool :=
	match n with
		| O => match m with
						| O => true
						| S m' => false
					 end
		| S n' => match m with
							| O => false
							| S m' => beq_nat n' m'
							end
	end.

Fixpoint ble_nat (n m : nat) : bool :=
	match n with
		| O => true
		| S n' => 
				match m with
					| O => false
					| S m' => ble_nat n' m'
				end
	end.

Example test_ble_nat1: (ble_nat (S (S O)) (S (S O))) = true.
Proof. simpl. reflexivity. Qed.
Example test_ble_nat2: (ble_nat (S (S O)) (S (S (S (S O))))) = true.
Proof. simpl. reflexivity. Qed.
Example test_ble_nat3: (ble_nat (S (S (S (S O)))) (S (S O))) = false.
Proof. simpl. reflexivity. Qed.

Definition blt_nat (n m : nat) : bool :=
		(andb (negb (beq_nat n m)) (ble_nat n m)).

Example test_blt_nat1: (blt_nat (S (S O)) (S (S O))) = false.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat3: (blt_nat (S (S (S (S O)))) (S (S O))) = false.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat2 : (blt_nat (S (S O)) (S (S (S (S O))))) = true.
Proof. simpl. reflexivity. Qed.

Theorem plus_O_n : forall n : nat, O + n = n.
Proof.
	simpl. reflexivity. Qed.

Theorem plus_O_n' : forall n : nat, O + n = n.
Proof.
	reflexivity. Qed.

Theorem plus_O_n'' : forall n : nat, O + n = n.
Proof.
	intros n. reflexivity. Qed.

Theorem plus_1_1 : forall n : nat, (S O) + n = S n.
Proof.
	intros n. reflexivity. Qed.

Theorem mult_0_1: forall n : nat, O * n = O.
Proof.
	intros n. reflexivity. Qed.

Theorem plus_id_example : forall n m:nat,
	n = m -> n + n = m + m.
Proof.
	intros n m.
	intros H.
	rewrite -> H.
	reflexivity. Qed.

Theorem plus_id_exercise : forall n m o: nat,
	n = m -> m = o -> n + m = m + o.
Proof.
	intros n m o.
	intros H.
	intros H'.
	rewrite -> H.
	rewrite <- H'.
	reflexivity.
	Qed.

Theorem mult_0_plus : forall n m : nat,
				(O + n) * m = n * m.
Proof.
	intros n m.
	rewrite -> plus_O_n.
	reflexivity. Qed.

Theorem mult_1_plus : forall n m: nat,
	((S O) + n) * m = m + (n * m).
Proof.
	intros n m.
	rewrite -> plus_1_1.
	reflexivity.
	Qed.

Theorem mult_1 : forall n : nat,
				n * (S O) = n.
Proof.
	intros n.
	induction n as [| n'].
	reflexivity.
	simpl.
	rewrite -> IHn'.
	reflexivity.
	Qed.

Theorem plus_1_neq_0 : forall n : nat,
				beq_nat (n + (S O)) O = false.
Proof.
	intros n.
	destruct n as [| n'].
	reflexivity.
	reflexivity.
	Qed.

Theorem zero_nbeq_plus_1 : forall n : nat,
				beq_nat O (n + (S O)) = false.
Proof.
	intros n.
	destruct n.
	reflexivity.
	reflexivity.
Qed.

Require String. Open Scope string_scope.

Ltac move_to_top x :=
match reverse goal with
| H : _ |- _ => try move x after H
end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
	let H := fresh in
	assert (x = v) as H by reflexivity;
	clear H.

	Tactic Notation "Case_aux" ident(x) constr(name) :=
		first [
		set (x := name); move_to_top x
		| assert_eq x name; move_to_top x
		| fail 1 "because we are working on a different case" ].

		Ltac Case name := Case_aux Case name.
		Ltac SCase name := Case_aux SCase name.
		Ltac SSCase name := Case_aux SSCase name.
		Ltac SSSCase name := Case_aux SSSCase name.
		Ltac SSSSCase name := Case_aux SSSSCase name.
		Ltac SSSSSCase name := Case_aux SSSSSCase name.
		Ltac SSSSSSCase name := Case_aux SSSSSSCase name.
		Ltac SSSSSSSCase name := Case_aux SSSSSSSCase name.

Theorem andb_true_elim1 : forall b c : bool,
				andb b c = true -> b = true.
Proof.
	intros b c H.
	destruct b.
	Case "b = true".
		reflexivity.
	Case "b = false".
		rewrite <- H. reflexivity. Qed.

Theorem plus_0_r : forall n : nat, n + O = n.
Proof.
	intros n. induction n as [| n'].
	Case "n = 0". reflexivity.
	Case "n = S n'". simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem minus_diag : forall n,
				minus n n = O.
Proof.
	intros n. induction n as [| n'].
	Case "n = 0".
		simpl. reflexivity.
	Case "n = S n'".
		simpl. rewrite -> IHn'. reflexivity. Qed.


Theorem mult_0_r : forall n:nat,
				n * O = O.
Proof.
	intros n. induction n as [| n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n'".
		simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_n_Sm : forall n m : nat,
				S (n + m) = n + (S m).
Proof.
	intros n m. induction n as [| n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n'".
		simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem plus_assoc : forall n m p : nat,
					n + (m + p) = (n + m) + p.
Proof.
	intros n m p.
	induction n as [| n'].
	reflexivity.
	simpl.
	rewrite -> IHn'.
	reflexivity. Qed.

Theorem plus_distr : forall n m: nat, S (n + m) = n + (S m).
Proof.
	intros n m.  induction n as [| n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n'".
		simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem mult_distr : forall n m: nat, n * ((S O) + m) = n * (S m).
Proof.
	intros n m.
	induction n as [| n'].
	reflexivity.
	reflexivity.
	Qed.

Theorem plus_comm : forall n m : nat,
	n + m = m + n.
Proof.
	intros n m.
	induction n as [| n'].
	Case "n = 0".
		simpl.
		rewrite -> plus_0_r.
		reflexivity.
	Case "n = S n'".
		simpl.
		rewrite -> IHn'.
		rewrite -> plus_distr.
		reflexivity. Qed.

Fixpoint double (n:nat) :=
	match n with
		| O => O
		| S n' => S (S (double n'))
	end.

Lemma double_plus : forall n, double n = n + n.
Proof.
	intros n. induction n as [| n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n'".
		simpl. rewrite -> IHn'.
		rewrite -> plus_distr. reflexivity.
		Qed.

Theorem beq_nat_refl : forall n : nat,
	true = beq_nat n n.
Proof.
	intros n. induction n as [| n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n".
		simpl. rewrite <- IHn'.
		reflexivity. Qed.

Theorem plus_rearrange: forall n m p q : nat,
				(n + m) + (p + q) = (m + n) + (p + q).
Proof.
	intros n m p q.
	assert(H: n + m = m + n).
		Case "Proof by assertion".
		rewrite -> plus_comm. reflexivity.
	rewrite -> H. reflexivity. Qed.

Theorem plus_swap : forall n m p: nat,
				n + (m + p) = m + (n + p).
Proof.
	intros n m p.
	rewrite -> plus_assoc.
	assert(H: m + (n + p) = (m + n) + p).
	rewrite -> plus_assoc.
	reflexivity.
	rewrite -> H.
	assert(H2: m + n = n + m).
	rewrite -> plus_comm.
	reflexivity.
	rewrite -> H2.
	reflexivity.
	Qed.

Theorem plus_swap' : forall n m p: nat,
				n + (m + p) = m + (n + p).
Proof.
	intros n m p.
	rewrite -> plus_assoc.
	assert(H: m + (n + p) = (m + n) + p).
	rewrite -> plus_assoc.
	reflexivity.
	rewrite -> H.
	replace (m + n) with (n + m).
	rewrite -> plus_comm.
	reflexivity.
	rewrite -> plus_comm.
	reflexivity.
	Qed.

Theorem mult_1_distr: forall m n: nat,
				n * ((S O) + m) = n * (S O) + n * m.
Proof.
	intros n m.
	rewrite -> mult_1.
	rewrite -> plus_1_1.
	simpl.
	induction m as [|m'].
	simpl.
	reflexivity.
	simpl.
	rewrite -> plus_swap.
	rewrite <- IHm'.
	reflexivity.
	Qed.

Theorem mult_comm: forall m n : nat,
				m * n = n * m.
Proof.
	intros m n.
	induction n as [| n'].
	Case "n = 0".
		simpl.
		rewrite -> mult_0_r.
		reflexivity.
	Case "n = S n'".
		simpl.
		rewrite <- mult_distr.
		rewrite -> mult_1_distr.
		rewrite -> mult_1.
		rewrite -> IHn'.
		reflexivity.
		Qed.

Theorem evenb_next : forall n : nat,
				evenb n = evenb (S (S n)).
Proof.
	intros n.
Admitted.

Theorem negb_negb : forall n : bool,
				n = negb (negb n).
Proof.
	intros n.
	destruct n.
	reflexivity.
	reflexivity.
	Qed.

Theorem evenb_n_oddb_Sn : forall n : nat,
				evenb n = negb (evenb (S n)).
Proof.
	intros n.
	induction n as [|n'].
	reflexivity.
	assert(H: evenb n' = evenb (S (S n'))).
	reflexivity.
	rewrite <- H.
	rewrite -> IHn'.
	rewrite <- negb_negb.
	reflexivity.
	Qed.

(*Fixpoint bad (n : nat) : bool :=
	match n with
		| O => true
		| S O => bad (S n)
		| S (S n') => bad n'
	end.*)

Theorem ble_nat_refl : forall n:nat,
				true = ble_nat n n.
Proof.
	intros n.
	induction n as [|n'].
	Case "n = 0".
		reflexivity.
	Case "n = S n".
		simpl.
		rewrite <- IHn'.
		reflexivity.
	Qed.

Theorem zero_nbeq_S : forall n: nat,
				beq_nat O (S n) = false.
Proof.
	intros n.
	reflexivity.
	Qed.

Theorem andb_false_r : forall b : bool,
				andb b false = false.
Proof.
	intros b.
	destruct b.
	reflexivity.
	reflexivity.
	Qed.

Theorem plus_ble_compat_1 : forall n m p : nat,
				ble_nat n m = true -> ble_nat (p + n) (p + m) = true.
Proof.
	intros n m p.
	intros H.
	induction p.
	Case "p = 0".
		simpl.
		rewrite -> H.
		reflexivity.
	Case "p = S p'".
		simpl.
		rewrite -> IHp.
		reflexivity.
		Qed.

Theorem S_nbeq_0 : forall n:nat,
				beq_nat (S n) O = false.
Proof.
	intros n.
	reflexivity.
	Qed.

Theorem mult_1_1 : forall n:nat, (S O) * n = n.
Proof.
	intros n.
	simpl.
	rewrite -> plus_0_r.
	reflexivity. Qed.

Theorem all3_spec : forall b c : bool,
	orb (andb b c)
			(orb (negb b)
			 		 (negb c))
	= true.
Proof.
	intros b c.
	destruct b.
	destruct c.
	reflexivity.
	reflexivity.
	reflexivity.
	Qed.

Lemma mult_plus_1 : forall n m : nat,
			S(m + n) = m + (S n).
Proof.
	intros n m.
	induction m.
	reflexivity.
	simpl.
	rewrite -> IHm.
	reflexivity.
	Qed.

Theorem mult_mult : forall n m : nat,
	n * (S m) = n * m + n.
Proof.
	intros n m.
	induction n.
	reflexivity.
	simpl.
	rewrite -> IHn.
	rewrite -> plus_assoc.
	rewrite -> mult_plus_1.
	reflexivity.
	Qed.

Theorem mult_plus_distr_r : forall n m p:nat,
				(n + m) * p = (n * p) + (m * p).
Proof.
	intros n m p.
	induction p.
	rewrite -> mult_0_r.
	rewrite -> mult_0_r.
	rewrite -> mult_0_r.
	reflexivity.
	rewrite -> mult_mult.
	rewrite -> mult_mult.
	rewrite -> mult_mult.
	rewrite -> IHp.
	assert(H1: ((n * p) + n) + (m * p + m) = (n * p) + (n + (m * p + m))).
	rewrite <- plus_assoc.
	reflexivity.
	rewrite -> H1.
	assert(H2: (n + (m * p + m)) = (m * p + (n + m))).
	rewrite -> plus_swap.
	reflexivity.
	rewrite -> H2.
	assert(H3: (n * p) + (m * p + (n + m)) = ((n * p ) + (m * p)) + (n + m)).
	rewrite -> plus_assoc.
	reflexivity.
	rewrite -> H3.
	reflexivity.
	Qed.

Theorem mult_assoc : forall n m p : nat,
				n * (m * p) = (n * m) * p.
Proof.
	intros n m p.
	induction n.
	simpl.
	reflexivity.
	simpl.
	rewrite -> mult_plus_distr_r.
	rewrite -> IHn.
	reflexivity.
	Qed.

Inductive bin : Type :=
	| BO : bin
	| D : bin -> bin
	| M : bin -> bin.

Fixpoint incbin (n : bin) : bin :=
	match n with
		| BO => M (BO)
		| D n' => M n'
		| M n' => D (incbin n')
	end.

Fixpoint bin2un (n : bin) : nat :=
	match n with
		| BO => O
		| D n' => double (bin2un n')
		| M n' => S (double (bin2un n'))
	end.

Theorem bin_comm : forall n : bin,
				bin2un(incbin n) = S (bin2un n).
Proof.
	intros n.
	induction n.
		reflexivity.
		reflexivity.
		simpl.
		rewrite -> IHn.
		reflexivity.
	Qed.

End Playground1.

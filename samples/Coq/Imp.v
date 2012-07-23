
Require Export SfLib.

Module AExp.

Inductive aexp : Type :=
	| ANum : nat -> aexp
	| APlus : aexp -> aexp -> aexp
	| AMinus : aexp -> aexp -> aexp
	| AMult : aexp -> aexp -> aexp.

Inductive bexp : Type :=
	| BTrue : bexp
	| BFalse : bexp
	| BEq : aexp -> aexp -> bexp
	| BLe : aexp -> aexp -> bexp
	| BNot : bexp -> bexp
	| BAnd : bexp -> bexp -> bexp.

Fixpoint aeval (e: aexp) : nat :=
	match e with
		| ANum n => n
		| APlus a1 a2 => (aeval a1) + (aeval a2)
		| AMinus a1 a2 => (aeval a1) - (aeval a2)
		| AMult a1 a2 => (aeval a1) * (aeval a2)
	end.

Example test_aeval1:
	aeval (APlus (ANum 2) (ANum 2)) = 4.
Proof. reflexivity. Qed.

Fixpoint beval (e: bexp) : bool :=
	match e with
		| BTrue => true
		| BFalse => false
		| BEq a1 a2 => beq_nat (aeval a1) (aeval a2)
		| BLe a1 a2 => ble_nat (aeval a1) (aeval a2)
		| BNot b1 => negb (beval b1)
		| BAnd b1 b2 => andb (beval b1) (beval b2)
	end.

Fixpoint optimize_0plus (a:aexp) : aexp :=
	match a with
		| ANum n => ANum n
		| APlus (ANum 0) e2 => optimize_0plus e2
		| APlus e1 e2 => APlus (optimize_0plus e1) (optimize_0plus e2)
		| AMinus e1 e2 => AMinus (optimize_0plus e1) (optimize_0plus e2)
		| AMult e1 e2 => AMult (optimize_0plus e1) (optimize_0plus e2)
	end.

Example test_optimize_0plus: 
  optimize_0plus (APlus (ANum 2) 
			(APlus (ANum 0) 
			 (APlus (ANum 0) (ANum 1)))) =
	APlus (ANum 2) (ANum 1).
Proof. reflexivity. Qed.

Theorem optimize_0plus_sound: forall e,
	aeval (optimize_0plus e) = aeval e.
Proof.
intros e. induction e.
  Case "ANum". reflexivity.
  Case "APlus". destruct e1.
    SCase "e1 = ANum n". destruct n.
      SSCase "n = 0". simpl. apply IHe2.
      SSCase "n <> 0". simpl. rewrite IHe2. reflexivity.
    SCase "e1 = APlus e1_1 e1_2".
      simpl. simpl in IHe1. rewrite IHe1. rewrite IHe2. reflexivity.
    SCase "e1 = AMinus e1_1 e1_2".
      simpl. simpl in IHe1. rewrite IHe1. rewrite IHe2. reflexivity.
    SCase "e1 = AMult e1_1 e1_2".
      simpl. simpl in IHe1. rewrite IHe1. rewrite IHe2. reflexivity.
  Case "AMinus".
    simpl. rewrite IHe1. rewrite IHe2. reflexivity.
  Case "AMult".
    simpl. rewrite IHe1. rewrite IHe2. reflexivity. Qed.

Theorem optimize_0plus_sound': forall e,
	aeval (optimize_0plus e) = aeval e.
Proof.
	intros e.
	induction e;
		try (simpl; rewrite IHe1; rewrite IHe2; reflexivity).
	Case "ANum". reflexivity.
	Case "APlus".
		destruct e1;
		try (simpl; simpl in IHe1; rewrite IHe1; rewrite IHe2; reflexivity).
		SCase "e1 = ANum n". destruct n;
			simpl; rewrite IHe2; reflexivity. Qed.

Theorem optimize_0plus_sound'': forall e,
	aeval (optimize_0plus e) = aeval e.
Proof.
	intros e.
	induction e;
		try (simpl; rewrite IHe1; rewrite IHe2; reflexivity);
		try reflexivity.
	Case "APlus".
		destruct e1;
			try (simpl; simpl in IHe1; rewrite IHe1; rewrite IHe2; reflexivity).
			SCase "e1 = ANum n". destruct n;
				simpl; rewrite IHe2; reflexivity. Qed.

Tactic Notation "simpl_and_try" tactic(c) :=
	simpl;
	try c.

Tactic Notation "aexp_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "ANum" | Case_aux c "APlus"
		| Case_aux c "AMinus" | Case_aux c "AMult" ].

Theorem optimize_0plus_sound''': forall e,
	aeval (optimize_0plus e) = aeval e.
Proof.
	intros e.
	aexp_cases (induction e) Case;
		try (simpl; rewrite IHe1; rewrite IHe2; reflexivity);
		try reflexivity.

		Case "APlus".
			aexp_cases (destruct e1) SCase;
				try (simpl; simpl in IHe1; rewrite IHe1; rewrite IHe2; reflexivity).
			SCase "ANum". destruct n;
				simpl; rewrite IHe2; reflexivity. Qed.


Fixpoint optimize_0plus_all (b:bexp) : bexp :=
	match b with
		| BTrue => BTrue
		| BFalse => BFalse
		| BEq a1 a2 => BEq (optimize_0plus a1) (optimize_0plus a2)
		| BLe a1 a2 => BLe (optimize_0plus a1) (optimize_0plus a2)
		| BNot b1 => BNot b1
		| BAnd b1 b2 => BAnd b1 b2
	end.

Tactic Notation "bexp_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "BTrue" | Case_aux c "BFalse"
			  | Case_aux c "BEq" | Case_aux c "BLe" | Case_aux c "BNot"
				| Case_aux c "BAnd" ].

Theorem optimize_0plus_all_sound: forall (e:bexp),
	beval (optimize_0plus_all e) = beval e.
Proof.
	intros e.
	bexp_cases (destruct e) Case;
	try (simpl; rewrite optimize_0plus_sound; rewrite optimize_0plus_sound);
	try reflexivity.
Qed.

Fixpoint optimize_and (b: bexp): bexp :=
	match b with
		| BTrue => BTrue
		| BFalse => BFalse
		| BEq a1 a2 => BEq a1 a2
		| BLe a1 a2 => BLe a1 a2
		| BNot a1 => BNot (optimize_and a1)
		| BAnd BFalse a2 => BFalse
		| BAnd a1 a2 => BAnd (optimize_and a1) (optimize_and a2)
	end.

Theorem optimize_and_sound: forall (e:bexp),
	beval (optimize_and e) = beval e.
Proof.
	intros e.
	bexp_cases (induction e) Case;
	try (simpl; rewrite IHe);
	try reflexivity.
	Case "BAnd". simpl.
		bexp_cases (destruct e1) SCase;
			try reflexivity;
			try (simpl; rewrite IHe2; reflexivity);
			try (simpl; simpl in IHe1; rewrite IHe2; rewrite IHe1; reflexivity).
Qed.

Module aevalR_first_try.

Inductive aevalR : aexp -> nat -> Prop :=
	| E_Anum : forall (n : nat),
							aevalR (ANum n) n
	| E_APlus : forall (e1 e2 : aexp) (n1 n2 : nat),
								aevalR e1 n1 -> aevalR e2 n2 -> aevalR (APlus e1 e2) (n1 + n2)
	| E_AMinus: forall (e1 e2 : aexp) (n1 n2 : nat),
								aevalR e1 n1 -> aevalR e2 n2 -> aevalR (AMinus e1 e2) (n1 - n2)
	| E_AMult : forall (e1 e2 : aexp) (n1 n2 : nat),
								aevalR e1 n1 -> aevalR e2 n2 -> aevalR (AMult e1 e2) (n1 * n2).

Notation "e '==>' n" := (aevalR e n) (at level 40).

End aevalR_first_try.

Reserved Notation "e '==>' n" (at level 40).

Inductive aevalR : aexp -> nat -> Prop :=
	| E_ANum : forall (n : nat), (ANum n) ==> n
	| E_APlus : forall (e1 e2 : aexp) (n1 n2 : nat),
								(e1 ==> n1) -> (e2 ==> n2) -> (APlus e1 e2) ==> (n1 + n2)
	| E_AMinus : forall (e1 e2 : aexp) (n1 n2 : nat),
								(e1 ==> n1) -> (e2 ==> n2) -> (AMinus e1 e2) ==> (n1 - n2)
	| E_AMult : forall (e1 e2 : aexp) (n1 n2 : nat),
								(e1 ==> n1) -> (e2 ==> n2) -> (AMult e1 e2) ==> (n1 * n2)
where "e '==>' n" := (aevalR e n).

Inductive bevalR : bexp -> bool -> Prop :=
	| E_BTrue : bevalR (BTrue) true
	| E_BFalse : bevalR (BFalse) false
	| E_BEq : forall (e1 e2: aexp) (n1 n2 : nat),
						aevalR e1 n1 -> aevalR e2 n2 -> bevalR (BEq e1 e2) (beq_nat n1 n2)
	| E_BLe : forall (e1 e2 : aexp) (n1 n2 : nat),
						aevalR e1 n1 -> aevalR e2 n2 -> bevalR (BLe e1 e2) (ble_nat n1 n2)
	| E_BNot : forall (e1 : bexp) (b : bool),
							bevalR e1 b -> bevalR (BNot e1) (negb b)
	| E_BAnd : forall (e1 e2 : bexp) (b1 b2 : bool),
							bevalR e1 b1 -> bevalR e2 b2 -> bevalR (BAnd e1 e2) (andb b1 b2).

Tactic Notation "aevalR_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "E_Anum" | Case_aux c "E_APlus"
		| Case_aux c "E_AMinus" | Case_aux c "E_AMult" ].

Theorem aeval_iff_aevalR : forall a n,
	(a ==> n) <-> aeval a = n.
Proof.
	split.
	Case "->".
		intros H; induction H; subst; reflexivity;
	Case "<-".
		generalize dependent n.
		induction a; simpl; intros; subst; constructor;
			try apply IHa1; try apply IHa2; reflexivity.
Qed.

Tactic Notation "bevalR_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "E_BTrue" | Case_aux c "E_BFalse"
		| Case_aux c "E_BEq" | Case_aux c "E_BLe" | Case_aux c "E_BNot"
		| Case_aux c "E_BAnd" ].

Theorem beval_iff_bevalR : forall a b,
	bevalR a b <-> beval a = b.
Proof.
split.
intros H.
induction H.
simpl in |- *.
reflexivity.

simpl in |- *.
reflexivity.

simpl in |- *.
apply aeval_iff_aevalR in H.
apply aeval_iff_aevalR in H0.
subst.
reflexivity.

simpl in |- *.
apply aeval_iff_aevalR in H0.
apply aeval_iff_aevalR in H.
subst.
reflexivity.

simpl in |- *.
rewrite IHbevalR in |- *.
reflexivity.

simpl in |- *.
rewrite IHbevalR1 in |- *.
rewrite IHbevalR2 in |- *.
reflexivity.
generalize dependent b.
induction a.
intros b H. subst. constructor.
intros b H.
subst.
constructor.

intros b H.
rewrite <- H in |- *.
constructor.
simpl in H.
remember (aeval a)as A in *.
assert (aeval a = A).
subst.
reflexivity.

apply aeval_iff_aevalR in H0.
apply H0.

remember (aeval a0)as A in *.
assert (aeval a0 = A).
subst.
reflexivity.

apply aeval_iff_aevalR in H0.
apply H0.

intros b.
intros H.
simpl in H.
rewrite <- H in |- *.
simpl in |- *.
constructor.
remember (aeval a)as A in *.
assert (aeval a = A).
subst.
reflexivity.

apply aeval_iff_aevalR in H0.
apply H0.

remember (aeval a0)as A in *.
assert (aeval a0 = A).
subst.
reflexivity.

apply aeval_iff_aevalR in H0.
apply H0.

intros b.
intros H.
simpl in |- *.
simpl in H.
rewrite <- H in |- *.
constructor.
apply IHa.
reflexivity.

intros b.
simpl in |- *.
intros H.
rewrite <- H in |- *.
constructor.
apply IHa1.
reflexivity.

apply IHa2.
reflexivity.
Qed.

End AExp.

Example silly_presburger_formula : forall m n o p,
	m + n <= n + o /\ o + 3 = p + 3 ->
	m <= p.
Proof.
	intros. omega.
Qed.

Module Id.

Inductive id : Type :=
	Id : nat -> id.

Definition beq_id id1 id2 :=
	match (id1, id2) with
		(Id n1, Id n2) => beq_nat n1 n2
	end.

Theorem beq_id_refl : forall i,
		true = beq_id i i.
Proof.
	intros. destruct i.
	apply beq_nat_refl. Qed.

Theorem beq_id_eq : forall i1 i2,
	true = beq_id i1 i2 -> i1 = i2.
Proof.
intros i1 i2.
destruct i1.
destruct i2.
unfold beq_id in |- *.
intros H.
apply beq_nat_eq in H.
subst.
reflexivity.
Qed.

Theorem beq_id_false_not_eq : forall i1 i2,
	beq_id i1 i2 = false -> i1 <> i2.
Proof.
intros i1 i2.
destruct i1.
destruct i2.
unfold beq_id in |- *.
intros H.
intros C.
unfold beq_id in |- *.
assert (false = beq_nat n n0).
subst.
rewrite <- H in |- *.
reflexivity.

apply beq_false_not_eq in H0.
apply H0.
subst.
inversion C.
reflexivity.
Qed.

Theorem not_eq_beq_id_false : forall i1 i2,
	i1 <> i2 -> beq_id i1 i2 = false.
Proof.
	intros i1 i2.
	destruct i1. destruct i2.
	unfold beq_id in |- *.
	intros H.
	apply not_eq_beq_false.
	intros C.
	apply H.
	subst.
	reflexivity.
Qed.

Theorem beq_nat_sym : forall (n m:nat), forall (b: bool),
	beq_nat n m = b -> beq_nat m n = b.
Proof.
	intros n.
	induction n as [| n'].
	Case "n = O".
		intros m b eq1.
		induction m.
		SCase "m = 0".
			apply eq1.
		SCase "m = S m'".
			apply eq1.
	Case "n = S n'".
		induction m.
		SCase "m = 0".
			intros b eq1.
			apply eq1.
		SCase "m = S m'".
			intros b eq1.
			apply IHn'.
			apply eq1.
		Qed.

Theorem beq_id_sym : forall i1 i2,
	beq_id i1 i2 = beq_id i2 i1.
Proof.
	intros i1 i2.
	destruct i1. destruct i2.
	unfold beq_id.
	apply beq_nat_sym.
	reflexivity.
Qed.

End Id.

Definition state := id -> nat.
Definition empty_state : state := fun _ => 0.
Definition update (st : state) (V:id) (n:nat) : state :=
	fun V' => if beq_id V V' then n else st V'.

Theorem update_eq : forall n V st,
	(update st V n) V = n.
Proof.
intros n V st.
unfold update.
rewrite <- beq_id_refl.
reflexivity.
Qed.

Theorem update_neq : forall V2 V1 n st,
	beq_id V2 V1 = false ->
		(update st V2 n) V1 = (st V1).
Proof.
intros V2 V1 n st.
intros H.
unfold update.
rewrite H.
reflexivity.
Qed.

Theorem update_example : forall (n:nat),
	(update empty_state (Id 2) n) (Id 3) = 0.
Proof.
unfold update.
intros n.
simpl.
unfold empty_state.
reflexivity.
Qed.

Theorem update_shadow : forall x1 x2 k1 k2 (f:state),
	(update (update f k2 x1) k2 x2) k1 = (update f k2 x2) k1.
Proof.
intros x1 x2 k1 k2 f.
unfold update in |- *.
destruct (beq_id k2 k1);
 reflexivity.
Qed.

Theorem update_same : forall x1 k1 k2 (f : state),
	f k1 = x1 ->
		(update f k1 x1) k2 = f k2.
Proof.
unfold update in |- *.
intros x1 k1 k2 f.
remember (beq_id k1 k2)as A in *.
destruct A.
apply beq_id_eq in HeqA.
subst.
intros H.
subst.
reflexivity.

intros H.
reflexivity.
Qed.


Theorem update_permute : forall x1 x2 k1 k2 k3 f,
	beq_id k2 k1 = false ->
		(update (update f k2 x1) k1 x2) k3 = (update (update f k1 x2) k2 x1) k3.
Proof.
intros x1 x2 k1 k2 k3 f.
intros H.
unfold update in |- *.
remember (beq_id k1 k3)as A1 in *.
destruct A1.
apply beq_id_eq in HeqA1.
rewrite HeqA1 in H.
destruct (beq_id k2 k3).
inversion H.

reflexivity.

remember (beq_id k2 k3)as B in *.
destruct B;
try reflexivity.
Qed.

Inductive aexp : Type := 
	| ANum : nat -> aexp
	| AId : id -> aexp (* <----- NEW *)
	| APlus : aexp -> aexp -> aexp
	| AMinus : aexp -> aexp -> aexp
	| AMult : aexp -> aexp -> aexp.

Tactic Notation "aexp_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "ANum" | Case_aux c "AId" | Case_aux c "APlus"
	| Case_aux c "AMinus" | Case_aux c "AMult" ].

Definition X : id := Id 0.
Definition Y : id := Id 1.
Definition Z : id := Id 2.

Inductive bexp : Type :=
	| BTrue : bexp
	| BFalse : bexp
	| BEq : aexp -> aexp -> bexp
	| BLe : aexp -> aexp -> bexp
	| BNot : bexp -> bexp
	| BAnd : bexp -> bexp -> bexp.

Tactic Notation "bexp_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "BTrue" | Case_aux c "BFalse" | Case_aux c "BEq"
	| Case_aux c "BLe" | Case_aux c "BNot" | Case_aux c "BAnd" ].

Fixpoint aeval (st : state) (e : aexp) : nat :=
  match e with
  | ANum n => n
  | AId i => st i (* <----- NEW *)
  | APlus a1 a2 => (aeval st a1) + (aeval st a2)
  | AMinus a1 a2 => (aeval st a1) - (aeval st a2)
  | AMult a1 a2 => (aeval st a1) * (aeval st a2)
  end.

Fixpoint beval (st : state) (e : bexp) : bool :=
  match e with 
  | BTrue => true
  | BFalse => false
  | BEq a1 a2 => beq_nat (aeval st a1) (aeval st a2)
  | BLe a1 a2 => ble_nat (aeval st a1) (aeval st a2)
  | BNot b1 => negb (beval st b1)
  | BAnd b1 b2 => andb (beval st b1) (beval st b2)
  end.

Example aexp1 : 
  aeval (update empty_state X 5) 
        (APlus (ANum 3) (AMult (AId X) (ANum 2))) 
  = 13.
Proof. reflexivity. Qed.

Example bexp1 :
  beval (update empty_state X 5) 
        (BAnd BTrue (BNot (BLe (AId X) (ANum 4))))
  = true.
Proof. reflexivity. Qed.

Inductive com : Type :=
	| CSkip : com
	| CAss : id -> aexp -> com
	| CSeq : com -> com -> com
	| CIf : bexp -> com -> com -> com
	| CWhile : bexp -> com -> com.

Tactic Notation "com_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "SKIP" | Case_aux c "::=" | Case_aux c ";"
	| Case_aux c "IFB" | Case_aux c "WHILE" ].

Notation "'SKIP'" := 
	CSkip.
Notation "l '::=' a" := 
	(CAss l a) (at level 60).
Notation "c1 ; c2" := 
	(CSeq c1 c2) (at level 80, right associativity).
Notation "'WHILE' b 'DO' c 'END'" := 
	(CWhile b c) (at level 80, right associativity).
Notation "'IFB' e1 'THEN' e2 'ELSE' e3 'FI'" := 
	(CIf e1 e2 e3) (at level 80, right associativity).

Definition plus2 : com :=
	X ::= (APlus (AId X) (ANum 2)).

Definition XtimesYinZ : com :=
	Z ::= (AMult (AId X) (AId Y)).

Definition subtract_slowly_body : com :=
	Z ::= AMinus (AId Z) (ANum 1);
	X ::= AMinus (AId X) (ANum 1).

Definition subtract_slowly : com :=
	WHILE BNot (BEq (AId X) (ANum 0)) DO
		subtract_slowly_body
	END.

Definition subtract_3_from_5_slowly : com :=
	X ::= ANum 3 ;
	Z ::= ANum 5 ;
	subtract_slowly.

Definition loop : com :=
  WHILE BTrue DO
		SKIP
	END.

Definition fact_body : com :=
  Y ::= AMult (AId Y) (AId Z) ;
	Z ::= AMinus (AId Z) (ANum 1).

Definition fact_loop : com :=
	WHILE BNot (BEq (AId Z) (ANum 0)) DO
		fact_body
	END.

Definition fact_com : com :=
	Z ::= AId X ;
	Y ::= ANum 1 ;
	fact_loop.

Fixpoint ceval_step1 (st:state) (c:com) : state :=
	match c with
		| SKIP => st
		| l ::= a1 =>
				update st l (aeval st a1)
		| c1 ; c2 =>
			let st' := ceval_step1 st c1 in
				ceval_step1 st' c2
		| IFB b THEN c1 ELSE c2 FI =>
			if (beval st b) then ceval_step1 st c1 else ceval_step1 st c2
		| WHILE b1 DO c1 END =>
			st (* bogus *)
	end.

Fixpoint ceval_step2 (st: state) (c:com) (i:nat) : state :=
	match i with
		| O => empty_state
		| S i' =>
				match c with
					| SKIP => st
					| l ::= a1 =>
							update st l (aeval st a1)
					| c1 ; c2 => let st' := ceval_step2 st c1 i' in
											ceval_step2 st' c2 i'
					| IFB b THEN c1 ELSE c2 FI =>
							if (beval st b) then ceval_step2 st c1 i' else ceval_step2 st c2 i'
					| WHILE b1 DO c1 END =>
							if (beval st b1)
							then let st' := ceval_step2 st c1 i' in
									ceval_step2 st' c i'
							else st
				end
	end.

Fixpoint ceval_step3 (st: state) (c:com) (i:nat) : option state :=
	match i with
		| O => None
		| S i' =>
				match c with
					| SKIP => Some st
					| l ::= a1 =>
							Some (update st l (aeval st a1))
					| c1 ; c2 => match (ceval_step3 st c1 i') with
												| Some st' => ceval_step3 st' c2 i'
												| None => None
											end
					| IFB b THEN c1 ELSE c2 FI =>
							if (beval st b) then ceval_step3 st c1 i' else ceval_step3 st c2 i'
					| WHILE b1 DO c1 END =>
							if (beval st b1)
							then match (ceval_step3 st c1 i') with
										| Some st' => ceval_step3 st' c i'
										| None => None
									end
							else Some st
				end
	end.

Definition bind_option {X Y : Type} (xo : option X) (f : X -> option Y) : option Y :=
	match xo with
		| None => None
		| Some x => f x
	end.

Fixpoint ceval_step (st: state) (c:com) (i:nat) : option state :=
	match i with
		| O => None
		| S i' =>
				match c with
					| SKIP => Some st
					| l ::= a1 =>
							Some (update st l (aeval st a1))
					| c1 ; c2 => bind_option (ceval_step st c1 i')
												(fun st' => ceval_step st' c2 i')
					| IFB b THEN c1 ELSE c2 FI =>
							if (beval st b) then ceval_step st c1 i' else ceval_step st c2 i'
					| WHILE b1 DO c1 END =>
							if (beval st b1)
							then bind_option (ceval_step st c1 i')
									  (fun st' => ceval_step st' c i')
							else Some st
				end
	end.

Definition test_ceval (st:state) (c:com) :=
	match ceval_step st c 500 with
		| None => None
		| Some st => Some (st X, st Y, st Z)
	end.

Definition pup_to_n: com :=
	(Y ::= (ANum 0) ;
	 WHILE (BLe (ANum 1) (AId X)) DO
	 	Y ::= (APlus (AId X) (AId Y)) ;
		 	X ::= (AMinus (AId X) (ANum 1))
	END) .

Example pup_to_n_1 :
	test_ceval (update empty_state X 5) pup_to_n
		= Some (0, 15, 0).
Proof. reflexivity. Qed.

Definition ceval_even : com :=
	WHILE (BLe (ANum 2) (AId X)) DO
		X ::= (AMinus (AId X) (ANum 2))
	END ;
	Z ::= (AId X).

Example ceval_even_test1 :
	test_ceval (update empty_state X 20) ceval_even
		= Some (0, 0, 0).
Proof. reflexivity. Qed.

Example ceval_even_test2 :
	test_ceval (update empty_state X 21) ceval_even
		= Some (1, 0, 1).
Proof. reflexivity. Qed.

Example ceval_even_test3 :
	test_ceval (update empty_state X 2) ceval_even
		= Some (0, 0, 0).
Proof. reflexivity. Qed.

Example ceval_even_test4 :
	test_ceval (update empty_state X 1) ceval_even
		= Some (1, 0, 1).
Proof. reflexivity. Qed.

Example ceval_even_test5 :
	test_ceval (update empty_state X 0) ceval_even
		= Some (0, 0, 0).
Proof. reflexivity. Qed.

Reserved Notation "cl '/' st '==>' st'" (at level 40, st at level 39).

Inductive ceval : com -> state -> state -> Prop :=
	| E_Skip : forall st,
		SKIP / st ==> st
	| E_Ass : forall st al n l,
		aeval st al = n ->
			(l ::= al) / st ==> (update st l n)
	| E_Seq : forall c1 c2 st st' st'',
		c1 / st ==> st' ->
			c2 / st' ==> st'' ->
				(c1 ; c2) / st ==> st''
	| E_IfTrue : forall st st' b1 c1 c2,
		beval st b1 = true ->
			c1 / st ==> st' ->
				(IFB b1 THEN c1 ELSE c2 FI) / st ==> st'
	| E_IfFalse : forall st st' b1 c1 c2,
		beval st b1 = false ->
			c2 / st ==> st' ->
				(IFB b1 THEN c1 ELSE c2 FI) / st ==> st'
	| E_WhileEnd : forall b1 st c1,
		beval st b1 = false ->
			(WHILE b1 DO c1 END) / st ==> st
	| E_WhileLoop : forall st st' st'' b1 c1,
		beval st b1 = true ->
			c1 / st ==> st' ->
				(WHILE b1 DO c1 END) / st' ==> st'' ->
					(WHILE b1 DO c1 END) / st ==> st''
	
	where "c1 '/' st '==>' st'" := (ceval c1 st st').

Tactic Notation "ceval_cases" tactic(first) ident(c) :=
	first;
	[ Case_aux c "E_Skip" | Case_aux c "E_Ass" | Case_aux c "E_Seq"
		| Case_aux c "E_IfTrue" | Case_aux c "E_IfFalse"
		| Case_aux c "E_WhileEnd" | Case_aux c "E_WhileLoop" ].

Example ceval_example1 :
	(X ::= ANum 2;
	 IFB BLe (AId X) (ANum 1)
	 	THEN Y ::= ANum 3
		ELSE Z ::= ANum 4
	FI)
	/ empty_state ==> (update (update empty_state X 2) Z 4).
Proof.
(* We must supply the intermediate state *)
	apply E_Seq with (update empty_state X 2).
	Case "assignment command".
	apply E_Ass. reflexivity.
	Case "if command".
	apply E_IfFalse.
	reflexivity.
	apply E_Ass. reflexivity. Qed.

Example ceval_example2:
	(X ::= ANum 0; Y ::= ANum 1; Z ::= ANum 2) / empty_state ==>
		(update (update (update empty_state X 0) Y 1) Z 2).
Proof.
apply E_Seq with (update empty_state X 0);
try (apply E_Ass; reflexivity).
	  
apply E_Seq with (update (update empty_state X 0) Y 1);
try (apply E_Ass; reflexivity).
Qed.

Theorem ceval_step__ceval : forall c st st',
				(exists i, ceval_step st c i = Some st') ->
					c / st ==> st'.
Proof.
intros c st st' H.
inversion H as (i, E).
clear H.
generalize dependent st'.
generalize dependent st.
generalize dependent c.
induction i as [| i'].
 intros c st st' H.
 inversion H.
 
 intros c st st' H.
 com_cases (destruct c) SCase; simpl in H; inversion H; subst; clear H.
  apply E_Skip.
  
  apply E_Ass.
  reflexivity.
  
  remember (ceval_step st c1 i')as r1 in *.
  destruct r1.
   apply E_Seq with s.
    apply IHi'.
    rewrite Heqr1 in |- *.
    reflexivity.
    
    apply IHi'.
    simpl in H1.
    assumption.
    
   inversion H1.
   
  remember (beval st b)as r in *.
  destruct r.
   apply E_IfTrue.
    rewrite Heqr in |- *.
    reflexivity.
    
    apply IHi'.
    assumption.
    
   apply E_IfFalse.
    rewrite Heqr in |- *.
    reflexivity.
    
    apply IHi'.
    assumption.
    
  remember (beval st b)as r in *.
  destruct r.
   remember (ceval_step st c i')as r1 in *.
   destruct r1.
    apply E_WhileLoop with s.
     rewrite Heqr in |- *.
     reflexivity.
     
     apply IHi'.
     rewrite Heqr1 in |- *.
     reflexivity.
     
     apply IHi'.
     simpl in H1.
     assumption.
     
    inversion H1.
    
   inversion H1.
   apply E_WhileEnd.
   rewrite Heqr in |- *.
   subst.
   reflexivity.
Qed.

Theorem ceval_step_more: forall i1 i2 st st' c,
	i1 <= i2 -> ceval_step st c i1 = Some st' ->
		ceval_step st c i2 = Some st'.
Proof.
induction i1 as [| i1']; intros i2 st st' c Hle Hceval.
inversion Hceval.

destruct i2 as [| i2'].
inversion Hle.

assert (i1' <= i2') as Hle' by complete omega.
com_cases (destruct c) SCase.
SCase "SKIP".
simpl in Hceval.
inversion Hceval.
reflexivity.

SCase "::=".
simpl in Hceval.
inversion Hceval.
reflexivity.

SCase ";".
simpl in Hceval.
simpl in |- *.
remember (ceval_step st c1 i1')as st1'o in *.
destruct st1'o.
SSCase "st1'o = Some".
symmetry  in Heqst1'o.
apply (IHi1' i2') in Heqst1'o; try assumption.
rewrite Heqst1'o in |- *.
simpl in |- *.
simpl in Hceval.
apply (IHi1' i2') in Hceval; try assumption.

inversion Hceval.

SCase "IFB".
simpl in Hceval.
simpl in |- *.
remember (beval st b)as bval in *.
destruct bval; apply (IHi1' i2') in Hceval; assumption.

SCase "WHILE".
simpl in Hceval.
simpl in |- *.
destruct (beval st b); try assumption.
remember (ceval_step st c i1')as st1'o in *.
destruct st1'o.
SSCase "st1'o = Some".
symmetry  in Heqst1'o.
apply (IHi1' i2') in Heqst1'o; try assumption.
rewrite Heqst1'o in |- *.
simpl in |- *.
simpl in Hceval.
apply (IHi1' i2') in Hceval; try assumption.

SSCase "i1'o = None".
simpl in Hceval.
inversion Hceval.
Qed.

Theorem ceval__ceval_step : forall c st st',
				c / st ==> st' ->
					exists i, ceval_step st c i = Some st'.
Proof.
	intros c st st' Hce.
	ceval_cases (induction Hce) Case.
	exists 1%nat.
	simpl in |- *.
	reflexivity.

	simpl in |- *.
	exists 1%nat.
	simpl in |- *.
	rewrite H in |- *.
	reflexivity.
	inversion IHHce1 as (x1).
	inversion IHHce2 as (x2).
	exists (S (x1 + x2)).
	simpl in |- *.
	assert (ceval_step st c1 (x1 + x2) = Some st').
	apply ceval_step_more with x1.
	omega.

	assumption.

	rewrite H1 in |- *.
	simpl in |- *.
	apply ceval_step_more with x2.
	omega.

	assumption.

	inversion IHHce.
	exists (S x).
	simpl in |- *.
	simpl in |- *.
	rewrite H in |- *.
	apply H0.

	inversion IHHce.
	exists (S x).
	simpl in |- *.
	simpl in |- *.
	rewrite H in |- *.
	assumption.

	exists 1%nat.
	simpl in |- *.
	rewrite H in |- *.
	reflexivity.

	inversion IHHce1.
	simpl in |- *.
	inversion IHHce2.
	exists (S (x + x0)).
	simpl in |- *.
	simpl in |- *.
	rewrite H in |- *.
	assert (ceval_step st c1 (x + x0) = Some st').
	apply ceval_step_more with x.
	omega.

	assumption.

	rewrite H2 in |- *.
	simpl in |- *.
	apply ceval_step_more with x0.
	omega.

	assumption.
Qed.

Theorem ceval_and_ceval_step_coincide: forall c st st',
				c / st ==> st' <-> exists i, ceval_step st c i = Some st'.
Proof.
	intros c st st'.
	split. apply ceval__ceval_step. apply ceval_step__ceval.
Qed.

Theorem ceval_deterministic : forall c st st1 st2,
	c / st ==> st1 ->
		c / st ==> st2 ->
			st1 = st2.
Proof.
intros c st st1 st2 He1 He2.
apply ceval__ceval_step in He1.
apply ceval__ceval_step in He2.
inversion He1 as (i1, E1).
inversion He2 as (i2, E2).
apply ceval_step_more with (i2 := i1 + i2) in E1.
apply ceval_step_more with (i2 := i1 + i2) in E2.
rewrite E1 in E2.
inversion E2.
reflexivity.

omega.

omega.
Qed.

Theorem plus2_spec : forall st n st',
	st X = n ->
		plus2 / st ==> st' ->
			st' X = n + 2.
Proof.
intros st n st' HX Heval.
inversion Heval.
subst.
apply update_eq.
Qed.


Theorem XtimesYinZ_spec : forall st nx ny st',
		st X = nx ->
			st Y = ny ->
				XtimesYinZ / st ==> st' ->
					st' Z = nx * ny.
Proof.
intros st nx ny st' H1 H2 Heval.
inversion Heval.
subst.  simpl.
apply update_eq.
Qed.

Theorem loop_never_stops : forall st st',
	~(loop / st ==> st').
Proof.
	intros st st' contra. unfold loop in contra.
	remember (WHILE BTrue DO SKIP END) as loopdef.
	induction contra.
	simpl in Heqloopdef.
	inversion Heqloopdef.

	inversion Heqloopdef.

	inversion Heqloopdef.

	inversion Heqloopdef.

	inversion Heqloopdef.

	inversion Heqloopdef.
	subst.
	simpl in H.
	inversion H.

	inversion Heqloopdef.
	subst.
	inversion contra1.
	subst.
	apply IHcontra2.
	reflexivity.
Qed.

Fixpoint no_whiles (c : com) : bool :=
	match c with
		| SKIP => true
		| _ ::= _ => true
		| c1 ; c2 => andb (no_whiles c1) (no_whiles c2)
		| IFB _ THEN ct ELSE cf FI => andb (no_whiles ct) (no_whiles cf)
		| WHILE _ DO _ END => false
	end.

Inductive no_Whiles : com -> Prop :=
	| noWhilesSKIP : no_Whiles (SKIP)
	| noWhilesAss : forall a1 a2, no_Whiles (a1 ::= a2)
	| noWhilesSeq : forall (a1 a2 : com), no_Whiles a1 -> no_Whiles a2 -> no_Whiles (a1 ; a2)
	| noWhilesIf : forall (b : bexp) (a1 a2 : com),
										no_Whiles a1 -> no_Whiles a2 ->
												no_Whiles (IFB b THEN a1 ELSE a2 FI).

Theorem no_whiles_eqv:
	forall c, no_whiles c = true <-> no_Whiles c.
Proof.
split.
induction c.
intros H.
apply noWhilesSKIP.

simpl in |- *.
intros H.
apply noWhilesAss.

intros H.
apply noWhilesSeq.
apply IHc1.
simpl in H.
simpl in H.
auto.
eauto   .
apply andb_true_elim1 in H.
apply H.

apply IHc2.
simpl in H.
apply andb_true_elim2 in H.
assumption.

simpl in |- *.
intros H.
apply noWhilesIf.
apply andb_true_elim1 in H.
apply IHc1.
assumption.

apply IHc2.
apply andb_true_elim2 in H.
assumption.

simpl in |- *.
intros contra.
inversion contra.

intros H.
induction H.
simpl in |- *.
reflexivity.

reflexivity.

simpl in |- *.
assert (no_whiles a1 = true /\ no_whiles a2 = true).
split.
assumption.

assumption.

eauto   .
auto.
apply andb_true_intro.
apply H1.

simpl in |- *.
assert (no_whiles a1 = true /\ no_whiles a2 = true).
split.
assumption.

assumption.

apply andb_true_intro.
assumption.
Qed.

Theorem no_whiles_terminate : forall (c : com) (st:state),
	no_whiles c = true ->
		exists i, exists st', ceval_step st c i = Some st'.
Proof.
induction c.
intros st.
simpl in |- *.
intros H.
exists 1%nat.
exists st.
reflexivity.

simpl in |- *.
intros st.
intros H.
exists 1%nat.
simpl in |- *.
exists (update st i (aeval st a)).
reflexivity.

intros st.
intros H.
inversion H.
assert (no_whiles c1 = true).
apply andb_true_elim1 in H1.
assumption.

assert (no_whiles c2 = true).
apply andb_true_elim2 in H1.
assumption.

apply IHc1 with st in H0.
inversion H0.
inversion H3.
apply IHc2 with x0 in H2.
inversion H2.
inversion H5.
exists (S (x + x1)).
exists x2.
simpl in |- *.
remember (ceval_step st c1 (x + x1))as r in *.
destruct r.
symmetry  in Heqr.
assert (x <= x + x1).
omega.

simpl in |- *.
apply ceval_step_more with x1.
omega.

apply ceval_step_more with (i2 := x + x1) in H4.
assert (Some x0 = Some s).
auto.
eauto   .
rewrite <- Heqr in |- *.
rewrite <- H4 in |- *.
reflexivity.

inversion H8.
rewrite <- H10 in |- *.
assumption.

omega.

simpl in |- *.
assert (x <= x + x1).
omega.

apply ceval_step_more with (i2 := x + x1) in H4.
rewrite H4 in Heqr.
inversion Heqr.

omega.

intros st.
simpl in |- *.
intros H1.
simpl in |- *.
assert (no_whiles c1 = true).
apply andb_true_elim1 in H1.
assumption.

assert (no_whiles c2 = true).
apply andb_true_elim2 in H1.
assumption.

apply IHc1 with st in H.
apply IHc2 with st in H0.
inversion H.
inversion H0.
exists (S (x + x0)).
simpl in |- *.
destruct (beval st b).
inversion H2.
exists x1.
apply ceval_step_more with x.
omega.

assumption.

inversion H3.
exists x1.
apply ceval_step_more with x0.
omega.

assumption.

intros st H.
inversion H.
Qed.

Fixpoint beval_short_circuit (st : state) (e: bexp) : bool :=
	match e with
		| BTrue => true
		| BFalse => false
		| BEq a1 a2 => beq_nat (aeval st a1) (aeval st a2)
		| BLe a1 a2 => ble_nat (aeval st a1) (aeval st a2)
		| BNot b1 => negb (beval_short_circuit st b1)
		| BAnd b1 b2 => match (beval_short_circuit st b1) with
											| true => (beval st b2)
											| false => false
									  end
	end.

Theorem beval_short_circuit_eqv : forall (e : bexp) (st : state),
	beval st e = beval_short_circuit st e.
Proof.
induction e.
intros st.
simpl in |- *.
reflexivity.

reflexivity.

simpl in |- *.
reflexivity.

reflexivity.

intros st.
simpl in |- *.
rewrite IHe in |- *.
reflexivity.

simpl in |- *.
intros st.
rewrite IHe1 in |- *.
rewrite IHe2 in |- *.
destruct (beval_short_circuit st e1).
simpl in |- *.
reflexivity.

reflexivity.
Qed.
		
Inductive sinstr : Type :=
	| SPush : nat -> sinstr
	| SLoad : id -> sinstr
	| SPlus : sinstr
	| SMinus : sinstr
	| SMult : sinstr.

Fixpoint s_execute (st : state) (stack : list nat) (prog : list sinstr)
		: list nat :=
match ( prog ) with
| [] =>stack
|a :: l
  => match (a) with
   | SPush n => s_execute st (cons n stack) l
   | SLoad i => s_execute st (cons (st i) stack) l
   | SPlus  => match stack with
              |[]=>nil
              |a :: al'=> match al' with
                         |[]=>nil
                         |b :: bl'=>s_execute st (cons (a+b) bl') l
                         end
              end
   | SMinus =>  match stack with 
              |[]=>nil
              |a :: al'=> match al' with
                         |[]=>nil
                         |b :: bl'=>s_execute st (cons (b-a) bl') l
                         end
              end
   | SMult => match stack with 
              |[]=>nil
              |a :: al'=> match al' with
                         |[]=>nil
                         |b :: bl'=>s_execute st (cons (a*b) bl') l
                         end
              end 
   end
end.

Example s_execute1 :
	s_execute empty_state [] [SPush 5, SPush 3, SPush 1, SMinus] = [2, 5].
Proof. reflexivity. Qed.

Example s_execute2:
	s_execute (update empty_state X 3) [3,4] [SPush 4, SLoad X, SMult, SPlus ]
		= [15, 4].
Proof. reflexivity. Qed.

Fixpoint s_compile (e : aexp) : list sinstr :=
	match e with
		| ANum v => [SPush v]
		| AId i => [SLoad i]
		| APlus a1 a2 => (s_compile a1) ++ (s_compile a2) ++ [SPlus]
		| AMinus a1 a2 => (s_compile a1) ++ (s_compile a2) ++ [SMinus]
		| AMult a1 a2 => (s_compile a1) ++ (s_compile a2) ++ [SMult]
	end.

Example s_compile1 :
	s_compile (AMinus (AId X) (AMult (ANum 2) (AId Y))) =
		[SLoad X, SPush 2, SLoad Y, SMult, SMinus].
Proof. reflexivity. Qed.

Theorem execute_theorem : forall (e : aexp) (st : state) (s1 : list nat) (other : list sinstr),
		s_execute st s1 (s_compile e ++ other) =
				s_execute st ((aeval st e) :: s1) other.
Proof.
induction e; try reflexivity.
simpl in |- *.
intros st s1 other.
assert
((s_compile e1 ++ s_compile e2 ++ [SPlus]) ++ other =
 s_compile e1 ++ s_compile e2 ++ [SPlus] ++ other).
rewrite -> app_ass.
rewrite -> app_ass.
reflexivity.

rewrite H in |- *.
assert
(s_execute st s1 (s_compile e1 ++ s_compile e2 ++ SPlus :: other) =
 s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SPlus :: other)).
apply IHe1.

simpl in |- *.
rewrite H0 in |- *.
assert
(s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SPlus :: other) =
 s_execute st (aeval st e2 :: aeval st e1 :: s1) (SPlus :: other)).
apply IHe2.

rewrite H1 in |- *.
simpl in |- *.
rewrite plus_comm in |- *.
reflexivity.

intros st s1 other.
assert
(s_execute st s1 (s_compile e1 ++ s_compile e2 ++ SMinus :: other) =
 s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SMinus :: other)).
apply IHe1.

simpl in |- *.
assert
((s_compile e1 ++ s_compile e2 ++ [SMinus]) ++ other =
 s_compile e1 ++ s_compile e2 ++ [SMinus] ++ other).
rewrite -> app_ass.
rewrite -> app_ass.
reflexivity.

simpl in |- *.
rewrite H0 in |- *.
simpl in |- *.
rewrite H in |- *.
assert
(s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SMinus :: other) =
 s_execute st (aeval st e2 :: aeval st e1 :: s1) (SMinus :: other)).
apply IHe2.

rewrite H1 in |- *.
simpl in |- *.
reflexivity.

intros st s1 other.
simpl in |- *.
simpl in |- *.
assert
((s_compile e1 ++ s_compile e2 ++ [SMult]) ++ other =
 s_compile e1 ++ s_compile e2 ++ [SMult] ++ other).
rewrite -> app_ass.
rewrite -> app_ass.
reflexivity.

rewrite H in |- *.
simpl in |- *.
assert
(s_execute st s1 (s_compile e1 ++ s_compile e2 ++ SMult :: other) =
 s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SMult :: other)).
apply IHe1.

rewrite H0 in |- *.
assert
(s_execute st (aeval st e1 :: s1) (s_compile e2 ++ SMult :: other) =
 s_execute st (aeval st e2 :: aeval st e1 :: s1) (SMult :: other)).
apply IHe2.

rewrite H1 in |- *.
simpl in |- *.
rewrite mult_comm in |- *.
reflexivity.
Qed.

Theorem s_compile_correct : forall (st : state) (e : aexp),
				s_execute st [] (s_compile e) = [ aeval st e].
Proof.
intros st e.
assert ([aeval st e] = s_execute st [aeval st e] []).
simpl in |- *.
reflexivity.

rewrite H in |- *.
assert (s_compile e ++ [] = s_compile e).
simpl in |- *.
rewrite -> app_nil_end.
reflexivity.

rewrite <- H0 in |- *.
apply execute_theorem.
Qed.


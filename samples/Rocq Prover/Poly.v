Require Export Lists.
Require Export Basics.

Import Playground1.

Inductive list (X : Type) : Type :=
	| nil : list X
	| cons : X -> list X -> list X.

Fixpoint length (X:Type) (l:list X) : nat :=
	match l with
		| nil => O
		| cons h t => S (length X t)
	end.

Fixpoint app (X : Type) (l1 l2 : list X)
										: (list X) :=
	match l1 with
		| nil => l2
		| cons h t => cons X h (app X t l2)
	end.

Fixpoint snoc (X:Type) (l:list X) (v:X) : (list X) :=
	match l with
		| nil => cons X v (nil X)
		| cons h t => cons X h (snoc X t v)
	end.

Fixpoint rev (X:Type) (l:list X) : list X :=
	match l with
		| nil => nil X
		| cons h t => snoc X (rev X t) h
	end.


Implicit Arguments nil [[X]].
Implicit Arguments cons [[X]].
Implicit Arguments length [[X]].
Implicit Arguments app [[X]].
Implicit Arguments rev [[X]].
Implicit Arguments snoc [[X]].

Definition list123 := cons 1 (cons 2 (cons 3 (nil))).

Notation "x :: y" := (cons x y) (at level 60, right associativity).
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y []) ..).
Notation "x ++ y" := (app x y) (at level 60, right associativity).

Fixpoint repeat (X : Type) (n : X) (count : nat) : list X :=
	match count with
		| O => nil
		| S count' => n :: (repeat _ n count')
	end.

Example test_repeat1:
	repeat bool true (S (S O)) = [true, true].
Proof. reflexivity. Qed.

Theorem nil_app : forall X:Type, forall l:list X,
	app [] l = l.
Proof.
	reflexivity.
	Qed.

Theorem rev_snoc : forall X : Type,
									 forall v : X,
									 forall s : list X,
	rev (snoc s v) = v :: (rev s).
Proof.
	intros X v s.
	induction s.
		reflexivity.
		simpl.
		rewrite -> IHs.
		reflexivity.
		Qed.

Theorem snoc_with_append : forall X : Type,
													 forall l1 l2 : list X,
													 forall v : X,
	snoc (l1 ++ l2) v = l1 ++ (snoc l2 v).
Proof.
	intros X l1 l2 v.
	induction l1.
		reflexivity.
		simpl.
		rewrite -> IHl1.
		reflexivity.
		Qed.

Inductive prod (X Y : Type) : Type :=
	pair : X -> Y -> prod X Y.

Implicit Arguments pair [X Y].

Notation "( x , y )" := (pair x y).
Notation "X * Y" := (prod X Y) : type_scope.

Definition fst (X Y : Type) (p : X * Y) : X :=
	match p with (x,y) => x end.
Definition snd (X Y : Type) (p : X * Y) : Y :=
	match p with (x,y) => y end.

Fixpoint combine (X Y : Type) (lx : list X) (ly : list Y)
			: list (X * Y) :=
	match lx, ly with
		| [], _ => []
		| _,[] => []
		| x::tx, y::ty => (x,y) :: (combine _ _ tx ty)
	end.

Implicit Arguments combine [X Y].

Fixpoint split {X Y: Type} (s : list (X * Y)) : (list X)*(list Y) :=
	match s with
		| nil => (nil, nil)
		| (x,y) :: tp => match split tp with
											| (lx, ly) => (x :: lx, y :: ly)
										 end
	end.

Inductive option (X : Type) : Type :=
	| Some : X -> option X
	| None : option X.

Implicit Arguments Some [X].
Implicit Arguments None [X].

Fixpoint index (X : Type) (n : nat)
							 (l : list X) : option X :=
	match n with
		| O => match l with
								| nil => None
								| x :: xs => Some x
					 end
		| S n' => match l with
									| nil => None
									| x :: xs => index X n' xs
							end
	end.

Definition hd_opt (X : Type) (l : list X) : option X :=
		match l with
			| nil => None
			| x :: xs => Some x
		end.

Implicit Arguments hd_opt [X].

Example test_hd_opt1 : hd_opt [S O, S (S O)] = Some (S O).
Proof. reflexivity. Qed.
Example test_hd_opt2 : hd_opt [[S O], [S (S O)]] = Some [S O].
Proof. reflexivity. Qed.

Definition plus3 := plus (S (S (S O))).

Definition prod_curry {X Y Z : Type}
		(f : X * Y -> Z) (x : X) (y : Y) : Z := f (x,y).

Definition prod_uncurry {X Y Z : Type}
	(f : X -> Y -> Z) (p : X * Y) : Z :=
	f (fst X Y p) (snd X Y p).

Theorem uncurry_uncurry : forall (X Y Z : Type) (f : X -> Y -> Z) x y,
				prod_curry (prod_uncurry f) x y = f x y.
Proof.
	reflexivity.
Qed.

Theorem curry_uncurry : forall (X Y Z : Type) (f : (X * Y) -> Z)
	(p : X * Y),
		prod_uncurry (prod_curry f) p = f p.
Proof.
	destruct p.
	reflexivity.
	Qed.

Fixpoint filter (X : Type) (test : X -> bool) (l:list X)
				: (list X) :=
		match l with
			| [] => []
		  | h :: t => if test h then h :: (filter _ test t)
									else filter _ test t
		end.

Definition countoddmembers' (l:list nat) : nat :=
	length (filter _ oddb l).

Definition partition (X : Type) (test : X -> bool) (l : list X)
							: list X * list X :=
	(filter _ test l, filter _ (fun el => negb (test el)) l).

Example test_partition1: partition _ oddb [S O, S (S O), S (S (S O)), S (S (S (S O))), S (S (S (S (S O))))] = ([S O, S (S (S O)), S (S (S (S (S O))))], [S (S O), S (S (S (S O)))]).
Proof. reflexivity. Qed.

Fixpoint map {X Y : Type} (f : X -> Y) (l : list X) : (list Y ) :=
		match l with
			| [] => []
			| h :: t => (f h) :: (map f t)
		end.

Example test_map1: map (plus (S (S (S O)))) [S (S O), O, S (S O)] = [S (S (S (S (S O)))), S (S (S O)), S (S (S (S (S O))))].
Proof. reflexivity. Qed.

 Theorem map_rev_1 : forall (X Y: Type) (f: X -> Y) (l : list X) (x : X),
 		map f (snoc l x) = snoc (map f l) (f x).
Proof.
	intros X Y f l x.
	induction l.
	reflexivity.
	simpl.
	rewrite -> IHl.
	reflexivity.
	Qed.

Theorem map_rev : forall (X Y : Type) (f : X -> Y) (l : list X),
				map f (rev l) = rev (map f l).
Proof.
	intros X Y f l.
	induction l.
		reflexivity.
		simpl.
		rewrite <- IHl.
		rewrite -> map_rev_1.
		reflexivity.
		Qed.

Fixpoint flat_map {X Y : Type} (f : X -> list Y) (l : list X)
			: (list Y) :=
	match l with
		| [] => []
		| x :: xs => (f x) ++ (flat_map f xs)
	end.

Definition map_option {X Y : Type} (f : X -> Y) (xo : option X)
		: option Y :=
	match xo with
		| None => None
		| Some x => Some (f x)
	end.

Fixpoint fold {X Y: Type} (f: X -> Y -> Y) (l:list X) (b:Y) : Y :=
	match l with
		| nil => b
		| h :: t => f h (fold f t b)
	end.

Example fold_example : fold andb [true, true, false, true] true = false.
Proof. reflexivity. Qed.

Definition constfun {X : Type} (x: X) : nat -> X :=
	fun (k:nat) => x.

Definition ftrue := constfun true.
Example constfun_example : ftrue O = true.
Proof. reflexivity. Qed.

Definition override {X : Type} (f: nat -> X) (k:nat) (x:X) : nat->X :=
	fun (k':nat) => if beq_nat k k' then x else f k'.

Definition fmostlytrue := override (override ftrue (S O) false) (S (S (S O))) false.

Example override_example1 : fmostlytrue O = true.
Proof. reflexivity. Qed.
Example override_example2 : fmostlytrue (S O) = false.
Proof. reflexivity. Qed.
Example override_example3 : fmostlytrue (S (S O)) = true.
Proof. reflexivity. Qed.
Example override_example4 : fmostlytrue (S (S (S O))) = false.
Proof. reflexivity. Qed.

Theorem override_example : forall (b: bool),
	(override (constfun b) (S (S (S O))) true) (S (S O)) = b.
Proof.
	reflexivity.
Qed.

Theorem unfold_example_bad : forall m n,
	(S (S (S O))) + n = m ->
	plus3 n = m.
Proof.
	intros m n H.
	unfold plus3.
	rewrite -> H.
	reflexivity.
	Qed.

Theorem override_eq : forall {X : Type} x k (f : nat -> X),
	(override f k x) k = x.
Proof.
	intros X x k f.
	unfold override.
	rewrite <- beq_nat_refl.
	reflexivity.
	Qed.

Theorem override_neq : forall {X : Type} x1 x2 k1 k2 (f : nat->X),
	f k1 = x1 ->
		beq_nat k2 k1 = false ->
			(override f k2 x2) k1 = x1.
Proof.
	intros X x1 x2 k1 k2 f eq1 eq2.
	unfold override.
	rewrite -> eq2.
	rewrite -> eq1.
	reflexivity.
Qed.

Theorem eq_add_S : forall (n m : nat),
	S n = S m ->
		n = m.
Proof.
	intros n m eq.
	inversion eq.
	reflexivity.
Qed.

Theorem silly4 : forall (n m : nat),
	[n] = [m] ->
		n = m.
Proof.
	intros n o eq.
	inversion eq.
	reflexivity.
Qed.

Theorem silly5 : forall (n m o : nat),
	[n,m] = [o,o] ->
		[n] = [m].
Proof.
	intros n m o eq.
	inversion eq.
	reflexivity.
Qed.

Theorem sillyex1 : forall (X : Type) (x y z : X) (l j : list X),
	x :: y :: l = z :: j ->
		y :: l = x :: j ->
			x = y.
Proof.
	intros X x y z l j.
	intros eq1 eq2.
	inversion eq1.
	inversion eq2.
	symmetry.
	rewrite -> H0.
	reflexivity.
Qed.

Theorem silly6 : forall (n : nat),
	S n = O ->
		(S (S O)) + (S (S O)) = (S (S (S (S (S O))))).
Proof.
	intros n contra.
	inversion contra.
Qed.

Theorem silly7 : forall (n m : nat),
				false = true ->
					[n] = [m].
Proof.
	intros n m contra.
	inversion contra.
Qed.

Theorem sillyex2 : forall (X : Type) (x y z : X) (l j : list X),
	x :: y :: l = [] ->
		y :: l = z :: j ->
			x = z.
Proof.
	intros X x y z l j contra.
	inversion contra.
	Qed.

Theorem beq_nat_eq : forall n m,
	true = beq_nat n m -> n = m.
Proof.
	intros n. induction n as [| n'].
	Case "n = O".
		intros m. destruct m as [| m'].
		SCase "m = 0". reflexivity.
		SCase "m = S m'". simpl. intros contra. inversion contra.
	Case "n = S n'".
		intros m. destruct m as [| m'].
		SCase "m = 0". simpl. intros contra. inversion contra.
		SCase "m = S m'". simpl. intros H.
			assert(n' = m') as Hl.
				SSCase "Proof of assertion". apply IHn'. apply H.
			rewrite -> Hl. reflexivity.
Qed.

Theorem beq_nat_eq' : forall m n,
				beq_nat n m = true -> n = m.
Proof.
	intros m. induction m as [| m'].
	Case "m = O".
		destruct n.
		SCase "n = O".
			reflexivity.
		SCase "n = S n'".
			simpl. intros contra. inversion contra.
	Case "m = S m'".
		simpl.
		destruct n.
		SCase "n = O".
			simpl. intros contra. inversion contra.
		SCase "n = S n'".
			simpl. intros H.
			assert (n = m') as Hl.
				apply IHm'.
				apply H.
			rewrite -> Hl.
			reflexivity.
Qed.

Theorem length_snoc' : forall (X : Type) (v : X)
(l : list X) (n : nat),
	length l = n ->
	length (snoc l v) = S n.
	Proof.
	intros X v l. induction l as [| v' l'].
	Case "l = []". intros n eq. rewrite <- eq. reflexivity.
	Case "l = v' :: l'". intros n eq. simpl. destruct n as [| n'].
	SCase "n = 0". inversion eq.
	SCase "n = S n'".
	assert (length (snoc l' v) = S n').
	SSCase "Proof of assertion". apply IHl'.
	inversion eq. reflexivity.
	rewrite -> H. reflexivity.
	Qed.

	Theorem beq_nat_O_l : forall n,
	true = beq_nat O n -> O = n.
	Proof.
	intros n. destruct n.
	reflexivity.
	simpl.
	intros contra.
	inversion contra.
	Qed.

Theorem beq_nat_O_r : forall n,
	true = beq_nat n O -> O = n.
Proof.
	intros n.
	induction n.
	Case "n = O".
		reflexivity.
	Case "n = S n'".
		simpl.
		intros contra.
		inversion contra.
	Qed.

Theorem double_injective : forall n m,
	double n = double m ->
		n = m.
Proof.
	intros n. induction n as [| n'].
	Case "n = O".
		simpl. intros m eq.
		destruct m as [|m'].
		SCase "m = O". reflexivity.
		SCase "m = S m'". inversion eq.
	Case "n = S n'". intros m eq. destruct m as [| m'].
		SCase "m = O". inversion eq.
		SCase "m = S m'".
			assert(n' = m') as H.
				SSCase "Proof of assertion". apply IHn'. inversion eq. reflexivity.
			rewrite -> H. reflexivity.
Qed.

Theorem silly3' : forall (n : nat),
	(beq_nat n (S (S (S (S (S O))))) = true ->
	 	beq_nat (S (S n)) (S (S (S (S (S (S (S O))))))) = true) ->
		true = beq_nat n (S (S (S (S (S O))))) ->
			true = beq_nat (S (S n)) (S (S (S (S (S (S (S O))))))).
Proof.
	intros n eq H.
	symmetry in H.
	apply eq in H.
	symmetry in H.
	apply H.
Qed.

Theorem plus_n_n_injective : forall n m,
					n + n = m + m ->
						n = m.
Proof.
	intros n. induction n as [| n'].
	Case "n = O".
		simpl. intros m.
		destruct m.
			SCase "m = O".
			reflexivity.
			SCase "m = S m'".
			simpl.
			intros contra.
			inversion contra.
	Case "n = S n".
		intros m.
		destruct m.
			SCase "m = O".
			intros contra.
			inversion contra.
			SCase "m = S m'".
			intros eq.
			inversion eq.
			rewrite <- plus_n_Sm in H0.
			rewrite <- plus_n_Sm in H0.
			inversion H0.
			apply IHn' in H1.
			rewrite -> H1.
			reflexivity.
	Qed.

Theorem override_shadow : forall {X : Type} x1 x2 k1 k2 (f : nat -> X),
	(override (override f k1 x2) k1 x1) k2 = (override f k1 x1) k2.
Proof.
	intros X x1 x2 k1 k2 f.
	unfold override.
	destruct (beq_nat k1 k2).
	reflexivity.
	reflexivity.
	Qed.

Theorem combine_split : forall (X : Type) (Y : Type) (l : list (X * Y)) (l1: list X) (l2: list Y),
				split l = (l1, l2) -> combine l1 l2 = l.
Proof.
	intros X Y l.
	induction l as [| x y].
	Case "l = nil".
		intros l1 l2.
		intros eq.
		simpl.
		simpl in eq.
		inversion eq.
		reflexivity.
	Case "l = ::".
		intros l1 l2.
		simpl.
		destruct x.
		destruct (split y).
		simpl.
		destruct l1.
		SCase "l1 = []".
			simpl.
			induction l2.
			SSCase "l2 = []".
				intros contra.
				inversion contra.
			SSCase "l2 = ::".
				intros contra.
				inversion contra.
		SCase "l1 = ::".
			induction l2.
			SSCase "l2 = []".
				simpl.
				intros contra.
				inversion contra.
			SSCase "l2 = ::".
				simpl.
				intros eq.
				inversion eq.
				simpl.
				rewrite IHy.
				reflexivity.
				simpl.
				rewrite H1.
				rewrite H3.
				reflexivity.
Qed.

Theorem split_combine : forall (X : Type) (Y : Type) (l1: list X) (l2: list Y),
				length l1 = length l2 -> split (combine l1 l2) = (l1, l2).
Proof.
intros X Y.
intros l1.
induction l1.
simpl.
intros l2.
induction l2.
reflexivity.

intros contra.
inversion contra.

destruct l2.
simpl.
intros contra.
inversion contra.

simpl.
intros eq.
inversion eq.
apply IHl1 in H0.
rewrite H0.
reflexivity.
Qed.

Definition sillyfun1 (n : nat) : bool :=
	 if beq_nat n (S (S (S O))) then true
	 else if beq_nat n (S (S (S (S (S O))))) then true
				else false.

Theorem beq_equal : forall (a b : nat),
		beq_nat a b = true ->
			a = b.
Proof.
intros a.
induction a.
destruct b.
reflexivity.

intros contra.
inversion contra.

destruct b.
intros contra.
inversion contra.

simpl.
intros eq.
apply IHa in eq.
rewrite eq.
reflexivity.
Qed.

Theorem override_same : forall {X : Type} x1 k1 k2 (f : nat->X),
	f k1 = x1 ->
		(override f k1 x1) k2 = f k2.
Proof.
	intros X x1 k1 k2 f eq.
	unfold override.
	remember (beq_nat k1 k2) as a.
	destruct a.
	rewrite <- eq.
	symmetry in Heqa.
	apply beq_equal in Heqa.
	rewrite -> Heqa.
	reflexivity.
	reflexivity.
	Qed.


Theorem filter_exercise : forall (X : Type) (test : X -> bool)
	(x : X) (l lf : list X),
	filter _ test l = x :: lf ->
		test x = true.
Proof.
intros X.
intros test.
intros x.
induction l.
simpl.
intros lf.
intros contra.
inversion contra.

simpl.
remember (test x0) as a.
destruct a.
simpl.
intros lf.
intros eq.
rewrite Heqa.
inversion eq.
reflexivity.

intros lf.
intros eq.
apply IHl in eq.
rewrite eq.
reflexivity.
Qed.

Theorem trans_eq : forall {X:Type} (n m o : X),
				n = m -> m = o -> n = o.
Proof.
	intros X n m o eq1 eq2. rewrite -> eq1. rewrite -> eq2.
	reflexivity.
Qed.

Example trans_eq_example' : forall (a b c d e f : nat),
				[a,b] = [c,d] ->
				[c,d] = [e,f] ->
				[a,b] = [e,f].
Proof.
	intros a b c d e f eq1 eq2.
	apply trans_eq with (m := [c,d]). apply eq1. apply eq2.
	Qed.

Theorem trans_eq_exercise : forall (n m o p : nat),
		m = (minustwo o) ->
		(n + p) = m ->
		(n + p) = (minustwo o).
Proof.
intros n m o p.
intros eq1 eq2.
rewrite eq2.
rewrite <- eq1.
reflexivity.
Qed.

Theorem beq_nat_trans : forall n m p,
				true = beq_nat n m ->
				true = beq_nat m p ->
				true = beq_nat n p.
Proof.
intros n m p.
intros eq1 eq2.
symmetry  in eq1.
symmetry  in eq2.
apply beq_equal in eq1.
apply beq_equal in eq2.
rewrite eq1.
rewrite <- eq2.
apply beq_nat_refl.
Qed.

Theorem override_permute : forall {X:Type} x1 x2 k1 k2 k3 (f : nat->X),
	false = beq_nat k2 k1 ->
	(override (override f k2 x2) k1 x1) k3 = (override (override f k1 x1) k2 x2) k3.
Proof.
intros X x1 x2 k1 k2 k3 f.
simpl.
unfold override.
remember (beq_nat k1 k3).
remember (beq_nat k2 k3).
destruct b.
destruct b0.
symmetry  in Heqb.
symmetry  in Heqb0.
apply beq_equal in Heqb.
apply beq_equal in Heqb0.
rewrite <- Heqb in Heqb0.
assert (k2 = k1 -> true = beq_nat k2 k1).
destruct k2.
destruct k1.
reflexivity.

intros contra.
inversion contra.

destruct k1.
intros contra.
inversion contra.

simpl.
intros eq.
inversion eq.
symmetry .
symmetry .
apply beq_nat_refl.

apply H in Heqb0.
rewrite <- Heqb0.
intros contra.
inversion contra.

intros eq.
reflexivity.

destruct b0.
intros eq.
reflexivity.

intros eq.
reflexivity.

Qed.

Definition fold_length {X : Type} (l : list X) : nat :=
	fold (fun _ n => S n) l O.

Example test_fold_length1 : fold_length [S (S (S (S O))), S (S (S (S (S (S (S O)))))), O] = S (S (S O)).
Proof. reflexivity. Qed.

Theorem fold_length_correct : forall X (l :list X),
	fold_length l = length l.
Proof.
	intros X l.
	unfold fold_length.
	induction l.
	Case "l = O".
		reflexivity.
	Case "l = ::".
		simpl.
		rewrite IHl.
		reflexivity.
	Qed.

Definition fold_map {X Y: Type} (f : X -> Y) (l : list X) : list Y :=
	fold (fun x total => (f x) :: total) l [].

Theorem fold_map_correct : forall (X Y: Type) (f : X -> Y) (l : list X),
	fold_map f l = map f l.
Proof.
	intros X Y f l.
	unfold fold_map.
	induction l.
	reflexivity.
	  
	simpl.
	rewrite IHl.
	reflexivity.
	Qed.

Fixpoint forallb {X : Type} (f : X -> bool) (l : list X) :=
	match l with
		| nil => true
		| x :: xs => andb (f x) (forallb f xs)
	end.

Fixpoint existsb {X : Type} (f : X -> bool) (l : list X) :=
	match l with
		| nil => false
		| x :: xs => orb (f x) (existsb f xs)
	end.

Definition existsb2 {X : Type} (f: X -> bool) (l : list X) :=
	negb (forallb (fun x => negb (f x)) l).

Theorem existsb_correct : forall (X : Type) (f : X -> bool) (l : list X),
		existsb f l = existsb2 f l.
Proof.
intros X f l.
induction l.
reflexivity.

simpl.
rewrite IHl.
unfold existsb2.
simpl.
destruct (forallb (fun x0 : X => negb (f x0)) l).
simpl.
destruct (f x).
reflexivity.

reflexivity.

destruct (f x).
reflexivity.

reflexivity.

Qed.

Theorem index_okx : forall (X:Type) (l : list X) (n : nat),
		length l = n -> index X (S n) l = None.
Proof.
	intros X l.
	induction l.
	reflexivity.

	intros n.
	destruct n.
	intros contra.
	inversion contra.

	intros eq.
	inversion eq.
	apply IHl.
	reflexivity.
Qed.

Inductive mumble : Type :=
  | a : mumble
	| b : mumble -> nat -> mumble
	| c : mumble.
Inductive grumble (X:Type) : Type :=
 | d : mumble -> grumble X
 | e : X -> grumble X.


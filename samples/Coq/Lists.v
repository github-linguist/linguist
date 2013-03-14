Require Export Basics.
Module NatList.
Import Playground1.

Inductive natprod : Type :=
	pair : nat -> nat -> natprod.

Definition fst (p : natprod) : nat :=
	match p with
		| pair x y => x
	end.

Definition snd (p : natprod) : nat :=
	match p with
		| pair x y => y
	end.

Notation "( x , y )" := (pair x y).

Definition swap_pair (p : natprod) : natprod :=
	match p with
			| (x, y) => (y, x)
	end.

Theorem surjective_pairing' : forall (n m : nat),
	(n, m) = (fst (n, m), snd (n, m)).
Proof.
	reflexivity.  Qed.

Theorem surjective_pairing : forall (p : natprod),
	p = (fst p, snd p).
Proof.
	intros p.
	destruct p as (n, m).
	simpl.
	reflexivity.
	Qed.

Theorem snd_fst_is_swap : forall (p : natprod),
	(snd p, fst p) = swap_pair p.
Proof.
	intros p.
	destruct p.
	reflexivity.
	Qed.

Theorem fst_swap_is_snd : forall (p : natprod),
	 fst (swap_pair p) = snd p.
Proof.
	intros p.
	destruct p.
	reflexivity.
	Qed.

Inductive natlist : Type :=
	| nil : natlist
	| cons : nat -> natlist -> natlist.

Definition l_123 := cons (S O) (cons (S (S O)) (cons (S (S (S O))) nil)).

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).

Fixpoint repeat (n count : nat) : natlist :=
	match count with
		| O => nil
		| S count' => n :: (repeat n count')
	end.

Fixpoint length (l:natlist) : nat :=
	match l with
		| nil => O
		| h :: t => S (length t)
	end.

Fixpoint app (l1 l2 : natlist) : natlist :=
	match l1 with
		| nil => l2
		| h :: t => h :: (app t l2)
	end.

Notation "x ++ y" := (app x y) (right associativity, at level 60).

(*
Example test_app1: [1,2,3] ++ [4,5] = [1,2,3,4,5].
Proof. reflexivity. Qed.
Example test_app2: nil ++ [4,5] = [4,5].
Proof. reflexivity. Qed.
Example test_app3: [1,2,3] ++ [] = [1,2,3].
Proof. reflexivity. Qed.
*)

Definition head (l : natlist) : nat :=
	match l with
		| nil => O
		| h :: t => h
	end.

Definition tl (l : natlist) : natlist :=
	match l with
		| nil => nil
		| h :: t => t
	end.

			(*
Example test_tl: tl [1,2,3] = [2,3].
Proof. reflexivity. Qed.
*)

Fixpoint nonzeros (l:natlist) : natlist :=
	match l with
		| nil => nil
		| O :: r => nonzeros r
		| n :: r => n :: (nonzeros r)
	end.

Example test_nonzeros: nonzeros [O,S O,O,S (S O), S (S (S O)),O,O] = [S O,S (S O), S (S (S O))].
Proof. reflexivity. Qed.

Fixpoint oddmembers (l:natlist) : natlist :=
	match l with
		| nil => nil
		| n :: r => match (oddb n) with
										| true => n :: (oddmembers r)
										| false => oddmembers r
								end
	end.

Example test_oddmembers: oddmembers [O, S O, O, S (S O), S (S (S O)), O, O] = [S O, S (S (S O))].
Proof. reflexivity. Qed.

Fixpoint countoddmembers (l:natlist) : nat :=
		length (oddmembers l).

Example test_countoddmembers2: countoddmembers [O, S (S O), S (S (S (S O)))] = O.
Proof. reflexivity. Qed.

Example test_countoddmembers3: countoddmembers [] = O.
Proof. reflexivity. Qed.

Fixpoint alternate (l1 l2 : natlist) : natlist :=
		match l1 with
				| nil => l2
				| a :: r1 => match l2 with
											| nil => l1
											| b :: r2 => a :: b :: (alternate r1 r2)
										end
		end.

Example test_alternative1: alternate [S O, S (S O), S (S (S O))] [S (S (S (S O))), S (S (S (S (S O)))), S (S (S (S (S (S O)))))] =
		[S O, S (S (S (S O))), S (S O), S (S (S (S (S O)))), S (S (S O)), S (S (S (S (S (S O)))))].
Proof. reflexivity. Qed.

Definition bag := natlist.

Fixpoint count (v : nat) (s: bag) : nat :=
	match s with
		| nil => O
		| v' :: r => match (beq_nat v' v) with
										| true => S (count v r)
										| false => count v r
								 end
	end.

Example test_count1: count (S O) [S O, S (S O), S (S (S O)), S O, S (S (S (S O))), S O] = S (S (S O)).
Proof. reflexivity. Qed.

Definition sum : bag -> bag -> bag := app.

Example test_sum1: count (S O) (sum [S O, S (S O), S (S (S O))] [S O, S (S (S (S O))), S O]) = S (S (S O)).
Proof. reflexivity. Qed.

Definition add (v:nat) (s:bag) : bag := v :: s.

Example test_add1: count (S O) (add (S O) [S O, S (S (S (S O))), S O]) = S (S (S O)).
Proof. reflexivity. Qed.

Definition member (v:nat) (s:bag) : bool :=
	ble_nat (S O) (count v s).

Example test_member1: member (S O) [S O, S (S (S (S O))), S O] = true.
Proof. reflexivity. Qed.

Example test_member2: member (S (S O)) [S O, S (S (S (S O))), S O] = false.
Proof. reflexivity. Qed.

Fixpoint remove_one (v:nat) (s:bag) : bag :=
	match s with
		| nil => nil
		| v' :: r => match (beq_nat v v') with
									| true => r
									| false => v' :: (remove_one v r)
								 end
	end.

Example test_remove_one1: count (S (S (S (S (S O)))))
																(remove_one (S (S (S (S (S O)))))
																[S (S O), S O, S (S (S (S (S O)))), S (S (S (S O))), S O]) = O.
Proof. reflexivity. Qed.

Fixpoint remove_all (v:nat) (s:bag) : bag :=
	match s with
		| nil => nil
		| v' :: r => match (beq_nat v v') with
										| true => remove_all v r
										| false => v' :: (remove_all v r)
								 end
	end.

Example test_remove_all1: count (S (S (S (S (S O)))))
																(remove_all (S (S (S (S (S O)))))
																[S (S O), S O, S (S (S (S (S O)))), S (S (S (S O))), S O]) = O.
Proof. reflexivity. Qed.

Fixpoint subset (s1:bag) (s2:bag) : bool :=
		match s1 with
			| nil => true
			| v :: r => andb (member v s2)
											 (subset r (remove_one v s2))
		end.

Definition test_subset1: subset [S O, S (S O)] [S (S O), S O, S (S (S (S O))), S O] = true.
Proof. reflexivity. Qed.
Definition test_subset2: subset [S O, S (S O), S (S O)] [S (S O), S O, S (S (S (S O))), S O] = false.
Proof. reflexivity. Qed.

Theorem bag_count_add : forall n t: nat, forall s : bag,
				count n s = t -> count n (add n s) = S t.
Proof.
	intros n t s.
	intros H.
	induction s.
	simpl.
	rewrite <- beq_nat_refl.
	rewrite <- H.
	reflexivity.
	rewrite <- H.
	simpl.
	rewrite <- beq_nat_refl.
	reflexivity.
Qed.

Theorem nil_app : forall l:natlist,
	[] ++ l = l.
Proof.
	reflexivity. Qed.

Theorem tl_length_pred : forall l:natlist,
	pred (length l) = length (tl l).
Proof.
	intros l. destruct l as [| n l'].
	Case "l = nil".
		reflexivity.
	Case "l = cons n l'".
		reflexivity. Qed.

Theorem app_ass:forall l1 l2 l3 : natlist,
	(l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
	intros l1 l2 l3. induction l1 as [| n l1'].
	Case "l1 = nil".
		reflexivity.
	Case "l1 = cons n l1'".
		simpl. rewrite -> IHl1'. reflexivity. Qed.

Theorem app_length: forall l1 l2 : natlist,
	length (l1 ++ l2) = (length l1) + (length l2).
Proof.
	intros l1 l2. induction l1 as [| n l1'].
	Case "l1 = nil".
		reflexivity.
	Case "l1 = cons".
		simpl. rewrite -> IHl1'. reflexivity. Qed.

Fixpoint snoc (l:natlist) (v:nat) : natlist :=
	match l with
		| nil => [v]
		| h :: t => h :: (snoc t v)
	end.

Fixpoint rev (l:natlist) : natlist :=
	match l with
		| nil => nil
		| h :: t => snoc (rev t) h
	end.

Example test_rev1: rev [S O, S (S O), S (S (S O))] = [S (S (S O)), S (S O), S O].
Proof. reflexivity. Qed.

Theorem length_snoc : forall n : nat, forall l : natlist,
	length (snoc l n) = S (length l).
Proof.
	intros n l. induction l as [| n' l'].
	Case "l = nil".
		reflexivity.
	Case "l = cons n' l'".
		simpl. rewrite -> IHl'. reflexivity. Qed.

Theorem rev_length : forall l : natlist,
	length (rev l) = length l.
Proof.
	intros l. induction l as [| n l'].
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl. rewrite -> length_snoc.
		rewrite -> IHl'. reflexivity. Qed.

Theorem app_nil_end : forall l :natlist,
	l ++ [] = l.
Proof.
	intros l.
	induction l.
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl. rewrite -> IHl. reflexivity. Qed.



Theorem rev_snoc : forall l: natlist, forall n : nat,
	rev (snoc l n) = n :: (rev l).
Proof.
	intros l n.
	induction l.
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl.
		rewrite -> IHl.
		reflexivity.
		Qed.

Theorem rev_involutive : forall l : natlist,
	rev (rev l) = l.
Proof.
	intros l.
	induction l.
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl.
		rewrite -> rev_snoc.
		rewrite -> IHl.
		reflexivity.
		Qed.

Theorem app_ass4 : forall l1 l2 l3 l4 : natlist,
	l1 ++ (l2 ++ (l3 ++ l4)) = ((l1 ++ l2) ++ l3) ++ l4.
Proof.
	intros l1 l2 l3 l4.
	rewrite -> app_ass.
	rewrite -> app_ass.
	reflexivity.
	Qed.

Theorem snoc_append : forall (l : natlist) (n : nat),
	snoc l n = l ++ [n].
Proof.
	intros l n.
	induction l.
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl.
		rewrite -> IHl.
		reflexivity.
		Qed.

Theorem nonzeros_length : forall l1 l2 : natlist,
	nonzeros (l1 ++ l2) = (nonzeros l1) ++ (nonzeros l2).
Proof.
	intros l1 l2.
	induction l1.
	Case "l1 = nil".
		reflexivity.
	Case "l1 = cons".
		simpl.
		rewrite -> IHl1.
		destruct n.
		reflexivity.
		reflexivity.
		Qed.

Theorem distr_rev : forall l1 l2 : natlist,
	rev (l1 ++ l2) = (rev l2) ++ (rev l1).
Proof.
	intros l1 l2.
	induction l1.
	Case "l1 = nil".
		simpl.
		rewrite -> app_nil_end.
		reflexivity.
	Case "l1 = cons".
		simpl.
		rewrite -> IHl1.
		simpl.
		rewrite -> snoc_append.
		rewrite -> snoc_append.
		rewrite -> app_ass.
		reflexivity.
		Qed.

Theorem count_number_nonzero : forall (s : bag),
	ble_nat O (count (S O) (S O :: s)) = true.
Proof.
	intros s.
	induction s.
		reflexivity.
		reflexivity.
		Qed.

Theorem ble_n_Sn : forall n,
	ble_nat n (S n) = true.
Proof.
	intros n. induction n as [| n'].
	Case "0".
		simpl. reflexivity.
	Case "S n'".
		simpl. rewrite -> IHn'. reflexivity. Qed.

Theorem remove_decreases_count: forall (s : bag),
	ble_nat (count O (remove_one O s)) (count O s) = true.
Proof.
	intros s.
	induction s.
	Case "s = nil".
		reflexivity.
	Case "s = cons".
		simpl.
		induction n.
		SCase "n = O".
			simpl.  rewrite -> ble_n_Sn.
			reflexivity.
		SCase "n = S n'".
			simpl.
			rewrite -> IHs.
			reflexivity.
			Qed.

Inductive natoption : Type :=
	| Some : nat -> natoption
	| None : natoption.

Fixpoint index (n:nat) (l:natlist) : natoption :=
	match l with
		| nil => None
		| a :: l' => if beq_nat n O then Some a else index (pred n) l'
  end.

Definition option_elim (o : natoption) (d : nat) : nat :=
	match o with
		| Some n' => n'
		| None => d
	end.

Definition hd_opt (l : natlist) : natoption :=
	match l with
		| nil => None
		| v :: r => Some v
	end.

Example test_hd_opt1 : hd_opt [] = None.
Proof. reflexivity. Qed.
Example test_hd_opt2 : hd_opt [S O] = Some (S O).
Proof. reflexivity. Qed.

Theorem option_elim_hd : forall l:natlist,
	head l = option_elim (hd_opt l) O.
Proof.
	intros l.
	destruct l.
	reflexivity.
	reflexivity.
	Qed.

Fixpoint beq_natlist (l1 l2 : natlist) : bool :=
	match l1 with
		| nil => match l2 with
							| nil => true
							| _ => false
						 end
		| v1 :: r1 => match l2 with
									 | nil => false
									 | v2 :: r2 => if beq_nat v1 v2 then beq_natlist r1 r2
																 else false
									end
	end.

Example test_beq_natlist1 : (beq_natlist nil nil = true).
Proof. reflexivity. Qed.
Example test_beq_natlist2 : (beq_natlist [S O, S (S O), S (S (S O))]
																				 [S O, S (S O), S (S (S O))] = true).
Proof. reflexivity. Qed.

Theorem beq_natlist_refl : forall l:natlist,
	beq_natlist l l = true.
Proof.
	intros l.
	induction l.
	Case "l = nil".
		reflexivity.
	Case "l = cons".
		simpl.
		rewrite <- beq_nat_refl.
		rewrite -> IHl.
		reflexivity.
	Qed.

Theorem silly1 : forall (n m o p : nat),
	n = m -> [n, o] = [n, p] -> [n, o] = [m, p].
Proof.
	intros n m o p eq1 eq2.
	rewrite <- eq1.
	apply eq2. Qed.

Theorem silly2a : forall (n m : nat),
	(n,n) = (m,m) ->
		(forall (q r : nat), (q, q) = (r, r) -> [q] = [r]) ->
			[n] = [m].
Proof.
	intros n m eq1 eq2.
	apply eq2.
	apply eq1.
	Qed.

Theorem silly_ex :
	(forall n, evenb n = true -> oddb (S n) = true) ->
	evenb (S (S (S O))) = true ->
	oddb (S (S (S (S O)))) = true.
Proof.
	intros eq1 eq2.
	apply eq1.
	apply eq2.
	Qed.

Theorem silly3 : forall (n : nat),
	true = beq_nat n (S (S (S (S (S O))))) ->
	beq_nat (S (S n)) (S (S (S (S (S (S (S O))))))) = true.
Proof.
	intros n H.
	symmetry.
	apply H.
	Qed.

Theorem rev_exercise : forall (l l' : natlist),
	l = rev l' -> l' = rev l.
Proof.
	intros l l' H.
	rewrite -> H.
	rewrite -> rev_involutive.
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

Theorem app_ass' : forall l1 l2 l3 : natlist,
	(l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
	intros l1. induction l1 as [ | n l1'].
	reflexivity.
	simpl.
	intros l2 l3.
	rewrite -> IHl1'.
	reflexivity.
	Qed.

End NatList.

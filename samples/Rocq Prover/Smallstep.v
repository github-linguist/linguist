Require Export Imp.
Require Export Relations.

Inductive tm : Type :=
   | tm_const : nat -> tm
   | tm_plus : tm -> tm -> tm.

Tactic Notation "tm_cases" tactic(first) ident(c) :=
   first;
   [ Case_aux c "tm_const" | Case_aux c "tm_plus" ].

Module SimpleArith0.

Fixpoint eval (t : tm) : nat :=
   match t with
      | tm_const n => n
      | tm_plus a1 a2 => eval a1 + eval a2
   end.

End SimpleArith0.

Module SimpleArith1.

Reserved Notation " t '===>' n " (at level 50, left associativity).

Inductive eval : tm -> nat -> Prop :=
   | E_Const : forall n,
                  tm_const n ===> n
   | E_Plus : forall t1 t2 n1 n2,
                  t1 ===> n1 ->
                  t2 ===> n2 ->
                  tm_plus t1 t2 ===> plus n1 n2

     where " t '===>' n " := (eval t n).

End SimpleArith1.

Reserved Notation " t '===>' t' " (at level 50, left associativity).

Inductive eval : tm -> tm -> Prop :=
   | E_Const : forall n1,
      tm_const n1 ===> tm_const n1
   | E_Plus : forall t1 n1 t2 n2,
      t1 ===> tm_const n1 ->
      t2 ===> tm_const n2 ->
         tm_plus t1 t2 ===> tm_const (plus n1 n2)
   where " t '===>' t' " := (eval t t').

Tactic Notation "eval_cases" tactic(first) ident(c) :=
   first;
   [ Case_aux c "E_Const" | Case_aux c "E_Plus" ].

Module SimpleArith2.

Reserved Notation " t '=>' t' " (at level 40).

Inductive step : tm -> tm -> Prop :=
   | ST_PlusConstConst : forall n1 n2,
      tm_plus (tm_const n1) (tm_const n2) => tm_const (plus n1 n2)
   | ST_Plus1 : forall t1 t1' t2,
      t1 => t1' ->
         tm_plus t1 t2 => tm_plus t1' t2
   | ST_Plus2 : forall n1 t2 t2',
      t2 => t2' ->
      tm_plus (tm_const n1) t2 => tm_plus (tm_const n1) t2'

   where " t '=>' t' " := (step t t').

Tactic Notation "step_cases" tactic(first) ident(c) :=
   first;
   [ Case_aux c "ST_PlusConstConst"
   | Case_aux c "ST_Plus1" | Case_aux c "ST_Plus2" ].

Example test_step_1 : 
      tm_plus 
        (tm_plus (tm_const 0) (tm_const 3))
        (tm_plus (tm_const 2) (tm_const 4))
      =>
      tm_plus 
        (tm_const (plus 0 3))
        (tm_plus (tm_const 2) (tm_const 4)).
Proof.
  apply ST_Plus1. apply ST_PlusConstConst. Qed.

Example test_step_2 : 
      tm_plus 
        (tm_const 0)
        (tm_plus 
          (tm_const 2) 
          (tm_plus (tm_const 0) (tm_const 3)))
      =>
      tm_plus 
        (tm_const 0)
        (tm_plus 
          (tm_const 2) 
          (tm_const (plus 0 3))).
Proof.
apply ST_Plus2.
simpl.
apply ST_Plus2.
apply ST_PlusConstConst.
Qed.

Theorem step_deterministic:
  partial_function step.
Proof.
  unfold partial_function. intros x y1 y2 Hy1 Hy2.
  generalize dependent y2.
  step_cases (induction Hy1) Case; intros y2 Hy2.
    Case "ST_PlusConstConst". step_cases (inversion Hy2) SCase.
      SCase "ST_PlusConstConst". reflexivity.
      SCase "ST_Plus1". inversion H2.
      SCase "ST_Plus2". inversion H2.
    Case "ST_Plus1". step_cases (inversion Hy2) SCase.
      SCase "ST_PlusConstConst". rewrite <- H0 in Hy1. inversion Hy1.
      SCase "ST_Plus1".
        rewrite <- (IHHy1 t1'0).
        reflexivity. assumption.
      SCase "ST_Plus2". rewrite <- H in Hy1. inversion Hy1.
    Case "ST_Plus2". step_cases (inversion Hy2) SCase.
      SCase "ST_PlusConstConst". rewrite <- H1 in Hy1. inversion Hy1.
      SCase "ST_Plus1". inversion H2.
      SCase "ST_Plus2".
        rewrite <- (IHHy1 t2'0).
        reflexivity. assumption. Qed.

End SimpleArith2.

Inductive value : tm -> Prop :=
   v_const: forall n, value (tm_const n).

Reserved Notation " t '=>' t' " (at level 40).

Inductive step : tm -> tm -> Prop :=
   | ST_PlusConstConst : forall n1 n2,
      tm_plus (tm_const n1) (tm_const n2)
         => tm_const (plus n1 n2)
   | ST_Plus1 : forall t1 t1' t2,
      t1 => t1' ->
         tm_plus t1 t2 => tm_plus t1' t2
   | ST_Plus2 : forall v1 t2 t2',
      value v1 ->
         t2 => t2' ->
            tm_plus v1 t2 => tm_plus v1 t2'

   where " t '=>' t' " := (step t t').

Tactic Notation "step_cases" tactic(first) ident(c) :=
   first;
   [ Case_aux c "ST_PlusConstConst"
      | Case_aux c "ST_Plus1" | Case_aux c "ST_Plus2" ].

Theorem step_deterministic :
   partial_function step.
Proof.
unfold partial_function.
intros x y1 y2 Hy1 Hy2.
generalize dependent y2.
step_cases (induction Hy1) Case; intros y2 Hy2.
 step_cases (inversion Hy2) SCase.
  reflexivity.
  
  inversion H2.
  
  inversion Hy2.
   subst.
   assumption.
   
   subst.
   inversion H3.
   
   subst.
   inversion H3.
   
 step_cases (inversion Hy2) SCase.
  rewrite <- H0 in Hy1.
  inversion Hy1.
  
  rewrite <- (IHHy1 t1'0).
   reflexivity.
   
   assumption.
   
  rewrite <- H in Hy1.
  rewrite <- H in H1.
  subst.
  inversion H1.
  subst.
  inversion Hy1.
  
 step_cases (inversion Hy2) SCase.
  subst.
  inversion Hy1.
  
  subst.
  inversion H.
  subst.
  inversion H3.
  
  subst.
  inversion H2.
  subst.
  rewrite <- (IHHy1 t2'0).
   reflexivity.
   
   assumption.
Qed.

Theorem strong_progress : forall t,
        value t \/ (exists t', t => t').
Proof.
   tm_cases (induction t) Case.
      Case "tm_const". left. apply v_const.
      Case "tm_plus". right. inversion IHt1.
         SCase "l". inversion IHt2.
            SSCase "l". inversion H. inversion H0.
               exists (tm_const (plus n n0)).
               apply ST_PlusConstConst.
            SSCase "r". inversion H0 as [t' H1].
               exists (tm_plus t1 t').
               apply ST_Plus2. apply H. apply H1.
         SCase "r". inversion H as [t' H0].
            exists (tm_plus t' t2).
            apply ST_Plus1. apply H0. Qed.

Definition normal_form {X:Type} (R: relation X) (t: X) : Prop :=
    ~ (exists t', R t t'). 

Lemma value_is_nf: forall t,
      value t -> normal_form step t.
Proof.
   unfold normal_form. intros t H. inversion H.
   intros contra. inversion contra. inversion H1.
   Qed.

Lemma nf_is_value: forall t,
      normal_form step t -> value t.
Proof.
   unfold normal_form. intros t H.
   assert (G: value t \/ (exists t', t => t')).
      SCase "Proof of assertion". apply strong_progress.
   inversion G.
      SCase "l". assumption.
      SCase "r". apply ex_falso_quodlibet. apply H. assumption. Qed.

Corollary nf_same_as_value : forall t,
            normal_form step t <-> value t.
Proof.
   split. apply nf_is_value. apply value_is_nf.
Qed.

Module Temp1.

Inductive value : tm -> Prop :=
| v_const : forall n, value (tm_const n)
| v_funny : forall t1 n2, (* <---- *)
              value (tm_plus t1 (tm_const n2)).

Reserved Notation " t '=>' t' " (at level 40).

Inductive step : tm -> tm -> Prop :=
  | ST_PlusConstConst : forall n1 n2,
      tm_plus (tm_const n1) (tm_const n2) => tm_const (plus n1 n2)
  | ST_Plus1 : forall t1 t1' t2,
      t1 => t1' ->
      tm_plus t1 t2 => tm_plus t1' t2
  | ST_Plus2 : forall v1 t2 t2',
      value v1 ->
      t2 => t2' ->
      tm_plus v1 t2 => tm_plus v1 t2'

  where " t '=>' t' " := (step t t').

Lemma value_not_same_as_normal_form:
   exists t, value t /\ ~ normal_form step t.
Proof.
intros.
unfold normal_form.
exists (tm_plus (tm_plus (tm_const 1) (tm_const 2)) (tm_const 2)).
split.
 apply v_funny.
 
 unfold not.
 intros.
 apply H.
 exists (tm_plus (tm_const (1 + 2)) (tm_const 2)).
 apply ST_Plus1.
 apply ST_PlusConstConst.
Qed.

End Temp1.

Module Temp2.

Inductive value : tm -> Prop :=
   | v_const : forall n, value (tm_const n).

(*Reserved Notation " t '===>' t' " (at level 40).*)

Inductive step : tm -> tm -> Prop :=
  | ST_Funny : forall n, (* <---- *)
      tm_const n ===> tm_plus (tm_const n) (tm_const 0)
  | ST_PlusConstConst : forall n1 n2,
      tm_plus (tm_const n1) (tm_const n2) ===> tm_const (plus n1 n2)
  | ST_Plus1 : forall t1 t1' t2,
      t1 ===> t1' ->
      tm_plus t1 t2 ===> tm_plus t1' t2
  | ST_Plus2 : forall v1 t2 t2',
      value v1 ->
      t2 ===> t2' ->
      tm_plus v1 t2 ===> tm_plus v1 t2'

  where " t '===>' t' " := (step t t').

Lemma value_not_same_as_normal_form :
  exists t, value t /\ ~ normal_form step t.
Proof.
exists (tm_const 0).
split.
 apply v_const.
 
 unfold normal_form.
 unfold not.
 intro H.
 apply H.
 exists (tm_plus (tm_const 0) (tm_const 0)).
 apply ST_Funny.
Qed.

End Temp2.

Module Temp3.

Inductive value : tm -> Prop :=
  | v_const : forall n, value (tm_const n).

(*Reserved Notation " t '===>' t' " (at level 40).*)

Inductive step : tm -> tm -> Prop :=
  | ST_PlusConstConst : forall n1 n2,
      tm_plus (tm_const n1) (tm_const n2) ===> tm_const (plus n1 n2)
  | ST_Plus1 : forall t1 t1' t2,
      t1 ===> t1' ->
      tm_plus t1 t2 ===> tm_plus t1' t2

  where " t '===>' t' " := (step t t').

Lemma value_not_same_as_normal_form:
   exists t, ~ value t /\ normal_form step t.
Proof.
exists (tm_plus (tm_const 1) (tm_plus (tm_const 0) (tm_const 0))).
split.
 intros H.
 inversion H.
 
 unfold normal_form.
 intros H.
 inversion H.
 inversion H0.
 inversion H4.
Qed.

End Temp3.

Module Temp4.
Inductive tm : Type :=
   | tm_true : tm
   | tm_false : tm
   | tm_if : tm -> tm -> tm -> tm.

Inductive value : tm -> Prop :=
   | v_true : value tm_true
   | v_false : value tm_false.

Inductive step : tm -> tm -> Prop :=
   | ST_IfTrue : forall t1 t2,
      tm_if tm_true t1 t2 ===> t1
   | ST_IfFalse : forall t1 t2,
      tm_if tm_false t1 t2 ===> t2
   | ST_If : forall t1 t1' t2 t3,
      t1 ===> t1' ->
         tm_if t1 t2 t3 ===> tm_if t1' t2 t3

   where " t '===>' t' " := (step t t').

Example bool_step_prop3 :
     tm_if
       (tm_if tm_true tm_true tm_true)
       (tm_if tm_true tm_true tm_true)
       tm_false
   ===>
     tm_if
       tm_true
       (tm_if tm_true tm_true tm_true)
       tm_false.
Proof.
apply ST_If.
apply ST_IfTrue.
Qed.

Theorem strong_progress: forall t,
        value t \/ (exists t', t ===> t').
Proof.
induction t.
 left.
 constructor.
 
 left.
 constructor.
 
 right.
 inversion IHt1.
  inversion H.
   exists t2.
   apply ST_IfTrue.
   
   exists t3.
   apply ST_IfFalse.
   
  inversion H.
  exists (tm_if x t2 t3).
  apply ST_If.
  assumption.
Qed.

Theorem step_deterministic :
  partial_function step.
Proof.
unfold partial_function.
intros x y1 y2 Hy1 Hy2.
generalize dependent y2.
induction Hy1.
 intros.
 inversion Hy2.
  reflexivity.
  
  subst.
  inversion H3.
  
 intros.
 inversion Hy2.
  reflexivity.
  
  inversion H3.
  
 intros.
 inversion Hy2.
  subst.
  inversion Hy1.
  
  subst.
  inversion Hy1.
  
  subst.
  apply IHHy1 in H3.
  subst.
  reflexivity.
Qed.

Module Temp5.


Inductive step : tm -> tm -> Prop :=
   | ST_IfTrue : forall t1 t2,
      tm_if tm_true t1 t2 ===> t1
   | ST_IfFalse : forall t1 t2,
      tm_if tm_false t1 t2 ===> t2
   | ST_If : forall t1 t1' t2 t3,
      t1 ===> t1' ->
         tm_if t1 t2 t3 ===> tm_if t1' t2 t3
   | ST_ShortCut : forall v t,
      value v ->
         tm_if t v v ===> v

   where " t '===>' t' " := (step t t').

Definition bool_step_prop4 :=
         tm_if
            (tm_if tm_true tm_true tm_true)
            tm_false
            tm_false
     ===>
         tm_false.

Example bool_step_prop4_holds : 
  bool_step_prop4.
Proof.
   unfold bool_step_prop4.
   apply ST_ShortCut.
   constructor.
Qed.

Theorem strong_progress: forall t,
        value t \/ (exists t', t ===> t').
Proof.
   induction t.
 left.
 constructor.
 
 left.
 constructor.
 
 inversion IHt1.
  right.
  inversion H.
   exists t2.
   constructor.
   
   exists t3.
   constructor.
   
  right.
  inversion H.
  exists (tm_if x t2 t3).
  apply ST_If.
  assumption.
Qed.

End Temp5.
End Temp4.

Definition stepmany := refl_step_closure step.

Notation " t '===>*' t' " := (stepmany t t') (at level 40).

Lemma test_stepmany_1:
   tm_plus
      (tm_plus (tm_const 0) (tm_const 3))
      (tm_plus (tm_const 2) (tm_const 4))
      ===>*
         tm_const (plus (plus 0 3) (plus 2 4)).
Proof.
   eapply rsc_step. apply ST_Plus1. apply ST_PlusConstConst.
  eapply rsc_step. apply ST_Plus2. apply v_const.
  apply ST_PlusConstConst.
  eapply rsc_step. apply ST_PlusConstConst.
  apply rsc_refl. Qed.

Lemma test_stepmany_2:
   tm_const 3 ===>* tm_const 3.
Proof.
   eapply rsc_refl.
Qed.

Lemma test_stepmany_3:
   tm_plus (tm_const 0) (tm_const 3)
      ===>*
   tm_plus (tm_const 0) (tm_const 3).
Proof.
   eapply rsc_refl.
Qed.

Lemma test_stepmany_4:
   tm_plus
      (tm_const 0)
      (tm_plus
         (tm_const 2)
         (tm_plus (tm_const 0) (tm_const 3)))
   ===>*
      tm_plus
         (tm_const 0)
         (tm_const (plus 2 (plus 0 3))).
Proof.
eapply rsc_step.
 apply ST_Plus2.
  apply v_const.
  
  apply ST_Plus2.
   apply v_const.
   
   apply ST_PlusConstConst.
   
 eapply rsc_step.
  apply ST_Plus2.
   apply v_const.
   
   apply ST_PlusConstConst.
   
  eapply rsc_refl.
Qed.

Definition step_normal_form := normal_form step.

Definition normal_form_of (t t' : tm) :=
  (t ===>* t' /\ step_normal_form t').

   (*
Theorem normal_forms_unique:
   partial_function normal_form_of.
Proof.
   unfold partial_function. unfold normal_form_of. intros x y1 y2 P1 P2.
   destruct P1 as [P11 P12]. destruct P2 as [P21 P22].
   generalize dependent y2.

   unfold step_normal_form in P12.
   unfold step_normal_form.
   unfold normal_form.
   unfold normal_form in P12.
   induction x.
   intros.
   unfold stepmany.
   inversion P11.
   subst.
   inversion P21.
   subst.
   reflexivity.

   subst.
   inversion P21.
   reflexivity.

   subst.
   inversion H1.

   inversion H.
   *)

Definition normalizing {X:Type} (R:relation X) :=
     forall t, exists t',
         (refl_step_closure R) t t' /\ normal_form R t'.

Lemma stepmany_congr_1 : forall t1 t1' t2,
      t1 ===>* t1' ->
         tm_plus t1 t2 ===>* tm_plus t1' t2.
Proof.
intros t1 t1' t2 H.
rsc_cases (induction H) Case.
 apply rsc_refl.
  
  apply rsc_step with (tm_plus y t2).
    apply ST_Plus1.
      apply H.
        
        apply IHrefl_step_closure.
Qed.

Lemma stepmany_congr2 : forall t1 t2 t2',
      value t1 ->
         t2 ===>* t2' ->
            tm_plus t1 t2 ===>* tm_plus t1 t2'.
Proof.
intros t1 t2 t2'.
intros H1.
intros H2.
induction H2.
 apply rsc_refl.
  
  apply rsc_step with (tm_plus t1 y).
    apply ST_Plus2.
       assumption.
          
          assumption.
             
            assumption.
Qed.

Theorem step_normalizing :
  normalizing step.
Proof.
  unfold normalizing.
  tm_cases (induction t) Case.
    Case "tm_const".
      exists (tm_const n).
      split.
      SCase "l". apply rsc_refl.
      SCase "r".
        (* We can use rewrite with "iff" statements, not
           just equalities: *)
        rewrite nf_same_as_value. apply v_const.
    Case "tm_plus".
      destruct IHt1 as [t1' H1]. destruct IHt2 as [t2' H2].
      destruct H1 as [H11 H12]. destruct H2 as [H21 H22].
      rewrite nf_same_as_value in H12. rewrite nf_same_as_value in H22.
      inversion H12 as [n1]. inversion H22 as [n2].
      rewrite <- H in H11.
      rewrite <- H0 in H21.
      exists (tm_const (plus n1 n2)).
      split.
        SCase "l".
          apply rsc_trans with (tm_plus (tm_const n1) t2).
          apply stepmany_congr_1. apply H11.
          apply rsc_trans with 
             (tm_plus (tm_const n1) (tm_const n2)).
          apply stepmany_congr2. apply v_const. apply H21.
          apply rsc_R. apply ST_PlusConstConst.
        SCase "r".
          rewrite nf_same_as_value. apply v_const. Qed.
          
 Lemma eval__value : forall t1 t2,
      eval t1 t2 ->
      value t2.
 Proof.
   intros t1 t2 HE.
   eval_cases (inversion HE) Case; apply v_const. Qed.
   

   (*
Theorem eval__stepmany: forall t v,
  eval t v -> t ===>* v.
Proof.   
*)

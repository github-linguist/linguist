
Require Export SfLib.

Module STLC.

Inductive ty : Type :=
   | ty_Bool : ty
   | ty_arrow : ty -> ty -> ty.

Inductive tm : Type :=
   | tm_var : id -> tm
   | tm_app : tm -> tm -> tm
   | tm_abs : id -> ty -> tm -> tm
   | tm_true : tm
   | tm_false : tm
   | tm_if : tm -> tm -> tm -> tm.

Tactic Notation "tm_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "tm_var" | Case_aux c "tm_app" 
  | Case_aux c "tm_abs" | Case_aux c "tm_true" 
  | Case_aux c "tm_false" | Case_aux c "tm_if" ].
  
Notation a := (Id 0).
Notation b := (Id 1).
Notation c := (Id 2).

Notation idB :=
   (tm_abs a ty_Bool (tm_var a)).
   
Notation idBB :=
   (tm_abs a (ty_arrow ty_Bool ty_Bool) (tm_var a)).
   
Notation idBBBB :=
   (tm_abs a (ty_arrow (ty_arrow ty_Bool ty_Bool) 
                         (ty_arrow ty_Bool ty_Bool)) 
       (tm_var a)).
       
Notation k := (tm_abs a ty_Bool (tm_abs b ty_Bool (tm_var a))).

Inductive value : tm -> Prop :=
  | v_abs : forall x T t,
      value (tm_abs x T t)
  | t_true : 
      value tm_true
  | t_false : 
      value tm_false.

Hint Constructors value.

Fixpoint subst (s:tm) (x:id) (t:tm) : tm :=
   match t with
   | tm_var x' => if beq_id x x' then s else t
   | tm_abs x' T t1 => tm_abs x' T (if beq_id x x' then t1 else (subst s x t1))
   | tm_app t1 t2 => tm_app (subst s x t1) (subst s x t2)
   | tm_true => tm_true
   | tm_false => tm_false
   | tm_if t1 t2 t3 => tm_if (subst s x t1) (subst s x t2) (subst s x t3)
end.

Reserved Notation "t1 '==>' t2" (at level 40).

Inductive step : tm -> tm -> Prop :=
 | ST_AppAbs : forall x T t12 v2,
        value v2 ->
        (tm_app (tm_abs x T t12) v2) ==> (subst v2 x t12)
 | ST_App1 : forall t1 t1' t2,
        t1 ==> t1' ->
        tm_app t1 t2 ==> tm_app t1' t2
 | ST_App2 : forall v1 t2 t2',
        value v1 ->
        t2 ==> t2' ->
        tm_app v1 t2 ==> tm_app v1 t2'
 | ST_IfTrue : forall t1 t2,
     (tm_if tm_true t1 t2) ==> t1
 | ST_IfFalse : forall t1 t2,
     (tm_if tm_false t1 t2) ==> t2
 | ST_If : forall t1 t1' t2 t3,
     t1 ==> t1' ->
     (tm_if t1 t2 t3) ==> (tm_if t1' t2 t3)

where "t1 '==>' t2" := (step t1 t2).

Tactic Notation "step_cases" tactic(first) ident(c) :=
 first;
 [ Case_aux c "ST_AppAbs" | Case_aux c "ST_App1" 
 | Case_aux c "ST_App2" | Case_aux c "ST_IfTrue" 
 | Case_aux c "ST_IfFalse" | Case_aux c "ST_If" ].

Notation stepmany := (refl_step_closure step).
Notation "t1 '==>*' t2" := (stepmany t1 t2) (at level 40).

Hint Constructors step.

Lemma step_example3 :
       (tm_app (tm_app idBBBB idBB) idB)
  ==>* idB.
Proof.
 eapply rsc_step.
 apply ST_App1.
 apply ST_AppAbs.
 apply v_abs.
 
 simpl.
 eapply rsc_step.
  apply ST_AppAbs.
  apply v_abs.
  
  simpl.
  apply rsc_refl.
Qed.

Definition context := partial_map ty.
Module Context.

Definition partial_map (A:Type) := id -> option A.
Definition empty {A:Type} : partial_map A := (fun _ => None).
Definition extend {A:Type} (Gamma : partial_map A) (x:id) (T : A) :=
   fun x' => if beq_id x x' then Some T else Gamma x'.
   
Lemma extend_eq : forall A (ctxt: partial_map A) x T,
   (extend ctxt x T) x = Some T.
Proof.
   intros. unfold extend. rewrite <- beq_id_refl. auto.
Qed.

Lemma extend_neq : forall A (ctxt: partial_map A) x1 T x2,
   beq_id x2 x1 = false ->
      (extend ctxt x2 T) x1 = ctxt x1.
Proof.
intros. unfold extend. rewrite H. auto.
Qed.

End Context.

Inductive has_type : context -> tm -> ty -> Prop :=
   | T_Var : forall Gamma x T,
      Gamma x = Some T ->
         has_type Gamma (tm_var x) T
   | T_Abs : forall Gamma x T11 T12 t12,
      has_type (extend Gamma x T11) t12 T12 ->
         has_type Gamma (tm_abs x T11 t12) (ty_arrow T11 T12)
   | T_App : forall T11 T12 Gamma t1 t2,
      has_type Gamma t1 (ty_arrow T11 T12) ->
         has_type Gamma t2 T11 ->
            has_type Gamma (tm_app t1 t2) T12
   | T_True : forall Gamma,
      has_type Gamma tm_true ty_Bool
   | T_False : forall Gamma,
      has_type Gamma tm_false ty_Bool
   | T_If : forall t1 t2 t3 T Gamma,
      has_type Gamma t1 ty_Bool ->
      has_type Gamma t2 T ->
      has_type Gamma t3 T ->
      has_type Gamma (tm_if t1 t2 t3) T.
      
Tactic Notation "has_type_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "T_Var" | Case_aux c "T_Abs" 
  | Case_aux c "T_App" | Case_aux c "T_True" 
  | Case_aux c "T_False" | Case_aux c "T_If" ].

Hint Constructors has_type.

Hint Unfold beq_id beq_nat extend.

Example typing_example_2_full :
  has_type empty
    (tm_abs a ty_Bool
       (tm_abs b (ty_arrow ty_Bool ty_Bool)
          (tm_app (tm_var b) (tm_app (tm_var b) (tm_var a)))))
    (ty_arrow ty_Bool (ty_arrow (ty_arrow ty_Bool ty_Bool) ty_Bool)).
Proof.
apply T_Abs.
apply T_Abs.
apply T_App with (T11 := ty_Bool).
 apply T_Var.
 unfold extend.
 simpl.
 reflexivity.
 
 apply T_App with (T11 := ty_Bool).
  apply T_Var.
  unfold extend.
  simpl.
  reflexivity.
  
  apply T_Var.
  unfold extend.
  simpl.
  reflexivity.
Qed.

Example typing_example_3 :
  exists T, 
    has_type empty
      (tm_abs a (ty_arrow ty_Bool ty_Bool)
         (tm_abs b (ty_arrow ty_Bool ty_Bool)
            (tm_abs c ty_Bool
               (tm_app (tm_var b) (tm_app (tm_var a) (tm_var c))))))
      T.

Proof with auto.
exists
 (ty_arrow (ty_arrow ty_Bool ty_Bool)
    (ty_arrow (ty_arrow ty_Bool ty_Bool) (ty_arrow ty_Bool ty_Bool))).
apply T_Abs.
apply T_Abs.
apply T_Abs.
apply T_App with (T11 := ty_Bool).
 apply T_Var.
 unfold extend.
 simpl.
 reflexivity.
 
 apply T_App with (T11 := ty_Bool).
  auto.
  
  auto.
Qed.


Theorem coiso : forall a b e,
   a ==>* b ->
      tm_app a e ==>* tm_app b e.
Proof.
intros.
induction H.
 apply rsc_refl.
 
 apply rsc_step with (tm_app y e).
  apply ST_App1.
  assumption.
  
  assumption.
Qed.

Theorem reptrans : forall a b c,
   a ==>* b ->
      b ==>* c ->
         a ==>* c.
Proof.

intros a b c H.
induction H.
 intros.
 assumption.
 
 intros H1.
 apply IHrefl_step_closure in H1.
 apply rsc_step with y.
  assumption.
  
  assumption.
Qed.

(* TODO
Example typing_nonexample_3 :
  ~ (exists S, exists T,
        has_type empty 
          (tm_abs a S
             (tm_app (tm_var a) (tm_var a)))
          T).
Proof.
*)

Inductive appears_free_in : id -> tm -> Prop :=
  | afi_var : forall x,
      appears_free_in x (tm_var x)
  | afi_app1 : forall x t1 t2,
      appears_free_in x t1 -> appears_free_in x (tm_app t1 t2)
  | afi_app2 : forall x t1 t2,
      appears_free_in x t2 -> appears_free_in x (tm_app t1 t2)
  | afi_abs : forall x y T11 t12,
      y <> x ->
      appears_free_in x t12 ->
      appears_free_in x (tm_abs y T11 t12)
  | afi_if1 : forall x t1 t2 t3,
      appears_free_in x t1 ->
      appears_free_in x (tm_if t1 t2 t3)
  | afi_if2 : forall x t1 t2 t3,
      appears_free_in x t2 ->
      appears_free_in x (tm_if t1 t2 t3)
  | afi_if3 : forall x t1 t2 t3,
      appears_free_in x t3 ->
      appears_free_in x (tm_if t1 t2 t3).
      
Tactic Notation "afi_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "afi_var"
  | Case_aux c "afi_app1" | Case_aux c "afi_app2" 
  | Case_aux c "afi_abs" 
  | Case_aux c "afi_if1" | Case_aux c "afi_if2" 
  | Case_aux c "afi_if3" ].

Hint Constructors appears_free_in.

Definition closed (t:tm) :=
  forall x, ~ appears_free_in x t.
  
Lemma free_in_context : forall x t T Gamma,
  appears_free_in x t ->
  has_type Gamma t T ->
  exists T', Gamma x = Some T'.
Proof.
  intros. generalize dependent Gamma. generalize dependent T.
  afi_cases (induction H) Case; 
         intros; try solve [inversion H0; eauto].
  Case "afi_abs".
    inversion H1; subst.
    apply IHappears_free_in in H7.
    apply not_eq_beq_id_false in H.
    rewrite extend_neq in H7; assumption.
Qed.

Corollary typable_empty__closed : forall t T, 
    has_type empty t T ->
    closed t.
Proof.
intros t T H x H1.
remember (@empty ty) as Gamma.
assert (exists t' : _, Gamma x = Some t').
 apply free_in_context with (t := t) (T := T).
  assumption.
  
  assumption.
  
 inversion H0.
 rewrite HeqGamma in H2.
 inversion H2.
Qed.

Lemma context_invariance : forall Gamma Gamma' t S,
     has_type Gamma t S ->
     (forall x, appears_free_in x t -> Gamma x = Gamma' x) ->
     has_type Gamma' t S.
Proof with auto.
intros.
generalize dependent Gamma'.
has_type_cases (induction H) Case; intros; auto.
 apply T_Var.
 rewrite <- H0...
 
 apply T_Abs.
 apply IHhas_type.
 intros x0 Hafi.
 unfold extend.
 remember (beq_id x x0) as e.
 destruct e.
  reflexivity.
  
  auto.
  apply H0.
  apply afi_abs.
   auto.
   eauto   .
   apply beq_id_false_not_eq.
   rewrite Heqe.
   reflexivity.
   
   assumption.
   
 apply T_App with T11.
  auto.
  
  auto.
Qed.

Lemma substitution_preserves_typing : forall Gamma x U v t T,
   has_type (extend Gamma x U) t T ->
      has_type empty v U ->
         has_type Gamma (subst v x t) T.
Proof with eauto.
   intros Gamma x U v t T Ht Hv.
   generalize dependent Gamma.
   generalize dependent T.
   tm_cases (induction t) Case; intros T Gamma H; inversion H; subst; simpl...
   Case "tm_var".
       rename i into y. remember (beq_id x y) as e. destruct e.
       SCase "x=y".
         apply beq_id_eq in Heqe. subst.
         rewrite extend_eq in H2.
         inversion H2; subst.
         clear H2.
           eapply context_invariance...
           intros x Hcontra.
           destruct (free_in_context _ _ T empty Hcontra) as (T', HT')...
           inversion HT'.

           apply T_Var.
           rewrite extend_neq in H2.
            assumption.

            rewrite Heqe.
            reflexivity.

          rename i into y.
          apply T_Abs.
          remember (beq_id x y) as e.
          destruct e.
           eapply context_invariance...
           apply beq_id_eq in Heqe.
           subst.
           intros x Hafi.
           unfold extend.
           destruct (beq_id y x).
            reflexivity.

            reflexivity.

           apply IHt.
           eapply context_invariance...
           intros x0 Hafi.
           unfold extend.
           remember (beq_id y x0) as Coiso1.
           remember (beq_id x x0) as Coiso2.
           destruct Coiso1.
            auto.
            eauto   .
            destruct Coiso2.
             eauto   .
             auto.
             apply beq_id_eq in HeqCoiso1.
             apply beq_id_eq in HeqCoiso2.
             subst.
             assert (x0 <> x0).
              apply beq_id_false_not_eq.
              rewrite Heqe.
              auto.

              apply ex_falso_quodlibet.
              apply H0.
              reflexivity.

             reflexivity.

            destruct Coiso2.
             auto.

             auto.
Qed.

Theorem preservation : forall t t' T,
     has_type empty t T ->
     t ==> t' ->
     has_type empty t' T.
Proof.
remember (@empty ty) as Gamma.
intros t t' T HT.
generalize dependent t'.
induction HT.
 intros t' H1.
 inversion H1.
 
 intros t' H1.
 inversion H1.
 
 intros t' H1.
 inversion H1.
  apply substitution_preserves_typing with T11.
   subst.
   inversion HT1.
   subst.
   apply H2.
   
   subst.
   assumption.
   
  subst.
  apply T_App with T11.
   apply IHHT1.
    reflexivity.
    
    assumption.
    
   assumption.
   
  subst.
  apply T_App with T11.
   assumption.
   
   apply IHHT2.
    reflexivity.
    
    assumption.
    
 intros t' H.
 inversion H.
 
 intros t' H.
 inversion H.
 
 intros t' H.
 inversion H.
  subst.
  assumption.
  
  subst.
  assumption.
  
  subst.
  apply T_If.
   apply IHHT1.
    reflexivity.
    
    assumption.
    
   assumption.
   
   assumption.
Qed.

Theorem progress : forall t T,
     has_type empty t T ->
     value t \/ exists t', t ==> t'.
Proof.
intros t T.
intros H.
remember (@empty ty) as Gamma.
induction H.
 rewrite HeqGamma in H.
 unfold empty in H.
 inversion H.
 
 left.
 apply v_abs.
 
 right.
 assert (value t1 \/ (exists t' : tm, t1 ==> t')).
  apply IHhas_type1.
  assumption.
  
  assert (value t2 \/ (exists t' : tm, t2 ==> t')).
   apply IHhas_type2.
   assumption.
   
   inversion H1.
    inversion H2.
     inversion H3.
      subst.
      exists (subst t2 x t).
      apply ST_AppAbs.
      assumption.
      
      subst.
      inversion H.
      
      subst.
      inversion H.
      
     inversion H4.
     exists (tm_app t1 x).
     apply ST_App2.
      assumption.
      
      assumption.
      
    inversion H3.
    exists (tm_app x t2).
    apply ST_App1.
    assumption.
    
 left.
 auto.
 
 left.
 auto.
 
 right.
 assert (value t1 \/ (exists t' : tm, t1 ==> t')).
  apply IHhas_type1.
  assumption.
  
  inversion H2.
   inversion H3.
    subst.
    inversion H.
    
    subst.
    exists t2.
    apply ST_IfTrue.
    
    subst.
    exists t3.
    apply ST_IfFalse.
    
   inversion H3.
   exists (tm_if x t2 t3).
   apply ST_If.
   assumption.
Qed.

Theorem progress' : forall t T,
     has_type empty t T ->
     value t \/ exists t', t ==> t'.
Proof.
intros t.
tm_cases (induction t) Case; intros T Ht; auto.
 inversion Ht.
 inversion H1.
 
 right.
 inversion Ht.
 subst.
 assert (value t1 \/ (exists t' : tm, t1 ==> t')).
  apply IHt1 with (T := ty_arrow T11 T).
  assumption.
  
  assert (value t2 \/ (exists t' : tm, t2 ==> t')).
   apply IHt2 with T11.
   assumption.
   
   inversion H.
    inversion H1.
     subst.
     inversion H0.
      exists (subst t2 x t).
      apply ST_AppAbs.
      assumption.
      
      inversion H3.
      exists (tm_app (tm_abs x T0 t) x0).
      apply ST_App2.
       assumption.
       
       assumption.
       
     subst.
     inversion H2.
     
     subst.
     inversion H2.
     
    inversion H1.
    exists (tm_app x t2).
    apply ST_App1.
    assumption.
    
 right.
 inversion Ht.
 subst.
 assert (value t1 \/ (exists t' : tm, t1 ==> t')).
  apply IHt1 with ty_Bool.
  assumption.
  
  assert (value t2 \/ (exists t' : tm, t2 ==> t')).
   apply IHt2 with T.
   assumption.
   
   assert (value t3 \/ (exists t' : tm, t3 ==> t')).
    apply IHt3 with T.
    assumption.
    
    inversion H.
     inversion H2.
      subst.
      inversion H3.
      
      subst.
      subst.
      exists t2.
      apply ST_IfTrue.
      
      exists t3.
      apply ST_IfFalse.
      
     inversion H2.
     exists (tm_if x t2 t3).
     apply ST_If.
     assumption.
Qed.

Theorem types_unique : forall t T Gamma,
   has_type Gamma t T ->
      (forall T', has_type Gamma t T' -> T' = T).
Proof.
intros t T Gamma H.
induction H.
 intros T'.
 intros H1.
 inversion H1.
 subst.
 subst.
 auto.
 eauto   .
 inversion H1.
 subst.
 assert (Some T = Some T').
  rewrite <- H3.
  rewrite <- H.
  reflexivity.
  
  inversion H0.
  reflexivity.
  
 intros T'.
 intros H1.
 inversion H1.
 subst.
 assert (T1 = T12).
  apply IHhas_type.
  assumption.
  
  rewrite H0.
  reflexivity.
  
 intros T' H1.
 subst.
 inversion H1.
 subst.
 assert (ty_arrow T0 T' = ty_arrow T11 T12).
  apply IHhas_type1.
  assumption.
  
  inversion H2.
  reflexivity.
  
 intros T'.
 intros H.
 inversion H.
 reflexivity.
 
 intros.
 inversion H.
 reflexivity.
 
 intros T'.
 intros H2.
 inversion H2.
 subst.
 apply IHhas_type2.
 assumption.
Qed.
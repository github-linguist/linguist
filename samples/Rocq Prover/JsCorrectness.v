Set Implicit Arguments.
Require Import Shared.
Require Import LibFix LibList.
Require Import JsSyntax JsSyntaxAux JsCommon JsCommonAux JsPreliminary.
Require Import JsInterpreterMonads JsInterpreter JsPrettyInterm JsPrettyRules.

Ltac tryfalse_nothing :=
  try match goal with x: nothing |- _ => destruct x end;
  tryfalse.

(**************************************************************)
(** ** Implicit Types -- copied from JsPreliminary *)

Implicit Type b : bool.
Implicit Type n : number.
Implicit Type k : int.
Implicit Type s : string.
Implicit Type i : literal.
Implicit Type l : object_loc.
Implicit Type w : prim.
Implicit Type v : value.
Implicit Type r : ref.
Implicit Type ty : type.

Implicit Type rt : restype.
Implicit Type rv : resvalue.
Implicit Type lab : label.
Implicit Type labs : label_set.
Implicit Type R : res.
Implicit Type o : out.
Implicit Type ct : codetype.

Implicit Type x : prop_name.
Implicit Type str : strictness_flag.
Implicit Type m : mutability.
Implicit Type Ad : attributes_data.
Implicit Type Aa : attributes_accessor.
Implicit Type A : attributes.
Implicit Type Desc : descriptor.
Implicit Type D : full_descriptor.

Implicit Type L : env_loc.
Implicit Type E : env_record.
Implicit Type Ed : decl_env_record.
Implicit Type X : lexical_env.
Implicit Type O : object.
Implicit Type S : state.
Implicit Type C : execution_ctx.
Implicit Type P : object_properties_type.
Implicit Type W : result.

Implicit Type e : expr.
Implicit Type p : prog.
Implicit Type t : stat.

Implicit Type T : Type.


(**************************************************************)
(** Correctness Properties *)

Record runs_type_correct runs :=
  make_runs_type_correct {
    runs_type_correct_expr : forall S C e o,
       runs_type_expr runs S C e = o ->
       red_expr S C (expr_basic e) o;
    runs_type_correct_stat : forall S C t o,
       runs_type_stat runs S C t = o ->
       red_stat S C (stat_basic t) o;
    runs_type_correct_prog : forall S C p o,
       runs_type_prog runs S C p = o ->
       red_prog S C (prog_basic p) o;

    runs_type_correct_call : forall S C l v vs o,
       runs_type_call runs S C l v vs = o ->
       red_expr S C (spec_call l v vs) o;

    runs_type_correct_call_prealloc : forall S C l B args o,
       runs_type_call_prealloc runs S C B l args = result_some (specret_out o) ->
       red_expr S C (spec_call_prealloc B l args) o;

    runs_type_correct_construct : forall S C co l args o, 
      runs_type_construct runs S C co l args = o ->
      red_expr S C (spec_construct_1 co l args) o;

    runs_type_correct_function_has_instance : forall S C (lo lv : object_loc) o,
       runs_type_function_has_instance runs S lo lv = o ->
       red_expr S C (spec_function_has_instance_2 lv lo) o;

    runs_type_correct_get_args_for_apply : forall S C array (index n : int) y,
       runs_type_get_args_for_apply runs S C array index n = result_some y ->
       red_spec S C (spec_function_proto_apply_get_args array index n) y;

    runs_type_correct_object_has_instance : forall S C B l v o,
       runs_type_object_has_instance runs S C B l v = result_some (specret_out o) ->
       red_expr S C (spec_object_has_instance_1 B l v) o;

    runs_type_correct_stat_while : forall S C rv ls e t o,
       runs_type_stat_while runs S C rv ls e t = o ->
       red_stat S C (stat_while_1 ls e t rv) o;
    runs_type_correct_stat_do_while : forall S C rv ls e t o,
       runs_type_stat_do_while runs S C rv ls e t = o ->
       red_stat S C (stat_do_while_1 ls t e rv) o;
    runs_type_correct_stat_for_loop : forall S C labs rv eo2 eo3 t o,
       runs_type_stat_for_loop runs S C labs rv eo2 eo3 t = o ->
       red_stat S C (stat_for_2 labs rv eo2 eo3 t) o;
    runs_type_correct_object_delete : forall S C l x str o,
       runs_type_object_delete runs S C l x str = o ->
       red_expr S C (spec_object_delete l x str) o;
    runs_type_correct_object_get_own_prop : forall S C l x sp,
       runs_type_object_get_own_prop runs S C l x = result_some sp ->
       red_spec S C (spec_object_get_own_prop l x) sp;
    runs_type_correct_object_get_prop : forall S C l x sp,
       runs_type_object_get_prop runs S C l x = result_some sp ->
       red_spec S C (spec_object_get_prop l x) sp;
    runs_type_correct_object_get : forall S C l x o,
       runs_type_object_get runs S C l x = o ->
       red_expr S C (spec_object_get l x) o;
    runs_type_correct_object_proto_is_prototype_of : forall S C lthis l o,
       runs_type_object_proto_is_prototype_of runs S lthis l = o ->
       red_expr S C (spec_call_object_proto_is_prototype_of_2_3 lthis l) o;
    runs_type_correct_object_put : forall S C l x v str o,
       runs_type_object_put runs S C l x v str = o ->
       red_expr S C (spec_object_put l x v str) o;
    runs_type_correct_equal : forall S C v1 v2 o,
       runs_type_equal runs S C v1 v2 = o ->
       red_expr S C (spec_equal v1 v2) o;
    runs_type_correct_to_integer : forall S C v o,
       runs_type_to_integer runs S C v = o ->
       red_expr S C (spec_to_integer v) o;
    runs_type_correct_to_string : forall S C v o,
       runs_type_to_string runs S C v = o ->
       red_expr S C (spec_to_string v) o;

    (* ARRAYS *)
    runs_type_correct_array_element_list : forall S C l oes o k,
       runs_type_array_element_list runs S C l oes k = o ->
       red_expr S C (expr_array_3 l oes k) o;

    runs_type_correct_object_define_own_prop_array_loop : 
      forall S C l newLen oldLen newLenDesc newWritable throw o 
             (def : state -> prop_name -> descriptor -> strictness_flag -> specres nothing) 
             (def_correct : forall S str o x Desc,
                def S x Desc str = res_out o ->
                red_expr S C (spec_object_define_own_prop_1 builtin_define_own_prop_default l x Desc str) o),
       runs_type_object_define_own_prop_array_loop runs S C l newLen oldLen newLenDesc newWritable throw def = o -> 
       red_expr S C (spec_object_define_own_prop_array_3l l newLen oldLen newLenDesc newWritable throw) o;

     runs_type_correct_array_join_elements : forall S C l k length sep s o, 
       runs_type_array_join_elements runs S C l k length sep s = result_some (specret_out o) ->
       red_expr S C (spec_call_array_proto_join_elements l k length sep s) o
  }.


(**************************************************************)
(** Useful Tactics *)

Ltac absurd_neg :=
  let H := fresh in
  introv H; inverts H; tryfalse.

Hint Constructors abort.


(**************************************************************)
(** General Lemmas *)

Lemma arguments_from_spec_1 : forall args,
  exists v, arguments_from args (v::nil)
         /\ get_arg 0 args = v.
Proof.
  Hint Constructors arguments_from.
  intros. destruct args as [|v vs].
  exists undef. splits*.
  exists v. splits*.
Qed.


Lemma res_overwrite_value_if_empty_empty : forall R,
  res_overwrite_value_if_empty resvalue_empty R = R.
Proof. introv. unfolds. cases_if~. destruct R; simpls; inverts~ e. Qed.

Lemma res_type_res_overwrite_value_if_empty : forall rv R,
  res_type R = res_type (res_overwrite_value_if_empty rv R).
Proof.
  introv. destruct R. unfold res_overwrite_value_if_empty. simpl.
  cases_if; reflexivity.
Qed.

Lemma res_label_res_overwrite_value_if_empty : forall rv R,
  res_label R = res_label (res_overwrite_value_if_empty rv R).
Proof.
  introv. destruct R. unfold res_overwrite_value_if_empty. simpl.
  cases_if; reflexivity.
Qed.

Lemma res_overwrite_value_if_empty_resvalue : forall rv1 rv2, exists rv3,
  res_normal rv3 = res_overwrite_value_if_empty rv1 rv2 /\ (rv3 = rv1 \/ rv3 = rv2).
Proof. introv. unfolds res_overwrite_value_if_empty. cases_if*. Qed.


Lemma get_arg_correct : forall args vs,
  arguments_from args vs ->
  forall num,
    num < length vs ->
    get_arg num args = LibList.nth num vs.
Proof.
  introv A. induction~ A.
   introv I. false I. lets (I'&_): (rm I). inverts~ I'.
   introv I. destruct* num. rewrite nth_succ. rewrite <- IHA.
    unfolds. repeat rewrite~ nth_def_nil.
    rewrite length_cons in I. nat_math.
   introv I. destruct* num. rewrite nth_succ. rewrite <- IHA.
    unfolds. rewrite~ nth_def_succ.
    rewrite length_cons in I. nat_math.
Qed.

Lemma get_arg_correct_0 : forall args,
  arguments_from args (get_arg 0 args :: nil).
Proof. introv. destruct args; do 2 constructors. Qed.

Lemma get_arg_correct_1 : forall args,
  arguments_from args (get_arg 0 args :: get_arg 1 args :: nil).
Proof. introv. destruct args as [|? [|? ?]]; do 3 constructors. Qed.

Lemma get_arg_correct_2 : forall args,
  arguments_from args (get_arg 0 args :: get_arg 1 args :: get_arg 2 args :: nil).
Proof. introv. destruct args as [|? [|? [|? ?]]]; do 4 constructors. Qed.

Lemma get_arg_first_and_rest_correct : forall args v lv,
  (v, lv) = get_arg_first_and_rest args <-> 
  arguments_first_and_rest args (v, lv).
Proof.
  induction args; introv; splits; introv Hyp; 
  unfolds get_arg_first_and_rest; unfolds get_arg; 
  simpls; inverts~ Hyp.
Qed.

Lemma and_impl_left : forall P1 P2 P3 : Prop,
  (P1 -> P2) ->
  P1 /\ P3 ->
  P2 /\ P3.
Proof. auto*. Qed.

Ltac applys_and_base L :=
  applys~ and_impl_left; [applys~ L|]; try reflexivity.

Tactic Notation "applys_and" constr(E) :=
  applys_and_base (>> E).

Tactic Notation "applys_and" constr(E) constr(A1) :=
  applys_and_base (>> E A1).

Tactic Notation "applys_and" constr(E) constr(A1) constr(A2) :=
  applys_and_base (>> E A1 A2).

Tactic Notation "applys_and" constr(E) constr(A1) constr(A2) constr(A3) :=
  applys_and_base (>> E A1 A2 A3).

Ltac constructors_and :=
  let H := fresh in
  eapply and_impl_left; [ intro H; constructors; exact H |].


Lemma run_callable_correct : forall S v co,
  run_callable S v = Some co ->
  callable S v co.
Proof.
  introv E. destruct v; simpls~.
   inverts~ E.
   sets_eq <- B: (pick_option (object_binds S o)). destruct B; simpls; tryfalse.
    exists o0. splits~. forwards~: @pick_option_correct EQB. inverts~ E.
Qed.


(**************************************************************)
(** Monadic Constructors, Lemmas *)

(* Shared defs *)

(** [eqabort o1 o] assert that [o1] and [o] are equal
    and satisfy the [abort] predicate. *)

Definition eqabort o1 o :=
  o = o1 /\ abort o.

Ltac prove_abort :=
  solve [ assumption | (constructor; absurd_neg) ].

(** [isout W Pred] asserts that [W] is in fact
    an outcome that satisfies [Pred]. *)

Definition isout W (Pred:out->Prop) :=
  exists o1, W = res_out o1 /\ Pred o1.

Hint Unfold isout.
Hint Unfold eqabort.

(* Generic *)

Lemma if_empty_label_out : forall T K S R (o : T),
  if_empty_label S R K = result_some o ->
  res_label R = label_empty /\ K tt = result_some o.
Proof. introv H. unfolds in H. cases_if; tryfalse. eexists; auto*. Qed.

Lemma if_some_out : forall (A B : Type) (oa : option A) K (b : B),
  if_some oa K = result_some b ->
  exists (a:A), oa = Some a /\ K a = result_some b.
Proof. introv E. destruct* oa; tryfalse. Qed.

Lemma if_result_some_out : forall (A B : Type) (W : resultof A) K (b : B),
  if_result_some W K = result_some b ->
  exists (y : A), W = result_some y /\ K y = result_some b.
Proof. introv H. destruct* W; tryfalse. Qed.

Lemma if_some_or_default_out : forall (A B : Type) (oa : option A) d K (b : B),
  if_some_or_default oa d K = b ->
     (oa = None /\ d = b)
  \/ (exists a, oa = Some a /\ K a = b).
Proof. introv E. destruct* oa; tryfalse. Qed.


(* Results *)

Definition if_ter_post (K : _ -> _ -> result) o o1 :=
     (o1 = out_div /\ o = o1)
  \/ (exists S R, o1 = out_ter S R /\ K S R = o).

Lemma if_ter_out : forall W K o,
  if_ter W K = res_out o ->
  isout W (if_ter_post K o).
Proof.
  introv H. destruct W as [[|o1]| | | ]; simpls; tryfalse_nothing.
  exists o1. splits~. unfolds. destruct o1 as [|S R].
   inverts* H.
   jauto.
Qed.

Definition if_success_state_post rv0 (K : _ -> _ -> result) o o1 :=
  (o1 = out_div /\ o = o1) \/
  (exists S R, o1 = out_ter S R /\ res_type R = restype_throw /\ o = out_ter S R) \/
  (exists S R, o1 = out_ter S R /\ res_type R <> restype_throw /\
    res_type R <> restype_normal /\ o = out_ter S (res_overwrite_value_if_empty rv0 R)) \/
  exists S rv, o1 = out_ter S (res_normal rv) /\
    K S (ifb rv = resvalue_empty then rv0 else rv) = res_out o.

Lemma if_success_state_out : forall rv W K o,
  if_success_state rv W K = o ->
  isout W (if_success_state_post rv K o).
Proof.
  introv E. forwards~ (o1&WE&P): if_ter_out (rm E). subst W. eexists. splits*.
  inversion_clear P as [?|(S&R&?&H)]. branch~ 1.
  substs. destruct R as [rt rv' rl]. destruct~ rt; simpls;
    try solve [branch 3; repeat eexists; [discriminate | discriminate | inverts~ H]].
   forwards~ (?&?): if_empty_label_out (rm H). simpls. substs.
    branch 4. repeat eexists. auto*.
   inverts H. branch 2. repeat eexists.
Qed.

Definition if_success_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S rv, o1 = out_ter S (res_normal rv) /\ K S rv = o.

Lemma if_success_out : forall W K o,
  if_success W K = res_out o ->
  isout W (if_success_post K o).
Proof.
  introv E. forwards~ (o1&WE&P): if_ter_out (rm E). subst W. eexists. splits*.
  inversion_clear P as [[? ?]|(S&R&?&H)]. substs. branch~ 1.
  substs. destruct R as [rt rv' rl]. destruct~ rt; simpls;
     try solve [inverts H; branch 1; splits~; prove_abort].
   forwards~ (?&?): if_empty_label_out (rm H). simpls. substs.
    branch 2. repeat eexists. auto*.
Qed.

  (* Documentation: same with unfolding:
    Lemma if_success_out : forall W K o,
      if_success W K = o ->
      exists o1, W = res_out o1 /\
       (   (o = o1 /\ abort o)
        \/ (exists S rv, o1 = out_ter S rv /\ K S rv = o)).
  *)

Definition if_void_post (K : _ -> result) o o1 :=
  eqabort o1 o \/
  exists S, o1 = out_void S /\ K S = o.

Lemma if_void_out : forall W K o,
  if_void W K = o ->
  isout W (if_void_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_success_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&?&?)]; subst; [ left* | right ].
  exists S. destruct R; tryfalse. auto.
Admitted. (*faster*)

(* if_not_throw *)

Definition if_not_throw_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  (exists S R, o1 = out_ter S R /\
     ((res_type R <> restype_throw /\ K S R = o) \/
      (res_type R  = restype_throw /\ o = o1))).

Hint Extern 1 (_ <> _ :> restype) => congruence.

Lemma if_not_throw_out : forall W K o,
  if_not_throw W K = o -> 
  isout W (if_not_throw_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1 & WE & P): if_ter_out (rm E).
  exists o1. split~.
  unfolds. unfolds in P.

  inversion_clear P as [[? ?] | (S & R & ? & ?)]. branch~ 1.
  splits~. substs~.
  right. exists S R; splits~.
  destruct (res_type R); try solve [left; splits~; discriminate].
  right; splits~. subst. inverts~ H0.
Qed.

Definition if_any_or_throw_post (K1 K2 : _ -> _ -> result) o o1 :=
  (o1 = out_div /\ o = o1) \/
  (exists S R, o1 = out_ter S R /\
    (   (res_type R <> restype_throw /\ K1 S R = o)
     \/ (res_type R = restype_throw /\ exists (v : value), res_value R = v
           /\ res_label R = label_empty /\ K2 S v = o))). (* Didn't worked when writing [exists (v : value), R = res_throw v]. *)

Lemma if_any_or_throw_out : forall W K1 K2 o,
  if_any_or_throw W K1 K2 = res_out o ->
  isout W (if_any_or_throw_post K1 K2 o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_ter_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&?&?)]; subst; [ left* | right ].
  exists S R. split~. destruct (res_type R); tryfalse; simple*.
  right. destruct (res_value R); tryfalse; simple*. split*.
  forwards*: if_empty_label_out.
Admitted. (*faster*)

Definition if_success_or_return_post (K1 : state -> result) (K2 : state -> resvalue -> result) o o1 :=
     (o1 = out_div /\ o = o1)
  \/ exists S R, o1 = out_ter S R /\
     (   (res_type R = restype_normal /\ res_label R = label_empty /\ K1 S = o)
      \/ (res_type R = restype_return /\ res_label R = label_empty /\ K2 S (res_value R) = o)
      \/ (res_type R <> restype_normal /\ res_type R <> restype_return /\ o1 = o)).

Lemma if_success_or_return_out : forall W (K1 : state -> result) (K2 : state -> resvalue -> result) o,
  if_success_or_return W K1 K2 = o ->
  isout W (if_success_or_return_post K1 K2 o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_ter_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  exists S R. split~. destruct (res_type R); tryfalse; simple*.
  branch 1. forwards*: if_empty_label_out.
  branch 3. inverts* E.
  branch 3. inverts* E.
  branch 2. forwards*: if_empty_label_out.
  branch 3. inverts* E.
Admitted. (*faster*)

(* TODO: misssing
    if_normal_continue_or_break *)

Definition if_break_post (K : _ -> _ -> result) o o1 :=
     (o1 = out_div /\ o = o1)
  \/ (exists S R, o1 = out_ter S R /\
      (   (res_type R <> restype_break /\ o1 = o)
       \/ (res_type R = restype_break /\ K S R = o))).

Lemma if_break_out : forall W K o,
  if_break W K = o ->
  isout W (if_break_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_ter_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  exists S R. split~. destruct (res_type R); try inverts E; simple*.
Admitted. (*faster*)

Definition if_value_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S v, o1 = out_ter S (res_val v) /\ K S v = o.

Lemma if_value_out : forall W K o,
  if_value W K = res_out o ->
  isout W (if_value_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_success_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_bool_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S z, o1 = out_ter S (res_val (prim_bool z)) /\ K S z = o.

Lemma if_bool_out : forall W K o,
  if_bool W K = res_out o ->
  isout W (if_bool_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)


Definition if_object_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S l, o1 = out_ter S (res_val (value_object l)) /\ K S l = o.

Lemma if_object_out : forall W K o,
  if_object W K = res_out o ->
  isout W (if_object_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Admitted. (*faster*)


Definition if_string_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S s, o1 = out_ter S (res_val (prim_string s)) /\ K S s = o.

Lemma if_string_out : forall W K o,
  if_string W K = res_out o ->
  isout W (if_string_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)


Definition if_number_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S n, o1 = out_ter S (res_val (prim_number n)) /\ K S n = o.

Lemma if_number_out : forall W K o,
  if_number W K = res_out o ->
  isout W (if_number_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1 & WE & P): if_value_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_prim_post (K : _ -> _ -> result) o o1 :=
  eqabort o1 o \/
  exists S w, o1 = out_ter S (res_val (value_prim w)) /\ K S w = o.

Lemma if_prim_out : forall W K o,
  if_prim W K = res_out o ->
  isout W (if_prim_post K o).
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_out (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Admitted. (*faster*)

Lemma if_abort_out : forall T o K (t : T),
  if_abort o K = result_some t ->
  abort o /\ K tt = result_some t.
Proof. introv H. destruct* o. simpls. cases_if*. Qed.

Definition if_spec_post (A B:Type) K (b:specret B) y :=
     (exists o, y = specret_out o /\ b = specret_out o /\ abort o)
  \/ (exists (S:state) (a:A), y = specret_val S a /\ K S a = result_some b).

Lemma if_spec_out : forall (A B : Type) (W : specres A) K (b : specret B),
  if_spec W K = result_some b ->
  exists y, W = result_some y /\ if_spec_post K b y.
Proof.
  introv E. unfolds in E. unfolds in E.
  destruct W; tryfalse. exists s. split~.
  unfolds. destruct s; [ right | left ].
    exists___*.
    lets (?&H): if_abort_out E. inverts H. exists___*.
Admitted. (* faster *)


Definition if_ter_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (R : res), o = out_ter S R /\ K S R = result_some y).

Lemma if_ter_spec : forall T W K (y : specret T),
  if_ter W K = result_some y ->
  isout W (if_ter_spec_post K y).
Proof.
  introv H. destruct W as [[|o1]| | | ]; simpls; tryfalse_nothing.
  exists o1. splits~. unfolds. destruct o1 as [|S R].
   inverts* H.
   jauto.
Qed.

Definition if_success_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (rv : resvalue), o = out_ter S rv /\ K S rv = result_some y).

Lemma if_success_spec : forall T W K (y : specret T),
  if_success W K = result_some y ->
  exists (o : out), W = o /\ if_success_spec_post K y o. (* LATER:  Change to [isout] *)
Proof.
  introv E. forwards~ (o1&WE&P): if_ter_spec (rm E). subst W. eexists. splits*.
  inversion_clear P as [[? ?]|(S&R&?&H)]. substs. branch~ 1.
  substs. destruct R as [rt rv' rl]. destruct~ rt; simpls;
     try solve [inverts H; branch 1; splits~; prove_abort].
   forwards~ (?&?): if_empty_label_out (rm H). simpls. substs.
    branch 2. repeat eexists. auto*.
Qed.

Definition if_value_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (v : value), o = out_ter S v /\ K S v = result_some y).

Lemma if_value_spec : forall T W K (y : specret T),
  if_value W K = result_some y ->
  exists (o : out), W = o /\ if_value_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_success_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_prim_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (w : prim), o = out_ter S w /\ K S w = result_some y).

Lemma if_prim_spec : forall T W K (y : specret T),
  if_prim W K = result_some y ->
  exists (o : out), W = o /\ if_prim_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_bool_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (b : bool), o = out_ter S b /\ K S b = result_some y).

Lemma if_bool_spec : forall T W K (y : specret T),
  if_bool W K = result_some y ->
  exists (o : out), W = o /\ if_bool_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_number_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (n : number), o = out_ter S n /\ K S n = result_some y).

Lemma if_number_spec : forall T W K (y : specret T),
  if_number W K = result_some y ->
  exists (o : out), W = o /\ if_number_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_string_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (s : string), o = out_ter S s /\ K S s = result_some y).

Lemma if_string_spec : forall T W K (y : specret T),
  if_string W K = result_some y ->
  exists (o : out), W = o /\ if_string_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. destruct p; tryfalse. exists___*.
Admitted. (*faster*)

Definition if_object_spec_post T K (y:specret T) o :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (l : object_loc), o = out_ter S l /\ K S l = result_some y).

Lemma if_object_spec : forall T W K (y : specret T),
  if_object W K = result_some y ->
  exists (o : out), W = o /\ if_object_spec_post K y o.
Proof.
  introv E. unfolds in E.
  forwards~ (o1&WE&P): if_value_spec (rm E). exists o1. split~.
  unfolds. unfolds in P.
  inversion_clear P as [[? ?]|(S&R&H&E)]; subst; [ left* | right ].
  destruct R; tryfalse. exists___*.
Qed.


(************************************************************)
(* ** Correctness Tactics *)

(** [prove_not_intercept] proves a goal of
    the form "~ abort_intercepted_* _" *)

Ltac prove_not_intercept :=
let H := fresh in intros H; try solve [ inversion H; false~ ].

Hint Extern 1 (~ abort_intercepted_expr _) => prove_not_intercept.
Hint Extern 1 (~ abort_intercepted_stat _) => prove_not_intercept.
Hint Extern 1 (~ abort_intercepted_prog _) => prove_not_intercept.

Ltac abort_tactic L :=
  try subst; apply L;
  [ simpl; congruence
  | try prove_abort
  | try prove_not_intercept ].

Tactic Notation "abort_expr" :=
    abort_tactic red_expr_abort.
Tactic Notation "abort_stat" :=
    abort_tactic red_stat_abort.
Tactic Notation "abort_prog" :=
    abort_tactic red_prog_abort.
Tactic Notation "abort_spec" :=
    abort_tactic red_spec_abort.
Tactic Notation "abort" :=
  match goal with
  | |- red_expr _ _ _ _ => abort_expr
  | |- red_stat _ _ _ _ => abort_stat
  | |- red_prog _ _ _ _ => abort_prog
  | |- red_spec _ _ _ _ => abort_spec
  end.

(** [run_select_ifres] selects the appropriate "out" lemma *)

Ltac run_select_extra T := fail.

Ltac run_select_ifres H :=
  match type of H with ?T = _ => match T with
  | @if_ter nothing _ _ => constr:(if_ter_out)
  | @if_success nothing _ _ => constr:(if_success_out)
  | @if_value nothing _ _ => constr:(if_value_out)
  | @if_void nothing _ _ => constr:(if_void_out)
  | if_break _ _ => constr:(if_break_out)
  | @if_object nothing _ _ => constr:(if_object_out)
  | @if_bool nothing _ _ => constr:(if_bool_out)
  | @if_string nothing _ _ => constr:(if_string_out)
  | @if_number nothing _ _ => constr:(if_number_out)
  | @if_prim nothing _ _ => constr:(if_prim_out)
  | if_ter _ _ => constr:(if_ter_spec)
  | if_success _ _ => constr:(if_success_spec)
  | if_value _ _ => constr:(if_value_spec)
  | if_bool _ _ => constr:(if_bool_spec)
  | if_string _ _ => constr:(if_string_spec)
  | if_object _ _ => constr:(if_object_spec)
  | if_number _ _ => constr:(if_number_spec)
  | if_prim _ _ => constr:(if_prim_spec)
  | if_spec _ _ => constr:(if_spec_out)
  | if_void _ _ => constr:(if_void_out)
  | if_not_throw _ _ => constr:(if_not_throw_out)
  | if_any_or_throw _ _ _ => constr:(if_any_or_throw_out)
  | if_success_or_return _ _ _ => constr:(if_success_or_return_out)
  | if_success_state _ _ _ => constr:(if_success_state_out)
  | ?x => run_select_extra T
  end end.

(* template:
Ltac run_select_extra T ::=
  match T with
  | if_any_or_throw _ _ _ => constr:(if_any_or_throw_out)
  end.
*)

(** [run_select_proj] is used to obtain automatically
    the right correctness lemma out of the correctness record *)

Ltac run_select_proj_extra_error HT := fail.
Ltac run_select_proj_extra_ref HT := fail.
Ltac run_select_proj_extra_conversions HT := fail.
Ltac run_select_proj_extra_construct HT := fail.
Ltac run_select_proj_extra_get_value HT := fail.

Ltac run_select_proj H :=
  match type of H with ?T = _ => let HT := get_head T in
  match HT with
  | runs_type_expr => constr:(runs_type_correct_expr)
  | runs_type_stat => constr:(runs_type_correct_stat)
  | runs_type_prog => constr:(runs_type_correct_prog)
  | runs_type_call => constr:(runs_type_correct_call)
  | runs_type_construct => constr:(runs_type_correct_construct)
  | runs_type_function_has_instance => constr:(runs_type_correct_function_has_instance)
  | runs_type_object_has_instance => constr:(runs_type_correct_object_has_instance)
  | runs_type_stat_while => constr:(runs_type_correct_stat_while)
  | runs_type_stat_do_while => constr:(runs_type_correct_stat_do_while)
  | runs_type_stat_for_loop => constr:(runs_type_correct_stat_for_loop)
  | runs_type_object_delete => constr:(runs_type_correct_object_delete)
  | runs_type_object_get_own_prop => constr:(runs_type_correct_object_get_own_prop)
  | runs_type_object_get_prop => constr:(runs_type_correct_object_get_prop)
  | runs_type_object_get => constr:(runs_type_correct_object_get)
  | runs_type_object_proto_is_prototype_of => constr:(runs_type_correct_object_proto_is_prototype_of)
  | runs_type_object_put => constr:(runs_type_correct_object_put)
  | runs_type_equal => constr:(runs_type_correct_equal)
  | runs_type_to_integer => constr:(runs_type_correct_to_integer)
  | runs_type_to_string => constr:(runs_type_correct_to_string)
  | runs_type_array_element_list => constr:(runs_type_correct_array_element_list)
  | runs_type_object_define_own_prop_array_loop => constr:(runs_type_correct_object_define_own_prop_array_loop)
  | ?x => run_select_proj_extra_error HT
  | ?x => run_select_proj_extra_ref HT
  | ?x => run_select_proj_extra_conversions HT
  | ?x => run_select_proj_extra_construct HT
  | ?x => run_select_proj_extra_get_value HT
  end end.

(** [prove_runs_type_correct] discharges the trivial goal
    that consists in invoking the induction hypothesis*)

Ltac prove_runs_type_correct :=
  match goal with |- runs_type_correct _ => assumption end.

(* [run_hyp H] exploits the induction hypothesis
   on [runs_type_correct] to the hypothesis [H] *)

Ltac run_hyp_core H R :=
  let H' := fresh in rename H into H';
  let Proj := run_select_proj H' in
  lets R: Proj (rm H');
  try prove_runs_type_correct.

(** [select_ind_hyp] returns the induction hypothesis
    on [runs_type_correct] *)

Ltac select_ind_hyp tt :=
  match goal with IH: runs_type_correct _ |- _ => constr:(IH) end.

(* old run_hyp H:
Ltac run_hyp_core H R :=
  let H' := fresh in rename H into H';
  let IH := select_ind_hyp tt in
  let Proj := run_select_proj H' in
  lets R: Proj IH (rm H').
*)

Tactic Notation "run_hyp" hyp(H) "as" simple_intropattern(R) :=
  run_hyp_core H R.

Tactic Notation "run_hyp" hyp(H) :=
  let T := fresh in rename H into T;
  run_hyp T as H.

Tactic Notation "run_hyp" :=
  match goal with H: _ = result_some _ |- _ => run_hyp H end.

Tactic Notation "run_hyp" "*" hyp(H) :=
  run_hyp H; auto*.

Tactic Notation "run_hyp" "*" :=
  run_hyp; auto*.


(* [run_pre] exploits the appropriate "out" lemma, whether it comes
   from the correctness record or it is an auxiliary lemma. *)

Ltac run_pre_ifres H o1 R1 K :=
   let L := run_select_ifres H in
   lets (o1&R1&K): L (rm H). (* deconstruction of the "isout" *)

Ltac run_pre_core H o1 R1 K :=
   run_pre_ifres H o1 R1 K;
   let O1 := fresh "O1" in
   try (rename R1 into O1; run_hyp O1 as R1).

Tactic Notation "run_pre" hyp(H) "as" ident(o1) ident(R1) ident(K) :=
  let T := fresh in rename H into T;
  run_pre_core T o1 R1 K.

Tactic Notation "run_pre_ifres" "as" ident(o1) ident(R1) :=
  unfold result_some_out in *; unfold res_to_res_void in * ; unfold result_out in *; unfold res_out in *;
  (* LATER: improve unfolds *)
  match goal with H: _ = result_some _ |- _ =>
    let T := fresh in rename H into T;
    run_pre_ifres T o1 R1 H end.

Tactic Notation "run_pre" "as" ident(o1) ident(R1) :=
  unfold result_some_out in *; unfold res_to_res_void in * ; unfold result_out in *; unfold res_out in *;
  (* LATER: improve unfolds *)
  match goal with H: _ = result_some _ |- _ =>
    let T := fresh in rename H into T;
    run_pre_core T o1 R1 H end.

Tactic Notation "run_pre" "as" ident(o1) :=
  let R1 := fresh "R1" o1 in
  run_pre as o1 R1.

Tactic Notation "run_pre" :=
  let o1 := fresh "o1" in let R1 := fresh "R1" in
  run_pre as o1 R1.

(** [run_apply Red o1 R1] applys a reduction rule on a given
    [o1] or reduction reaching [o1]. *)

Tactic Notation "run_apply" constr(Red) constr(o1orR1) :=
  applys Red o1orR1.

Tactic Notation "run_apply" constr(Red) constr(o1) constr(R1) :=
  first [ applys Red (rm R1)
        | applys Red o1 ].

(** [run_post] decomposes the conclusion of the "out"
    lemma *)

Ltac run_post_run_expr_get_value := fail.

Ltac run_post_extra := fail.

Ltac run_post_core :=
  let Er := fresh "Er" in
  let Ab := fresh "Ab" in
  let S := fresh "S" in
  let O1 := fresh "O1" in
  let go H X :=
    destruct H as [(Er&Ab)|(S&X&O1&H)];
    [ try abort | try subst_hyp O1 ] in
  match goal with
  | H: if_ter_post _ _ _ |- _ =>
    let R := fresh "R" in go H R
  | H: if_success_post _ _ _ |- _ =>
    let rv := fresh "rv" in go H rv
  | H: if_value_post _ _ _ |- _ =>
    let v := fresh "v" in go H v
  | H: if_void_post _ _ _ |- _ =>
    destruct H as [(Er&Ab)|(S&O1&H)];
    [ try abort | try subst_hyp O1 ]
  | H: if_not_throw_post _ _ _ |- _ =>
    let R := fresh "R" in
    let N := fresh "N" in let v := fresh "v" in
    let E := fresh "E" in let L := fresh "L" in
    destruct H as [(Er & Ab) | (S & R & O1 & [(N & H) | (N & H)])];
    [try abort | try subst_hyp O1 | try abort]
  | H: if_break_post _ _ _ |- _ =>
    let R := fresh "R" in let E := fresh "E" in
    let HT := fresh "HT" in
    destruct H as [(Er&E)|(S&R&O1&[(HT&E)|(HT&H)])];
    [ try abort | try subst_hyp O1 | try subst_hyp O1 ]
  | H: if_object_post _ _ _ |- _ =>
    let l := fresh "l" in go H l
  | H: if_bool_post _ _ _ |- _ =>
    let b := fresh "b" in go H b
  | H: if_string_post _ _ _ |- _ =>
    let s := fresh "s" in go H s
  | H: if_number_post _ _ _ |- _ =>
    let m := fresh "m" in go H m
  | H: if_prim_post _ _ _ |- _ =>
    let w := fresh "w" in go H w
  | H: if_ter_spec_post _ _ _ |- _ =>
    let R := fresh "R" in go H R
  | H: if_success_spec_post _ _ _ |- _ =>
    let rv := fresh "rv" in go H rv
  | H: if_value_spec_post _ _ _ |- _ =>
    let v := fresh "v" in go H v
  | H: if_bool_spec_post _ _ _ |- _ =>
    let b := fresh "b" in go H b
  | H: if_string_spec_post _ _ _ |- _ =>
    let s := fresh "s" in go H s
  | H: if_object_spec_post _ _ _ |- _ =>
    let l := fresh "l" in go H l
  | H: if_number_spec_post _ _ _ |- _ =>
    let m := fresh "m" in go H m
  | H: if_prim_spec_post _ _ _ |- _ =>
    let w := fresh "w" in go H w
  | H: if_spec_post _ _ _ |- _ =>
    let o := fresh "o" in let Er' := fresh "Er" in
    let S := fresh "S" in let a := fresh "a" in
    destruct H as [(o&Er&Er'&Ab)|(S&a&O1&H)];
    [ try abort | try subst_hyp O1 ]
  | H: if_any_or_throw_post _ _ _ _ |- _ =>
    let R := fresh "R" in
    let N := fresh "N" in let v := fresh "v" in
    let E := fresh "E" in let L := fresh "L" in
    destruct H as [(Er&Ab)|(S&R&O1&[(N&H)|(N&v&E&L&H)])];
    [ try subst_hyp Er; try subst_hyp Ab | try subst_hyp O1 | try subst_hyp O1 ]
  | H: if_success_or_return_post _ _ _ _ |- _ =>
     let S := fresh "S" in let R := fresh "R" in
     let o1 := fresh "o1" in let W1 := fresh "W1" in let O1 := fresh "O1" in
     let E1 := fresh "E" in let E2 := fresh "E" in
     destruct H as [(Er&Ab)|(S&R&O1&[(E1&E2&K)|[(E1&E2&K)|(E1&E2&K)]])];
    [ try subst_hyp Er; try subst_hyp Ab; try abort
    | try subst_hyp O1 | try subst_hyp O1 | try subst_hyp O1 ]
  | H: if_success_state_post _ _ _ _ |- _ =>
    (* LATER: improve the statement of the lemma *)
     let S := fresh "S" in let R := fresh "R" in
     let O1 := fresh "O1" in
     let E1 := fresh "E" in let E2 := fresh "E" in let rv := fresh "rv" in
     destruct H as [(Er&Ab)|[(S&R&O1&E1&H)|[(S&R&O1&E1&E2&H)|(S&rv&O1&H)]]];
    [ try subst_hyp Er; try subst_hyp Ab; try abort
    | try subst_hyp O1 | try subst_hyp O1 | try subst_hyp O1 ]
  | |- _ => run_post_run_expr_get_value
  | |- _ => run_post_extra
  end.

(* template
Ltac run_post_extra ::=
  let Er := fresh "Er" in let Ab := fresh "Ab" in
  let O1 := fresh "O1" in let S := fresh "S" in
  match goal with
  | H: if_any_or_throw_post _ _ _ _ |- _ => ...
  end.

*)

Tactic Notation "run_post" :=
  run_post_core.

(** [run_inv] simplifies equalities in goals
    by performing inversions on equalities. *)

Ltac run_inv :=
  unfold result_some_out in *; unfold res_to_res_void in * ; unfold out_retn in *; unfold result_out in *;
  repeat
  match goal with
  | H: resvalue_value ?v = resvalue_value ?v |- _ => clear H
  | H: resvalue_value _ = resvalue_value _ |- _ => inverts H
  | H: res_spec _ _ = _ |- _ => unfold res_spec in H
  | H: _ = res_spec _ _ |- _ => unfold res_spec in H
  | H: res_out _ = _ |- _ => unfold res_out in H
  | H: _ = res_out _ |- _ => unfold res_out in H
  | H: res_ter _ _ = _ |- _ => unfold res_ter in H
  | H: _ = res_ter _ _ |- _ => unfold res_ter in H
  | H: result_some ?o = result_some ?o |- _ => clear H
  | H: result_some _ = result_some _ |- _ => inverts H
  | H: out_ter ?S ?R = out_ter ?S ?R |- _ => clear H
  | H: out_ter _ _ = out_ter _ _ |- _ => inverts H
  | H: res_intro ?t ?v ?l = res_intro ?t ?v ?l |- _ => clear H
  | H: res_intro _ _ _ = res_intro _ _ _ |- _ => inverts H
  | H: ret _ _ = _ |- _ => unfold ret in H
  | H: _ = ret _ _ |- _ => unfold ret in H
  | H: ret_void _ = _ |- _ => unfold ret_void in H
  | H: _ = ret_void _ |- _ => unfold ret_void in H
  | H: res_void _ = _ |- _ => unfold res_void in H
  | H: _ = res_void _ |- _ => unfold res_void in H
  | H: specret_val ?S ?R = specret_val ?S ?R |- _ => clear H
  | H: specret_val _ _ = specret_val _ _ |- _ => inverts H
  | H: specret_out ?o = specret_out ?o |- _ => clear H
  | H: specret_out _ = _ |- _ => inverts H
  | H: _ = specret_out _ |- _ => inverts H
  | H: out_from_retn ?sp = out_from_retn ?sp |- _ => clear H
  | H: out_from_retn _ = out_from_retn _ |- _ => inverts H
  end.

(** [runs_inv] is the same as [run_inv] followed by subst. *)

Ltac runs_inv :=
  run_inv; subst.

(** Auxiliary tactics to select/check the current "out" *)

Ltac run_get_current_out tt :=
  match goal with
  | |- red_expr _ _ _ ?o => o
  | |- red_stat _ _ _ ?o => o
  | |- red_prog _ _ _ ?o => o
  | |- red_spec _ _ _ ?o => o
  | |- red_javascript _ ?o => o
  end.

Ltac run_check_current_out o :=
  match goal with
  | |- red_expr _ _ _ o => idtac
  | |- red_stat _ _ _ o => idtac
  | |- red_prog _ _ _ o => idtac
  | |- red_spec _ _ _ o => idtac
  | |- red_javascript _ o => idtac
  end.

(** [run_step Red] combines [run_pre], [run_apply Red] and calls
    [run_post] on the main reduction subgoal, followed
    with a cleanup using [run_inv] *)

Ltac run_step Red :=
  let o1 := fresh "o1" in let R1 := fresh "R1" in
  run_pre as o1 R1;
  match Red with ltac_wild => idtac | _ =>
    let o := run_get_current_out tt in
    run_apply Red o1 R1;
    try (run_check_current_out o; run_post; run_inv; try assumption)
  end.

(** [run_step_using Red Lem] combines [run_pre],
    a forward to exploit the correctness lemma [Lem],
    [run_apply Red] and calls
    [run_post] on the main reduction subgoal, followed
    with a cleanup using [run_inv] *)

Ltac run_step_using Red Lem :=
  let o1 := fresh "o1" in let O1 := fresh "O1" in
  let R1 := fresh "R1" in
  run_pre_ifres as o1 O1;
  lets R1: Lem (rm O1);
  try prove_runs_type_correct;
  match Red with ltac_wild => idtac | _ =>
    let o := run_get_current_out tt in
    run_apply Red o1 R1;
    try (run_check_current_out o; run_post; run_inv; try assumption)
  end.

(** [run_simpl] is intended for simplyfing simple monads
    that do not match over a result, then run
    [run_inv] to clean up the goal. *)

Ltac run_simpl_run_error H T K := fail.

Ltac run_simpl_base H K :=
  let E := fresh "E" in
  match type of H with ?T = _ => match T with
  | if_some _ _ =>
     let x := fresh "x" in
     lets (x&E&K): if_some_out (rm H)
  | if_some_or_default _ _ _ =>
     let x := fresh "x" in
     let E1 := fresh "E" in let E2 := fresh "E" in
     lets [(E1&E2)|(n&E&K)]: if_some_or_default_out (rm H)
  | if_empty_label _ _ _ =>
     lets (E&K): if_empty_label_out (rm H)
  | ?x => run_simpl_run_error H T K
  | ?x => run_hyp_core H K
  end end.


Ltac run_simpl_core H K :=
  run_simpl_base H K; run_inv.

Tactic Notation "run_simpl" ident(H) "as" ident(K) :=
  let T := fresh in rename H into T;
  run_simpl_core T K.

Tactic Notation "run_simpl" :=
  unfold result_some_out in *; unfold res_to_res_void in * ; unfold result_out in *; unfold res_out in *;
  (* LATER: handle unfold *)
  match goal with H: _ = result_some _ |- _ =>
    let H' := fresh in rename H into H';
    run_simpl_core H' H
  end.

(** [run Red] is the same as [run_step Red].
    [run] without arguments is the same as [run_simpl].
    [runs] is same as [run] followed with [subst]. *)

Tactic Notation "run" constr(Red) :=
  run_step Red.

Tactic Notation "run" "~" constr(Red) :=
  run Red; auto~.

Tactic Notation "run" "*" constr(Red) :=
  run Red; auto*.

Tactic Notation "run" constr(Red) "using" constr(Lem) :=
  run_step_using Red Lem.

Tactic Notation "run" "~" constr(Red) "using" constr(Lem) :=
  run Red using Lem; auto~.

Tactic Notation "run" "*" constr(Red) "using" constr(Lem) :=
  run Red using Lem; auto*.


Tactic Notation "runs" constr(Red) :=
  run Red; subst.

Tactic Notation "runs" "~" constr(Red) :=
  run Red; subst; auto~.

Tactic Notation "runs" "*" constr(Red) :=
  run Red; subst; auto*.

Tactic Notation "run" :=
  run_simpl.

Tactic Notation "run" "~" :=
  run; auto~.

Tactic Notation "run" "*" :=
  run; auto*.

Tactic Notation "runs" :=
  run; subst.

Tactic Notation "runs" "~" :=
  runs; auto~.

Tactic Notation "runs" "*" :=
  runs; auto*.


(* debugging of [run Red]:
  run_pre as o1 R1.
    or: run_pre H as o1 R1 K. (* where H is the hypothesis *)
    or: run_pre_core H o1 R1 K. (* where H is the hypothesis *)
    or: run_pre_lemma H o1 R1 K. (* where H is the hypothesis *)
  run_apply __my_red_lemma__ R1. (* where R1 is the red hypothesis *)
  run_post.
  run_inv.
*)


(************************************************************)
(* ** Correctness Lemmas *)

Lemma type_of_prim_not_object : forall w,
  type_of w <> type_object.
Proof. destruct w; simpl; try congruence. Qed.

Hint Resolve type_of_prim_not_object.

Lemma is_lazy_op_correct : forall op,
  match is_lazy_op op with
  | None => regular_binary_op op
  | Some b => lazy_op op b
  end.
Proof.
  Hint Constructors lazy_op.
  unfold regular_binary_op.
  intros. destruct op; simple*.
Admitted. (* faster *)

Lemma run_object_method_correct : forall Z (Proj : _ -> Z) S l (z : Z),
  run_object_method Proj S l = Some z ->
  object_method Proj S l z.
Proof.
  introv B. unfolds. forwards (O&Bi&E): LibOption.map_on_inv B.
  forwards: @pick_option_correct Bi. exists* O.
Qed.

Lemma run_object_heap_set_extensible_correct : forall b S l S',
  run_object_heap_set_extensible b S l = Some S' ->
  object_heap_set_extensible b S l S'.
Proof.
  introv R. unfolds in R. forwards (O&H&E): LibOption.map_on_inv (rm R).
  forwards: @pick_option_correct (rm H). exists O. splits~.
Qed.

Lemma build_error_correct : forall S C vproto vmsg o,
  build_error S vproto vmsg = o ->
  red_expr S C (spec_build_error vproto vmsg) o.
Proof.
  introv R. unfolds in R.
  match goal with H: context [object_alloc ?s ?o] |- _ => sets_eq X: (object_alloc s o) end.
  destruct X as (l&S'). cases_if.
  applys~ red_spec_build_error EQX. run_inv.
   applys~ red_spec_build_error_1_no_msg.
Qed.

Lemma run_error_correct' : forall T S ne o C,
  run_error S ne = (res_out o : specres T) ->
  red_expr S C (spec_error ne) o /\ abort o.
Proof.
  introv R. unfolds in R. run_pre as o1 R1. forwards R0: build_error_correct (rm R1).
  applys_and red_spec_error R0. run_post.
   run_inv. splits~. abort.
   run_inv. splits; [|prove_abort]. apply~ red_spec_error_1.
Qed.

Lemma run_error_correct : forall T S ne o C,
  run_error S ne = (res_out o : specres T) ->
  red_expr S C (spec_error ne) o.
Proof. intros. applys* run_error_correct'. Qed.

Lemma run_error_correct_2 : forall T S (ne : native_error) o C,
  run_error S ne = (res_out o : specres T) -> red_expr S C (spec_error ne) o.
Proof. intros. apply* run_error_correct. Qed.

Hint Resolve run_error_correct run_error_correct_2.

Ltac run_simpl_run_error H T K ::=
  match T with run_error _ _ =>
     let N := fresh "N" in
     let C := match goal with |- _ _ ?C _ _ => constr:(C) end in
     lets (K&N): run_error_correct C (rm H)
  end.

Lemma run_error_not_some_out_res : forall S v T,
  ~ (run_error S native_error_type = result_some (@specret_out T (out_ter S (res_val v)))).
Proof.
  set (C := execution_ctx_intro nil nil (value_prim (prim_bool true)) false). introv Hyp.
  apply run_error_correct' with (C := C) in Hyp. 
  destruct Hyp as (Hred & Habort).
  inverts Hred. simpls. inverts H.
  inverts H3. simpls. inverts H.
  inverts H0. simpls. inverts H.
  inverts H9. simpls. inverts H.
  inverts Habort. unfolds abrupt_res.
  false~ H0. false~ H3.
Admitted. (*faster*)

Lemma out_error_or_void_correct : forall S C str (ne : native_error) o,
  out_error_or_void S str ne = o ->
  red_expr S C (spec_error_or_void str ne) o /\
    (~ abort o -> o = out_void S).
Proof.
  introv E. unfolds in E. cases_if.
   applys_and red_spec_error_or_void_true.
   forwards~ (RC&Cr): run_error_correct' E. splits*.
   inverts E. splits~. apply~ red_spec_error_or_void_false.
Qed.

Lemma out_error_or_cst_correct' : forall S C str (ne : native_error) v o,
  out_error_or_cst S str ne v = o ->
  red_expr S C (spec_error_or_cst str ne v) o /\
    (~ abort o -> o = out_ter S v).
Proof.
  introv E. unfolds in E. cases_if.
   applys_and red_spec_error_or_cst_true. forwards~ (RC&Cr): run_error_correct' E. splits*.
   inverts E. splits~. apply~ red_spec_error_or_cst_false.
Qed.

(* LATER: clean up redundant proof for the direct case *)
Lemma out_error_or_cst_correct : forall S C str ne v o,
  out_error_or_cst S str ne v = o ->
  red_expr S C (spec_error_or_cst str ne v) o.
Proof.
  introv HR. unfolds in HR. case_if.
  applys* red_spec_error_or_cst_true.
  run_inv. applys* red_spec_error_or_cst_false.
Qed.


Ltac run_select_proj_extra_error HT ::=
  match HT with
  | run_error => constr:(run_error_correct)
  | run_object_method => constr:(run_object_method_correct)
  end.


(**************************************************************)
(** ** Object Get *)

Lemma object_has_prop_correct : forall runs S C l x o,
  runs_type_correct runs ->
  object_has_prop runs S C l x = o ->
  red_expr S C (spec_object_has_prop l x) o.
Proof.
  introv IH HR. unfolds in HR. run_simpl. run_hyp E as M.
  applys~ red_spec_object_has_prop M. destruct x0.
  run red_spec_object_has_prop_1_default using runs_type_correct_object_get_prop.
  apply~ red_spec_object_has_prop_2. rewrite decide_def. repeat cases_if~.
Qed.

Lemma run_object_get_prop_correct : forall runs S C l x y,
  runs_type_correct runs ->
  run_object_get_prop runs S C l x = result_some y ->
  red_spec S C (spec_object_get_prop l x) y.
Proof.
  introv IH HR. unfolds in HR.
  run. applys* red_spec_object_get_prop.
   applys* run_object_method_correct. clear E.
  destruct x0; tryfalse.
  run red_spec_object_get_prop_1_default. case_if.
   subst. run. applys red_spec_object_get_prop_2_undef.
    applys* run_object_method_correct.
    destruct x0; tryfalse.
      destruct p; tryfalse. run_inv. applys red_spec_object_get_prop_3_null.
      applys red_spec_object_get_prop_3_not_null. run_hyp*.
  run_inv. destruct a; tryfalse.
   applys* red_spec_object_get_prop_2_not_undef.
Admitted. (*faster*)

Lemma object_get_builtin_correct : forall runs S C B (vthis:value) l x o,
  runs_type_correct runs ->
  object_get_builtin runs S C B vthis l x = o ->
  red_expr S C (spec_object_get_1 B vthis l x) o.
Proof.
  introv IH HR. unfolds in HR.
  let_name as Mdefault.
  asserts Mdefault_correct: (forall S l o,
    Mdefault S l = res_out o ->
    red_expr S C (spec_object_get_1 builtin_get_default vthis l x) o).
    clear HR o. subst. introv HR.
   run red_spec_object_get_1_default. destruct a as [|[Ad|Aa]].
    run_inv. applys* red_spec_object_get_2_undef.
    run_inv. applys* red_spec_object_get_2_data.
    applys red_spec_object_get_2_accessor.
     destruct (attributes_accessor_get Aa); tryfalse.
       destruct p; tryfalse. run_inv.
        applys* red_spec_object_get_3_accessor_undef.
       applys* red_spec_object_get_3_accessor_object. run_hyp*.
  clear EQMdefault.
  let_name as Mfunction.
  asserts Mfunction_correct: (forall S o,
    Mfunction S = res_out o ->
    red_expr S C (spec_object_get_1 builtin_get_function vthis l x) o).
    clear HR o. subst. introv HR.
    run* red_spec_object_get_1_function. clear R1.
    case_if.
     applys* red_spec_function_get_1_error.
     run_inv. applys* red_spec_function_get_1_normal.
      clear EQMfunction. destruct B; tryfalse.
      applys~ Mdefault_correct.
      applys~ Mfunction_correct.
  (* argument object *)
  run. forwards* obpm: run_object_method_correct.
  run. substs. run~ red_spec_object_get_args_obj.
  destruct a. (* LTAC ARTHUR:  This [a] wasn't properly named. *)
   apply* red_spec_object_get_args_obj_1_undef.
   run_hyp. apply~ red_spec_object_get_args_obj_1_attrs.
Admitted. (* faster *)


Lemma run_object_get_correct : forall runs S C l x o,
  runs_type_correct runs ->
  run_object_get runs S C l x = o ->
  red_expr S C (spec_object_get l x) o.
Proof.
  introv IH HR. unfolds in HR. run.
  applys* red_spec_object_get.
   applys* run_object_method_correct. clear E.
  applys* object_get_builtin_correct.
Qed.


Lemma object_can_put_correct : forall runs S C l x o,
  runs_type_correct runs ->
  object_can_put runs S C l x = o ->
  red_expr S C (spec_object_can_put l x) o.
Proof.
  introv IH HR. unfolds in HR. run. run_hyp E as CP.
  applys~ red_spec_object_can_put CP. destruct x0.
  run red_spec_object_can_put_1_default. destruct a.
   run. run_hyp E as P. applys~ red_spec_object_can_put_2_undef P.
    destruct x0 as [()|lproto]; tryfalse.
     run. run_hyp E as E. apply~ red_spec_object_can_put_4_null.
     run red_spec_object_can_put_4_not_null using run_object_get_prop_correct.
      destruct a as [|[Ad|Aa]].
       run. run_hyp E as E. apply~ red_spec_object_can_put_5_undef.
       run. run_hyp E as E. applys~ red_spec_object_can_put_5_data E. destruct x0.
        applys~ red_spec_object_can_put_6_extens_true.
        applys~ red_spec_object_can_put_6_extens_false.
       run_inv. apply~ red_spec_object_can_put_5_accessor. rewrite decide_def.
        repeat cases_if~.
   destruct a; run_inv.
    apply~ red_spec_object_can_put_2_data.
    apply~ red_spec_object_can_put_2_accessor. rewrite decide_def. repeat cases_if~.
Qed.

Lemma object_default_value_correct : forall runs S C l pref o,
  runs_type_correct runs ->
  object_default_value runs S C l pref = o ->
  red_expr S C (spec_object_default_value l pref) o.
Proof.
  introv IH HR. unfolds in HR.
  run. lets H: run_object_method_correct (rm E).
  applys* red_spec_object_default_value (rm H).
  destruct x.
  let_name as M.
  asserts M_correct: (forall S x (F:state->result) K (o:out),
      (M S x F = res_out o) ->
      (forall S' o', (F S' = o') -> red_expr S' C K o') ->
      red_expr S C (spec_object_default_value_sub_1 l x K) o).
    clears HR S o. introv HR HK. subst M.
    run red_spec_object_default_value_sub_1
      using run_object_get_correct.
    run. forwards R1: run_callable_correct (rm E).
    destruct x0.
      simpls. run. destruct v; tryfalse.
       run* red_spec_object_default_value_sub_2_callable.
       destruct v; run_inv.
         applys* red_spec_object_default_value_sub_3_prim.
         applys* red_spec_object_default_value_sub_3_object.
      applys* red_spec_object_default_value_sub_2_not_callable.
    clear EQM.
  let_name.
  applys* red_spec_object_default_value_1_default.
  applys* red_spec_object_default_value_2.
  subst. applys* M_correct.
  clears S o. intros S o HR. simpls.
  applys* red_spec_object_default_value_3.
  subst. applys* M_correct.
  clears S o. intros S o HR. simpls.
  applys* red_spec_object_default_value_4.
Admitted. (* faster *)

(** Conversions *)

Lemma to_primitive_correct : forall runs S C v o prefo,
  runs_type_correct runs ->
  to_primitive runs S C v prefo = o ->
  red_expr S C (spec_to_primitive v prefo) o.
Proof.
  introv IH HR. unfolds in HR. destruct v.
  run_inv. applys* red_spec_to_primitive_pref_prim.
  applys* red_spec_to_primitive_pref_object.
  applys* object_default_value_correct.
  run_pre. rewrite R1. run_post; substs~.
Qed.

Lemma to_number_correct : forall runs S C v o,
  runs_type_correct runs ->
  to_number runs S C v = o ->
  red_expr S C (spec_to_number v) o.
Proof.
  introv IH HR. unfolds in HR. destruct v.
  run_inv. applys* red_spec_to_number_prim.
  run red_spec_to_number_object using to_primitive_correct.
  applys* red_spec_to_number_1.
Qed.

Lemma to_string_correct : forall runs S C v o,
  runs_type_correct runs ->
  to_string runs S C v = o ->
  red_expr S C (spec_to_string v) o.
Proof.
  introv IH HR. unfolds in HR. destruct v.
  run_inv. applys* red_spec_to_string_prim.
  run red_spec_to_string_object using to_primitive_correct.
  applys* red_spec_to_string_1.
Qed.

Lemma to_integer_correct : forall runs S C v o,
  runs_type_correct runs ->
  to_integer runs S C v = o ->
  red_expr S C (spec_to_integer v) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_to_integer using to_number_correct.
  applys* red_spec_to_integer_1.
Qed.

Lemma to_int32_correct : forall runs S C v (y:specret int),
  runs_type_correct runs ->
  to_int32 runs S C v = result_some y ->
  red_spec S C (spec_to_int32 v) y.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_to_int32 using to_number_correct.
  applys* red_spec_to_int32_1.
Qed.

Lemma to_uint32_correct : forall runs S C v (y:specret int),
  runs_type_correct runs ->
  to_uint32 runs S C v = result_some y ->
  red_spec S C (spec_to_uint32 v) y.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_to_uint32 using to_number_correct.
  applys* red_spec_to_uint32_1.
Qed.

Ltac run_select_proj_extra_conversions HT ::=
  match HT with
  | to_primitive => constr:(to_primitive_correct)
  | to_number => constr:(to_number_correct)
  | to_string => constr:(to_string_correct)
  | to_int32 => constr:(to_int32_correct)
  | to_uint32 => constr:(to_uint32_correct)
  end.

Lemma run_object_define_own_prop_array_loop_correct :
  forall runs S C l newLen oldLen newLenDesc newWritable throw o 
             (def : state -> prop_name -> descriptor -> strictness_flag -> specres nothing) 
             (def_correct : forall S str o x Desc,
                def S x Desc str = res_out o ->
                red_expr S C (spec_object_define_own_prop_1 builtin_define_own_prop_default l x Desc str) o),
    runs_type_correct runs ->
    run_object_define_own_prop_array_loop runs S C l newLen oldLen newLenDesc newWritable throw def = o -> 
    red_expr S C (spec_object_define_own_prop_array_3l l newLen oldLen newLenDesc newWritable throw) o.
Proof.
  introv Hyp HR IH.
  unfolds run_object_define_own_prop_array_loop.
  cases_if*. let_name. 
  applys~ red_spec_object_define_own_prop_array_3l_condition_true. 
  rewrite <- EQoldLen'.
  run~ red_spec_object_define_own_prop_array_3l_ii.
  run~ red_spec_object_define_own_prop_array_3l_ii_1.
  destruct b; cases_if*; clear n.
  applys* red_spec_object_define_own_prop_array_3l_ii_2.
  eapply runs_type_correct_object_define_own_prop_array_loop; eassumption.

  applys* red_spec_object_define_own_prop_array_3l_ii_2_3. 
  eapply red_spec_object_define_own_prop_array_3l_iii_1. reflexivity.
  destruct newWritable; try solve [false].
  let_name as newLenDesc''. rewrite <- EQnewLenDesc''. 
  apply red_spec_object_define_own_prop_array_3l_iii_2_true.
  let_name. cases_if*. subst newLenDesc0.
  run~ red_spec_object_define_own_prop_array_3l_iii_3.
  applys* red_spec_object_define_own_prop_array_3l_iii_4. 
  applys* red_spec_object_define_own_prop_reject.
  applys* out_error_or_cst_correct.
  let_name as newLenDesc''. rewrite <- EQnewLenDesc''. 
  apply red_spec_object_define_own_prop_array_3l_iii_2_false.
  let_name. cases_if*. subst newLenDesc0.
  run~ red_spec_object_define_own_prop_array_3l_iii_3.
  applys* red_spec_object_define_own_prop_array_3l_iii_4. 
  applys* red_spec_object_define_own_prop_reject.
  applys* out_error_or_cst_correct.
  
  applys~ red_spec_object_define_own_prop_array_3l_condition_false.
  destruct newWritable; cases_if*; clear n.
  inverts IH. applys* red_spec_object_define_own_prop_array_3n.

  applys* red_spec_object_define_own_prop_array_3m.
Qed.

Lemma object_define_own_prop_correct : forall runs S C l x Desc str o,
  runs_type_correct runs ->
  object_define_own_prop runs S C l x Desc str = o ->
  red_expr S C (spec_object_define_own_prop l x Desc str) o.
Proof.
  introv IH HR. unfolds in HR.
  let_name as rej. asserts Rej: (forall S str o,
      rej S str = o ->
      red_expr S C (spec_object_define_own_prop_reject str) o).
    clear HR S str o. introv HR. subst.
    applys* red_spec_object_define_own_prop_reject.
    applys* out_error_or_cst_correct.
  let_name as def. asserts Def: (forall S str o x Desc,
      def S x Desc str = res_out o ->
      red_expr S C (spec_object_define_own_prop_1 builtin_define_own_prop_default l x Desc str) o).
    clear HR S str o Desc x. introv HR. subst.
    run red_spec_object_define_own_prop_1_default.
    run. applys* red_spec_object_define_own_prop_2.
      applys* run_object_method_correct. clear E.
    destruct a.
      case_if.
        let_name. run. forwards B: @pick_option_correct (rm E).
         applys* red_spec_object_define_own_prop_3_undef_true A.
         case_if; case_if*.
        subst. applys* red_spec_object_define_own_prop_3_undef_false.
      let_name as wri. asserts Wri: (forall S A o,
          wri S A = res_out o ->
          red_expr S C (spec_object_define_own_prop_write l x A Desc str) o).
        clear HR o. introv HR. subst.
        run. forwards B: @pick_option_correct (rm E).
         applys* red_spec_object_define_own_prop_write.
        clear EQwri.
      case_if.
        run_inv. applys* red_spec_object_define_own_prop_3_includes.
        applys* red_spec_object_define_own_prop_3_not_include.
        case_if.
          run_inv. applys* red_spec_object_define_own_prop_4_reject.
          applys* red_spec_object_define_own_prop_4_not_reject. case_if.
            applys* red_spec_object_define_own_prop_5_generic. case_if.
              applys* red_spec_object_define_own_prop_5_a. case_if;
               [ | applys* red_spec_object_define_own_prop_6a_reject].
    let_name. run. forwards B: @pick_option_correct (rm E).
     applys* red_spec_object_define_own_prop_6a_accept A'.
     case_if as HC1.
       destruct a; inverts n2; tryfalse.
         applys* red_spec_object_define_own_prop_5_b. case_if.
           applys* red_spec_object_define_own_prop_6b_false_reject.
           applys* red_spec_object_define_own_prop_6b_false_accept.
       case_if. destruct a; tryfalse.
        applys* red_spec_object_define_own_prop_5_c. case_if.
          applys* red_spec_object_define_own_prop_6c_1.
          applys* red_spec_object_define_own_prop_6c_2.
    clear EQdef.
  run.
  applys* red_spec_object_define_own_prop.
    applys* run_object_method_correct.
  clear E. destruct x0. (* LTAC ARTHUR:  This [x0] wasn't properly named. *)
    (* default *)
    applys* Def.

    (* Array object *)
    run red_spec_object_define_own_prop_array_1. 
    destruct a; [inverts HR | ]. destruct a; [ | inverts HR].
    let_name. subst oldLen. 
    eapply red_spec_object_define_own_prop_array_2. reflexivity.
    destruct (attributes_data_value a); [ | inverts HR].
    let_name. let_name. subst descValueOpt.
    eapply red_spec_object_define_own_prop_array_2_1. reflexivity.
    eapply red_spec_to_uint32. apply red_spec_to_number_prim. reflexivity.
    apply red_spec_to_uint32_1. rewrite <- EQoldLen. 
    case_if. subst x.
    
    apply red_spec_object_define_own_prop_array_branch_3_4_3.
    assert (Hyp : {v | descriptor_value Desc = Some v} + {descriptor_value Desc = None}).
    {
      destruct (descriptor_value Desc); [left | right]; auto. exists~ v.
    } inverts Hyp as Hyp. 

    (* Step 3b *) 
    destruct Hyp as (v & EQv); rewrite EQv in *.
    run~ red_spec_object_define_own_prop_array_3_3c; rename a0 into newLen.
    run~ red_spec_object_define_own_prop_array_3c; rename m into newLenN.
    case_if*. applys~ red_spec_object_define_own_prop_array_3d.
    applys* run_error_correct. let_name.
    applys~ red_spec_object_define_own_prop_array_3e.
    clear dependent newLenN. case_if*.
    subst; applys* red_spec_object_define_own_prop_array_3f.
    case_if*. applys* red_spec_object_define_own_prop_array_3g.
    applys~ red_spec_object_define_own_prop_array_3g_to_h.
    rewrite <- EQnewLenDesc. let_name. let_name as newLenDesc'. 
    cases_if*; lets HnW : n1; rewrite EQnewWritable in n1. 
    apply red_spec_object_define_own_prop_array_3i.
    destruct (descriptor_writable newLenDesc); jauto.
    cases_if*. false~. clear n1 EQnewWritable. rewrite <- EQnewLenDesc'.
    replace false with newWritable by (destruct newWritable; auto; false).
    run* red_spec_object_define_own_prop_array_3j. destruct b; case_if*; clear n1.
    apply red_spec_object_define_own_prop_array_to_3l.
    applys* run_object_define_own_prop_array_loop_correct.
    
    inverts HR. apply red_spec_object_define_own_prop_array_3k.
    apply red_spec_object_define_own_prop_array_3h.
    destruct (descriptor_writable newLenDesc); jauto.
    cases_if*. clear n1 EQnewWritable. subst newLenDesc'. 
    replace true with newWritable by (destruct newWritable; auto; false).
    run* red_spec_object_define_own_prop_array_3j. destruct b; case_if*; clear n1.
    apply red_spec_object_define_own_prop_array_to_3l.
    applys* run_object_define_own_prop_array_loop_correct.

    inverts HR. applys* red_spec_object_define_own_prop_array_3k.
    
    (* Step 3a *) 
    rewrite Hyp in HR. applys~ red_spec_object_define_own_prop_array_3_3a.

    (* Branching between Step 4 and Step 5 *)
    applys~ red_spec_object_define_own_prop_array_branch_3_4_4.
    run red_spec_object_define_own_prop_array_branch_4_5.
    run red_spec_object_define_own_prop_array_branch_4_5_a.
    case_if. rename a0 into ilen, s into slen.
    applys~ red_spec_object_define_own_prop_array_branch_4_5_b_4.
    run red_spec_object_define_own_prop_array_4a.
    case_if; rename a0 into index.
    applys~ red_spec_object_define_own_prop_array_4b.
    run~ red_spec_object_define_own_prop_array_4c.
    destruct b; case_if*. case_if.
    eapply red_spec_object_define_own_prop_array_4c_e. auto. reflexivity. auto.
    run_inv. applys~ red_spec_object_define_own_prop_array_4f.
    applys~ red_spec_object_define_own_prop_array_4c_d.
    applys~ red_spec_object_define_own_prop_array_branch_4_5_b_5.
    applys~ red_spec_object_define_own_prop_array_5. 

    (* arguments object *)
    run. forwards~ obpm: run_object_method_correct (rm E).
    run. subst. run~ red_spec_object_define_own_prop_args_obj.
    run~ red_spec_object_define_own_prop_args_obj_1. cases_if; substs.
     let_name. asserts Follow: (forall S o,
         follow S = result_some (specret_out o) ->
         red_expr S C spec_args_obj_define_own_prop_6 o).
       introv RES. rewrite EQfollow in RES. inverts RES.
       apply* red_spec_object_define_own_prop_args_obj_6.
     clear EQfollow. destruct a as [|A]. (* LTAC ARTHUR: this [a] has been defined by tactics. *)
      apply~ red_spec_object_define_own_prop_args_obj_2_true_undef.
      cases_if.
       run~ red_spec_object_define_own_prop_args_obj_2_true_acc.
        apply* red_spec_object_define_own_prop_args_obj_5.
       let_name as next. asserts Next: (forall S o,
           next S = result_some (specret_out o) ->
           red_expr S C (spec_args_obj_define_own_prop_4 l x Desc str x1) o).
         introv RES. rewrite EQnext in RES. cases_if.
          run~ red_spec_object_define_own_prop_args_obj_4_false.
           apply~ red_spec_object_define_own_prop_args_obj_5.
          apply~ red_spec_object_define_own_prop_args_obj_4_not_false.
        clear EQnext. sets_eq <- dvDesc: (descriptor_value Desc). destruct dvDesc.
         run~ red_spec_object_define_own_prop_args_obj_2_true_not_acc_some.
          apply~ red_spec_object_define_own_prop_args_obj_3.
         apply~ red_spec_object_define_own_prop_args_obj_2_true_not_acc_none.
     apply~ red_spec_object_define_own_prop_args_obj_2_false.
Admitted. (* faster *)

Lemma prim_new_object_correct : forall S C w o,
  prim_new_object S w = o ->
  red_expr S C (spec_prim_new_object w) o.
Proof.
 introv H. destruct w; tryfalse;
 unfolds in H;  repeat let_simpl;
 match goal with H: context [object_alloc ?s ?o] |- _ => sets_eq X: (object_alloc s o) end;
 destruct X as (l&S').
 inversion H. applys* red_spec_prim_new_object_bool.
 inversion H. applys* red_spec_prim_new_object_number.
 run. applys* red_spec_prim_new_object_string.
 apply pick_option_correct in E. auto.
Qed.

(* todo: move to the right place above here *)
Lemma to_object_correct : forall S C v o,
  to_object S v = o ->
  red_expr S C (spec_to_object v) o.
Proof.
  hint run_error_correct_2, prim_new_object_correct.
  introv HR. unfolds in HR. destruct v as [w|l].
    destruct w.
      applys* red_spec_to_object_undef_or_null.
      applys* red_spec_to_object_undef_or_null.
      applys* red_spec_to_object_prim. rew_logic*. splits; congruence.
      applys* red_spec_to_object_prim. rew_logic*. splits; congruence.
      applys* red_spec_to_object_prim. rew_logic*. splits; congruence.
    run_inv. applys* red_spec_to_object_object.
Qed.

Lemma run_object_prim_value_correct : forall S l o,
  run_object_prim_value S l = o ->
  exists (v : value), o = out_ter S v /\
  object_prim_value S l v.
Proof.
  introv HR. unfolds in HR. do 2 runs. eexists. splits*.
  forwards~: run_object_method_correct E.
Qed.

Lemma prim_value_get_correct : forall runs S C v x o,
  runs_type_correct runs ->
  prim_value_get runs S C v x = o ->
  red_expr S C (spec_prim_value_get v x) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_prim_value_get using to_object_correct.
  applys* red_spec_prim_value_get_1.
  applys* object_get_builtin_correct.
Admitted. (*faster*)

Lemma object_put_complete_correct : forall runs S C B vthis l x v str o,
  runs_type_correct runs ->
  object_put_complete runs B S C vthis l x v str = o ->
  red_expr S C (spec_object_put_1 B vthis l x v str) o.
Proof.
  introv IH HR. unfolds in HR. destruct B.
  run red_spec_object_put_1_default using object_can_put_correct. cases_if.
   run red_spec_object_put_2_true. let_name.
    asserts follows_correct: (forall Aa o,
        a = full_descriptor_undef \/ (a = attributes_accessor_of Aa) ->
        follow tt = res_out o ->
        red_expr S0 C (spec_object_put_3 vthis l x v str (specret_val S2 a)) o).
      clear HR. introv N E. substs.
      run red_spec_object_put_3_not_data using run_object_get_prop_correct. apply* N.
      clear N. tests Acc: (exists Aa', a0 = attributes_accessor_of Aa').
       lets (Aa'&?): (rm Acc). let_simpl. substs.
        sets_eq va': (attributes_accessor_set Aa'). destruct va' as [|la']; tryfalse.
        run* red_spec_object_put_4_accessor. rewrite <- EQva'. discriminate.
        apply~ red_spec_object_put_5_return.
       let_name. asserts E': (follow' tt = o0).
         destruct a0 as [|()]; try solve [false~ Acc]; exact E.
        asserts (?&H'): (exists (Ad : attributes_data),
          a0 = full_descriptor_undef \/ a0 = Ad).
         destruct a0 as [|()]. exists* (arbitrary : attributes_data). exists* a0. false~ Acc.
        clear E. substs. destruct vthis.
         forwards (H&_): out_error_or_void_correct C (rm E').
          applys* red_spec_object_put_4_not_accessor_prim H.
         let_simpl. run* red_spec_object_put_4_not_accessor_object using
           object_define_own_prop_correct. apply~ red_spec_object_put_5_return.
     destruct a as [|[Ad|Aa]]. applys~ follows_correct (arbitrary : attributes_accessor).
     clear EQfollow follow follows_correct.
     destruct vthis as [wthis|lthis].
      apply~ red_spec_object_put_3_data_prim. apply~ out_error_or_void_correct.
      let_simpl. run* red_spec_object_put_3_data_object
        using object_define_own_prop_correct. apply~ red_spec_object_put_5_return.
     apply~ follows_correct.
   apply~ red_spec_object_put_2_false. apply~ out_error_or_void_correct.
Qed.

Lemma prim_value_put_correct : forall runs S C w x v str o,
  runs_type_correct runs ->
  prim_value_put runs S C w x v str = o ->
  red_expr S C (spec_prim_value_put w x v str) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_prim_value_put using to_object_correct.
  applys* red_spec_prim_value_put_1.
  applys* object_put_complete_correct.
Qed.



(*************************************************************)

Lemma env_record_get_binding_value_correct : forall runs S C L rn rs o,
  runs_type_correct runs ->
  env_record_get_binding_value runs S C L rn rs = o ->
  red_expr S C (spec_env_record_get_binding_value L rn rs) o.
Proof.
  introv IH HR. unfolds in HR.
  run_simpl. forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_get_binding_value B. destruct x.
   run_simpl. rewrite <- Heap.binds_equiv_read_option in E. destruct x as [mu v].
    cases_if.
     applys~ red_spec_env_record_get_binding_value_1_decl_uninitialized E.
      apply~ out_error_or_cst_correct.
     applys~ red_spec_env_record_get_binding_value_1_decl_initialized E.
      run_inv. apply~ red_spec_returns.
   run red_spec_env_record_get_binding_value_1_object using object_has_prop_correct.
    cases_if; run_inv.
     apply~ red_spec_env_record_get_binding_value_obj_2_true.
      applys~ run_object_get_correct HR.
     apply~ red_spec_env_record_get_binding_value_obj_2_false.
      applys~ out_error_or_cst_correct HR.
Qed.


Lemma throw_result_run_error_correct : forall runs S C ne T (y:specret T),
  runs_type_correct runs ->
  throw_result (run_error S ne) = result_some y ->
  red_spec S C (spec_error_spec ne) y.
Proof.
  introv IH HR. unfolds throw_result.
  lets ([|y1]&E&K): if_result_some_out (rm HR); tryfalse_nothing. run_inv.
  lets (E2&Ab): run_error_correct' (rm E).
  applys* red_spec_error_spec.
  abort.
Qed.


Lemma ref_kind_env_record_inv : forall r,
  ref_kind_of r = ref_kind_env_record ->
  exists L, ref_base r = ref_base_type_env_loc L.
Proof.
  introv E. unfolds ref_kind_of.
  destruct (ref_base r).
    destruct v; tryfalse. destruct p; tryfalse.
    exists___*.
Qed.

Lemma ref_kind_base_object_inv : forall r,
  (ref_kind_of r = ref_kind_primitive_base \/
     ref_kind_of r = ref_kind_object) ->
  exists v, ref_base r = ref_base_type_value v.
Proof.
  introv E. unfolds ref_kind_of.
  destruct E; destruct (ref_base r); tryfalse;
    destruct v; tryfalse; try solve [exists___*].
Qed.


Lemma ref_get_value_correct : forall runs S C rv y,
  runs_type_correct runs ->
  ref_get_value runs S C rv = result_some y ->
  red_spec S C (spec_get_value rv) y.
Proof.
  introv IH HR. unfolds in HR. destruct rv; tryfalse.
  run_inv. applys* red_spec_ref_get_value_value.
  let_name as M.
  asserts M_correct: (
    (  ref_kind_of r = ref_kind_primitive_base
    \/ ref_kind_of r = ref_kind_object) ->
    M tt = result_some y ->
    red_spec S C (spec_get_value r) y).
   clear HR. introv EQ HR. subst M.
   asserts: (ref_is_property r). unfolds. destruct* EQ.
   lets (v&Ev): ref_kind_base_object_inv EQ. rewrite Ev in HR.
   unfolds ref_has_primitive_base. case_if.
     run* red_spec_ref_get_value_ref_b_has_primitive_base using prim_value_get_correct.
      applys* red_spec_ref_get_value_ref_b_1.
     destruct EQ; tryfalse. destruct v as [|l]; tryfalse.
     run* red_spec_ref_get_value_ref_b_has_not_primitive_base using run_object_get_correct.
      applys* red_spec_ref_get_value_ref_b_1.
    clear EQM.
  sets_eq k: (ref_kind_of r). destruct k; tryfalse.
  (* case undef *)
  applys* red_spec_ref_get_value_ref_a. unfolds*.
   applys* throw_result_run_error_correct.
  (* case prim *)
  applys* M_correct.
  (* case object *)
  applys* M_correct.
  (* case env_record *)
  lets (L&EQL): ref_kind_env_record_inv (sym_eq EQk).
  rewrite EQL in HR.
  run* red_spec_ref_get_value_ref_c using env_record_get_binding_value_correct.
  applys* red_spec_ref_get_value_ref_c_1.
Admitted. (*faster*)


Lemma object_put_correct : forall runs S C l x v str o,
  runs_type_correct runs ->
  object_put runs S C l x v str = o ->
  red_expr S C (spec_object_put l x v str) o.
Proof.
  introv IH HR. unfolds in HR.
  run. applys red_spec_object_put. apply* run_object_method_correct.
  applys* object_put_complete_correct.
Admitted. (*faster*)

Lemma env_record_set_mutable_binding_correct : forall runs S C L x v str o,
  runs_type_correct runs ->
  env_record_set_mutable_binding runs S C L x v str = o ->
  red_expr S C (spec_env_record_set_mutable_binding L x v str) o.
Proof.
  introv IH HR. unfolds in HR.
  run_simpl. forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_set_mutable_binding B. destruct x0.
   run_simpl. rewrite <- Heap.binds_equiv_read_option in E. destruct x0 as [mu ?].
    cases_if; run_inv.
     applys~ red_spec_env_record_set_mutable_binding_1_decl_mutable E.
      apply~ red_spec_returns.
     applys~ red_spec_env_record_set_mutable_binding_1_decl_non_mutable E.
      apply~ out_error_or_void_correct.
   apply~ red_spec_env_record_set_mutable_binding_1_object.
    applys~ object_put_correct HR.
Qed.


Lemma ref_is_property_from_not_unresolvable_value : forall r v,
  ~ ref_is_unresolvable r ->
  ref_base r = ref_base_type_value v ->
  ref_is_property r.
Proof.
  introv N E. unfolds ref_is_property, ref_is_unresolvable, ref_kind_of.
  destruct (ref_base r); tryfalse. destruct* v0. destruct* p.
Admitted. (* faster *)


Lemma ref_put_value_correct : forall runs S C rv v o,
  runs_type_correct runs ->
  ref_put_value runs S C rv v = o ->
  red_expr S C (spec_put_value rv v) o.
Proof.
  introv IH HR. unfolds in HR.
  destruct rv; tryfalse.
   applys* red_spec_ref_put_value_value.
   case_if.
    case_if.
     applys~ red_spec_ref_put_value_ref_a_1.
      applys* run_error_correct.
     applys~ red_spec_ref_put_value_ref_a_2.
      applys* object_put_correct.
    case_if.
     cases (ref_base r); tryfalse.
      case_if; destruct v0; tryfalse.
       applys* red_spec_ref_put_value_ref_b_has_primitive_base.
        applys* prim_value_put_correct.
       applys* red_spec_ref_put_value_ref_b_has_not_primitive_base.
        applys* object_put_correct.
      cases (ref_base r); tryfalse.
       applys* red_spec_ref_put_value_ref_c.
       applys* env_record_set_mutable_binding_correct.
Admitted. (* faster *)


Lemma run_expr_get_value_correct : forall runs S C e y,
  runs_type_correct runs ->
  run_expr_get_value runs S C e = result_some y ->
  red_spec S C (spec_expr_get_value e) y.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_expr_get_value.
  applys* red_spec_expr_get_value_1.
   applys* ref_get_value_correct.
Admitted. (* faster *)


Ltac run_select_proj_extra_ref HT ::=
  match HT with
  | object_put => constr:(object_put_correct)
  | ref_put_value => constr:(ref_put_value_correct)
  | run_expr_get_value => constr:(run_expr_get_value_correct)
  | object_define_own_prop => constr:(object_define_own_prop_correct)
  end.

Lemma env_record_create_mutable_binding_correct : forall runs S C L x deletable_opt o,
  runs_type_correct runs ->
  env_record_create_mutable_binding runs S C L x deletable_opt = o ->
  red_expr S C (spec_env_record_create_mutable_binding L x deletable_opt) o.
Proof.
  introv IH HR. unfolds in HR. let_simpl.
  run_simpl. forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_create_mutable_binding B.
  destruct x0.
   cases_if; run_inv. let_simpl. run_inv.
    apply~ red_spec_env_record_create_mutable_binding_1_decl_indom.
   run red_spec_env_record_create_mutable_binding_1_object
     using object_has_prop_correct. cases_if. let_simpl.
    run* red_spec_env_record_create_mutable_binding_obj_2.
    apply~ red_spec_env_record_create_mutable_binding_obj_3.
Qed.

Lemma env_record_create_set_mutable_binding_correct : forall runs S C L x deletable_opt v str o,
  runs_type_correct runs ->
  env_record_create_set_mutable_binding runs S C L x deletable_opt v str = o ->
  red_expr S C (spec_env_record_create_set_mutable_binding L x deletable_opt v str) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_env_record_create_set_mutable_binding
    using env_record_create_mutable_binding_correct.
  forwards: env_record_set_mutable_binding_correct IH (rm HR).
  apply~ red_spec_env_record_create_set_mutable_binding_1.
Qed.

Lemma env_record_create_immutable_binding_correct : forall S C L x o,
  env_record_create_immutable_binding S L x = o ->
  red_expr S C (spec_env_record_create_immutable_binding L x) o.
Proof.
  introv HR. unfolds in HR.
  run_simpl. forwards B: @pick_option_correct (rm E).
  destruct x0; tryfalse. cases_if. run_inv.
  applys~ red_spec_env_record_create_immutable_binding B.
Qed.

Lemma env_record_initialize_immutable_binding_correct : forall S C L x v o,
  env_record_initialize_immutable_binding S L x v = o ->
  red_expr S C (spec_env_record_initialize_immutable_binding L x v) o.
Proof.
  introv HR. unfolds in HR.
  run. forwards B: @pick_option_correct (rm E). destruct x0; tryfalse.
  run. forwards B': @pick_option_correct (rm E). cases_if. let_simpl. run_inv. substs.
  applys~ red_spec_env_record_initialize_immutable_binding B B'.
Qed.

(************************************************************)
(* Treatement of [spec_expr_get_value_conv] *)

Definition if_spec_ter_post_bool (K:state->bool->result) o (y:specret value) :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (b:bool), y = specret_val S (value_prim b)
       /\ K S b = o).

Ltac run_post_if_spec_ter_post_bool H := (* todo: integrate into run_post *)
  let Ab := fresh "Ab" in
  let Eq := fresh "Eq" in
  let S1 := fresh "S" in
  let b := fresh "b" in
  let O1 := fresh "O1" in
  destruct H as [(Er&Ab)|(S1&b&O1&H)];
  [ try abort | try subst_hyp O1 ].

Lemma if_spec_post_to_bool : forall (K:state->bool->result) S C e o y1,
  red_spec S C (spec_expr_get_value e) y1 ->
  if_spec_post
   (fun S v => 'let b := convert_value_to_boolean v in K S b) (specret_out o) y1 ->
  exists y2,
     red_spec S C (spec_expr_get_value_conv spec_to_boolean e) y2
  /\ if_spec_ter_post_bool K o y2.
Proof.
  introv HR HP. run_post.
  exists y1. splits.
    subst. apply* red_spec_expr_get_value_conv. abort.
    subst. left. splits; run_inv; auto~.
  exists (specret_val S1 (value_prim (convert_value_to_boolean a))). splits.
    applys* red_spec_expr_get_value_conv.
     applys* red_spec_expr_get_value_conv_1.
     applys* red_spec_to_boolean.
     applys* red_spec_expr_get_value_conv_2.
    right. exists S1 __. split. reflexivity. auto.
Qed.

(* LATER: avoid the copy-paste, and rename the tactic *)

Definition if_spec_ter_post_object (K:state->object_loc->result) o (y:specret value) :=
     (y = specret_out o /\ abort o)
  \/ (exists S, exists (l:object_loc), y = specret_val S (value_object l)
       /\ K S l = o).


Lemma if_spec_post_to_object : forall (K:state->object_loc->result) S C e o y1,
  red_spec S C (spec_expr_get_value e) y1 ->
  if_spec_post
   (fun S v => if_object (to_object S v) K) (specret_out o) y1 ->
  exists y2,
     red_spec S C (spec_expr_get_value_conv spec_to_object e) y2
  /\ if_spec_ter_post_object K o y2.
Proof.
  introv HR HP. run_post.
  exists y1. splits.
    subst. apply* red_spec_expr_get_value_conv. abort.
    subst. left. splits; run_inv; auto~.
  run_pre. lets*: to_object_correct C (rm R1). run_post.
    subst. exists (specret_out (T:=value) o1). split.
      applys* red_spec_expr_get_value_conv.
       applys* red_spec_expr_get_value_conv_1. abort.
      left. splits~.
    exists (specret_val S0 (value_object l)). split.
      applys* red_spec_expr_get_value_conv.
       applys* red_spec_expr_get_value_conv_1.
       applys* red_spec_expr_get_value_conv_2.
      right. exists___*.
Qed.


Definition lift2 T (C:T->value) y :=
  match y with
  | specret_val S' (x1,x2) => specret_val S' (C x1, C x2)
  | specret_out o => specret_out o
  end.


Lemma convert_twice_primitive_correct : forall runs S C v1 v2 y,
  runs_type_correct runs ->
  convert_twice_primitive runs S C v1 v2 = result_some y ->
  red_spec S C (spec_convert_twice (spec_to_primitive_auto v1) (spec_to_primitive_auto v2)) (lift2 value_prim y).
Proof.
  introv IH HR. unfolds in HR. unfolds in HR.
  run red_spec_convert_twice.
  run red_spec_convert_twice_1.
  unfolds lift2. applys red_spec_convert_twice_2.
Admitted. (*faster*)


Lemma convert_twice_number_correct : forall runs S C v1 v2 y,
  runs_type_correct runs ->
  convert_twice_number runs S C v1 v2 = result_some y ->
  red_spec S C (spec_convert_twice (spec_to_number v1) (spec_to_number v2)) (lift2 (fun n=>n:value) y).
Proof.
  introv IH HR. unfolds in HR. unfolds in HR.
  run red_spec_convert_twice.
  run red_spec_convert_twice_1.
  unfolds lift2. applys red_spec_convert_twice_2.
Admitted. (*faster*)

Lemma convert_twice_string_correct : forall runs S C v1 v2 y,
  runs_type_correct runs ->
  convert_twice_string runs S C v1 v2 = result_some y ->
  red_spec S C (spec_convert_twice (spec_to_string v1) (spec_to_string v2)) (lift2 (fun s=>s:value) y).
Proof.
  introv IH HR. unfolds in HR. unfolds in HR.
  run red_spec_convert_twice.
  run red_spec_convert_twice_1.
  unfolds lift2. applys red_spec_convert_twice_2.
Admitted. (*faster*)

Lemma get_puremath_op_correct : forall op F,
  get_puremath_op op = Some F ->
  puremath_op op F.
Proof.
  Hint Constructors puremath_op.
  introv HR. destruct op; simpls; inverts* HR.
Admitted. (*faster*)

Lemma get_inequality_op_correct : forall op b1 b2,
  get_inequality_op op = Some (b1,b2) ->
  inequality_op op b1 b2.
Proof.
  Hint Constructors inequality_op.
  introv HR. destruct op; simpls; inverts* HR.
Admitted. (*faster*)

Lemma get_shift_op_correct : forall op F b,
  get_shift_op op = Some (b,F) ->
  shift_op op b F.
Proof.
  Hint Constructors shift_op.
  introv HR. destruct op; simpls; inverts* HR.
Admitted. (*faster*)

Lemma get_bitwise_op_correct : forall op F,
  get_bitwise_op op = Some F ->
  bitwise_op op F.
Proof.
  Hint Constructors bitwise_op.
  introv HR. destruct op; simpls; inverts* HR.
Admitted. (*faster*)


(**************************************************************)

Lemma run_object_get_own_prop_correct : forall runs S C l x y,
  runs_type_correct runs ->
  run_object_get_own_prop runs S C l x = result_some y ->
  red_spec S C (spec_object_get_own_prop l x) y.
Proof.
  introv IH HR. unfolds in HR. run.
  applys* red_spec_object_get_own_prop.
    applys* run_object_method_correct. clear E.
  let_name as M. asserts M_correct: (forall S y,
    M S = result_some y ->
    red_spec S C (spec_object_get_own_prop_1 builtin_get_own_prop_default l x) y).
    clears HR S. subst. introv HR. run.
     sets_eq <- Ao: (Heap.read_option x1 x).
     applys~ red_spec_object_get_own_prop_1_default. eexists. splits.
      applys run_object_method_correct E.
      rewrite~ EQAo.
     clear E. destruct Ao.
      applys* red_spec_object_get_own_prop_2_some_data.
      applys* red_spec_object_get_own_prop_2_none.
    clear EQM.
  destruct x0.
  (* default *)
  subst*.
  (* argument object *)
  run~ red_spec_object_get_own_prop_args_obj. destruct a as [|A]. (* LTAC ARTHUR: this [a] has been defined by tactics. *)
   inverts HR. applys~ red_spec_object_get_own_prop_args_obj_1_undef.
   run. forwards~ obpm: run_object_method_correct (rm E).
   run. subst. run~ red_spec_object_get_own_prop_args_obj_1_attrs.
   let_name. asserts Follow: (forall S A y,
       follow S A = result_some y ->
       red_spec S C (spec_args_obj_get_own_prop_4 A) y).
     introv RES. rewrite EQfollow in RES. inverts RES.
     apply~ red_spec_object_get_own_prop_args_obj_4.
   clear EQfollow. destruct a. (* LTAC ARTHUR: idem. *)
    apply* red_spec_object_get_own_prop_args_obj_2_undef.
    run~ red_spec_object_get_own_prop_args_obj_2_attrs using run_object_get_correct.
    destruct A as [Ad|]; tryfalse.
    apply~ red_spec_object_get_own_prop_args_obj_3.
  (* string *)
  run~ red_spec_object_get_own_prop_string. destruct a as [|A]. (* LTAC ARTHUR: this [a] has been defined by tactics. *)
   run red_spec_object_get_own_prop_string_1_undef using to_int32_correct.
    run red_spec_object_get_own_prop_string_2.
    cases_if.
     inverts HR. apply~ red_spec_object_get_own_prop_string_3_different.
     subst x. run_pre. forwards* (v&EQo&Opv): run_object_prim_value_correct. run_post.
       inverts Ab as Ab'; false. inverts H0. false Ab'. reflexivity.
     inverts EQo. applys~ red_spec_object_get_own_prop_string_3_same Opv.
     run~ red_spec_object_get_own_prop_string_4.
     let_name. apply~ red_spec_object_get_own_prop_string_5. cases_if.
      inverts HR. apply~ red_spec_object_get_own_prop_string_6_outofbounds. math.
      inverts HR. apply~ red_spec_object_get_own_prop_string_6_inbounds. math.
   inverts HR. apply~ red_spec_object_get_own_prop_string_1_attrs.
Admitted. (*faster*)


Lemma run_function_has_instance_correct : forall runs S C (lo lv : object_loc) o,
  runs_type_correct runs ->
  run_function_has_instance runs S lo lv = o ->
  red_expr S C (spec_function_has_instance_2 lv lo) o.
Proof.
  intros runs IH lo S C lv o HR. unfolds in HR. run_simpl.
  forwards~ M: run_object_method_correct (rm E).
  applys~ red_spec_function_has_instance_2 M.
  destruct x as [()|lproto]; tryfalse; run_inv.
   apply~ red_spec_function_has_instance_3_null.
   cases_if; run_inv.
    apply~ red_spec_function_has_instance_3_eq.
    apply~ red_spec_function_has_instance_3_neq.
     applys~ runs_type_correct_function_has_instance HR.
Qed.


Lemma run_object_has_instance_correct : forall runs S C B l v o,
  runs_type_correct runs ->
  run_object_has_instance runs S C B l v = result_some (specret_out o) ->
  red_expr S C (spec_object_has_instance_1 B l v) o.
Proof.
  introv IH HR. unfolds in HR. destruct B.
  destruct v.
  run_inv. applys* red_spec_object_has_instance_1_function_prim.
  run red_spec_object_has_instance_1_function_object
    using run_object_get_correct.
  destruct v.
  applys* red_spec_function_has_instance_1_prim.
  applys red_spec_function_has_instance_1_object.
   applys* runs_type_correct_function_has_instance.

   repeat run; apply run_object_method_correct in E;
   apply run_object_method_correct in E1; subst.
   apply red_spec_object_has_instance_after_bind.
   applys~ red_spec_function_has_instance_after_bind_1. eassumption. 
   destruct x1. applys* red_spec_function_has_instance_after_bind_2_some.
   applys* runs_type_correct_object_has_instance.
   applys* red_spec_function_has_instance_after_bind_2_none.
Admitted. (* faster*)


Lemma run_binary_op_correct : forall runs S C (op : binary_op) v1 v2 o,
  runs_type_correct runs ->
  run_binary_op runs S C op v1 v2 = o ->
  red_expr S C (expr_binary_op_3 op v1 v2) o.
Proof.

  introv IH HR. unfolds in HR.
  (* Add *)
  case_if. subst.
  run red_expr_binary_op_add using convert_twice_primitive_correct.
  destruct a as [w1 w2]. case_if.
  run* red_expr_binary_op_add_1_string using convert_twice_string_correct.
  destruct a as [s1 s2]. run_inv.
  applys* red_expr_binary_op_add_string_1.
  run* red_expr_binary_op_add_1_number using convert_twice_number_correct.
  destruct a as [n1 n2]. run_inv.
  applys* red_expr_puremath_op_1.
  (* Puremath *)
  case_if. run.
  run red_expr_puremath_op using convert_twice_number_correct.
  applys* get_puremath_op_correct.
  destruct a as [n1 n2]. run_inv.
  applys* red_expr_puremath_op_1.
  (* Shiftop *)
  case_if. run. destruct x as [b F].
  lets M: red_expr_shift_op b. case_if; subst.
  run* M. applys* get_shift_op_correct.
    run red_expr_shift_op_1. applys* red_expr_shift_op_2.
  run* M. applys* get_shift_op_correct.
    run red_expr_shift_op_1. applys* red_expr_shift_op_2.
  (* bitwise *)
  case_if. run.
  run red_expr_bitwise_op. applys* get_bitwise_op_correct.
  run red_expr_bitwise_op_1. applys* red_expr_bitwise_op_2.
  (* inequality *)
  clear n H H0 H1.
  case_if. run. destruct x as [b1 b2].
  applys red_expr_inequality_op. applys* get_inequality_op_correct.
  run red_expr_inequality_op_1 using convert_twice_primitive_correct.
  destruct a as [w1 w2]. let_name. destruct p as [wa wb]. simpls.
  sets_eq wr: (inequality_test_primitive wa wb).
  run_inv. applys_eq* (>> red_expr_inequality_op_2 EQp EQwr) 1.
   fequals. case_if; case_if; case_if*; case_if*; case_if*; case_if*; case_if*; case_if*.
  (* instanceof *)
  case_if. subst.
  destruct v2.
  applys* red_expr_binary_op_instanceof_non_object.
  run. lets M: run_object_method_correct (rm E).
  destruct x.
  applys* red_expr_binary_op_instanceof_normal.
   simpls.
   applys* red_spec_object_has_instance.
   applys* run_object_has_instance_correct.
  applys* red_expr_binary_op_instanceof_non_instance.
  (* in *)
  case_if. subst. destruct v2.
  applys* red_expr_binary_op_in_non_object.
  run red_expr_binary_op_in_object.
    applys* red_expr_binary_op_in_1.
    applys* object_has_prop_correct.
  (* equal *)
  clear n n0 H. case_if. subst.
  applys* red_expr_binary_op_equal.
    applys* runs_type_correct_equal.
  (* disequal *)
  case_if. subst.
  run red_expr_binary_op_disequal.
  applys* red_expr_binary_op_disequal_1.
  (* strict equality *)
  case_if. subst.
  run_inv. applys* red_expr_binary_op_strict_equal.
  (* strict disequality *)
  case_if. subst.
  run_inv. applys* red_expr_binary_op_strict_disequal.
  (* coma *)
  case_if. subst.
  run_inv. applys* red_expr_binary_op_coma.
Admitted. (*faster*)

(**************************************************************)
(* Auxiliary results for [array_args_map_loop] *)

Lemma array_args_map_loop_no_abort : forall oes runs S o l C k,
  array_args_map_loop runs S C l oes k = o -> exists S', o = out_ter S' res_empty.
Proof.
  inductions oes; introv Hyp.

  + simpls. exists S. inverts~ Hyp. 
  + simpls. run. eapply IHoes; eassumption.
Qed.

Lemma array_args_map_loop_correct : forall oes runs S S' l C k,
   array_args_map_loop runs S C l oes k = res_void S' ->
   red_expr S C (spec_call_array_new_3 l oes k) (out_ter S' l).
Proof.
  induction oes; introv Hyp.

  + simpls. inverts Hyp. apply red_spec_call_array_new_3_empty.
  + simpls. unfolds res_void. run. rename x into S''.
    apply pick_option_correct in E.
    applys~ red_spec_call_array_new_3_nonempty. exact E.
    jauto.
Qed.

(**************************************************************)
(* Auxiliary results for [spec_expr_get_value_conv] *)

Lemma run_construct_prealloc_correct : forall runs S C B args o,
  runs_type_correct runs ->
  run_construct_prealloc runs S C B args = o ->
  red_expr S C (spec_construct_prealloc B args) o.
Proof.
  introv IH HR. unfolds in HR.
  destruct B.
  (* prealloc_global *)
  discriminate.
  (* prealloc_global_eval *)
  discriminate.
  (* prealloc_global_parse_int *)
  discriminate.
  (* prealloc_global_parse_float *)
  discriminate.
  (* prealloc_global_is_finite *)
  discriminate.
  (* prealloc_global_is_nan *)
  discriminate.
  (* prealloc_global_decode_uri *)
  discriminate.
  (* prealloc_global_decode_uri_component *)
  discriminate.
  (* prealloc_global_encode_uri *)
  discriminate.
  (* prealloc_global_encode_uri_component *)
  discriminate.
  (* prealloc_object *)
  let_name. subst.
  applys* red_spec_call_object_new.
    applys* get_arg_correct_0.
  destruct (get_arg 0 args) as [p | l]; unfolds call_object_new; [destruct p | ]; simpls; repeat let_name; try destruct p as (l & S'); substs.
  inverts HR. applys* red_spec_call_object_new_1_null_or_undef.
  inverts HR. applys* red_spec_call_object_new_1_null_or_undef.
  applys* red_spec_call_object_new_1_prim.
  applys* to_object_correct.
  applys* red_spec_call_object_new_1_prim.
  applys* to_object_correct.
  applys* red_spec_call_object_new_1_prim.
  applys* to_object_correct.
  inverts HR. applys* red_spec_call_object_new_1_object.  
  (* prealloc_object_get_proto_of *)
  discriminate.
  (* prealloc_object_get_own_prop_descriptor *)
  discriminate.
  (* prealloc_object_get_own_prop_name *)
  discriminate.
  (* prealloc_object_create *)
  discriminate.
  (* prealloc_object_define_prop *)
  discriminate.
  (* prealloc_object_define_props *)
  discriminate.
  (* prealloc_object_seal *)
  discriminate.
  (* prealloc_object_freeze *)
  discriminate.
  (* prealloc_object_prevent_extensions *)
  discriminate.
  (* prealloc_object_is_sealed *)
  discriminate.
  (* prealloc_object_is_frozen *)
  discriminate.
  (* prealloc_object_is_extensible *)
  discriminate.
  (* prealloc_object_keys *)
  discriminate.
  (* prealloc_object_keys_call *)
  discriminate.
  (* prealloc_object_proto *)
  discriminate.
  (* prealloc_object_proto_to_string *)
  discriminate.
  (* prealloc_object_proto_value_of *)
  discriminate.
  (* prealloc_object_proto_has_own_prop *)
  discriminate.
  (* prealloc_object_proto_is_prototype_of *)
  discriminate.
  (* prealloc_object_proto_prop_is_enumerable *)
  discriminate.
  (* prealloc_function *)
  discriminate. (* LATER *)
  (* prealloc_function_proto *)
  discriminate.
  (* prealloc_function_proto_to_string *)
  discriminate.
  (* prealloc_function_proto_apply *)
  discriminate.
  (* prealloc_function_proto_call *)
  discriminate.
  (* prealloc_function_proto_bind *)
  discriminate.
  (* prealloc_bool *)
  repeat let_name.
  applys* red_spec_construct_bool.
  apply get_arg_correct_0.
  applys* red_spec_to_boolean.
  destruct p as (l & S').
  inverts HR.
  applys* red_spec_construct_bool_1.
  substs~. 
  (* prealloc_bool_proto *)
  discriminate.
  (* prealloc_bool_proto_to_string *)
  discriminate.
  (* prealloc_bool_proto_value_of *)
  discriminate.
  (* prealloc_number *)
  let_name. cases_if*.
  subst. repeat let_name. 
  remember (object_alloc S O) as p.
  destruct p as (l & S1).
  inverts HR.
  applys* red_spec_construct_number_nil.
  applys* red_spec_construct_number_1. substs~.
  let_name. subst.
  run~ red_spec_construct_number_not_nil.
  applys~ get_arg_correct_0.
  repeat let_name.
  remember (object_alloc S0 O) as p.
  destruct p as (l & S1).
  inverts HR.
  applys* red_spec_construct_number_1.
  substs~.
  (* prealloc_number_proto *)
  discriminate.
  (* prealloc_number_proto_to_string *)
  discriminate.
  (* prealloc_number_proto_value_of *)
  discriminate.
  (* prealloc_number_proto_to_fixed *)
  discriminate.
  (* prealloc_number_proto_to_exponential *)
  discriminate.
  (* prealloc_number_proto_to_precision *)
  discriminate.

  (* prealloc_array *)
  repeat let_name. destruct p as (l & S'). repeat let_name.
  subst arg_len. destruct args. case_if*. 

  applys~ red_spec_call_array_new_no_args. 
  eapply red_spec_call_array_new_1; try eassumption. 
  run. apply pick_option_correct in E.
  applys* red_spec_call_array_new_2.
  simpls. run. apply red_spec_call_array_new_3_empty.
  destruct args. case_if*. clear e.
  
  unfolds get_arg. unfolds nth_def. subst v. 
  applys~ red_spec_call_array_new_single_arg.
  applys~ red_spec_call_array_new_single_allocate; try (subst; eassumption).

  destruct v0; [destruct p | ]; subst;
  try solve [run; rename x into S''; run; rename x into S'''; apply pick_option_correct in E; apply pick_option_correct in E0;
             apply (red_spec_call_array_new_single_not_prim_number S' S'') with (n := JsNumber.of_int 0); jauto;
             introv Heq; inverts Heq].
  
  run~ red_spec_call_array_new_single_prim_number. cases_if*.
  run; rename x into S''; apply pick_option_correct in E.
  applys~ red_spec_call_array_new_single_number_correct.
  applys~ red_spec_call_array_new_single_set_length.
  applys~ red_spec_call_array_new_single_number_incorrect.
  applys run_error_correct HR.
  
  cases_if*. eapply red_spec_call_array_new_multiple_args. reflexivity.
  eapply red_spec_call_array_new_1; try eassumption.
  run. apply pick_option_correct in E.
  applys* red_spec_call_array_new_2.
  remember (v1 :: args) as args'; clear dependent v1. 
  run_pre. run_post; subst.
  apply array_args_map_loop_no_abort in R1.
  destruct R1 as (S'' & Heq_o'). subst. inverts Ab.
  false~ H0. inverts HR.
  eapply array_args_map_loop_correct. eassumption.

  (* prealloc_array_is_array *)
  discriminate.
  (* prealloc_array_proto *)
  discriminate.
  (* prealloc_array_proto_to_string *)
  discriminate.
  (* prealloc_array_proto_join *)
  discriminate.
  (* prealloc_array_proto_pop *)
  discriminate.
  (* prealloc_array_proto_push *)
  discriminate.
  (* prealloc_string *)
  repeat let_name.
  cases_if*. subst.
  symmetry in EQarg_len. apply length_zero_inv in EQarg_len. subst.
  apply red_spec_construct_string_empty. 
  let_name.
  match goal with H: context [object_alloc ?s ?o] |- _ => sets_eq X: (object_alloc s o) end.
  destruct X as (l & S'). let_name. subst.
  run. rename x into S''. apply pick_option_correct in E.
  remember (object_with_primitive_value
             (object_with_get_own_property
                (object_new prealloc_string_proto "String")
                builtin_get_own_prop_string) "") as O.
  applys* red_spec_construct_string_2; substs~.
  applys* red_spec_construct_string_non_empty.
  subst. destruct args; jauto; discriminate.
  apply get_arg_correct_0. subst.
  run red_spec_construct_string_1 using to_string_correct.
  let_name.
  match goal with H: context [object_alloc ?s ?o] |- _ => sets_eq X: (object_alloc s o) end.
  destruct X as (l & S'). let_name. subst.
  run. rename x into S''. apply pick_option_correct in E.
  remember (object_with_primitive_value
             (object_with_get_own_property
                (object_new prealloc_string_proto "String")
                builtin_get_own_prop_string) "") as O.
  applys* red_spec_construct_string_2; substs~.
  (* prealloc_string_proto *)
  discriminate.
  (* prealloc_string_proto_to_string *)
  discriminate.
  (* prealloc_string_proto_value_of *)
  discriminate.
  (* prealloc_string_proto_char_at *)
  discriminate.
  (* prealloc_string_proto_char_code_at *)
  discriminate.
  (* prealloc_math *)
  discriminate.
  (* prealloc_mathop *)
  discriminate.
  (* prealloc_date *)
  discriminate.
  (* prealloc_regexp *)
  discriminate.
  (* prealloc_error *)
  let_name. apply~ red_spec_construct_error.
    apply~ get_arg_correct_0.
  substs. apply* build_error_correct.  
  (* prealloc_error_proto *)
  discriminate.
  (* prealloc_native_error *)  
  let_name. apply~ red_spec_construct_native_error.
    apply~ get_arg_correct_0.
  substs. apply* build_error_correct.
  (* prealloc_native_error_proto *)
  discriminate. (* TODO *)
  (* prealloc_error_proto_to_string *)
  discriminate.
  (* prealloc_throw_type_error *)
  discriminate.
  (* prealloc_json *)
  discriminate.
Admitted. (*faster*)

Lemma run_construct_default_correct : forall runs S C l args o,
  runs_type_correct runs ->
  run_construct_default runs S C l args = res_out o ->
  red_expr S C (spec_construct_default l args) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_construct_default using run_object_get_correct.
  let_simpl. let_simpl. let_name. destruct p as [l' S2].
  run* red_spec_construct_default_1. rewrite* EQp. case_if; case_if*.
  let_simpl. run_inv.
  applys* red_spec_function_construct_2. case_if; case_if*.
Admitted. (*faster*)

Lemma run_construct_correct : forall runs S C co l args o,
  runs_type_correct runs ->
  run_construct runs S C co l args = o ->
  red_expr S C (spec_construct_1 co l args) o.
Proof.
  introv IH HR. unfolds in HR.
  destruct co.
    applys* red_spec_construct_1_default. applys* run_construct_default_correct.

    repeat run. apply run_object_method_correct in E.  
    apply run_object_method_correct in E1; subst.
    applys* red_spec_construct_1_after_bind.
    destruct x1. repeat run. let_name. 
    apply run_object_method_correct in E0; subst. 
    applys* red_spec_construct_1_after_bind_1_some.
    applys* runs_type_correct_construct.
    applys* red_spec_construct_1_after_bind_1_none.

    applys* red_spec_construct_1_prealloc. applys* run_construct_prealloc_correct.
Admitted. (* faster *)


Lemma creating_function_object_proto_correct : forall runs S C l o,
  runs_type_correct runs ->
  creating_function_object_proto runs S C l = o ->
  red_expr S C (spec_creating_function_object_proto l) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_creating_function_object_proto
    using run_construct_prealloc_correct.
  let_simpl. run red_spec_creating_function_object_proto_1.
  let_simpl. applys* red_spec_creating_function_object_proto_2. run_hyp*.
Admitted. (* faster *)


Lemma creating_function_object_correct : forall runs S C names bd X str o,
  runs_type_correct runs ->
  creating_function_object runs S C names bd X str = o ->
  red_expr S C (spec_creating_function_object names bd X str) o.
Proof.
  introv IH HR. unfolds in HR.
  let_simpl. let_simpl. let_simpl. let_simpl. let_name. destruct p as [l S1].
  let_simpl. run* red_spec_creating_function_object.
  run red_spec_creating_function_object_1
    using creating_function_object_proto_correct.
  case_if; destruct str; tryfalse.
    run_inv. applys* red_spec_creating_function_object_2_not_strict.
    let_simpl. let_simpl.
     run* red_spec_creating_function_object_2_strict. clear EQp.
     run* red_spec_creating_function_object_3.
     applys* red_spec_creating_function_object_4.
Admitted. (* faster*)


Lemma env_record_has_binding_correct : forall runs S C L x o,
  runs_type_correct runs ->
  env_record_has_binding runs S C L x = o ->
  red_expr S C (spec_env_record_has_binding L x) o.
Proof.
  introv IH HR. unfolds in HR. run_simpl.
  forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_has_binding B. destruct x0; run_inv.
   apply~ red_spec_env_record_has_binding_1_decl.
    rewrite decide_def; auto.
   apply~ red_spec_env_record_has_binding_1_object.
    apply* object_has_prop_correct.
Qed.


Lemma binding_inst_function_decls_correct : forall runs S C args L fds str bconfig o,
  runs_type_correct runs ->
  binding_inst_function_decls runs S C L fds str bconfig = o ->
  red_expr S C (spec_binding_inst_function_decls args L fds str bconfig) o.
Proof.
  introv IH HR. gen S o. induction fds; introv HR.
  simpls. run_inv. applys* red_spec_binding_inst_function_decls_nil.
  simpls. let_name. let_name. let_name. let_name.
  run (@red_spec_binding_inst_function_decls_cons str_fd)
    using creating_function_object_correct. subst*. subst*. clear R1.
  let_name as M. rename a into fd. rename l into fo.
  asserts M_correct: (forall S o,
      M S = o ->
      red_expr S C (spec_binding_inst_function_decls_5 args L fd fds str fo bconfig) o).
    clears HR S o. introv HR. subst M.
    subst fname. run red_spec_binding_inst_function_decls_5
      using env_record_set_mutable_binding_correct.
    applys* red_spec_binding_inst_function_decls_6.
    clear EQM.
  subst fname. run red_spec_binding_inst_function_decls_1
    using env_record_has_binding_correct.
  case_if; subst.
    case_if; try subst L.
      run red_spec_binding_inst_function_decls_2_true_global
        using run_object_get_prop_correct.
       destruct a; tryfalse. case_if.
         let_name. run* red_spec_binding_inst_function_decls_3_true.
          applys* red_spec_binding_inst_function_decls_4.
         applys red_spec_binding_inst_function_decls_3_false.
           destruct (attributes_configurable a); tryfalse; auto. (* LATER: cleanup *)
          case_if.
            applys* red_spec_binding_inst_function_decls_3a_type_error.
            applys* red_spec_binding_inst_function_decls_3a_no_error.
        applys* red_spec_binding_inst_function_decls_2_true.
    run red_spec_binding_inst_function_decls_2_false
      using env_record_create_mutable_binding_correct.
     applys* red_spec_binding_inst_function_decls_4.
Admitted. (* faster *)


Lemma binding_inst_var_decls_correct : forall runs S C L vds bconfig str o,
  runs_type_correct runs ->
  binding_inst_var_decls runs S C L vds bconfig str = o ->
  red_expr S C (spec_binding_inst_var_decls L vds bconfig str) o.
Proof.
  introv IH HR. gen S o. induction vds; introv HR.
  simpls. run_inv. applys* red_spec_binding_inst_var_decls_nil.
  simpls. let_simpl.
  run red_spec_binding_inst_var_decls_cons
    using env_record_has_binding_correct.
  case_if; subst.
    applys* red_spec_binding_inst_var_decls_1_true.
    run red_spec_binding_inst_var_decls_1_false
      using env_record_create_set_mutable_binding_correct.
     applys* red_spec_binding_inst_var_decls_2.
Admitted. (* faster *)

Lemma binding_inst_formal_params_correct : forall runs S C L args names str o,
  runs_type_correct runs ->
  binding_inst_formal_params runs S C L args names str = o ->
  red_expr S C (spec_binding_inst_formal_params args L names str) o.
Proof.
  introv IH HR. gen S args o. induction names; introv HR.
  simpls. run_inv. applys* red_spec_binding_inst_formal_params_empty.
  simpls.
  let_name. let_name.
  run (@red_spec_binding_inst_formal_params_non_empty v args')
    using env_record_has_binding_correct.
    subst v args'. destruct* args.
  let_name as M. asserts M_correct: (forall S o,
      M S = o ->
      red_expr S C (spec_binding_inst_formal_params_3 args' L a names str v) o).
    clears HR S o. introv HR. subst M.
    run red_spec_binding_inst_formal_params_3
      using env_record_set_mutable_binding_correct.
    applys* red_spec_binding_inst_formal_params_4.
    clear EQM.
  case_if; subst.
    applys* red_spec_binding_inst_formal_params_1_declared.
     run red_spec_binding_inst_formal_params_1_not_declared
       using env_record_create_mutable_binding_correct.
    applys* red_spec_binding_inst_formal_params_2.
Admitted. (* faster *)


Lemma make_arg_getter_correct : forall runs S C x X o,
  runs_type_correct runs ->
  make_arg_getter runs S C x X = o ->
  red_expr S C (spec_make_arg_getter x X) o.
Proof.
  introv IH HR. unfolds in HR.
  apply~ red_spec_make_arg_getter. applys~ creating_function_object_correct HR.
Qed.

Lemma make_arg_setter_correct : forall runs S C x X o,
  runs_type_correct runs ->
  make_arg_setter runs S C x X = o ->
  red_expr S C (spec_make_arg_setter x X) o.
Proof.
  introv IH HR. unfolds in HR.
  apply~ red_spec_make_arg_setter. applys~ creating_function_object_correct HR.
Qed.

Lemma arguments_object_map_loop_correct : forall runs S C l xs len args args' X str lmap xsmap o,
  runs_type_correct runs ->
  len = length args ->
  arguments_object_map_loop runs S C l xs len args X str lmap xsmap = o ->
  red_expr S C (spec_arguments_object_map_2 l xs (args ++ args') X str lmap xsmap (len - 1)) o.
Proof.
  introv IH EQlen HR. gen o args args' S xsmap. induction len; introv EQlen HR.
   simpls. apply~ red_spec_arguments_object_map_2_negative. math. cases_if.
     substs. inverts HR. apply~ red_spec_arguments_object_map_8_nil.
     run. let_name. inverts HR. forwards~ B: @pick_option_correct E.
     applys~ red_spec_arguments_object_map_8_cons B. substs*.
   unfolds in HR. fold arguments_object_map_loop in HR.
    let_name. destruct tdl as (rmlargs&largs).
    forwards EQargs: take_drop_last_spec EQtdl; [destruct args; tryfalse; discriminate|].
    forwards EQlargs: take_drop_last_length EQtdl; [destruct args; tryfalse; discriminate|].
    clear EQtdl. simpl in HR.
    let_name. let_name. asserts Loop: (forall S xsmap o,
        arguments_object_map_loop' S xsmap = o ->
        red_expr S C (spec_arguments_object_map_2 l xs (rmlargs ++ largs :: args') X str lmap xsmap (len - 1)) o).
      clear HR. introv RES. subst arguments_object_map_loop'. apply* IHlen. math.
    clear EQarguments_object_map_loop'.
    asserts_rewrite (Datatypes.S len - 1 = len). math.
    run~ red_spec_arguments_object_map_2_positive using object_define_own_prop_correct.
      clear HR. subst args. rew_app. applys~ ZNth_app_r ZNth_here. math.
      subst A. auto*.
    clear R1 EQA. cases_if.
     apply~ red_spec_arguments_object_map_3_next.
       apply~ nat_int_ge.
      rewrite EQargs. rew_app. apply~ Loop.
     let_name. asserts ZN: (ZNth len xs x).
        apply Nth_to_ZNth. forwards (x'&N): length_Nth_lt len xs. math.
        forwards EQx': Nth_to_nth_def "" N. subst x'. rewrite~ <- EQx in N.
      cases_if.
       applys~ red_spec_arguments_object_map_3_cont_next ZN.
        rewrite EQargs. rew_app. apply~ Loop.
       applys~ red_spec_arguments_object_map_3_cont_cont ZN.
        rew_logic in n0. destruct n0 as [? NI]. splits.
         destruct~ str; false.
         auto*.
        run red_spec_arguments_object_map_4 using make_arg_getter_correct.
        run red_spec_arguments_object_map_5 using make_arg_setter_correct.
        let_name. run~ red_spec_arguments_object_map_6.
          rewrite EQA' in R1. simpl in R1.
          simpl. auto*.
        apply~ red_spec_arguments_object_map_7.
        rewrite EQargs. rew_app. apply~ Loop.
Admitted. (* faster *)

Lemma arguments_object_map_correct : forall runs S C l xs args X str o,
  runs_type_correct runs ->
  arguments_object_map runs S C l xs args X str = o ->
  red_expr S C (spec_arguments_object_map l xs args X str) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_arguments_object_map using run_construct_prealloc_correct.
  apply~ red_spec_arguments_object_map_1.
  rewrite <- (app_nil_r args).
  apply* arguments_object_map_loop_correct; rew_app~.
Qed.

Lemma create_arguments_object_correct : forall runs S C lf xs args X str o,
  runs_type_correct runs ->
  create_arguments_object runs S C lf xs args X str = o ->
  red_expr S C (spec_create_arguments_object lf xs args X str) o.
Proof.
  introv IH HR. unfolds in HR. let_name. let_name. destruct p as [l S'].
  let_name. run* red_spec_create_arguments_object; try solve [ substs* ].
  clear EQA EQO EQp A. run red_spec_create_arguments_object_1
    using arguments_object_map_correct. cases_if.
   let_name. let_name.
    run* red_spec_create_arguments_object_2_strict; try solve [ substs* ].
    clear EQA. run red_spec_create_arguments_object_3.
    apply~ red_spec_create_arguments_object_4.
   let_name.
    run* red_spec_create_arguments_object_2_non_strict; try solve [ substs* ].
    clear EQA. apply~ red_spec_create_arguments_object_4.
Qed.

Lemma binding_inst_arg_obj_correct : forall runs S C lf p xs args L o,
  runs_type_correct runs ->
  binding_inst_arg_obj runs S C lf p xs args L = o ->
  red_expr S C (spec_binding_inst_arg_obj lf p xs args L) o.
Proof.
  introv IH HR. unfolds in HR. let_name.
  run~ red_spec_binding_inst_arg_obj using create_arguments_object_correct.
  cases_if.
   run red_spec_binding_inst_arg_obj_1_strict
     using env_record_create_immutable_binding_correct.
    apply~ red_spec_binding_inst_arg_obj_2.
    apply~ env_record_initialize_immutable_binding_correct.
   apply~ red_spec_binding_inst_arg_obj_1_not_strict.
    applys~ env_record_create_set_mutable_binding_correct HR.
Qed.


Lemma execution_ctx_binding_inst_correct : forall runs S C ct funco (p:prog) args o,
  runs_type_correct runs ->
  execution_ctx_binding_inst runs S C ct funco p args = o ->
  red_expr S C (spec_binding_inst ct funco p args) o.
Proof.
  introv IH HR. unfolds in HR.
  cases (execution_ctx_variable_env C); tryfalse. rename e into L.
  applys* red_spec_binding_inst. clears TEMP. let_simpl.
  let_name as M. asserts M_correct: (forall S xs o,
      M S xs = o ->
      red_expr S C (spec_binding_inst_3 ct funco p xs args L) o).
    clear HR S o. introv HR. subst M.
    let_name. let_name.
    run red_spec_binding_inst_3
      using binding_inst_function_decls_correct.
        subst bconfig. rewrite decide_def. auto.
        auto.
    applys red_spec_binding_inst_4.
    run red_spec_binding_inst_5 using env_record_has_binding_correct.
    let_name as N. asserts N_correct: (forall S o,
      N S = o ->
      red_expr S C (spec_binding_inst_8 p bconfig L) o).
      clear HR S o. introv HR. subst N.
      applys red_spec_binding_inst_8.
        applys* binding_inst_var_decls_correct.
      clear EQN.
    destruct ct.
      destruct funco.
        destruct b; tryfalse.
          applys* red_spec_binding_inst_6_no_arguments. rew_logic*.
          run red_spec_binding_inst_6_arguments
            using binding_inst_arg_obj_correct.
           applys* red_spec_binding_inst_7.
        destruct b; tryfalse.
         applys* red_spec_binding_inst_6_no_arguments. rew_logic*.
      applys* red_spec_binding_inst_6_no_arguments.
       rew_logic*. left. congruence.
      applys* red_spec_binding_inst_6_no_arguments.
       rew_logic*. left; congruence.
  destruct ct; destruct funco; tryfalse.
    run. lets H: run_object_method_correct (rm E).
     run. subst. run* red_spec_binding_inst_1_function
       using binding_inst_formal_params_correct.
     applys* red_spec_binding_inst_2.
    applys* red_spec_binding_inst_1_not_function. congruence.
    applys* red_spec_binding_inst_1_not_function. congruence.
Admitted. (* faster *)


Lemma entering_eval_code_correct : forall runs S C bdirect bd F K o,
  runs_type_correct runs ->
  entering_eval_code runs S C bdirect bd F = o ->
  (forall S' C' o', F S' C' = o' -> red_expr S' C' K o') ->
  red_expr S C (spec_entering_eval_code bdirect bd K) o.
Proof.
  introv IH HR HK. unfolds in HR.
  let_name. let_name.
  applys* red_spec_entering_eval_code str C'. case_if; case_if*.
  let_name. destruct p as [lex S'].
  let_name. let_name.
  run_pre. applys* red_spec_entering_eval_code_1 str lex S' C1 o1.
    rewrite EQp. case_if; case_if*.
    subst C1. case_if; case_if*.
    subst p. applys* execution_ctx_binding_inst_correct R1.
    run_post. clear R1.
  applys* red_spec_entering_eval_code_2.
Admitted. (* faster *)


Lemma run_eval_correct : forall runs S C (is_direct_call : bool) vs o,
  runs_type_correct runs ->
  run_eval runs S C is_direct_call vs = o ->
  red_expr S C (spec_call_global_eval is_direct_call vs) o.
Proof.
  introv IH HR. unfolds in HR.
  lets (v&H&E): arguments_from_spec_1 vs. rewrites (rm E) in *.
  applys* red_spec_call_global_eval (rm H).
  destruct v;
   [| run_inv; applys* red_spec_call_global_eval_1_not_string; simpl; congruence].
  destruct p; run_inv;
     try (applys* red_spec_call_global_eval_1_not_string; simpl; congruence).
  let_name. destruct (pick_option (parse s str)) eqn:P.
   forwards B: @pick_option_correct (rm P).
    applys* red_spec_call_global_eval_1_string_parse.
    applys* entering_eval_code_correct (rm HR).
    clear - IH. introv HR. run red_spec_call_global_eval_2.
    sets_eq RT: (res_type R). destruct RT; tryfalse.
     run. cases (res_value R); tryfalse; run_inv.
      applys* red_spec_call_global_eval_3_normal_empty.
      destruct R. simpls. subst.
       applys* red_spec_call_global_eval_3_normal_value.
     run_inv. applys* red_spec_call_global_eval_3_throw.
   applys red_spec_call_global_eval_1_string_not_parse.
    introv Pa. forwards (?&Par): @pick_option_defined (ex_intro _ p Pa).
     rewrite Par in P. false.
    applys run_error_correct HR.
Admitted. (*faster*)

Lemma run_list_expr_correct : forall runs S C es y,
  runs_type_correct runs ->
  run_list_expr runs S C nil es = result_some y ->
  red_spec S C (spec_list_expr es) y.
Proof.
  introv IH. cuts M: (forall es S C vs y,
      run_list_expr runs S C vs es = result_some y ->
      red_spec S C (spec_list_expr_1 (rev vs) es) y).
    intros HR. apply red_spec_list_expr. applys* M (@nil value).
  clears S C es y. intros es. induction es; introv HR.
  simpls. run_inv. applys* red_spec_list_expr_1_nil.
  simpls. run red_spec_list_expr_1_cons.
   applys red_spec_list_expr_2. forwards M: IHes HR.
   rew_list in M. auto.
Admitted. (*faster*)


Lemma run_call_default_correct : forall runs S C lf o,
  runs_type_correct runs ->
  run_call_default runs S C lf = o ->
  red_expr S C (spec_call_default_1 lf) o.
Proof.
  introv IH HR. unfolds in HR. let_simpl.
  run. applys* red_spec_call_default_1.
    applys* run_object_method_correct. clear E.
  destruct x.
    case_if.
      applys* red_spec_call_default_2_empty_body.
       run_inv. applys* red_spec_call_default_3_normal.
      run* red_spec_call_default_2_body.
        destruct R as [RT RV RL]; simpls. subst.
         applys red_spec_call_default_3_normal.
        destruct R as [RT RV RL]; simpls. subst.
         applys red_spec_call_default_3_return.
        subst. abort.
    applys* red_spec_call_default_2_empty_body.
     run_inv. applys* red_spec_call_default_3_normal.
Admitted. (* faster *)


Lemma env_record_implicit_this_value_correct : forall S C L v,
  env_record_implicit_this_value S L = Some v ->
  red_expr S C (spec_env_record_implicit_this_value L) (out_ter S v).
Proof.
  introv HR. unfolds in HR.
  run_simpl HR as H; tryfalse. inverts H. forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_implicit_this_value B. destruct n.
   applys~ red_spec_env_record_implicit_this_value_1_decl.
   applys~ red_spec_env_record_implicit_this_value_1_object.
Qed.

Lemma run_expr_call_correct : forall runs S C e1 e2s o,
  runs_type_correct runs ->
  run_expr_call runs S C e1 e2s = o ->
  red_expr S C (expr_call e1 e2s) o.
Proof.
  introv IH HR. unfolds in HR.
  let_name. run red_expr_call.
  run red_expr_call_1 using ref_get_value_correct.
  run red_expr_call_2 using run_list_expr_correct.
  destruct a.
    applys* red_expr_call_3.
  case_if.
  applys* red_expr_call_3_callable.
  rename o0 into l. rename a0 into vs.
  let_name as M. asserts M_correct: (forall S0 vthis o,
      M vthis = o ->
      red_expr S0 C (expr_call_5 l is_eval_direct vs (out_ter S3 vthis)) o).
    clear HR S o. introv HR. subst M.
    case_if.
      subst. applys red_expr_call_5_eval. applys* run_eval_correct.
      applys* red_expr_call_5_not_eval. apply* IH.
    clear EQM.
  subst. destruct rv; tryfalse.
    applys* red_expr_call_4_not_ref.
    cases (ref_base r).
      case_if. applys* red_expr_call_4_prop.
      run. applys* red_expr_call_4_env.
       applys* env_record_implicit_this_value_correct.
  (* other branch *)
  applys* red_expr_call_3.
Admitted. (*faster*)

Ltac run_select_proj_extra_construct HT ::=
  match HT with
  | run_construct_prealloc => constr:(run_construct_prealloc_correct)
  | run_construct => constr:(run_construct_correct)
  | run_call_default => constr:(run_call_default_correct)
  | creating_function_object_proto => constr:(creating_function_object_proto_correct)
  | creating_function_object => constr:(creating_function_object_correct)
  | run_list_expr => constr:(run_list_expr_correct)
  | execution_ctx_binding_inst => constr:(execution_ctx_binding_inst_correct)
  end.


(**************************************************************)
(** ** Property descriptors *)

Lemma from_prop_descriptor_correct : forall runs S0 S C D o,
  runs_type_correct runs ->
  from_prop_descriptor runs S C D = o ->
  red_expr S0 C (spec_from_descriptor (ret S D)) o.
Proof.
  introv IH HR. unfolds in HR. destruct D.
  run_inv. applys* red_spec_from_descriptor_undef.
  run* red_spec_from_descriptor_some.
  rename a into A.
  let_name as M. asserts M_correct: (forall S0 S b o,
    M S b = res_out o ->
    red_expr S0 C (spec_from_descriptor_4 l A (out_ter S b)) o).
   clear HR S o. introv HR. subst M.
    let_name. run* red_spec_from_descriptor_4. congruence.
    let_name. run* red_spec_from_descriptor_5. congruence.
    applys* red_spec_from_descriptor_6.
    clear EQM.
  destruct A.
    let_name. run* red_spec_from_descriptor_1_data. congruence.
     let_name. run red_spec_from_descriptor_2_data. congruence.
     applys* M_correct.
    let_name. run red_spec_from_descriptor_1_accessor. congruence.
     let_name. run red_spec_from_descriptor_3_accessor. congruence.
     applys* M_correct.
Admitted. (*faster*)


(**************************************************************)
(** ** Object Initialisation *)

Lemma create_new_function_in_correct : forall runs S C args bd o,
  runs_type_correct runs ->
  create_new_function_in runs S C args bd = o ->
  red_expr S C (spec_create_new_function_in C args bd) o.
Proof.
  introv IH HR. unfolds in HR. applys red_spec_create_new_function_in.
  applys* creating_function_object_correct.
Qed.

Lemma init_object_correct : forall runs S C l (pds : propdefs) o,
  runs_type_correct runs ->
  init_object runs S C l pds = o ->
  red_expr S C (expr_object_1 l pds) o.
Proof.
  introv IH. gen S. induction pds as [|(pn&pb) pds]; introv HR.
  simpls. run_inv. applys red_expr_object_1_nil.
  simpls. let_name. let_name.
  asserts follows_correct: (forall S Desc, follows S Desc = res_out o ->
      red_expr S C (expr_object_4 l x Desc pds) o).
    subst follows. clear HR. introv HR.
    run red_expr_object_4 using object_define_own_prop_correct.
     applys* red_expr_object_5.
    clear EQfollows.
  applys* red_expr_object_1_cons x.
  destruct pb.
   run red_expr_object_2_val.
    applys* red_expr_object_3_val.
   run red_expr_object_2_get using create_new_function_in_correct.
    applys* red_expr_object_3_get.
   run red_expr_object_2_set using create_new_function_in_correct.
    applys* red_expr_object_3_set.
Qed.

Lemma red_expr_array_3_object_loc_eq : forall ElementList S S' C l l' k,
  red_expr S C (expr_array_3 l ElementList k) (out_ter S' l') -> l = l'.
Proof.
  induction ElementList using (measure_induction length).
  destruct ElementList; introv Hyp.

  + inverts~ Hyp. inverts~ H0. 

  + destruct o.
    - inverts Hyp. inverts H0. 
      inverts H8.  inverts H1. unfolds abrupt_res. false~ H4.
      inverts H10. inverts H1. unfolds abrupt_res. false~ H4.
      inverts H12. inverts H1. unfolds abrupt_res. false~ H4.
      inverts H13. inverts H1. unfolds abrupt_res. false~ H4.
      inverts H14. inverts H1. unfolds abrupt_res. false~ H4.
      specializes~ H H6. rew_length; nat_math.

    - inverts Hyp. inverts H0.
      specializes~ H H10. 
      inverts H4.
      * rew_length; nat_math.
      * destruct H as (e & oes' & Heq). subst.
        rewrite H3. rew_length.
        destruct Elision. rewrite app_nil_l in H3.
        inverts H3. rew_length; nat_math.
Admitted. (*faster*)

Lemma run_array_element_list_correct : forall runs S C l oes o k,
  runs_type_correct runs ->
  run_array_element_list runs S C l oes k = o ->
  red_expr S C (expr_array_3 l oes k) o.
Proof.
  introv IH HR. gen runs S C l o. 
  destruct oes; intros. 

  + inverts HR. apply red_expr_array_3_nil.

  + destruct o.
    - unfolds run_array_element_list.       
      let_name. subst. 
      run~ red_expr_array_3_some_val; rename a into v.
      run~ red_expr_array_3_get_len using run_object_get_correct.
      run~ red_expr_array_3_convert_len.
      run~ red_expr_array_3_add_len.
      let_name. subst. run~ red_expr_array_3_def_own_prop.
      run red_expr_array_3_next. substs~. 
      
    - simpls. let_name.   
      eapply red_expr_array_3_none.
      * apply elision_head_decomposition. 
      * jauto. * jauto. * jauto.                        
      * substs. applys* runs_type_correct_array_element_list. 
Qed.

Lemma init_array_correct : forall runs S C l oes o,
  runs_type_correct runs ->
  init_array runs S C l oes = o ->
  red_expr S C (expr_array_1 l oes) o.
Proof.
  introv IH HR. unfolds in HR. let_name. let_name. 
  apply red_expr_array_1 with (ElementList := ElementList) 
                              (Elision := list_repeat None ElisionLength) 
                              (ElisionLength := ElisionLength); 
    try solve [try rewrite my_Z_of_nat_def; substs~]. 
  
  run red_expr_array_2.
  eapply run_array_element_list_correct; eassumption.
  apply run_array_element_list_correct in R1; auto. 
  apply red_expr_array_3_object_loc_eq in R1. subst l0.
    
  apply red_expr_array_add_length.  
    run red_expr_array_add_length_0 using run_object_get_correct.
    run red_expr_array_add_length_1. run red_expr_array_add_length_2. 
    run red_expr_array_add_length_3. apply red_expr_array_add_length_4.
Qed.

Lemma lexical_env_get_identifier_ref_correct : forall runs S C lexs x str y,
  runs_type_correct runs ->
  lexical_env_get_identifier_ref runs S C lexs x str = result_some y ->
  red_spec S C (spec_lexical_env_get_identifier_ref lexs x str) y.
Proof.
  introv IH. gen S C. induction lexs; introv HR.
  simpls. run_inv.
   applys* red_spec_lexical_env_get_identifier_ref_nil.
  simpls.
  applys red_spec_lexical_env_get_identifier_ref_cons.
  run red_spec_lexical_env_get_identifier_ref_cons_1 using env_record_has_binding_correct.
  cases_if; run_inv.
   apply~ red_spec_lexical_env_get_identifier_ref_cons_2_true.
   apply~ red_spec_lexical_env_get_identifier_ref_cons_2_false.
Qed.

Lemma run_typeof_value_correct : forall S v,
  run_typeof_value S v = typeof_value S v.
Proof. intros. destruct v; simpl. auto. case_if; case_if*. Qed.

Ltac run_select_proj_extra_get_value HT ::=
  match HT with
  | ref_get_value => constr:(ref_get_value_correct)
  end.


(**************************************************************)
(** ** Main theorem *)

Hint Extern 1 (regular_unary_op _) =>
    intros ?; false_invert.

Lemma prepost_op_correct : forall u F ispre,
  run_prepost_op u = Some (F,ispre) ->
  prepost_op u F ispre.
Proof.
  Hint Constructors prepost_op.
  introv HR. destruct u; simpls; inverts* HR.
Qed.


Lemma object_delete_default_correct : forall runs S C l x str o,
  runs_type_correct runs ->
  object_delete_default runs S C l x str = o ->
  red_expr S C (spec_object_delete_1 builtin_delete_default l x str) o.
Proof.
  introv IH HR. unfolds in HR. run red_spec_object_delete_1_default. destruct a.
   run_inv. applys red_spec_object_delete_2_undef. (* This rule is erroneous, the conclusion should contains [S0] instead [S]. *)
   case_if.
     run. forwards B: @pick_option_correct (rm E).
       applys_eq* red_spec_object_delete_2_some_configurable 1.
     applys* red_spec_object_delete_3_some_non_configurable.
      applys* out_error_or_cst_correct.
Qed.

Lemma object_delete_correct : forall runs S C l x str o,
  runs_type_correct runs ->
  object_delete runs S C l x str = o ->
  red_expr S C (spec_object_delete l x str) o.
Proof.
  introv IH HR. unfolds in HR. run. rename x0 into B. (* LTAC ARTHUR *)
  applys* red_spec_object_delete.
   applys* run_object_method_correct. clear E.
  destruct B.
  (* default *)
  applys~ object_delete_default_correct HR.
  (* argument object *)
  run. forwards* obpm: run_object_method_correct.
  run. substs. run~ red_spec_object_delete_args_obj.
  run red_spec_object_delete_args_obj_1 using object_delete_default_correct.
  cases_if. destruct a. (* LTAC ARTHUR *)
   apply~ red_spec_object_delete_args_obj_2_else.
    inverts HR. apply~ red_spec_object_delete_args_obj_4.
   run red_spec_object_delete_args_obj_2_if.
    apply~ red_spec_object_delete_args_obj_3.
    apply~ red_spec_object_delete_args_obj_4.
   apply~ red_spec_object_delete_args_obj_2_else.
    inverts HR. apply~ red_spec_object_delete_args_obj_4.
Qed.


Lemma env_record_delete_binding_correct : forall runs S C L x o,
  runs_type_correct runs ->
  env_record_delete_binding runs S C L x = o ->
  red_expr S C (spec_env_record_delete_binding L x) o.
Proof.
  introv IH HR. unfolds in HR.
  run_simpl. forwards B: @pick_option_correct (rm E).
  applys~ red_spec_env_record_delete_binding B. destruct x0.
   sets_eq <- ero E: (Heap.read_option d x). destruct ero as [[mu ?]|].
    rewrite <- Heap.binds_equiv_read_option in E. destruct mu; run_inv;
     applys~ red_spec_env_record_delete_binding_1_decl_indom E; case_if*.
    rewrite <- Heap.not_indom_equiv_read_option in E. run_inv.
     applys~ red_spec_env_record_delete_binding_1_decl_not_indom E.
   run. apply~ red_spec_env_record_delete_binding_1_object.
Qed.


Lemma identifier_resolution_correct : forall runs S C x y,
  runs_type_correct runs ->
  identifier_resolution runs S C x = result_some y ->
  red_spec S C (spec_identifier_resolution C x) y.
Proof.
  introv IH HR.
  unfolds spec_identifier_resolution, identifier_resolution.
  applys* lexical_env_get_identifier_ref_correct.
Qed.

Lemma run_expr_correct : forall runs S C e o,
  runs_type_correct runs ->
  run_expr runs S C e = o ->
  red_expr S C (expr_basic e) o.
Proof.
  introv IH R. unfolds in R.
  destruct e as [ | | | pds | oes | | |  | | | | | | ].
  (* this *)
  run_inv. apply~ red_expr_this.
  (* identifier *)
  run_inv. run red_expr_identifier using identifier_resolution_correct.
  applys* red_expr_identifier_1.
  (* literal *)
  run_inv. apply~ red_expr_literal.
  (* object *)
  run red_expr_object using run_construct_prealloc_correct.
  applys red_expr_object_0.
  applys* init_object_correct.

  (* _ARRAYS_ *)
  run red_expr_array using run_construct_prealloc_correct.
  applys red_expr_array_0.
  applys* init_array_correct.

  (* function *)
  unfolds in R. destruct o0.
    let_name. destruct p as (lex'&S').
     destruct lex' as [|L lex']; simpls; tryfalse.
     run_simpl. forwards: @pick_option_correct (rm E).
     run* red_expr_function_named using env_record_create_immutable_binding_correct.
     run red_expr_function_named_1 using creating_function_object_correct.
     run red_expr_function_named_2 using env_record_initialize_immutable_binding_correct.
     apply~ red_expr_function_named_3.
    apply~ red_expr_function_unnamed. applys~ creating_function_object_correct IH.
  (* Access *)
  unfolds in R. run red_expr_access.
  run red_expr_access_1. cases_if.
    forwards [R1 N]: run_error_correct' C (rm R). applys red_expr_access_2.
      applys* red_spec_check_object_coercible_undef_or_null.
      abort.
    applys red_expr_access_2.
      applys* red_spec_check_object_coercible_return.
     run red_expr_access_3.
     applys* red_expr_access_4.
  (* member *)
  run_hyp R. apply~ red_expr_member.
  (* new *)
  unfolds in R. run red_expr_new.
  run red_expr_new_1.
  destruct a; tryfalse.
    applys* red_expr_new_2_type_error_not_object.
    run. lets M: run_object_method_correct (rm E).
    destruct x; tryfalse.
      applys red_expr_new_2_construct.
       applys* red_spec_constructor.
       applys* run_construct_correct.
      applys* red_expr_new_2_type_error_no_construct.
  (* call *)
  applys* run_expr_call_correct.
  (* unary operators *)
  unfolds in R. case_if as N.
    run* red_expr_prepost. run red_expr_prepost_1_valid.
     run red_expr_prepost_2. run. destruct x as [F ispre].
     let_simpl. let_name. lets: prepost_op_correct (rm E).
     run* red_expr_prepost_3. subst. applys* red_expr_prepost_4.
    destruct u; try solve [ false n; unfolds; do 2 eexists; constructors ].
    (* delete *)
    run red_expr_delete. destruct rv; run_inv.
      apply~ red_expr_delete_1_not_ref. intro; false_invert.
      apply~ red_expr_delete_1_not_ref. intro; false_invert.
      case_if; run_inv.
        apply~ red_expr_delete_1_ref_unresolvable. cases_if.
         apply~ red_expr_delete_2_strict. apply* run_error_correct.
         run_inv. apply~ red_expr_delete_2_not_strict.
        cases (ref_base r).
          run* red_expr_delete_1_ref_property using to_object_correct.
            apply* ref_is_property_from_not_unresolvable_value.
            apply~ red_expr_delete_3. runs~.
          rename e0 into L. apply* red_expr_delete_1_ref_env_record. cases_if.
            apply~ red_expr_delete_4_strict. apply* run_error_correct.
            apply~ red_expr_delete_4_not_strict. applys* env_record_delete_binding_correct.
    (* void *)
    run* red_expr_unary_op. applys red_expr_unary_op_1.
     applys* red_expr_unary_op_void.
    (* typeof *)
    run red_expr_typeof. destruct rv; tryfalse.
      applys* red_expr_typeof_1_value. run_inv. applys* red_expr_typeof_2.
        applys run_typeof_value_correct.
      case_if.
        run_inv. applys* red_expr_typeof_1_ref_unresolvable.
        run* red_expr_typeof_1_ref_resolvable.
         applys* red_expr_typeof_2.
         applys* run_typeof_value_correct.
   (* add *)
   run* red_expr_unary_op. applys red_expr_unary_op_1.
    applys red_expr_unary_op_add. run_hyp*.
   (* neg *)
   run* red_expr_unary_op. applys red_expr_unary_op_1.
    run red_expr_unary_op_neg. applys* red_expr_unary_op_neg_1.
   (* bitwise not *)
   run* red_expr_unary_op. applys red_expr_unary_op_1.
    run red_expr_unary_op_bitwise_not.
    applys* red_expr_unary_op_bitwise_not_1.
   (* not *)
   run* red_expr_unary_op. applys red_expr_unary_op_1.
   forwards* M: red_spec_to_boolean a.
    applys* red_expr_unary_op_not. applys* red_expr_unary_op_not_1.
  (* binary operators *)
  unfolds in R. rename b into op.
  lets: (is_lazy_op_correct op). cases (is_lazy_op op).
    run* red_expr_binary_op_lazy.
     let_name. applys* red_expr_lazy_op_1. applys* red_spec_to_boolean.
     case_if; subst; run_inv.
       applys* red_expr_lazy_op_2_first.
       run* red_expr_lazy_op_2_second.
       applys* red_expr_lazy_op_2_second_1.
    run* red_expr_binary_op.
     run red_expr_binary_op_1.
     applys* red_expr_binary_op_2.
     inverts R as M. applys* run_binary_op_correct M.
  (* conditionnal *)
  unfolds in R.
  run_pre. lets (y1&R2&K): if_spec_post_to_bool (rm R1) (rm R).
   applys* red_expr_conditional (rm R2). run_post_if_spec_ter_post_bool K.
  let_name. run red_expr_conditional_1. case_if in EQe; case_if*.
  applys* red_expr_conditional_2.
  (* assign *)
  unfolds in R. run red_expr_assign. let_name. rename rv into rv1.
  asserts follow_correct: (forall S0 S rv o, follow S rv = o ->
     exists v, rv = resvalue_value v /\ red_expr S0 C (expr_assign_4 rv1 (ret S v)) o).
    subst follow. clear R. introv HR.
    destruct rv; tryfalse. exists v. split~.
    run red_expr_assign_4_put_value.
    applys* red_expr_assign_5_return.
    clear EQfollow.
  destruct o0.
    run red_expr_assign_1_compound using ref_get_value_correct.
      run red_expr_assign_2_compound_get_value.
        run red_expr_assign_3_compound_op using run_binary_op_correct.
        forwards (v&?&?): follow_correct (rm R). subst.
        applys* red_expr_assign_3'.
    run red_expr_assign_1_simple.
    forwards (v&?&?): follow_correct (rm R). run_inv. auto*.
Admitted. (*faster*)


(* Hints for automatically applying "run_hyp" in obvious cases *)
Hint Extern 1 (red_stat ?S ?C ?s ?o) =>
  match goal with H: _ = result_some o |- _ => run_hyp H end.
Hint Extern 1 (red_expr ?S ?C ?s ?o) =>
  match goal with H: _ = result_some o |- _ => run_hyp H end.


Lemma run_var_decl_item_correct : forall runs S C x eo o,
  runs_type_correct runs ->
  run_var_decl_item runs S C x eo = o ->
  red_stat S C (stat_var_decl_item (x,eo)) o.
Proof.
  introv IH HR. unfolds in HR. destruct eo.
  run red_stat_var_decl_item_some using identifier_resolution_correct.
   run red_stat_var_decl_item_1. run red_stat_var_decl_item_2.
   applys* red_stat_var_decl_item_3.
  run_inv. applys* red_stat_var_decl_item_none.
Admitted. (* faster *)

Lemma run_var_decl_correct : forall runs S C ls o,
  runs_type_correct runs ->
  run_var_decl runs S C ls = o ->
  red_stat S C (stat_var_decl ls) o.
Proof.
  introv IH. gen S. induction ls as [|[x eo]]; introv HR.
  simpls. run_inv. applys* red_stat_var_decl_nil.
  simpls. run red_stat_var_decl_cons using run_var_decl_item_correct.
   applys* red_stat_var_decl_1.
Admitted. (* faster *)


Lemma run_elements_correct : forall runs S C str ls o,
  runs_type_correct runs ->
  run_elements runs S C ls = o ->
  red_prog S C (prog_intro str (rev ls)) o.
Proof.
  introv IH HR. gen S C str o.
  induction ls; introv HR; unfolds in HR; rew_list.
  run_inv. applys* red_prog_nil.
  run_pre. eauto. applys* red_prog_cons. run_post. clear R1.
   (* run* red_prog_cons. ==> LATER: should work*)
  destruct a.
  run red_prog_1_stat. applys* red_prog_2.
  run_inv. applys red_prog_1_funcdecl.
Admitted. (*faster*)


Lemma run_block_correct : forall runs S C ls o,
  runs_type_correct runs ->
  run_block runs S C ls = o ->
  red_stat S C (stat_block (rev ls)) o.
Proof.
  introv IH HR. gen S C o.
  induction ls; introv HR; unfolds in HR; rew_list.
  run_inv. applys* red_stat_block_nil.
  run_pre. eauto. applys* red_stat_block_cons.
  run_post. clear R1.
   (* run* red_stat_block_cons. ==> LATER: should work*)
  run red_stat_block_1.
  subst. applys* red_stat_block_2_throw.
  subst. applys* red_stat_block_2_not_throw.
  applys* red_stat_block_2_not_throw. simple*.
   unfolds res_overwrite_value_if_empty. case_if; case_if*.
Admitted. (*faster*)

Lemma run_stat_switch_no_default_end_correct : forall runs S C rv scs o,
  runs_type_correct runs ->
  run_stat_switch_end runs S C rv scs = o ->
  red_stat S C (stat_switch_nodefault_5 rv scs) o.
Proof.
  introv IH HR. gen S C rv o. induction scs; introv HR; unfolds in HR.
   run_inv. apply~ red_stat_switch_nodefault_5_nil.
   destruct a as [e ts]. run red_stat_switch_nodefault_5_cons.
    forwards~ H: run_block_correct R1. rew_list~ in H.
    substs. abort.
    substs. tests: (res_is_normal R).
     apply~ red_stat_switch_nodefault_6_abrupt.
    apply~ red_stat_switch_nodefault_6_normal. apply* IHscs. repeat case_if*.
Qed.

Lemma run_stat_switch_no_default_correct : forall runs S C vi rv scs o,
  runs_type_correct runs ->
  run_stat_switch_no_default runs S C vi rv scs = o ->
  red_stat S C (stat_switch_nodefault_1 vi rv scs) o.
Proof.
  introv IH HR. gen S C vi rv o. induction scs; introv HR; unfolds in HR.
   run_inv. apply~ red_stat_switch_nodefault_1_nil.
    apply~ red_stat_switch_nodefault_5_nil.
   destruct a. run red_stat_switch_nodefault_1_cons. let_simpl.
   apply~ red_stat_switch_nodefault_2. case_if.
    run red_stat_switch_nodefault_3_true using run_block_correct. rew_list~ in R1.
     apply~ red_stat_switch_nodefault_4.
     applys~ run_stat_switch_no_default_end_correct HR.
    apply~ red_stat_switch_nodefault_3_false.
Qed.

Lemma run_stat_switch_with_default_end_correct : forall runs S C rv scs o,
  runs_type_correct runs ->
  run_stat_switch_end runs S C rv scs = o ->
  red_stat S C (stat_switch_default_7 rv scs) o.
Proof.
  introv IH HR. gen S C rv o. induction scs; introv HR; unfolds in HR.
   run_inv. apply~ red_stat_switch_default_7_nil.
   destruct a as [e ts]. run red_stat_switch_default_7_cons.
    forwards~ H: run_block_correct R1. rew_list~ in H.
    substs. abort.
    substs. tests: (res_is_normal R).
     apply~ red_stat_switch_default_8_abrupt.
    apply~ red_stat_switch_default_8_normal. apply* IHscs. repeat case_if*.
Qed.

Lemma run_stat_switch_with_default_default_correct : forall runs S C vi rv ts scs o,
  runs_type_correct runs ->
  run_stat_switch_with_default_default runs S C ts scs = o ->
  red_stat S C (stat_switch_default_5 vi rv ts scs) o.
Proof.
  introv IH HR. unfolds in HR. run red_stat_switch_default_5
    using run_block_correct. rew_list~ in R1.
  apply~ red_stat_switch_default_6.
  applys~ run_stat_switch_with_default_end_correct HR.
Qed.

Lemma run_stat_switch_with_default_B_correct : forall runs S C vi rv ts scs o,
  runs_type_correct runs ->
  run_stat_switch_with_default_B runs S C vi rv ts scs = o ->
  red_stat S C (stat_switch_default_B_1 vi rv ts scs) o.
Proof.
  introv IH HR. gen S C vi rv ts o. induction scs; introv HR; unfolds in HR.
   apply~ red_stat_switch_default_B_1_nil.
    applys~ run_stat_switch_with_default_default_correct HR.
   destruct a. run red_stat_switch_default_B_1_cons. let_simpl.
    apply~ red_stat_switch_default_B_2. case_if.
    run red_stat_switch_default_B_3_true using run_block_correct. rew_list~ in R1.
     apply~ red_stat_switch_default_B_4.
     applys~ run_stat_switch_with_default_end_correct HR.
    apply~ red_stat_switch_default_B_3_false.
Qed.

Lemma run_stat_switch_with_default_A_correct : forall runs S C found vi rv scs1 ts scs2 o,
  runs_type_correct runs ->
  run_stat_switch_with_default_A runs S C found vi rv scs1 ts scs2 = o ->
  red_stat S C (stat_switch_default_A_1 found vi rv scs1 ts scs2) o.
Proof.
  introv IH HR. gen S C found vi rv ts scs2 o. induction scs1; introv HR; unfolds in HR.
   case_if.
    apply~ red_stat_switch_default_A_1_nil_true.
     applys~ run_stat_switch_with_default_default_correct HR.
    apply~ red_stat_switch_default_A_1_nil_false.
     applys~ run_stat_switch_with_default_B_correct HR.
   destruct a. let_name. asserts follow_correct: (forall S o,
     follow S = res_out o ->
     red_stat S C (stat_switch_default_A_4 rv vi l scs1 ts scs2) o).
     clear HR. introv E. substs. run red_stat_switch_default_A_4
       using run_block_correct. rew_list~ in R1. abort.
      substs. applys~ red_stat_switch_default_A_5_abrupt.
      apply~ red_stat_switch_default_A_5. apply~ IHscs1. repeat case_if~.
    clear EQfollow. case_if.
     apply~ red_stat_switch_default_A_1_cons_true.
     run red_stat_switch_default_A_1_cons_false.
     apply~ red_stat_switch_default_A_2. let_simpl. cases_if.
      apply~ red_stat_switch_default_A_3_true.
      apply~ red_stat_switch_default_A_3_false.
Qed.

Lemma run_stat_switch_correct : forall runs S C labs e sb o,
  runs_type_correct runs ->
  run_stat_switch runs S C labs e sb = o ->
  red_stat S C (stat_switch labs e sb) o.
Proof.
  introv IH HR. unfolds in HR.
  run red_stat_switch. let_name. asserts follow_correct: (forall S C o1 o,
      follow o1 = res_out o -> red_stat S C (stat_switch_2 o1 labs) o).
    clear HR. introv HR. substs.
    do 2 (run_pre; run_post; run_inv; substs); try solve [abort].
     case_if; run_inv.
      destruct R. simpls. substs. apply* red_stat_switch_2_break.
      abort.
     apply~ red_stat_switch_2_normal.
     case_if; run_inv; tryfalse.
      destruct R. simpls. substs. apply* red_stat_switch_2_break.
  asserts follow_arg: (forall W o,
    follow W = res_out o -> exists (o1 : out), W = o1).
    clear HR follow_correct. introv R. substs.
    do 2 (run_pre; run_post; run_inv; substs); tryfalse; auto*.
  clear EQfollow. destruct sb.
   forwards~ (o1&E): follow_arg HR.
    applys~ red_stat_switch_1_nodefault o1.
    applys~ run_stat_switch_no_default_correct E.
    apply~ follow_correct. rewrite~ <- E.
   forwards~ (o1&E): follow_arg HR.
    applys~ red_stat_switch_1_default o1.
    applys~ run_stat_switch_with_default_A_correct E.
    apply~ follow_correct. rewrite~ <- E.
Qed.

Lemma run_stat_for_correct : forall runs S C labs eo1 eo2 eo3 t o,
  runs_type_correct runs ->
  run_stat_for runs S C labs eo1 eo2 eo3 t = o ->
  red_stat S C (stat_for labs eo1 eo2 eo3 t) o.
Proof.
  introv IH R. unfolds in R. destruct eo1.
   run red_stat_for_some. run_hyp.
    apply~ red_stat_for_1.
   run_hyp R. apply~ red_stat_for_none.
Qed.

Lemma run_stat_for_var_correct : forall runs S C labs ds eo2 eo3 t o,
  runs_type_correct runs ->
  run_stat_for_var runs S C labs ds eo2 eo3 t = o ->
  red_stat S C (stat_for_var labs ds eo2 eo3 t) o.
Proof.
  introv IH R. unfolds in R.
  run red_stat_for_var. run_hyp. apply~ red_stat_for_var_1.
Qed.


Lemma run_stat_correct : forall runs S C t o,
  runs_type_correct runs ->
  run_stat runs S C t = o ->
  red_stat S C (stat_basic t) o.
Proof.
  introv RC R. unfolds in R.
  destruct t as [ | | ls | ls | e t1 t2o | labs t e | labs e t | e t
     | e | eo | labo | labo | t co fo | labs e1 e2 e3 t
     | labs xeo1s e2 e3 t | labs e1 e2 e3 t | labs str eo e t | eo | ].
  (* Expression *)
  run red_stat_expr. apply red_stat_expr_1.
  (* Label *)
  unfolds in R. run red_stat_label.
    tests HC: (res_is_normal R0).
      inverts HC. run_inv. subst. applys* red_stat_label_1_normal.
      subst. applys* red_stat_abort. intro M. inverts M. simpls. false.
    case_if.
      applys* red_stat_label_1_break_eq. destruct R0; simpls. fequal*.
      applys* red_stat_abort. constructors. intro N. inverts N. false.
       intro M. inverts M. simpls. false.
      (* LATER: change interpreter to make it more faithful *)
  (* Block *)
  forwards* E: run_block_correct (rev ls). rew_list* in E.
  (* Variable declaration *)
  applys* run_var_decl_correct.
  (* If *)
  unfolds in R.
  run_pre. lets (y1&R2&K): if_spec_post_to_bool (rm R1) (rm R).
   applys* red_stat_if (rm R2). run_post_if_spec_ter_post_bool K.
   case_if.
     applys~ red_stat_if_1_true. apply~ RC.
     destruct t2o.
       applys~ red_stat_if_1_false.  apply~ RC.
       run_inv. applys* red_stat_if_1_false_implicit.
  (* Do-while *)
  applys* red_stat_do_while. applys* runs_type_correct_stat_do_while.
  (* While *)
  apply~ red_stat_while. applys* runs_type_correct_stat_while.
  (* With *)
  unfolds in R.
  run_pre. lets (y1&R2&K): if_spec_post_to_object (rm R1) (rm R).
   applys* red_stat_with (rm R2). run_post_if_spec_ter_post_bool K.
  let_name. let_name. destruct p as [lex' S3]. let_name.
  subst lex. applys* red_stat_with_1. subst C'. run_inv. run_hyp*.
  (* Throw *)
  unfolds in R.
  run red_stat_throw. applys* red_stat_throw_1.
  (* Return *)
  unfolds in R. destruct eo.
    run red_stat_return_some. apply* red_stat_return_1.
    inverts* R. applys red_stat_return_none.
  (* Break *)
  run_inv. applys* red_stat_break.
  (* Continue *)
  run_inv. applys* red_stat_continue.
  (* Try *)
  unfolds in R. let_name.
  asserts finally_correct: (forall S (R:res),
      finally S R = res_out o ->
      red_stat S C (stat_try_4 R fo) o).
    subst finally. clear R. introv HR.
    destruct fo.
      simpls. run red_stat_try_4_finally.
       applys* red_stat_try_5_finally_result.
      run_inv. applys* red_stat_try_4_no_finally.
    clear EQfinally.
  run red_stat_try. abort.
    applys* red_stat_try_1_no_throw.
    destruct co as [c|].
      destruct c as [x t2]. let_name. let_name.
       destruct p as [lex' S']. destruct lex'; tryfalse.
       subst lex. run* red_stat_try_1_throw_catch
        using env_record_create_set_mutable_binding_correct.
       run red_stat_try_2_catch.
        applys~ red_stat_try_3_catch_result finally_correct.
      applys~ red_stat_try_1_throw_no_catch. applys~ finally_correct.
      rewrite <- R. fequal. destruct R0; simpls; substs~.
  (* For *)
  apply* run_stat_for_correct.
  (* For-var *)
  apply* run_stat_for_var_correct.
  (* For-in *)
  discriminate.
  (* For-in-var *)
  discriminate.
  (* Debugger *)
  run_inv. apply red_stat_debugger.
  (* switch *)
  applys~ run_stat_switch_correct R.
Admitted. (*faster*)

Lemma run_prog_correct : forall runs S C p o,
  runs_type_correct runs ->
  run_prog runs S C p = o ->
  red_prog S C (prog_basic p) o.
Proof.
  introv RC R. unfolds in R. destruct p.
  forwards*: run_elements_correct (rev l). rew_list* in H.
Admitted. (*faster*)

(* LATER: generalize statement to handle continuations *)
Lemma entering_func_code_correct : forall runs S C lf vthis args o,
  runs_type_correct runs ->
  entering_func_code runs S C lf vthis args = result_some (specret_out o) ->
  red_expr S C (spec_entering_func_code lf vthis args (spec_call_default_1 lf)) o.
Proof.
  introv IH HR. unfolds in HR. sets_eq K: (spec_call_default_1 lf).
  run. run. subst x. lets H: run_object_method_correct (rm E).
  let_name. let_name as M. rename x0 into bd.
  asserts M_correct: (forall S v o,
      M S v = res_out o ->
      red_expr S C (spec_entering_func_code_3 lf args str bd v K) o).
    clears HR S o. introv HR. subst M.
    run. run. subst x. lets H: run_object_method_correct (rm E).
    let_name. destruct p as [lex' S1]. let_name.
    run* (@red_spec_entering_func_code_3 lex' S1 C').
    applys* red_spec_entering_func_code_4.
    subst K. applys* run_call_default_correct.
    clear EQM.
  applys* red_spec_entering_func_code str.
  case_if; subst str.
    applys* red_spec_entering_func_code_1_strict.
    destruct vthis.
      destruct p. (* LATER: improve *)
        applys* red_spec_entering_func_code_1_null_or_undef.
        applys* red_spec_entering_func_code_1_null_or_undef.
        run red_spec_entering_func_code_1_not_object.
          simpls. splits; congruence.
          applys* to_object_correct.
          applys* red_spec_entering_func_code_2.
        run red_spec_entering_func_code_1_not_object.
          simpls. splits; congruence.
          applys* to_object_correct.
          applys* red_spec_entering_func_code_2.
        run red_spec_entering_func_code_1_not_object.
          simpls. splits; congruence.
          applys* to_object_correct.
          applys* red_spec_entering_func_code_2.
      applys* red_spec_entering_func_code_1_object.
Admitted. (* faster *)

Lemma if_spec_throw_result : forall S K, if_spec (@throw_result descriptor (run_error S native_error_type)) K = @throw_result descriptor (run_error S native_error_type).
Proof.
  intros. repeat unfolds.
  remember (run_error S native_error_type) as Error.
  unfolds run_error. unfolds if_object.
  unfolds if_value. unfolds if_success. unfolds if_ter.
  unfolds if_out_some. unfolds if_result_some. unfolds build_error. 
  cases_if*; rewrite decide_def in H; cases_if*; clear H.
  remember (object_alloc S (object_new (prealloc_native_error_proto native_error_type) "Error")) as O.
  destruct O as (l & S'). simpls.
  unfolds if_empty_label.
  cases_if*. subst. simpls. cases_if*.
Qed.

Lemma run_to_descriptor_correct : forall runs S C v y,
  runs_type_correct runs ->
  run_to_descriptor runs S C v = result_some y ->
  red_spec S C (spec_to_descriptor v) y.
Proof.
  introv IH HR. unfold run_to_descriptor in HR.
  destruct v as [p | l]. 

  apply~ red_spec_to_descriptor_not_object.
  applys* throw_result_run_error_correct.

  applys* red_spec_to_descriptor_object.
  run red_spec_to_descriptor_1a using object_has_prop_correct.
  cases_if*; destruct b; inverts H.
  + apply red_spec_to_descriptor_1b_false.
    run red_spec_to_descriptor_2a using object_has_prop_correct.
    cases_if*; destruct b; inverts H.
    - apply red_spec_to_descriptor_2b_false.
      run red_spec_to_descriptor_3a using object_has_prop_correct.
      cases_if*; destruct b; inverts H.
      * apply red_spec_to_descriptor_3b_false.
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
      * run red_spec_to_descriptor_3b_true using run_object_get_correct.
        simpls. applys* red_spec_to_descriptor_3c. 
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
    - run red_spec_to_descriptor_2b_true using run_object_get_correct.
      simpls. applys* red_spec_to_descriptor_2c.
      run red_spec_to_descriptor_3a using object_has_prop_correct.
      cases_if*; destruct b; inverts H.
      * apply red_spec_to_descriptor_3b_false.
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
      * run red_spec_to_descriptor_3b_true using run_object_get_correct.
        simpls. applys* red_spec_to_descriptor_3c. 
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
         }
  + run red_spec_to_descriptor_1b_true using run_object_get_correct.
    simpls. applys* red_spec_to_descriptor_1c.
    run red_spec_to_descriptor_2a using object_has_prop_correct.
    cases_if*; destruct b; inverts H.
    - apply red_spec_to_descriptor_2b_false.
      run red_spec_to_descriptor_3a using object_has_prop_correct.
      cases_if*; destruct b; inverts H.
      * apply red_spec_to_descriptor_3b_false.
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
      * run red_spec_to_descriptor_3b_true using run_object_get_correct.
        simpls. applys* red_spec_to_descriptor_3c. 
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
    - run red_spec_to_descriptor_2b_true using run_object_get_correct.
      simpls. applys* red_spec_to_descriptor_2c.
      run red_spec_to_descriptor_3a using object_has_prop_correct.
      cases_if*; destruct b; inverts H.
      * apply red_spec_to_descriptor_3b_false.
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
        }
      * run red_spec_to_descriptor_3b_true using run_object_get_correct.
        simpls. applys* red_spec_to_descriptor_3c. 
        run red_spec_to_descriptor_4a using object_has_prop_correct.
        { 
          cases_if*; destruct b; inverts H.
          + apply red_spec_to_descriptor_4b_false.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
          + run red_spec_to_descriptor_4b_true using run_object_get_correct.
            simpls. applys* red_spec_to_descriptor_4c.
            run red_spec_to_descriptor_5a using object_has_prop_correct.
            cases_if*; destruct b; inverts H.
            - apply red_spec_to_descriptor_5b_false.
              run red_spec_to_descriptor_6a using object_has_prop_correct.
              cases_if*; destruct b; inverts H.
              * apply red_spec_to_descriptor_6b_false.
                { 
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
              * run red_spec_to_descriptor_6b_true using run_object_get_correct.
                {
                  cases_if*.
                  rewrite if_spec_throw_result in *.
                  applys* red_spec_to_descriptor_6c_error.
                  applys* throw_result_run_error_correct.
                  simpls. applys* red_spec_to_descriptor_6c_ok.
                  cases_if*. 
                  + applys~ red_spec_to_descriptor_7_error.
                    applys* throw_result_run_error_correct.
                  + unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
            - run red_spec_to_descriptor_5b_true using run_object_get_correct.
              cases_if*.
              * rewrite if_spec_throw_result in *.
                applys* red_spec_to_descriptor_5c_error.
                applys* throw_result_run_error_correct.
              * simpls. applys* red_spec_to_descriptor_5c_ok.
                run red_spec_to_descriptor_6a using object_has_prop_correct.
                {
                  cases_if*; destruct b; inverts H.
                  + apply red_spec_to_descriptor_6b_false.
                    cases_if*. 
                    - applys~ red_spec_to_descriptor_7_error.
                      applys* throw_result_run_error_correct.
                    - unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                  + run red_spec_to_descriptor_6b_true using run_object_get_correct.
                    cases_if*.
                    - rewrite if_spec_throw_result in *.
                      applys* red_spec_to_descriptor_6c_error.
                      applys* throw_result_run_error_correct.
                    - simpls. applys* red_spec_to_descriptor_6c_ok.
                      cases_if*. 
                      * applys~ red_spec_to_descriptor_7_error.
                        applys* throw_result_run_error_correct.
                      * unfolds in HR. inverts HR. applys~ red_spec_to_descriptor_7_ok. 
                }
         }
Admitted. (* Faster *)


Lemma run_object_freeze_correct : forall runs S C l xs o,
  runs_type_correct runs ->
  run_object_freeze runs S C l xs = result_some (specret_out o) ->
  red_expr S C (spec_call_object_freeze_2 l xs) o.
Proof.
  introv IH HR. gen S. induction xs; introv HR; unfolds in HR.
   run. apply~ red_spec_call_object_freeze_2_nil.
    apply~ run_object_heap_set_extensible_correct.
   run red_spec_call_object_freeze_2_cons.
    destruct a0 as [|A]; tryfalse.
    applys~ red_spec_call_object_freeze_3.
    run red_spec_call_object_freeze_4.
     clear. rew_refl. destruct A as [()|()]; simpls; repeat cases_if;
       simpls; fold_bool; rew_refl in *; intuit; tryfalse; repeat (fequals); tryfalse*.
     applys~ red_spec_call_object_freeze_5.
Qed.

Lemma run_object_is_sealed_correct : forall runs S C l xs o,
  runs_type_correct runs ->
  run_object_is_sealed runs S C l xs = result_some (specret_out o) ->
  red_expr S C (spec_call_object_is_sealed_2 l xs) o.
Proof.
  introv IH HR. gen S. induction xs; introv HR; unfolds in HR.
   run. apply~ red_spec_call_object_is_sealed_2_nil.
    apply~ run_object_method_correct.
   run red_spec_call_object_is_sealed_2_cons.
    destruct a0 as [|A]; tryfalse. cases_if as CF.
     inverts HR. applys~ red_spec_call_object_is_sealed_3_prop_configurable.
     applys~ red_spec_call_object_is_sealed_3_prop_not_configurable.
Qed.

Lemma run_object_is_frozen_correct : forall runs S C l xs o,
  runs_type_correct runs ->
  run_object_is_frozen runs S C l xs = result_some (specret_out o) ->
  red_expr S C (spec_call_object_is_frozen_2 l xs) o.
Proof.
  introv IH HR. gen S o. induction xs; introv HR; unfolds in HR.
   run. apply~ red_spec_call_object_is_frozen_2_nil.
    apply~ run_object_method_correct.
   run red_spec_call_object_is_frozen_2_cons. let_name.
   asserts CC: (forall A o, check_configurable A = result_some (specret_out o) ->
       red_expr S1 C (spec_call_object_is_frozen_5 l xs A) o).
     rewrite EQcheck_configurable. clear - IHxs. introv HR. cases_if as AC.
      inverts HR. apply~ red_spec_call_object_is_frozen_5_prop_configurable.
      apply~ red_spec_call_object_is_frozen_5_prop_not_configurable.
    clear EQcheck_configurable. destruct a0 as [|[Ad|Aa]].
     discriminate.
     apply red_spec_call_object_is_frozen_3_desc_is_data; simpls~. cases_if as W.
      inverts HR. applys~ red_spec_call_object_is_frozen_4_prop_is_writable.
      apply~ red_spec_call_object_is_frozen_4_prop_is_not_writable.
      apply~ red_spec_call_object_is_frozen_3_desc_is_not_data.
Qed.

Lemma run_object_seal_correct : forall runs S C l xs o,
  runs_type_correct runs ->
  run_object_seal runs S C l xs = result_some (specret_out o) ->
  red_expr S C (spec_call_object_seal_2 l xs) o.
Proof.
  introv IH HR. gen o S. induction xs; introv HR; unfolds in HR.
   run. apply~ red_spec_call_object_seal_2_nil.
    apply~ run_object_heap_set_extensible_correct.
   run red_spec_call_object_seal_2_cons.
    destruct a0 as [|A]; tryfalse.
    run red_spec_call_object_seal_3.
      clear. repeat cases_if~. destruct~ A as [()|()].
    applys~ red_spec_call_object_seal_4.
Qed.

Lemma run_function_proto_apply_get_args_correct : forall runs S C array (index n : int) y,
  runs_type_correct runs ->
  run_get_args_for_apply runs S C array index n = result_some y ->
  red_spec S C (spec_function_proto_apply_get_args array index n) y.
Proof.
  introv IH HR; unfolds run_get_args_for_apply. cases_if*.
  + run~ red_spec_function_apply_get_args_true.
    run red_spec_function_apply_get_args_1 using run_object_get_correct.
    let_name; subst.
    run red_spec_function_apply_get_args_2 using runs_type_correct_get_args_for_apply.
    apply red_spec_function_apply_get_args_3.
  + inverts HR. applys~ red_spec_function_apply_get_args_false.
Qed.

Lemma push_correct : forall S S' C l args a o runs,
  runs_type_correct runs ->
  push runs S' C l args a = result_some (specret_out o) ->
  red_expr S C (spec_call_array_proto_push_3 l args (specret_val S' a)) o.
Proof.
  introv IH HR. 
  apply red_spec_call_array_proto_push_3. 
  gen a o S S' C l runs. inductions args; intros.
  + simpls; let_name; subst. 
    apply red_spec_call_array_proto_push_4_empty.
    run red_spec_call_array_proto_push_5.
    apply red_spec_call_array_proto_push_6.
  + unfold push in HR. unfolds let_binding. (* Why doesn't let_name work here? *)
    apply red_spec_call_array_proto_push_4_nonempty. 
    run red_spec_call_array_proto_push_4_nonempty_1. 
    run red_spec_call_array_proto_push_4_nonempty_2. 
    apply red_spec_call_array_proto_push_4_nonempty_3. 
    applys* IHargs.
Qed.

Lemma vtsfj_correct : forall runs S C l index sR,
  runs_type_correct runs ->
  valueToStringForJoin runs S C l index = result_some sR ->
  red_spec S C (spec_call_array_proto_join_vtsfj l index) sR.
Proof.
  introv IH HR. unfolds in HR.
  run red_spec_call_array_proto_join_vtsfj. 
  run red_spec_call_array_proto_join_vtsfj_1 using run_object_get_correct.
  destruct v as [p | loc].
  + destruct p; try solve [inverts HR; applys* red_spec_call_array_proto_join_vtsfj_2_undef_null];
    run* red_spec_call_array_proto_join_vtsfj_2_other; try solve [intuition; inverts H0]; 
    applys* red_spec_call_array_proto_join_vtsfj_3.
  + run* red_spec_call_array_proto_join_vtsfj_2_other; try solve [intuition; inverts H0]; 
    applys* red_spec_call_array_proto_join_vtsfj_3.
Qed.

Lemma run_array_join_elements_correct : forall runs S C l k length sep sR o,
  runs_type_correct runs -> 
  run_array_join_elements runs S C l k length sep sR = o ->
  red_expr S C (spec_call_array_proto_join_elements l k length sep sR) o.
Proof.
  introv IH HR. unfolds in HR. cases_if*.
  + repeat let_name; subst.
    applys* red_spec_call_array_proto_join_elements_continue.
    run* red_spec_call_array_proto_join_elements_1 using vtsfj_correct. 
    let_name; subst.
    apply red_spec_call_array_proto_join_elements_2.
    applys* runs_type_correct_array_join_elements.
  + inverts HR. applys* red_spec_call_array_proto_join_elements_exit.
Qed.

Lemma run_call_prealloc_correct : forall runs S C B vthis args o,
  runs_type_correct runs ->
  run_call_prealloc runs S C B vthis args = o ->
  red_expr S C (spec_call_prealloc B vthis args) o.
Proof.
  introv IH HR. unfolds in HR.
  destruct B.
  (* prealloc_global *)
  discriminate.
  (* prealloc_global_eval *)
  discriminate.
  (* prealloc_global_parse_int *)
  discriminate.
  (* prealloc_global_parse_float *)
  discriminate.
  (* prealloc_global_is_finite *)
  let_name. run red_spec_call_global_is_finite.
    substs. apply~ get_arg_correct_0.
  applys red_spec_call_global_is_finite_1.
  cases_if; fold_bool; rew_refl~.
  (* prealloc_global_is_nan *)
  let_name. run red_spec_call_global_is_nan.
    substs. apply~ get_arg_correct_0.
  applys red_spec_call_global_is_nan_1.
  cases_if; fold_bool; rew_refl~.
  (* prealloc_global_decode_uri *)
  discriminate.
  (* prealloc_global_decode_uri_component *)
  discriminate.
  (* prealloc_global_encode_uri *)
  discriminate.
  (* prealloc_global_encode_uri_component *)
  discriminate.
  (* prealloc_object *)
  let_name. subst.
  applys* red_spec_call_object_call.
    applys* get_arg_correct_0.
  destruct (get_arg 0 args) as [p | l].
  destruct p.
  applys* red_spec_call_object_call_1_null_or_undef.
  apply run_construct_prealloc_correct in HR; auto.  
  applys* red_spec_call_object_call_1_null_or_undef.
  apply run_construct_prealloc_correct in HR; auto.  
  applys* red_spec_call_object_call_1_other.
  splits; discriminate.
  applys~ to_object_correct.
  applys* red_spec_call_object_call_1_other.
  splits; discriminate.
  applys~ to_object_correct.
  applys* red_spec_call_object_call_1_other.
  splits; discriminate.
  applys~ to_object_correct.
  applys* red_spec_call_object_call_1_other.
  splits; discriminate.
  applys~ to_object_correct.  
  (* prealloc_object_get_proto_of *)
  let_name. apply~ red_spec_call_object_get_proto_of.
    substs. apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_get_proto_of_1_not_object.
    apply* run_error_correct.
   run. apply~ red_spec_call_object_get_proto_of_1_object.
    apply* run_object_method_correct.
  (* prealloc_object_get_own_prop_descriptor *)
  let_name. apply~ red_spec_call_object_get_own_prop_descriptor.
    apply* get_arg_correct_1.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_get_own_prop_descriptor_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run red_spec_call_object_get_own_prop_descriptor_1_object.
    run red_spec_call_object_get_own_prop_descriptor_2.
    apply* from_prop_descriptor_correct.
  (* prealloc_object_get_own_prop_name *)
  discriminate.
  (* prealloc_object_create *)
  discriminate.
  (* prealloc_object_define_prop *)
  let_name. let_name. let_name.
  apply~ red_spec_call_object_object_define_prop.
    apply* get_arg_correct_2.
  rewrite <- EQo0 in *. rewrite <- EQp in *. rewrite <- EQattr in *.
  destruct o0.
   apply red_spec_call_object_object_define_prop_1_not_object.
     destruct p0; discriminate.
    apply* run_error_correct.
   run red_spec_call_object_object_define_prop_1_object.
    run red_spec_call_object_object_define_prop_2.
      apply* run_to_descriptor_correct.
    run red_spec_call_object_object_define_prop_3.
    apply* red_spec_call_object_object_define_prop_4.
  (* prealloc_object_define_props *)
  discriminate.
  (* prealloc_object_seal *)
  let_name. apply~ red_spec_call_object_seal.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_seal_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. forwards~ B: @pick_option_correct E.
    applys~ red_spec_call_object_seal_1_object B.
    applys~ run_object_seal_correct HR.
  (* prealloc_object_freeze *)
  let_name. apply~ red_spec_call_object_freeze.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_freeze_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. forwards~ B: @pick_option_correct E.
    applys~ red_spec_call_object_freeze_1_object B.
    applys~ run_object_freeze_correct HR.
  (* prealloc_object_prevent_extensions *)
  let_name. apply~ red_spec_call_object_prevent_extensions.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_prevent_extensions_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. forwards~ B: @pick_option_correct E.
    applys~ red_spec_call_object_prevent_extensions_object B.
  (* prealloc_object_is_sealed *)
  let_name. apply~ red_spec_call_object_is_sealed.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_is_sealed_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. forwards~ B: @pick_option_correct E.
    applys~ red_spec_call_object_is_sealed_1_object B.
    applys~ run_object_is_sealed_correct HR.
  (* prealloc_object_is_frozen *)
  let_name. apply~ red_spec_call_object_is_frozen.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_is_frozen_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. forwards~ B: @pick_option_correct E.
    applys~ red_spec_call_object_is_frozen_1_object B.
    applys~ run_object_is_frozen_correct HR.
  (* prealloc_object_is_extensible *)
  let_name. apply~ red_spec_call_object_is_extensible.
    apply* get_arg_correct_0.
  rewrite <- EQv in *. destruct v.
   apply red_spec_call_object_is_extensible_1_not_object.
     destruct p; discriminate.
    apply* run_error_correct.
   run. apply~ red_spec_call_object_is_extensible_1_object.
    apply~ run_object_method_correct.
  (* prealloc_object_keys *)
  discriminate.
  (* prealloc_object_keys_call *)
  discriminate.
  (* prealloc_object_proto *)
  discriminate.
  (* prealloc_object_proto_to_string *)
  apply red_spec_call_object_proto_to_string.
  destruct vthis as [p | l]; [destruct p | ].
  inverts HR. apply red_spec_call_object_proto_to_string_1_undef.
  inverts HR. apply red_spec_call_object_proto_to_string_1_null.
  run red_spec_call_object_proto_to_string_1_other using to_object_correct. rew_logic; splits; discriminate.
  run. apply run_object_method_correct in E.
  applys* red_spec_call_object_proto_to_string_2.
  run red_spec_call_object_proto_to_string_1_other using to_object_correct. rew_logic; splits; discriminate.
  run. apply run_object_method_correct in E.
  applys* red_spec_call_object_proto_to_string_2.
  run red_spec_call_object_proto_to_string_1_other using to_object_correct. rew_logic; splits; discriminate.
  run. apply run_object_method_correct in E.
  applys* red_spec_call_object_proto_to_string_2.
  run red_spec_call_object_proto_to_string_1_other using to_object_correct. rew_logic. splits; discriminate.
  run. apply run_object_method_correct in E.
  applys* red_spec_call_object_proto_to_string_2.
  (* prealloc_object_proto_value_of *)
  apply~ red_spec_call_object_proto_value_of.
  apply~ to_object_correct.
  (* prealloc_object_proto_has_own_prop *)
  let_name. run red_spec_call_object_proto_has_own_prop.
    substs. apply~ get_arg_correct_0.
  run red_spec_call_object_proto_has_own_prop_1 using to_object_correct.
  run red_spec_call_object_proto_has_own_prop_2.
  destruct a. (* LTAC ARTHUR *)
   inverts HR. apply~ red_spec_call_object_proto_has_own_prop_3_undef.
   inverts HR. apply~ red_spec_call_object_proto_has_own_prop_3_not_undef.
  (* prealloc_object_proto_is_prototype_of *)
  let_name. destruct v as [p | l].
  inverts HR. applys* red_spec_call_object_proto_is_prototype_of_not_object.
  applys* get_arg_correct_0.
  applys* red_spec_call_object_proto_is_prototype_of_1_not_object. 
  rewrite~ <- EQv. 
  applys* red_spec_call_object_proto_is_prototype_of_not_object. 
  apply get_arg_correct_0.
  rewrite <- EQv. run red_spec_call_object_proto_is_prototype_of_1_object using to_object_correct.
  apply red_spec_call_object_proto_is_prototype_of_2.
  applys* runs_type_correct_object_proto_is_prototype_of.
  (* prealloc_object_proto_prop_is_enumerable *)
  let_name. 
  applys* red_spec_call_object_proto_prop_is_enumerable.
  apply get_arg_correct_0. subst.
  run red_spec_call_object_proto_prop_is_enumerable_1.
  run red_spec_call_object_proto_prop_is_enumerable_2 using to_object_correct.
  run red_spec_call_object_proto_prop_is_enumerable_3.
  destruct a; inverts HR.
  apply red_spec_call_object_proto_prop_is_enumerable_4_undef.
  applys* red_spec_call_object_proto_prop_is_enumerable_4_not_undef.
  (* prealloc_function *)
  discriminate. (* LATER *)
  (* prealloc_function_proto *)
  inverts HR. apply red_spec_call_function_proto_invoked.

  (* prealloc_function_proto_to_string *)
  cases_if*. applys* red_spec_function_proto_to_string_not_callable.

  (* prealloc_function_proto_apply *)
  repeat let_name.
  cases_if*; [ | applys* red_spec_function_apply_1].
  destruct vthis as [p | func]; [inverts i; inverts H | ]. 
  applys* red_spec_function_apply_1_2; [apply get_arg_correct_1 | substs].
  destruct (get_arg 1 args) as [p | array].
  destruct p;
    try solve [apply runs_type_correct_call in HR; auto;
               applys* red_spec_function_apply_2];
    try solve [apply red_spec_function_apply_3 with (array := func); [splits; discriminate | applys* run_error_correct]].
  run~ red_spec_function_apply_4 using run_object_get_correct.
  run red_spec_function_apply_5.
  run red_spec_function_apply_6 using run_function_proto_apply_get_args_correct. 
  apply red_spec_function_apply_7. applys* runs_type_correct_call.

  (* prealloc_function_proto_call *)
  cases_if*; [ | applys* red_spec_call_function_not_callable].
  destruct vthis as [p | l]; [inverts i; inverts H |].
  remember (get_arg_first_and_rest args) as gargs; destruct gargs.
  applys* red_spec_call_function_callable.
  rewrite* <- get_arg_first_and_rest_correct.
  applys* runs_type_correct_call.

  (* prealloc_function_proto_bind *)
  cases_if*; [ | applys* red_spec_function_bind_1].
  destruct vthis as [p | this]; [inverts HR | ].
  remember (get_arg_first_and_rest args) as gargs; destruct gargs as (thisArg & A).
  applys* red_spec_function_bind_2.
  rewrite* <- get_arg_first_and_rest_correct.
  repeat let_simpl; 
  match goal with H: context [object_alloc ?s ?o] |- _ => sets_eq X: (object_alloc s o) end;
  destruct X as (l & S').  
  applys* red_spec_function_bind_3.
  let_name. subst. run red_spec_function_bind_4.
  run. cases_if*; subst; apply run_object_method_correct in E.
  applys* red_spec_function_bind_length_true. 
  run red_spec_function_bind_length_1 using run_object_get_correct.
  run red_spec_function_bind_length_2. cases_if*; inverts R1.
  applys* red_spec_function_bind_length_3_zero.
  applys* red_spec_function_bind_length_3_L.
  inverts R1. applys* red_spec_function_bind_length_false.
  repeat let_name. 
  run; rename x into S''. run. repeat let_name.
  forwards B: @pick_option_correct (rm E0).
  applys* red_spec_function_bind_5.
  applys* red_spec_function_bind_6. rewrite* <- EQA0. substs.
  run* red_spec_function_bind_7. 
  run* red_spec_function_bind_8. 
  apply red_spec_function_bind_9.

  (* prealloc_bool *)
  inverts HR. apply~ red_spec_call_bool.
    apply~ get_arg_correct_0.
  apply~ red_spec_to_boolean.
  (* prealloc_bool_proto *)
  discriminate.
  (* prealloc_bool_proto_to_string *)  
  destruct vthis as [p | l].
    destruct p; try solve [
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]]. 
    inverts HR. applys* red_spec_call_bool_proto_to_string_bool. constructor.
    remember (run_object_method object_class_ S l). destruct o0.
    simpls. cases_if*. 
    remember (run_object_method object_prim_value_ S l). destruct o0.
    simpls. destruct o0. destruct v. 
    destruct p.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      inverts HR. subst. 
      symmetry in Heqo1, Heqo0. 
      apply run_object_method_correct in Heqo0. 
      apply run_object_method_correct in Heqo1. 
      applys* red_spec_call_bool_proto_to_string_bool.
      constructor*.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      simpls.  apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H3 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        eexists; jauto.
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo1.
      simpls. inverts Heqo1.
      apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. assert (a = O).
      {
        apply pick_option_correct in Heq.
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. rewrite Hv in Heqo0. false*.
      simpls. apply red_spec_call_bool_proto_to_string_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. inverts Heqo0. 
  (* prealloc_bool_proto_value_of *)
  destruct vthis as [p | l].
    destruct p; try solve [
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]]. 
    inverts HR. apply red_spec_call_bool_proto_value_of_bool. constructor.
    remember (run_object_method object_class_ S l). destruct o0.
    simpls. cases_if*. 
    remember (run_object_method object_prim_value_ S l). destruct o0.
    simpls. destruct o0. destruct v. 
    destruct p.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      inverts HR. subst. apply red_spec_call_bool_proto_value_of_bool.
      symmetry in Heqo1, Heqo0. 
      apply run_object_method_correct in Heqo0. 
      apply run_object_method_correct in Heqo1. 
      constructor*.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      simpls.  apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H3 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        eexists; jauto.
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo1.
      simpls. inverts Heqo1.
      apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. assert (a = O).
      {
        apply pick_option_correct in Heq.
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. rewrite Hv in Heqo0. false*.
      simpls. apply red_spec_call_bool_proto_value_of_not_bool;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. inverts Heqo0. 
  (* prealloc_number *)
  cases_if.
  substs. inverts HR. apply~ red_spec_call_number_nil.
  inverts HR. apply~ red_spec_call_number_not_nil.
    apply~ get_arg_correct_0.
  apply* to_number_correct.
  (* prealloc_number_proto *)
  discriminate.
  (* prealloc_number_proto_to_string *)
  discriminate.
  (* prealloc_number_proto_value_of *)
  destruct vthis as [p | l].
    destruct p; try solve [
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]]. 
    inverts HR. apply red_spec_call_number_proto_value_of_number. constructor.
    remember (run_object_method object_class_ S l). destruct o0.
    simpls. cases_if*. 
    remember (run_object_method object_prim_value_ S l). destruct o0.
    simpls. destruct o0. destruct v. 
    destruct p.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      inverts HR. subst. apply red_spec_call_number_proto_value_of_number.
      symmetry in Heqo1, Heqo0. 
      apply run_object_method_correct in Heqo0. 
      apply run_object_method_correct in Heqo1. 
      constructor*.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      symmetry in Heqo1. apply run_object_method_correct in Heqo1.
      destruct Heqo1 as (O1 & Hb1 & Hv1).
      destruct H3 as (O2 & Hb2 & Hv2).
      assert (O1 = O2).
      {
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. false~.
      simpls.  apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H3 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        eexists; jauto.
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo1.
      simpls. inverts Heqo1.
      apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. assert (a = O).
      {
        apply pick_option_correct in Heq.
        applys* Heap_binds_func.
        apply object_loc_comparable. 
      } subst. rewrite Hv in Heqo0. false*.
      simpls. apply red_spec_call_number_proto_value_of_not_number;
      [introv Hv; inverts Hv | applys* run_error_correct]. 
      destruct H0 as (O & Hb & Hv).
      unfolds run_object_method.
      assert (exists a, object_binds S l a).
        exists~ O. 
      lets Hyp : (@pick_option_defined _ (object_binds S l) (object_binds_pickable_option S l) H). 
      destruct Hyp as (a & Heq). 
      rewrite Heq in Heqo0.
      simpls. inverts Heqo0. 
  (* prealloc_number_proto_to_fixed *)
  discriminate.
  (* prealloc_number_proto_to_exponential *)
  discriminate.
  (* prealloc_number_proto_to_precision *)
  discriminate.

  (* prealloc_array *)
  apply run_construct_prealloc_correct in HR; auto.
  applys* red_spec_call_to_construct_array. auto.

  (* prealloc_array_is_array *)
  let_name; subst. 
  applys* red_spec_call_array_is_array_fetch_arg.
    applys* get_arg_correct_0.
  destruct (get_arg 0 args). 
    inverts HR. applys* red_spec_call_array_is_array_1. 
    run. apply run_object_method_correct in E. 
    applys* red_spec_call_array_is_array_2_branch.
    cases_if*; inverts HR.
      applys* red_spec_call_array_is_array_2.
      applys* red_spec_call_array_is_array_3.

  (* prealloc_array_proto *)
  discriminate.

  (* prealloc_array_proto_to_string *)
  run red_spec_call_array_proto_to_string using to_object_correct.
  run red_spec_call_array_proto_to_string_1 using run_object_get_correct.
  cases_if*. 
  destruct v as [p | array]; inverts HR.
  applys* red_spec_call_array_proto_to_string_2_true.
  applys* runs_type_correct_call.
  applys* red_spec_call_array_proto_to_string_2_false.
  applys* runs_type_correct_call_prealloc.
  
  (* prealloc_array_proto_join *)
  let_name; subst.
  run red_spec_call_array_proto_join using to_object_correct.
  run red_spec_call_array_proto_join_1 using run_object_get_correct.
  run red_spec_call_array_proto_join_2. let_name; subst.
  applys* red_spec_call_array_proto_join_3. 
  apply get_arg_correct_0. cases_if*.
  run~ red_spec_call_array_proto_join_3_other.
  cases_if*. inverts HR. applys* red_spec_call_array_proto_join_4.
  let_name; subst.
  run~ red_spec_call_array_proto_join_5 using vtsfj_correct. 
  apply red_spec_call_array_proto_join_6. applys* run_array_join_elements_correct.
  run~ red_spec_call_array_proto_join_3_undef.
  cases_if*. inverts HR. applys* red_spec_call_array_proto_join_4.
  let_name; subst.
  run~ red_spec_call_array_proto_join_5 using vtsfj_correct. 
  apply red_spec_call_array_proto_join_6. applys* run_array_join_elements_correct.

  (* prealloc_array_proto_pop *)
  run red_spec_call_array_proto_pop using to_object_correct. 
  run red_spec_call_array_proto_pop_1 using run_object_get_correct.
  run red_spec_call_array_proto_pop_2. cases_if*. subst.
  apply red_spec_call_array_proto_pop_3_empty.
  run red_spec_call_array_proto_pop_3_empty_1.
  apply red_spec_call_array_proto_pop_3_empty_2.
  applys~ red_spec_call_array_proto_pop_3_nonempty.
  run red_spec_call_array_proto_pop_3_nonempty_1.
  run red_spec_call_array_proto_pop_3_nonempty_2 using run_object_get_correct.
  run red_spec_call_array_proto_pop_3_nonempty_3 using object_delete_default_correct. 
  run red_spec_call_array_proto_pop_3_nonempty_4.
  applys~ red_spec_call_array_proto_pop_3_nonempty_5.
  
  (* prealloc_array_proto_push *)
  run red_spec_call_array_proto_push using to_object_correct. 
  run red_spec_call_array_proto_push_1 using run_object_get_correct.
  run red_spec_call_array_proto_push_2.
  applys* push_correct.

  (* prealloc_string *)

  cases_if; substs.
    inverts HR. apply red_spec_call_string_empty.
    let_name; substs. run red_spec_call_string_non_empty.
      apply get_arg_correct_0.
    apply red_spec_call_string_non_empty_1.

  (* prealloc_string_proto *)
  discriminate. (* LATER *)
  (* prealloc_string_proto_to_string *)
  applys* red_spec_call_string_proto_to_string.
  destruct vthis as [p | l]. cases_if*. 
  inverts HR. applys* red_spec_call_string_proto_value_of_prim_string.
  applys* red_spec_call_string_proto_value_of_bad_type.
  run. apply run_object_method_correct in E.
  cases_if*. subst.
  apply run_object_prim_value_correct in HR.
  destruct HR as (v & Heq & Hprim). subst.
  applys* red_spec_call_string_proto_value_of_obj_string.
  applys* red_spec_call_string_proto_value_of_obj_other.
  destruct E as (y & Hbind & Hclass).
  introv (y' & Hbind' & Hclass').
  assert (y = y'). applys* Heap_binds_func.
  apply object_loc_comparable. subst. false~.
   (* prealloc_string_proto_value_of *)
  destruct vthis as [p | l]. cases_if*. inverts HR. 
  applys* red_spec_call_string_proto_value_of_prim_string.
  applys* red_spec_call_string_proto_value_of_bad_type.
run. apply run_object_method_correct in E.
  cases_if*. subst.
  apply run_object_prim_value_correct in HR.
  destruct HR as (v & Heq & Hprim). subst.
  applys* red_spec_call_string_proto_value_of_obj_string.
  applys* red_spec_call_string_proto_value_of_obj_other.
  destruct E as (y & Hbind & Hclass).
  introv (y' & Hbind' & Hclass').
  assert (y = y'). applys* Heap_binds_func.
  apply object_loc_comparable. subst. false~.
  (* prealloc_string_proto_char_at *)
  discriminate.
  (* prealloc_string_proto_char_code_at *)
  discriminate.
  (* prealloc_math *)
  discriminate.
  (* prealloc_mathop *)
  discriminate.
  (* prealloc_date *)
  discriminate.
  (* prealloc_regexp *)
  discriminate.     
  (* prealloc_error *)
  let_name. apply~ red_spec_call_error.
    apply~ get_arg_correct_0.
  substs. apply* build_error_correct.
  (* prealloc_error_proto *)
  discriminate.
  (* prealloc_native_error *)
  let_name. applys* red_spec_call_native_error.
    apply~ get_arg_correct_0.
  substs; applys* build_error_correct.
  (* prealloc_native_error_proto *)
  discriminate.
  (* prealloc_error_proto_to_string *)
  discriminate.
  (* prealloc_throw_type_error *)
  apply~ red_spec_call_throw_type_error.
  apply* run_error_correct.
  (* prealloc_json *)
  discriminate.
Admitted. (* faster *)

Lemma run_call_correct : forall runs S C l v vs o,
  runs_type_correct runs ->
  run_call runs S C l v vs = o ->
  red_expr S C (spec_call l v vs) o.
Proof.
  introv IH HR. simpls. unfolds in HR.
  run. run. subst. lets H: run_object_method_correct (rm E).
  applys* red_spec_call. clear H.
  destruct x0.
    applys* red_spec_call_1_default. applys* red_spec_call_default.
    applys* entering_func_code_correct.

    repeat run. let_name. subst.
    apply run_object_method_correct in E; apply run_object_method_correct in E1; 
    apply run_object_method_correct in E3.
    applys* red_spec_call_1_after_bind_full.
    applys* runs_type_correct_call.

    applys* red_spec_call_1_prealloc. applys* run_call_prealloc_correct.
Admitted. (* faster *)


Lemma run_stat_while_correct : forall runs S C rv ls e t o,
  runs_type_correct runs ->
  run_stat_while runs S C rv ls e t = o ->
  red_stat S C (stat_while_1 ls e t rv) o.
Proof.
  intros runs IH ls e t S C rv o R. unfolds in R.
  run_pre. lets (y1&R2&K): if_spec_post_to_bool (rm R1) (rm R).
   applys~ red_stat_while_1 (rm R2). run_post_if_spec_ter_post_bool K.
    case_if.
    run red_stat_while_2_true.
     let_name. let_simpl. applys red_stat_while_3 rv'. case_if; case_if*.
     case_if in K.
       applys red_stat_while_4_not_continue. rew_logic*. case_if in K.
         run_inv. applys* red_stat_while_5_break.
         applys* red_stat_while_5_not_break. case_if in K; run_inv.
           applys* red_stat_while_6_abort.
           applys* red_stat_while_6_normal. run_hyp*.
       rew_logic in *. applys* red_stat_while_4_continue. run_hyp*.
   run_inv. applys red_stat_while_2_false.
Admitted. (*faster*)

Lemma run_stat_do_while_correct : forall runs S C rv ls e t o,
  runs_type_correct runs ->
  run_stat_do_while runs S C rv ls e t = o ->
  red_stat S C (stat_do_while_1 ls t e rv) o.
Proof.
  introv IH R. unfolds in R.
  run red_stat_do_while_1. do 2 let_name.
  applys~ red_stat_do_while_2 rv'.
    repeat cases_if~. clear EQrv'.
  asserts loop_correct: (forall o, loop tt = res_out o ->
      red_stat S0 C (stat_do_while_6 ls t e rv') o).
    clear R. introv H. subst loop.
     run_pre. lets (y1&R2&K): if_spec_post_to_bool (rm R1) (rm H).
     applys~ red_stat_do_while_6 (rm R2). run_post_if_spec_ter_post_bool K.
     cases_if.
      apply~ red_stat_do_while_7_true. apply* IH.
      run_inv. apply* red_stat_do_while_7_false.
  clear EQloop. cases_if in R.
   apply~ red_stat_do_while_3_continue. rewrite decide_def in H. cases_if~ in H.
   apply~ red_stat_do_while_3_not_continue.
     rewrite decide_def in H. cases_if~ in H. clear H. cases_if.
    run_inv. apply~ red_stat_do_while_4_break.
    apply~ red_stat_do_while_4_not_break. cases_if; run_inv.
     apply~ red_stat_do_while_5_abort.
     apply~ red_stat_do_while_5_normal.
Admitted. (*faster*)

Lemma run_stat_for_loop_correct : forall runs S C labs rv eo2 eo3 t o,
  runs_type_correct runs ->
  run_stat_for_loop runs S C labs rv eo2 eo3 t = o ->
  red_stat S C (stat_for_2 labs rv eo2 eo3 t) o.
Proof.
  introv IH R. unfolds in R. let_name.
  asserts follows_correct: (forall S o, follows S = res_out o ->
    red_stat S C (stat_for_4 labs rv eo2 eo3 t) o).
    clear R. introv R. rewrite EQfollows in R. clear EQfollows.
    run red_stat_for_4. do 2 let_name. applys~ red_stat_for_5 rv'.
     repeat cases_if*.
    clear EQrv'. cases_if.
     run_inv. apply~ red_stat_for_6_break.
     apply~ red_stat_for_6_not_break. rew_logic~ in *. cases_if.
      apply red_stat_for_7_continue. rew_logic~ in *. destruct eo3.
       run red_stat_for_8_some. subst loop. run_hyp.
        apply~ red_stat_for_9.
       subst loop. run_hyp. apply~ red_stat_for_8_none.
      run_inv. apply~ red_stat_for_7_abort. rew_logic* in *.
  clear EQfollows. destruct eo2.
   run_pre. lets (y1&R2&K): if_spec_post_to_bool (rm R1) (rm R).
    applys~ red_stat_for_2_some (rm R2). run_post_if_spec_ter_post_bool K.
    cases_if; run_inv.
     apply~ red_stat_for_3_not_false. discriminate.
     apply~ red_stat_for_3_false.
   apply~ red_stat_for_2_none.
Admitted. (*faster*)


Lemma object_proto_is_prototype_of_correct : forall runs S C lthis l o,
  runs_type_correct runs ->
  object_proto_is_prototype_of runs S lthis l = o ->
  red_expr S C (spec_call_object_proto_is_prototype_of_2_3 lthis l) o.
Proof.
  introv IH HR. unfolds in HR.
  run. forwards* Omp: run_object_method_correct.
  applys~ red_spec_call_object_proto_is_prototype_of_3 Omp.
  destruct x as [p|]. (* LTAC ARTHUR *)
   destruct p; inverts HR.
    apply~ red_spec_call_object_proto_is_prototype_of_4_null.
   cases_if; substs; inverts HR.
    apply~ red_spec_call_object_proto_is_prototype_of_4_equal.
    run_hyp. apply* red_spec_call_object_proto_is_prototype_of_4_not_equal.
Admitted. (*faster*)


Lemma run_equal_correct : forall runs S C v1 v2 o,
  runs_type_correct runs ->
  run_equal runs S C v1 v2 = o ->
  red_expr S C (spec_equal v1 v2) o.
Proof.
  introv IH R. unfolds in R. let_simpl.
  apply~ red_spec_equal. cases_if.
   run_inv. rewrite e. apply~ red_spec_equal_1_same_type.
   apply~ red_spec_equal_1_diff_type. let_name.
   asserts dc_conv_correct: (forall v1 F Ext v2 o,
     dc_conv v1 F v2 = res_out o ->
     (forall S v o, F S v = o -> red_expr S C (Ext v) o) ->
     red_expr S C (spec_equal_3 v1 Ext v2) o).
     clear R. introv E Cor. substs. run red_spec_equal_3_convert_and_recurse.
       run_inv. apply* Cor.
     run_hyp. apply~ red_spec_equal_4_recurse.
   clear EQdc_conv.
  Ltac eqcas R :=
     match type of R with context [ ifb ?P then _ else _ ] =>
       let x := fresh "x" in set (x := P) in * end;
     case_if in R as C; [ rewrite If_l; try assumption
                        | rewrite If_r; try assumption ].
   eqcas R. run_inv. applys red_spec_equal_2_return.
   eqcas R. run_inv. applys red_spec_equal_2_return.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_number_correct E.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_number_correct E.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_number_correct E.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_number_correct E.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_primitive_correct E.
   eqcas R. applys dc_conv_correct R. introv E. applys* to_primitive_correct E.
   run_inv. applys red_spec_equal_2_return.
Admitted. (* faster *)

Theorem runs_correct : forall num,
  runs_type_correct (runs num).
Proof.
  induction num.
   constructors;
     try (introv M; inverts M; introv P; inverts P). 
     introv Hyp M; inverts M. 
   constructors.
     introv. apply~ run_expr_correct.
     introv. apply~ run_stat_correct.
     introv. apply~ run_prog_correct.
     introv. apply~ run_call_correct.
     introv. apply~ run_call_prealloc_correct.
     introv. apply~ run_construct_correct.
     introv. apply~ run_function_has_instance_correct.
     introv. apply~ run_function_proto_apply_get_args_correct.
     introv. apply~ run_object_has_instance_correct.
     introv. apply~ run_stat_while_correct.
     introv. apply~ run_stat_do_while_correct.
     introv. apply~ run_stat_for_loop_correct.
     introv. apply~ object_delete_correct.
     introv. apply~ run_object_get_own_prop_correct.
     introv. apply~ run_object_get_prop_correct.
     introv. apply~ run_object_get_correct.
     introv. apply~ object_proto_is_prototype_of_correct.
     introv. apply~ object_put_correct.
     introv. apply~ run_equal_correct.
     introv. apply~ to_integer_correct.
     introv. apply~ to_string_correct.
     introv. apply~ run_array_element_list_correct.
     introv Hyp. apply~ run_object_define_own_prop_array_loop_correct.
     introv. apply~ run_array_join_elements_correct.
Qed.

Theorem run_javascript_correct : forall runs p o,
  runs_type_correct runs ->
  run_javascript runs p = o ->
  red_javascript p o.
Proof.
  introv IH HR. unfolds in HR. run_pre as o1 R1.
  applys* red_javascript_intro R1. run_post. run_inv. run_hyp.
  apply~ red_javascript_intro_1.
Qed.

Corollary run_javascript_correct_num : forall num p o,
  run_javascript (runs num) p = result_out o ->
  red_javascript p o.
Proof.
  introv IH. applys~ run_javascript_correct IH.
  apply~ runs_correct.
Qed. 


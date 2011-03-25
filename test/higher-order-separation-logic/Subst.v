Require Export Expr.

Set Implicit Arguments.
Unset Strict Implicit.

Program Definition sm_subst_expr {A} (g: sm A) (e: expr) x : sm A :=
  Build_sm
    (fun s => g (SM.add x (e s) s))
    (\y, (free g x /\ free e y) \/ (x <> y /\ free g y))
    _.
Next Obligation.
  introv H.
  apply sm_prop.
  intros v HF.
  destruct (string_dec x v); [subst v |].
  - do 2 rewrite stack_lookup_add; apply sm_prop; intros y HFy.
    apply H; left; split; assumption.
  do 2 (rewrite stack_lookup_add2; [| assumption]).
  apply H; right; split; assumption.
Qed.

Program Definition sm_subst_exprs {A} (g: sm A) (sub: list (var*expr)) :=
  Build_sm (fun s => g (sub :@: s +:+ s)) 
  (\y, (exists v e, In (v, e) sub /\ free g v /\ free e y)
    \/ (~ In y (map fst sub) /\ free g y)) _.
Next Obligation.
  introv Hagree; apply sm_prop; intros x HF.
  assert (Hag : agree_on (\y, (exists v e, In (v, e) sub /\ free g v /\ free e y)
    \/ ((forall rst, In y (rst ++ map fst sub) -> In x rst) /\ free g y)) s s').
  - intros y Hy; apply Hagree; destruct Hy as [HL | [HR HFy]]; [left; assumption |].
    right; split; [| assumption].
    specialize (HR []); simpl in HR; assumption.
  clear Hagree; rename Hag into Hagree.
  induction sub; simpl in *.
  - apply Hagree; simpl.
    right; split; [| assumption]; intro rst; rewrite app_nil_r; tauto.
  destruct a as [v e]; destruct (string_dec v x); [subst v |].
  - do 2 rewrite stack_lookup_add; simpl; apply sm_prop; intros y HFy.
    apply Hagree; left; exists x e; intuition.
  simpl; do 2 (rewrite stack_lookup_add2; [| assumption]).
  apply IHsub; intros y HFy.
  apply Hagree.
  destruct HFy as [[v' [e' [HIn' [HFg HFe']]]] | [HNIn HFg]].
  - left; exists v' e'; intuition.
  simpl; right.
  split; [| assumption].
  intros rst HIn; specialize (HNIn (rst ++ [v])).
  rewrite <- app_assoc in HNIn; apply HNIn in HIn; apply in_app_or in HIn.
  destruct HIn as [HA | [HEq | HC]]; [assumption | congruence | contradiction].
Qed.

(* Binds tighter than function application *)
(* We have the e at precedence level one less than the precedence of the infix
   // operator to make parsing work. *)
Notation "g [ e // x ]" := (sm_subst_exprs g [(x, e)]) (at level 9, e at level 39,
  format "g [ e // x ]").

Notation " g '//' es " := (sm_subst_exprs g es) (at level 40).

Lemma subst_un_pointwise : forall {A B} es g (op: A -> B),
  sm_eq ((sm_un op g) // es) (sm_un op (g // es)).
Proof. intros. intro. reflexivity. Qed.

Lemma subst_bin_pointwise : forall {A B C} es g1 g2 (op: A -> B -> C),
  sm_eq ((sm_bin op g1 g2) // es) (sm_bin op (g1 // es) (g2 // es)).
Proof.
  unfold sm_eq; simpl; reflexivity.
Qed.

Lemma subst_tern_pointwise {A B C D} es g1 g2 g3 (op: A -> B -> C -> D) :
  sm_eq ((sm_tern op g1 g2 g3) // es) (sm_tern op (g1 // es) (g2 // es) (g3 // es)).
Proof.
  unfold sm_eq; simpl; reflexivity.
Qed.

Lemma subst_bin_pointwise_free : forall {A B C} (e: expr) g1 g2 x
  (op: A -> B -> C) y,
  free (sm_bin op g1 g2)[e//x] y <->
  free (sm_bin op g1[e//x] g2[e//x]) y.
Proof. firstorder. Qed.

Lemma subst_singleton {A}: forall x e (P:sm A),
  sm_eq (P[e//x]) (P // [(x, e)]).
Proof.
  unfold sm_eq; simpl; reflexivity.
Qed.

Lemma subst_sm_expr_nil {A} : forall (e:sm A),
  sm_eq (e // []) e.
Proof.
  unfold sm_eq. simpl. reflexivity.
Qed.

Definition substs := list (var * expr).

Lemma subst_expr_cons_ineq {A} : forall (e: sm A) (es: substs) x a
  (Hfree: ~ free e x),
  sm_eq (e // ((x, a)::es)) (e // es).
Proof.
  intros. unfold sm_eq. intros. simpl.
  eapply sm_prop. apply symmetry. eapply agree_on_add. assumption.
Qed.
 
Lemma subst_var_cons_eq: forall (x:var) es e,
  sm_eq ((var_expr x) // ((x, e)::es)) e.
Proof.
  unfold sm_eq; intros. simpl. rewrite stack_lookup_add; reflexivity.
Qed.

Lemma subst_var_cons_ineq: forall (x:var) y es e
  (Hneq: x <> y),
  sm_eq ((var_expr x) // ((y, e)::es)) ((var_expr x) // es).
Proof.
  intros. eapply subst_expr_cons_ineq. simpl. intuition.
Qed.

Lemma substs_not_free_sm : forall {A} {P: sm A} {es},
  (forall x, In x (map (@fst var expr) es) -> ~ free P x) ->
  sm_eq (P // es) P.
Proof.
  intros. unfold sm_eq. simpl. induction es.
  - simpl. reflexivity.
  - simpl. intro s. rewrite notin_fv_add.
    eapply IHes. intros x HIn HFree.
    apply (H x). simpl. intuition.
    assumption.    
    intro HFree.
    apply (H (fst a)). simpl. intuition.
    assumption.
Qed.

Lemma val_expr_substs : forall v es,
  sm_eq ((val_expr v) // es) (val_expr v).
Proof.
  intros.
  apply substs_not_free_sm. simpl. intuition.
Qed.

Add Parametric Morphism A : (@sm_subst_exprs A) with signature
  sm_eq ==> @eq _ ==> sm_eq
  as sm_eq_subst_exprs_m.
Proof.
  unfold sm_eq. intros. simpl.
  eapply H.
Qed.

Lemma subst_sm_bind {A B} (f: (A -> B) -> B) (g: A -> sm B) es :
  sm_eq ((sm_bind f g) // es) (sm_bind f (fun x => (g x) // es)).
Proof.
  unfold sm_eq; simpl; firstorder.
Qed.

Lemma stack_lookup_not_in : forall x xs es (s s' : stack)
  (HNin : ~ In x xs)
  (HLen : length xs = length es),
  s' x = (zip xs es :@: s +:+ s') x.
Proof.
  induction xs; intros; simpl in *.
  destruct es; [simpl; reflexivity | discriminate].
  destruct es; [discriminate | simpl in *].
  destruct (SM'.eq_dec a x); [subst; rewrite stack_lookup_add
    | rewrite stack_lookup_add2; [| assumption ]].
  - contradiction HNin; left; reflexivity.
  apply IHxs.
  - intro HIn; apply HNin; right; assumption.
  injection HLen; intros HLen'; assumption.
Qed.

Lemma sim_subst_front : forall x xs es (s s0 s1 : stack)
  (HIn  : In x xs)
  (HLen : length xs = length es),
  (zip xs es :@: s +:+ s0) x = (zip xs es :@: s +:+ s1) x.
Proof.
  induction xs; intros.
  - destruct es; [contradiction | discriminate].
  destruct es; [discriminate |]; simpl.
  destruct (SM'.eq_dec a x); [subst |].
  - repeat rewrite stack_lookup_add; reflexivity.
  repeat (rewrite stack_lookup_add2; [| assumption]).
  simpl in HIn; destruct HIn as [HEq | HIn];
    [subst; contradiction n; reflexivity |].
  simpl in HLen; injection HLen; clear HLen; intros HLen;
    apply IHxs; assumption.
Qed.

Lemma sim_subst_at : forall y x v xs es (s s0 : stack)
  (HNin : forall e, In e es -> ~ free e x)
  (HLen : length xs = length es),
  (zip xs es :@: SM.add x v s +:+ s0) y = (zip xs es :@: s +:+ s0) y.
Proof.
  induction xs; intros.
  - destruct es; [simpl; reflexivity | discriminate].
  destruct es; [discriminate | simpl].
  destruct (SM'.eq_dec a y); [subst |].
  - repeat rewrite stack_lookup_add; apply sm_prop; intros z HF.
    destruct (SM'.eq_dec x z); [subst |].
    - apply HNin in HF; [contradiction | left; reflexivity].
    rewrite stack_lookup_add2; [reflexivity | assumption].
  repeat (rewrite stack_lookup_add2; [| assumption]); apply IHxs.
  - intros e HIn; apply HNin; right; assumption.
  injection HLen; trivial.
Qed.

Definition free_in A (P : sm A) xs :=
  forall x, free P x -> In x xs.

Lemma agree_on_pre_call : forall A (P : sm A) ps ps' es y (s : stack)
  (HND   : NoDup ("this"%string :: ps))
  (HND0  : NoDup ("this"%string :: ps'))
  (HFP   : free_in P ("this"%string :: ps))
  (HLen  : length ps = length ps')
  (HLen0 : length ps' = length es),
  let s0 := zip ("this"%string :: ps') (y :: es) :@: s +:+ SM.empty val
    in agree_on (free P) (zip ps (map var_expr ps') :@: s0 +:+ s0)
    (zip ("this"%string :: ps) (y :: es) :@: s +:+ s).
Proof.
  simpl; intros; intros x HF; specialize (HFP _ HF).
  destruct (SM'.eq_dec "this"%string x); [subst |].
  - rewrite stack_lookup_add, <- stack_lookup_not_in, stack_lookup_add;
    [reflexivity | | rewrite map_length; assumption].
    rewrite NoDup_cons_iff in HND; destruct HND as [HND _]; assumption.
  rewrite stack_lookup_add2; [| assumption]; simpl in HFP; destruct HFP
    as [HEq | HFP]; [subst; contradiction n; reflexivity |].
  generalize dependent ps'; generalize dependent es; induction ps; intros.
  - destruct ps'; destruct es; discriminate || contradiction.
  destruct ps'; [discriminate |]; destruct es; [discriminate |]; simpl.
  destruct (SM'.eq_dec a x); [subst |].
  - repeat rewrite stack_lookup_add; rewrite stack_lookup_add2,
    stack_lookup_add; [reflexivity |].
    rewrite NoDup_cons_iff in HND0; destruct HND0 as [HN _]; intro HEq;
      subst; apply HN; left; reflexivity.
  do 2 (rewrite stack_lookup_add2; [| assumption]);
    rewrite <- IHps with (ps' := ps'); clear IHps.
  - injection HLen; injection HLen0; clear HLen HLen0; intros HLen HLen0;
    simpl in HFP; destruct HFP as [HEq | HFP]; [subst; contradiction n0;
      reflexivity |]; clear HND; repeat rewrite NoDup_cons_iff in HND0;
    destruct HND0 as [HNin0 [HNin1 _]].
    rewrite sim_subst_at; [| apply free_var_expr; intro HIn; apply HNin0;
      right; assumption | rewrite map_length; assumption].
    rewrite sim_subst_at; [| apply free_var_expr; assumption
      | rewrite map_length; assumption].
    rewrite sim_subst_at; [| apply free_var_expr; intro HIn; apply HNin0;
      right; assumption | rewrite map_length; assumption].
    apply sim_subst_front; [assumption |rewrite map_length; assumption].
  - rewrite NoDup_cons_iff; rewrite NoDup_cons_iff in HND; destruct HND
    as [HNin HND]; split; [intros HIn; apply HNin; right; assumption |
      clear HNin].
    rewrite NoDup_cons_iff in HND; destruct HND; assumption.
  - destruct HFP as [HEq | HFP]; [subst; contradiction n0; reflexivity
    | assumption].
  - rewrite NoDup_cons_iff; rewrite NoDup_cons_iff in HND0; destruct HND0
    as [HNin HND0]; split; [ intros HIn; apply HNin; right; assumption |
      clear HNin].
    rewrite NoDup_cons_iff in HND0; destruct HND0; assumption.
  - simpl in *; injection HLen; trivial.
  simpl in *; injection HLen0; trivial.
Qed.

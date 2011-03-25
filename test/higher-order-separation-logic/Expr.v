Require Export Stack.

Definition agree_on (V: var -> Prop) (s s': stack) :=
  forall x, V x -> s x = s' x.

Instance agree_on_equiv (V : var -> Prop) : Equivalence (agree_on V).
Proof.
  intros. split; [firstorder | firstorder |].
  red. unfold agree_on. eauto using trans_eq.
Qed.

Lemma agree_on_add : forall s V x v, ~ V x -> agree_on V s (SM.add x v s).
Proof.
  introv Hnotin. unfold agree_on. intros x' Hin.
  unfold stack_lookup; rewrite SM'.add_o.
  destruct (SS'.Dec.F.eq_dec x x'); simpl; auto.
  subst; contradiction.
Qed.

Lemma agree_on_add_iff: forall a b c (s s':stack) (fv:var->Prop),
  agree_on fv (SM.add a b s) (SM.add a c s') <-> 
  ((fv a -> b = c) /\ agree_on (fun x => x <> a /\ fv x) s s').
Proof.
  unfold agree_on. split. intros Hl.
  - split.
    - intro Hfv. specialize (Hl _ Hfv). repeat rewrite stack_lookup_add in Hl. 
      assumption.
    - intros x [Hneq Hfv]. specialize (Hl _ Hfv).
      repeat (rewrite stack_lookup_add2 in Hl; eauto).  
  - intros [Heq HIn] x Hfv.
    destruct (string_dec x a).
    - subst. repeat rewrite stack_lookup_add. intuition.
    - repeat (rewrite stack_lookup_add2; eauto).
Qed.

Lemma agree_on_add_id : forall f x (s:stack),
  agree_on f (SM.add x (s x) s) s.
  unfold agree_on. intros.
  destruct (string_dec x x0).
  - subst. rewrite stack_lookup_add. reflexivity.
  - rewrite stack_lookup_add2; try reflexivity; try intuition.
Qed.

Lemma agree_on_overwrite: forall f x v1 v2 s,
  agree_on f (SM.add x v1 (SM.add x v2 s)) (SM.add x v1 s).
Proof.
  unfold agree_on. intros.
  destruct (string_dec x x0).
  - subst. repeat rewrite stack_lookup_add. reflexivity.
  - repeat rewrite stack_lookup_add2; try reflexivity; try intuition.
Qed.

Lemma agree_on_add_iff2 : forall a b s s' (fv : var -> Prop),
  agree_on fv (SM.add a b s) s' <->
  (fv a -> s' a = b) /\ (agree_on (fun x => x <> a /\ fv x) s s').
Proof.
  unfold agree_on. split. intros Hl.
  - split.
    - intro Hfv. specialize (Hl _ Hfv). rewrite stack_lookup_add in Hl. intuition.
    - intros x [Hneq Hfv]. specialize (Hl _ Hfv).
      rewrite stack_lookup_add2 in Hl. intuition. intuition.
  - intros [Heq HIn] x Hfv.
    destruct (string_dec x a).
    - subst. rewrite stack_lookup_add. intuition.
    - rewrite stack_lookup_add2; intuition.
Qed.

Lemma agree_on_imp : forall (fv fv':var->Prop) s s'
  (HImp: forall x, fv x -> fv' x)
  (Hag: agree_on fv' s s'),
  agree_on fv s s'.
Proof.  
  unfold agree_on. intros. intuition.
Qed.

(* Stack Monad *)
Record sm {A} := { sm_fun  :> stack -> A;
                   free    : var -> Prop;
                   sm_prop : forall s s' (HFree : agree_on free s s'),
                     sm_fun s = sm_fun s' }.

Implicit Arguments sm [].
Implicit Arguments Build_sm [A].

Lemma notin_fv_add {A} (e:sm A) x :
  ~ free e x -> forall s v, e (SM.add x v s) = e s.
Proof.
intros. rewrite <- (sm_prop e s); auto. apply agree_on_add. assumption.
Qed.

(* monadic return *)
Program Definition sm_const {A} (a:A) : sm A :=
  Build_sm (\_, a) (\_, False) _.

(* monadic bind *)
Program Definition sm_mbind {A B} (e: sm A) (f: A -> sm B) : sm B :=
  Build_sm (\s, f (e s) s)
           (\y, free e y \/ exists a, free (f a) y)
           _.
Next Obligation.
intros. rewrite (sm_prop e _ s').
- rewrite (sm_prop (f _) _ s'); auto.
  eapply agree_on_imp; eauto. firstorder.
- eapply agree_on_imp; eauto. firstorder.
Qed.

(* Monad laws *)
Lemma sm_monad_1 {A B} (a:A) (f: A -> sm B) :
  sm_fun (sm_mbind (sm_const a) f) = sm_fun (f a).
Proof. auto using @functional_extensionality. Qed.

Lemma sm_monad_2 {A} (e: sm A) :
  sm_fun (sm_mbind e sm_const) = sm_fun e.
Proof. auto using @functional_extensionality. Qed.

Lemma sm_monad_3 {A B C} (e: sm A) (f: A -> sm B) (g: B -> sm C) :
  sm_fun (sm_mbind (sm_mbind e f) g) = sm_fun (sm_mbind e (\a, sm_mbind (f a) g)).
Proof. auto using @functional_extensionality. Qed.

Program Let sm_lift_step {A B} (hd: sm A) (tl: sm (A -> B)) : sm B :=
Build_sm (fun s => tl s (hd s)) (\x, free hd x \/ free tl x) _.
Next Obligation.
  intros. rewrite (sm_prop tl _ s').
  - rewrite (sm_prop hd _ s'); firstorder.
  - firstorder.
Qed.

Definition sm_lift (As: list Type) (R: Type) (op: arrows As R) :=
  lfoldl_dep R sm sm (fun _ _ => sm_lift_step) As (sm_const op).

Definition sm_un {A B} : (A -> B) -> (sm A -> sm B) :=
  sm_lift (A::nil) B.

Definition sm_bin {A B C} : (A -> B -> C) -> (sm A -> sm B -> sm C) :=
  sm_lift (A::B::nil) C.

Definition sm_tern {A B C D} : (A -> B -> C -> D) -> (sm A -> sm B -> sm C -> sm D) :=
  sm_lift (A::B::C::nil) D.

Program Definition sm_bind {A B} (f: (A -> B) -> B) (g : A -> sm B) : sm B :=
  Build_sm (fun s => f (fun x => (g x) s)) (fun y => exists x, free (g x) y) _.
Next Obligation.
  intros; f_equal.
  apply functional_extensionality; intros x.
  erewrite (sm_prop (g x)); eauto.
  intros y HF; apply HFree; eauto.
Qed.  

Definition sm_eq {A} (a b: sm A) : Prop :=
  forall s, (sm_bin eq a b) s.

Lemma eq_sm_eq A (g g': sm A) : impl (g = g') (sm_eq g g').
unfold sm_eq. simpl. congruence.
Qed.

Add Parametric Morphism A : (@sm_eq A) with signature
  sm_eq ==> sm_eq ==> iff
  as sm_eq_iff_m.
Proof.
  intuition congruence.
Qed.

Add Parametric Morphism A B : (@sm_un A B) with signature
  eq ==> sm_eq ==> sm_eq 
  as sm_un_eq.
Proof.
  unfold sm_eq. simpl.
  intros. rewrite H. reflexivity.
Qed.

Add Parametric Morphism A B C : (@sm_bin A B C) with signature
  eq ==> sm_eq ==> sm_eq ==> sm_eq 
  as sm_bin_eq.
Proof.
  unfold sm_eq. simpl.
  intros. rewrite H, H0. reflexivity.
Qed.

Add Parametric Morphism A B C D : (@sm_tern A B C D) with signature
  eq ==> sm_eq ==> sm_eq ==> sm_eq ==> sm_eq
  as sm_tern_eq.
Proof.
  unfold sm_eq. simpl.
  intros. rewrite H, H0, H1. reflexivity.
Qed.

Instance sm_eq_Equivalence A : Equivalence (@sm_eq A).
Proof.
  intuition congruence.
Qed.

Program Definition sm_subst_val {A} (g: sm A) v x :=
  Build_sm
    (fun s => g (SM.add x v s))
    (\y, free g y /\ y <> x)
    _.
Next Obligation.
  intros. apply (sm_prop g). unfold agree_on in *. SM'.simp.
  introv HF; unfold stack_lookup in *; repeat rewrite SM'.add_o.
  destruct (SS'.Dec.F.eq_dec x x0); simpl; auto.
Qed.

Program Definition sm_subst_vals {A} (g: sm A) (sub: list (var*val)) :=
  Build_sm (fun s =>
      g (fold_left (\st, \v:var*val, (SM.add (fst v) (snd v) st)) sub s))
    (\y, free g y /\ forall x, In x (map (fst (B:=val)) sub) ->  y <> x)
    _.
Next Obligation.
  intros; apply (sm_prop g); generalize dependent s'; revert s; induction sub;  
    simpl in *.
  unfold agree_on in *; eauto.
  intros; apply IHsub; clear IHsub; unfold agree_on in *.
  introv H; destruct a as [z vz]; simpl.
  unfold stack_lookup in *; repeat rewrite SM'.add_o.
  destruct (SS'.Dec.F.eq_dec z x); simpl; auto.
  apply HFree; simpl; subst; intuition eauto.
Qed.

Definition expr := sm val.

Program Coercion var_expr (x: var) : expr :=
  Build_sm (fun s => s x) (\y, y=x) _.

Coercion val_expr (v: val) : expr := sm_const v.

Definition simult_add_pair_list_stack lst (s s' : stack) :=
  fold_right (\v:var * expr, \s' : stack, SM.add (fst v) (snd v s) s') s' lst.
Definition add_list_with_val lst v s :=
  fold_right (\x:var, \s':stack, SM.add x v s') s lst.

Notation " xs ':v:' y '+:+' s " := (add_list_with_val xs y s)
  (at level 69, right associativity).
Notation " xs ':@:' s '+:+' s' " := (simult_add_pair_list_stack xs s s')
  (at level 69, right associativity).

Lemma agree_on_simult_add_pair_list_stack_unfold :
  forall (lst : list (var * expr)) (fv:var->Prop) (s s' s'':stack)
  (Hag: agree_on fv (lst :@: s' +:+ s'') s)
  (HDup: NoDup (map (@fst var expr) lst)),
    (agree_on (fun x => not (exists y, In (x, y) lst) /\ fv x) s'' s /\ 
               forall v e, fv v -> In (v, e) lst -> s v = (e s')).
Proof.
  induction lst.
  - simpl. intros. unfold agree_on. 
    - intros. intuition.
  - simpl. intros.
    assert (NoDup (map (@fst var expr) lst)) by (inversion HDup; assumption).
    rewrite agree_on_add_iff2 in Hag. destruct Hag. 
    specialize (IHlst (fun x => x <> fst a /\ fv x) s s' s'' H1 H). split.
    - apply agree_on_imp with (fv':=fun x => (~ exists y, In (x, y) lst) /\ 
                                               x <> fst a /\ fv x).
      intros x [HIn1 Hfx]. split; try assumption. intros [y HIn2]. 
      contradict HIn1. exists y. right. assumption. intuition.
      contradict HIn1. destruct a. exists e. simpl in *. subst. intuition.
      intuition.
    - intros. destruct (string_dec v (fst a)).
        - destruct a. destruct H3.
          - inversion H3. clear e0. subst. intuition.
          - rewrite NoDup_cons_iff in HDup. assert (v = v0) by intuition. subst.
            destruct HDup. contradiction H5. simpl. simpl in H3.
            clear IHlst H0 H1 H5 H6 H H2 H4. induction lst. intuition. 
            simpl in *. intuition. left. destruct a. inversion H.
            intuition. destruct H3.
            destruct a. inversion H3. subst.  intuition.
            intuition.
Qed.

Lemma agree_on_simult_add_pair_list_stack_iff : 
  forall (lst : list (var * expr)) (fv:var->Prop) (s s' s'':stack)
  (HIn: forall v e, fv v -> In (v, e) lst -> s v = (e s')),
  agree_on fv (lst :@: s' +:+ s'') s <->
    agree_on (fun x => not (exists y, In (x, y) lst) /\ fv x) s'' s.
Proof.
  induction lst.
  - simpl. intros. unfold agree_on. split.
    - intros. intuition.
    - intros. firstorder.
  - simpl. intros.
    assert (forall v e, (v <> fst a /\ fv v) -> In (v, e) lst -> s v = e s') 
      as H by intuition.
    rewrite agree_on_add_iff2. split. 
    - intros [Heq Hag].
      apply agree_on_imp with (fv':=fun x => (~ exists y, In (x, y) lst) /\      
                                               x <> fst a /\ fv x).
      intros x [HIn1 Hfx]. split; try assumption. intros [y HIn2]. 
      contradict HIn1. exists y. right. assumption. intuition.
      contradict HIn1. destruct a. exists e. simpl in *. subst. intuition.
      specialize (IHlst (fun x => x <> fst a /\ fv x) s s' s'' H).
      destruct IHlst as [IHl IHr].
      apply IHl. assumption.

      intros Hag. split. destruct a. simpl. intros. intuition.
      specialize (IHlst (fun x => x <> fst a /\ fv x) s s' s'' H). 
      destruct IHlst as [IHl IHr].
      apply IHr. 
      eapply agree_on_imp with (fv:=fun x => (~ exists y, In (x, y) lst) /\ 
                                               x <> fst a /\ fv x) in Hag.
      assumption.
      intros x [HIn1 Hfx]. split; try assumption. intros [y HIn2]. 
      contradict HIn1. exists y. destruct a.
      destruct HIn2. inversion H0. subst. intuition. assumption. intuition.
Qed.

Lemma agree_on_add_list_with_val_unfold :
  forall (lst : list var) (fv:var->Prop) (s s':stack) v
  (Hag: agree_on fv (lst :v: v +:+ s') s)
  (HDup: NoDup lst),
    (agree_on (fun x => not (In x lst) /\ fv x) s' s /\ 
               forall x, fv x -> In x lst -> s x = v).
Proof.
  induction lst.
  - simpl. intros. unfold agree_on. 
    intros. intuition.
  - simpl. intros.
    assert (NoDup lst) by (inversion HDup; assumption).
    rewrite agree_on_add_iff2 in Hag. destruct Hag. 
      specialize (IHlst (fun x => x <> a /\ fv x) s s' v H1 H). split.
      - apply agree_on_imp with (fv':=fun x => not (In x lst) /\ 
                                                 x <> a /\ fv x).
        intros x [HIn1 Hfx]. split; try assumption. intros HIn2 .
        contradict HIn1. right. assumption. intuition.
        intuition.
      - intros. destruct H3.
        - subst. intuition.
        - destruct IHlst. apply H5; [|assumption].
          split; [|assumption]. intro H6. subst. rewrite NoDup_cons_iff in HDup.
          intuition.
Qed.

Lemma agree_on_add_list_with_val_iff : 
  forall (lst : list var) (fv:var->Prop) (s s':stack) v
  (HIn: forall x, fv x -> In x lst -> s x = v),
  agree_on fv (lst :v: v +:+ s') s <->
    agree_on (fun x => not (In x lst) /\ fv x) s' s.
Proof.
  induction lst.
  - simpl. intros. unfold agree_on. split.
    - intros. intuition.
    - intros. firstorder.
  - simpl. intros.
    assert (forall x, (x <> a /\ fv x) -> In x lst -> s x = v) as H by intuition.
    rewrite agree_on_add_iff2. split. 
    - intros [Heq Hag].
      apply agree_on_imp with (fv':=fun x => not (In x lst) /\ 
                                               x <> a /\ fv x).
      intros x [HIn1 Hfx]. split; try assumption. intros HIn2 .
      contradict HIn1. right. assumption. intuition.
      specialize (IHlst (fun x => x <> a /\ fv x) s s' v H).
      destruct IHlst as [IHl IHr].
      apply IHl. assumption.

      intros Hag. split. intuition.
      specialize (IHlst (fun x => x <> a /\ fv x) s s' v H). 
      destruct IHlst as [IHl IHr].
      apply IHr. 
      eapply agree_on_imp with (fv:=fun x => (~ In x lst) /\ 
                                               x <> a /\ fv x) in Hag.
      assumption.
      intros x [HIn1 Hfx]. split; try assumption. intros [y | HIn2].
      intuition.
      contradict HIn1. assumption. intuition.
Qed.

Lemma free_var_expr : forall x xs
  (HNIn : ~ In x xs)
  e
  (HIn : In e (map var_expr xs)),
  ~ free e x.
Proof.  
  induction xs; simpl; intros; [contradiction |].
  destruct HIn as [HEq | HIn]; [subst |].
  - simpl; intros HEq; subst; apply HNIn; left; reflexivity.
  apply IHxs; [| assumption].
  intros HR; apply HNIn; right; assumption.
Qed.

Require Export Util.

Set Implicit Arguments.
Unset Strict Implicit.

Definition bin_lift_option (T : Type) (f : T -> T -> option T) (x y : option T) :=
  match x, y with
    | Some t1, Some t2 => f t1 t2
    | _, _ => None
  end.
Definition lift_option_rel (T : Type) (R : relation T) : relation (option T) :=
  fun x y => match x, y with
               | Some x, Some y => R x y
               | None, None => True
               | _, _ => False
             end.
Definition lift_some_rel (T : Type) (R : relation T) : relation (option T) :=
  fun x y => match x, y with
               | Some x, Some y => R x y
               | _, _ => False
             end.

Record sep_alg_prop (T : Type) : Type := SepAlgMixin {
  sa_un : T;
  sa_op : T -> T -> option T;
  sa_equiv : relation T;
  sa_equiv_prop : Equivalence sa_equiv;
  sa_op_comm  : forall a b, lift_option_rel sa_equiv (sa_op a b) (sa_op b a);
  sa_op_assoc : forall a b c,
    lift_option_rel sa_equiv (bin_lift_option sa_op (sa_op a b) (Some c))
    (bin_lift_option sa_op (Some a) (sa_op b c));
  sa_op_unit  : forall a, lift_option_rel sa_equiv (sa_op a sa_un) (Some a);
  sa_op_mon   : forall a b c, sa_equiv b c -> lift_option_rel sa_equiv (sa_op a b) (sa_op a c);
  sa_op_cancel: forall a b c d,
    lift_option_rel sa_equiv (sa_op a b) (Some d) ->
    lift_option_rel sa_equiv (sa_op a c) (Some d) ->
    sa_equiv b c}.

Record sep_alg := SepAlg {
  sa_carrier :> Type;
  sep_props  : sep_alg_prop sa_carrier}.

Instance sa_eq_rel T (SAP : sep_alg_prop T) : Equivalence (sa_equiv SAP).
Proof. exact (sa_equiv_prop SAP). Qed.

Instance opt_eq_rel T (R : relation T) (Eq : Equivalence R) : Equivalence (lift_option_rel R).
Proof.
  destruct Eq as [RR RS RT]; split.
  intros [x |]; simpl; intuition.
  intros [x |] [y |]; simpl; intuition.
  intros [x |] [y |] [z |]; simpl; intuition eauto.
Qed.

Instance some_symm_rel T (R : relation T) (RS : Symmetric R) : Symmetric (lift_some_rel R).
Proof.
  intros [x |] [y |]; simpl; intuition.
Qed.
Instance some_trans_rel T (R : relation T) (RT : Transitive R) : Transitive (lift_some_rel R).
Proof.
  intros [x |] [y |] [z |]; simpl; intuition eauto.
Qed.

Add Parametric Morphism T (SAP : sep_alg_prop T) : (sa_equiv SAP) with signature
  (sa_equiv SAP) ==> (sa_equiv SAP) ==> iff
as sa_equiv_iff_m.
Proof.
  intros x y Heqxy t u Heqtu; split; [intro Heqxt | intro Heqyu].
  rewrite <- Heqxy; rewrite Heqxt; exact Heqtu.
  rewrite Heqxy; rewrite Heqtu; exact Heqyu.
Qed.

Add Parametric Morphism T (R : relation T) (M : Proper (R ==> R ==> iff) R)
  : (lift_option_rel R) with signature
  (lift_option_rel R) ==> (lift_option_rel R) ==> iff
as lift_option_iff_m.
Proof.
  intros [x |] [y |] Heqxy [t |] [u |] Heqtu; simpl in *; try contradiction; try tauto.
  apply M; auto.
Qed.

Add Parametric Morphism T (SAP : sep_alg_prop T) : (sa_op SAP) with signature
  (sa_equiv SAP) ==> (sa_equiv SAP) ==> (lift_option_rel (sa_equiv SAP))
as sa_op_equiv_m.
Proof.
  intros x y Heqxy t u Heqtu.
  transitivity (sa_op SAP x u).
  apply sa_op_mon; auto.
  rewrite sa_op_comm; rewrite (sa_op_comm SAP y).
  apply sa_op_mon; auto.
Qed.

Add Parametric Morphism T (R : relation T) (op : T -> T -> option T)
  (M : Proper (R ==> R ==> lift_option_rel R) op) : (bin_lift_option op) with signature
  (lift_option_rel R) ==> (lift_option_rel R) ==> (lift_option_rel R)
as lift_sa_op_equiv_m.
Proof.
  intros [x |] [y |] Heqxy [t |] [u |] Heqtu; simpl in *; try contradiction; try tauto.
  apply M; auto.
Qed.

Add Parametric Morphism T (R : relation T) : (@Some T) with signature
  R ==> (lift_option_rel R)
  as lift_Some_m.
Proof.
  intros x y Heqxy; simpl; exact Heqxy.
Qed.

Section Defs.

  Variable S : sep_alg.
  Implicit Types a b : S.

  Definition sa_eq   := sa_equiv (sep_props S).
  Definition sa_eq_l := lift_option_rel sa_eq.
  Definition sa_mul  := sa_op (sep_props S).
  Definition sa_unit := sa_un (sep_props S).
  Definition compat a b := exists s, sa_eq_l (sa_mul a b) (Some s).
  Definition subheap a b := exists c, sa_eq_l (sa_mul a c) (Some b).
  Program Definition sa_mul_good (a b : S) (HC : compat a b) :=
    match sa_mul a b with
      | Some c => c
      | None => _
    end.
  Definition sub_opt := lift_some_rel subheap.

End Defs.

Module SepAlgNotations.
Notation "a = b"   := (@sa_eq  _ a b) : sa_scope.
Notation "a == b"   := (@sa_eq_l _ a b) (at level 70, no associativity) : sa_scope.
Notation "a '·' b" := (@sa_mul _ a b) (at level 50, left associativity) : sa_scope.
Notation "^" := (@sa_unit _) : sa_scope.
Notation "a # b" := (@compat _ a b) (at level 70, no associativity) : sa_scope.
Notation "a <= b" := (@subheap _ a b) (at level 70, no associativity) : sa_scope.
Notation "a <=| b" := (@sub_opt _ a b) (at level 70, no associativity) : sa_scope.
Notation "a '··' b" := (bin_lift_option (@sa_mul _) a b) (at level 50, left associativity) : sa_scope.
End SepAlgNotations.

Import SepAlgNotations.

Delimit Scope sa_scope with sa.

Instance subheap_refl (S : sep_alg) : Reflexive (@subheap S).
Proof.
  intros s; eexists; apply sa_op_unit.
Qed.

Instance subheap_trans (S : sep_alg) : Transitive (@subheap S).
Proof.
  intros s0 s1 s2 [s10 HEq1] [s21 HEq2].
  unfold sa_mul in *; rewrite sa_op_comm in HEq2.
  remember (sa_op (sep_props S) s0 s10) as st; destruct st as [st |]; [|contradiction]; simpl in *.
  rewrite <- HEq1 in HEq2; rewrite sa_op_comm in HEq2.
  remember (sa_op (sep_props S) st s21) as su; destruct su as [su |]; [|contradiction]; simpl in *.
  assert (HT := sa_op_assoc (sep_props S) s0 s10 s21).
  rewrite <- Heqst in HT; simpl in HT; rewrite <- Heqsu in HT.
  remember (sa_op (sep_props S) s10 s21) as s_ex; destruct s_ex as [s_ex |]; simpl.
  rewrite HEq2 in HT; symmetry in HT.
  exists s_ex; simpl; auto.
  contradiction.
Qed.

Instance sub_opt_trans (S : sep_alg) : Transitive (@sub_opt S).
Proof.
  intros [s0 |] [s1 |] [s2 |]; simpl; try apply subheap_trans; intuition.
Qed.

Instance compat_symm (S : sep_alg) : Symmetric (@compat S).
Proof.
  intros s s0 [s1 Hs]; exists s1; unfold sa_mul; rewrite sa_op_comm; auto.
Qed.

Add Parametric Morphism (S : sep_alg) : (@subheap S) with signature
  (@sa_eq S) ==> (@sa_eq S) ==> iff
as sa_subheap_equiv_m.
Proof.
  intros x y Heqxy t u Heqtu; split; [intros [s Heqxst] | intros [s Heqysu]]; exists s.
  rewrite <- Heqxy; rewrite <- Heqtu; exact Heqxst.
  rewrite Heqxy; rewrite Heqtu; exact Heqysu.
Qed.

Add Parametric Morphism (S : sep_alg) : (lift_some_rel (@subheap S)) with signature
  (@sa_eq_l S) ==> (@sa_eq_l S) ==> iff
as lift_sa_subheap_equiv_m.
Proof.
  intros [x |] [y |] Heqxy [u |] [t |] Heqtu; simpl in *; try contradiction || tauto.
  apply sa_subheap_equiv_m; assumption.
Qed.

Section Properties.
  Variable S : sep_alg.
  Implicit Types r s t : S.
  Open Scope sa_scope.

  Definition sa_mulC : forall a b, a · b == b · a := sa_op_comm (sep_props S).
  Definition sa_mulA : forall a b c, a · b ·· Some c == Some a ·· (b · c) := sa_op_assoc (sep_props S).
  Definition sa_unitI : forall a, a · ^ == Some a := sa_op_unit (sep_props S).
  Definition sa_cancel : forall a b c d, a · b == Some d -> a · c == Some d -> (b = c)%sa :=
    @sa_op_cancel _ (sep_props S).

  Lemma mul_some_right : forall r s t u, t · u == Some s -> r · s == Some r ·· (t · u).
  Proof.
    intros r s t u H; rewrite H; simpl; reflexivity.
  Qed.

  Lemma compat_subheap : forall r s t, r <= s -> s # t -> r # t.
  Proof.
    intros r s t [sr Hsr] [st Hst].
    rewrite sa_mulC in Hst; erewrite mul_some_right in Hst; [| eassumption].
    rewrite <- sa_mulA in Hst; rewrite sa_mulC in Hst; unfold compat; remember (r · t) as rt;
      destruct rt as [rt |]; simpl in *; [ exists rt; reflexivity | contradiction ].
  Qed.

  Lemma cancel_leq : forall r s t, r <= s -> s # t -> r · t <=| s · t.
  Proof.
    intros r s t [u Hequ] [v Heqv]; unfold sub_opt.
    remember (r · t) as rt; destruct rt as [rt |]; symmetry in Heqrt.
    remember (s · t) as st; destruct st as [st |]; [symmetry in Heqst | contradiction].
    exists u; rewrite sa_mulC; erewrite mul_some_right; [| rewrite <- Heqrt; reflexivity ].
    rewrite <- sa_mulA; rewrite sa_mulC; rewrite Hequ; simpl; auto.
    rewrite Heqst; reflexivity.
    rewrite sa_mulC in Heqv; erewrite mul_some_right in Heqv; [| eassumption].
    rewrite <- sa_mulA in Heqv; rewrite sa_mulC in Heqv; rewrite Heqrt in Heqv; contradiction.
  Qed.

  Lemma sub_opt_mul_compat : forall (s s1 s2 : S) (Hs : s1 · s2 <=| Some s), s1 # s2.
  Proof.
    intros; remember (s1 · s2) as st; destruct st as [st |]; simpl in *;
      [exists st; rewrite Heqst; reflexivity | contradiction].
  Qed.

  Lemma sub_opt_mul_r : forall s s1 s2 (Hs : s1 · s2 <=| Some s), s2 <= s.
  Proof.
    intros; destruct (sub_opt_mul_compat Hs) as [st Heqst]. rewrite Heqst in Hs.
    apply subheap_trans with st; [| trivial].
    exists s1; rewrite sa_mulC; auto.
  Qed.

  Lemma sub_opt_mul_l : forall s s1 s2 (Hs : s1 · s2 <=| Some s), s1 <= s.
  Proof.
    intros. rewrite sa_mulC in Hs. eapply sub_opt_mul_r. eassumption.
  Qed.

  Lemma sub_opt_mul_t : forall (s s1 s2 s3 : S) (Hsub : s1 <= s2) (Hso : s2 · s3 <=| Some s),
    s1 · s3 <=| Some s.
  Proof.
    intros; apply sub_opt_trans with (s2 · s3); [| assumption].
    apply cancel_leq; [assumption |].
    apply sub_opt_mul_compat with s; assumption.
  Qed.

  Lemma sub_opt_mul_lr : forall (s s1 s2 s1' s2' : S)
    (Hs : s1 <= s1') (Ht: s2 <= s2') (Hso : s1' · s2' <=| Some s),
    s1 · s2 <=| Some s.
  Proof.
    intros.
    eapply sub_opt_mul_t; [eassumption|]. rewrite sa_mulC.
    eapply sub_opt_mul_t; [eassumption|]. rewrite sa_mulC.
    assumption.
  Qed.

  Close Scope sa_scope.

End Properties.

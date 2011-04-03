Add LoadPath "/Users/hannes/tomeso/git/semantics/Coq".
Require Import Tactics.
Require Import LiftOp.
Require Import SubstAbstractAsn.
Open Scope string_scope.
Open Scope list_scope.
Module Fac <: PROGRAM.

Definition fac_body := (cif (egt (var_expr "n") 0) (cseq (cscall "x" "FacC" "fac" ((eminus (var_expr "n") 1) :: nil)) (cassign "x" (etimes (var_expr "n") (var_expr "x")))) (cassign "x" 1)).

Definition facM := Build_Method ("n" :: nil) fac_body (var_expr "x").

Definition FacC :=
  Build_Class (SS.empty)
              (SM.add "fac" facM (SM.empty _)).

Definition Prog := Build_Program (SM.add "FacC" FacC (SM.empty _)).

Lemma unique_method_names :
      forall C m mrec,
      method_lookup Prog C m mrec ->
      NoDup (m_params mrec).
Proof.
  search (search_unique_names Prog).
Qed.
End Fac.
Module Fac_spec.
Import Fac.
Module Import SR := Rules Fac.
Fixpoint fac n :=
  match n with
      | S n => (S n) * fac n
      | 0 => 1
  end.
  
Lemma fac_step : forall n,
  n > 0 ->
  (n * (fac (n - 1))) = fac n.
Proof.
  induction n.
  - intuition.
  - simpl; intuition.
Qed.

Definition facZ := fun (n:Z) =>
  match ((n ?= 0)%Z) with
      | Lt => 0
      | _ => Z_of_nat (fac (Zabs_nat n))
  end.

Lemma fac_Z_nat : forall n,
  (n >= 0)%Z ->
  facZ n = Z_of_nat(fac(Zabs_nat n)).
Proof.
  destruct n. simpl.
  - unfold facZ. simpl. reflexivity.
  - unfold facZ. simpl. reflexivity.
  - firstorder.
Qed.
Lemma Zabs_nat_shrink : forall n,
  (n > 0)%Z ->
  Zabs_nat (n - 1) = (Zabs_nat n) - 1.
Proof.
  intros.
  rewrite Zabs_nat_Zminus. firstorder.
  firstorder.
Qed.
Lemma Zabs_nat_scope_mult : forall n m,
  (n >= 0)%Z ->
  ((n * Z_of_nat m)%Z) = Z_of_nat((Zabs_nat n) * m).
Proof.
  intros.
  rewrite <- (Zabs_nat_Z_of_nat m).
  rewrite <- Zabs_nat_mult.
  rewrite Zabs_nat_Z_of_nat.
  rewrite inj_Zabs_nat.
  rewrite Zabs_eq; [|intuition]. reflexivity.
Qed.
Lemma facZ_step : forall (n:Z),
  (n > 0)%Z ->
  ((n * (facZ (n - 1)))%Z) = facZ n.
Proof.
  intros.
  repeat (rewrite fac_Z_nat; [|omega]).
  rewrite Zabs_nat_shrink; [|omega].
  rewrite Zabs_nat_scope_mult; [|omega].
  rewrite fac_step. reflexivity.
  assert ((Z_of_nat (Zabs_nat n) > 0)%Z).
  rewrite inj_Zabs_nat. rewrite Zabs_eq; [|firstorder].
  assumption.
  intuition.
Qed.
Definition fac_Z : expr -> expr := sm_un (fun v => vint (facZ (val_to_int v))).

Definition fac_pre : hasn :=
  (<pure> (ege "n" 0))%asn.
Definition fac_post : hasn := 
  (<pure> (("ret":expr) ·=· (fac_Z ("n":expr))))%asn.
Definition FacC_spec := "FacC" :.: "fac" |->  ("n" :: nil) {{ fac_pre }}-{{ "ret", fac_post }}.
Lemma substs_fac : forall {e:expr} {es},
  sm_eq ((Fac_spec.fac_Z e) // es) (Fac_spec.fac_Z (e // es)).
Proof.
  split;  intros s h HPre; simpl in *; auto.
Qed.
Add Morphism Fac_spec.fac_Z with signature
  @sm_eq _ _ ==> @sm_eq _ _
  as sm_eq_fac_Z_m.
Proof.
  unfold sm_eq; simpl; intros x y H s. rewrite H. reflexivity.
Qed.
Open Scope spec_scope.
Open Scope asn_scope.
  Ltac frame r :=
    match type of r with
      | vpure =>
        match goal with
          | |- (_ |= c_triple ?p ?q ?c)%spec =>
            eapply roc_pre with (P' := p <*> <pure>r); [
              rewrite <- pure_and_sc_asnR; simplify_asn |
                eapply roc_post; [
                  (apply rule_frame; [try frame_free|]) |
                    try (apply bientails_asn_alt; apply pure_and_sc_asnR)
                ]
            ]
        end
    end.
Lemma valid_Fac : |= FacC_spec.
Proof.
  lob.
    - unfold FacC_spec.
    unfold_method_spec_zero.
  - unfold FacC_spec at 2.
    unfold_method_spec.
    unfold fac_body; spec_substitution.
    forward.
    forward.
    frame  (expr_pure (egt (vvar_expr "n") (val_to_int 0%Z))).
    - asn_solve. (* TODO: Fix reflection to kill this *)
    (* TODO: fix forward and kill the following *)
    eapply rule_static_complete.
    - unfold FacC_spec; reflexivity.
    - unfold fac_pre. subst_h.
      etransitivity; [| rewrite sc_asnC; apply true_asn_unitL].
      solve [unentail; intuition].
    - apply xist_asnEIL; intros v; unfold fac_post. subst_h. subst_h.
      rewrite sc_asnC; apply true_asn_unitR.
    forward.
    subst_h. unentail. intros. intuition. subst.
    rewrite Fac_spec.facZ_step. reflexivity. assumption.
    (* TODO: Fix forward *)
    eapply rule_assign_fwd2; [apply xist_asnEIL; intro; subst_h; asn_unify].
    unentail; simpl in *.
    destruct (Z_dec (val_to_int k) 0)%Z as [[HLt | HGt] | HEq];
    intros [HGe HLe].
  - apply Zlt_not_le in HLt; intuition.
  - apply Zgt_not_le in HGt; apply Zge_le in HGe; intuition.
  rewrite HEq; reflexivity.
Qed.
  

End Fac_spec.

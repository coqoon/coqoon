Require Import Tactics.
Require Import LiftOp.
Require Import SubstAbstractAsn.

Open Scope string_scope.
Open Scope list_scope.

Module Fac <: PROGRAM.

  Definition fac_body :=
    cif ((egt ("n":var) (val_to_int (vint 0))))
      (cseq
        (ccall "x" "this" "fac"
          ([eminus (var_expr"n":expr) (val_to_int (vint 1))]))
        (cassign "x" (etimes (var_expr "n":expr) (var_expr "x":expr))))
      (cassign "x" (val_to_int (vint 1))).

  Definition Facm :=
    Build_Method (["n"]) fac_body (var_expr "x").

  Definition FacC :=
    Build_Class (SS.empty) (SM'.singleton "fac" Facm).

  Definition Prog := Build_Program (SM'.singleton "FacC" FacC).

  Lemma unique_names :
    forall C m mrec,
      method_lookup Prog C m mrec ->
      NoDup ("this" :: m_params mrec).
  Proof.
    search (search_unique_names Prog).
  Qed.

(* Alternative:
Definition unique_names := option_proof (search_unique_names P). *)

End Fac.

Module Fac_spec.

Import Fac.
Module Import SR := Rules Fac.

Fixpoint fac n :=
  match n with
    | S n => (S n) * fac n
    | O => 1
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

Definition fac_Z := sm_un (fun v => vint (facZ (val_to_int v))).

Definition fac_pre : hasn :=
  (<pure> (ege "n" 0) </\> <pure> ((("this":expr) ::: "FacC")))%asn.
Definition fac_post : hasn :=
  (<pure> (("r":expr) ·=· (fac_Z ("n":expr))))%asn.

Definition FacSpec :=
  "FacC" :.: "fac" |-> (["n":var]) {{ fac_pre }}-{{ "r" , fac_post }}.

Lemma substs_fac : forall {e:expr} {es},
  sm_eq ((Fac_spec.fac_Z e) // es) (Fac_spec.fac_Z (e // es)).
Proof.
  split;  intros s h HPre; simpl in *; auto.
Qed.

Add Morphism Fac_spec.fac_Z with signature
  sm_eq ==> sm_eq
  as sm_eq_fac_Z_m.
Proof.
  unfold sm_eq; simpl; intros x y H s. rewrite H. reflexivity.
Qed.

Open Scope spec_scope.
Open Scope asn_scope.

Lemma valid_Fac : |= FacSpec.
Proof.
  lob.
  - unfold FacSpec.
    unfold_method_spec_zero.
  - unfold FacSpec at 2. unfold_method_spec.

    unfold fac_body, fac_pre, fac_post; spec_substitution.
    forward.
    forward.
    rewrite <- and_idempotent_asn with (p := <pure> (egt (var_expr "n") (val_to_int 0%Z))).
    rewrite <- and_asnA, pure_and_sc_asnR; apply rule_frame.
    - simpl; intros; apply modifies_syn_sem; simpl; rewrite SS'.singleton_iff;
      intuition congruence.
    forward. 
    unfold fac_pre. substitution.
    solve [unentail; intuition].
    unfold fac_post. substitution. substitution. reflexivity.

    forward.
    rewrite <- pure_and_sc_asnR; unentail. intros. intuition. subst.
    rewrite Fac_spec.facZ_step. reflexivity. assumption.
    
    forward.
    unentail.
    destruct (Z_dec (val_to_int k0) 0)%Z as [[HLt | HGt] | HEq];
    intros [[HGe Heq] HLe].
  - apply Zlt_not_le in HLt; intuition.
  - apply Zgt_not_le in HGt; apply Zge_le in HGe; intuition.
  rewrite HEq; reflexivity.
Qed.

End Fac_spec.

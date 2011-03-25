Require Import Subst.
Require Import HeapAsn.

Definition eadd : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vint (Zplus (val_to_int v1) (val_to_int v2))).

Lemma eadd_subst : forall e1 e2 es,
  sm_eq ((eadd e1 e2) // es) (eadd (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Definition eminus : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vint (Zminus (val_to_int v1) (val_to_int v2))).

Lemma eminus_subst : forall e1 e2 es,
  sm_eq ((eminus e1 e2) // es) (eminus (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Definition etimes : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vint (Zmult (val_to_int v1) (val_to_int v2))).

Lemma etimes_subst : forall e1 e2 es,
  sm_eq ((etimes e1 e2) // es) (etimes (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Definition enot : expr -> expr :=
  sm_un (fun v => vbool (negb (val_to_bool v))).

Definition Zge : Z -> Z -> bool := 
  fun x y => match ((x ?= y)%Z) with
               | Lt => false
               | _ => true
             end.

Definition Zgt : Z -> Z -> bool :=
  fun x y => match ((x ?= y)%Z) with
               | Gt => true
               | _ => false
             end.

Definition Zeq : Z -> Z -> bool :=
  fun x y => match (x ?= y)%Z with
               | Eq => true
               | _  => false
             end.

Lemma Zge_iff : forall x y,
  Zge x y = true <-> (x >= y)%Z.
Proof.
  intros. unfold Zge. destr_zcompare; intuition; discriminate.
Qed.

Lemma Zgt_iff : forall x y,
  Zgt x y = true <-> (x > y)%Z.
Proof.
  intros. unfold Zgt. destr_zcompare; intuition; discriminate.
Qed.

Lemma Zeq_iff : forall x y,
  Zeq x y = true <-> (x = y)%Z.
Proof.
  intros. unfold Zeq. destr_zcompare; intuition; discriminate.
Qed.

Lemma Zge_inv : forall x y,
  Zge x y = false <-> (x < y)%Z.
Proof.
  intros. unfold Zge.
  destr_zcompare; intuition; discriminate || (apply False_ind; omega).
Qed.
  
Lemma Zgt_inv : forall x y,
  Zgt x y = false <-> (x <= y)%Z.
Proof.
  intros. unfold Zgt.
  destr_zcompare; intuition; discriminate || (apply False_ind; omega).
Qed.

Lemma Zeq_inv : forall x y,
  Zeq x y = false <-> (x <> y)%Z.
Proof.
  intros. unfold Zeq. destr_zcompare; intuition; discriminate.
Qed.

Definition ege : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vbool (Zge (val_to_int v1) (val_to_int v2))).

Lemma ege_subst : forall e1 e2 es,
  sm_eq ((ege e1 e2) // es) (ege (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Definition egt : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vbool (Zgt (val_to_int v1) (val_to_int v2))).

Lemma egt_subst : forall e1 e2 es,
  sm_eq ((egt e1 e2) // es) (egt (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Definition eeq : expr -> expr -> expr :=
  sm_bin (fun v1 v2 => vbool (Zeq (val_to_int v1) (val_to_int v2))).

Lemma eeq_subst : forall e1 e2 es,
  sm_eq ((eeq e1 e2) // es) (eeq (e1 // es) (e2 // es)).
Proof.
  intros. unfold sm_eq. simpl. reflexivity.
Qed.

Add Morphism egt with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_egt_m.
Proof.
  unfold sm_eq; simpl; intros x1 y1 H1 x2 y2 H2 s.
  rewrite H1, H2. reflexivity.
Qed.

Add Morphism ege with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_ege_m.
Proof.
  unfold sm_eq; simpl; intros x1 y1 H1 x2 y2 H2 s.
  rewrite H1, H2. reflexivity.
Qed.

Add Morphism eeq with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_eeq_m.
Proof.
  unfold sm_eq; simpl; intros x1 y1 H1 x2 y2 H2 s.
  rewrite H1, H2. reflexivity.
Qed.

Add Morphism eminus with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_eminus_m.
Proof.
  unfold sm_eq; simpl; intros x1 y1 H1 x2 y2 H2 s.
  rewrite H1, H2. reflexivity.
Qed.

Add Morphism etimes with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_etimes_m.
Proof.
  unfold sm_eq; simpl; intros x1 y1 H1 x2 y2 H2 s.
  rewrite H1, H2. reflexivity.
Qed.

Hint Rewrite ege_subst egt_subst eadd_subst eminus_subst etimes_subst : subst_rewrites.
Hint Rewrite Zge_iff Zge_inv Zgt_iff Zgt_inv : unentail_post.
Require Export Expr.

Definition pure := sm Prop.

Definition Prop_pure (P : Prop) : pure :=
  sm_const P.

Definition entails_pure (P Q: pure) := forall s, P s -> Q s.
Definition bientails_pure (P Q: pure) := entails_pure P Q /\ entails_pure Q P.

Instance subrelation_bientails_entails_pure : subrelation bientails_pure entails_pure.
Proof. firstorder. Qed.

Instance subrelation_bientails_flip_entails_pure : subrelation bientails_pure (flip entails_pure).
Proof. firstorder. Qed.

Instance subrelation_sm_eq_bientails_pure : subrelation sm_eq bientails_pure.
Proof. firstorder congruence. Qed.

Instance Preorder_entails_pure : PreOrder entails_pure.
Proof. firstorder. Qed.

Instance Equivalence_bientails_pure : Equivalence bientails_pure.
Proof. firstorder. Qed.

Coercion expr_pure (e : expr) : pure := sm_un (fun v => val_to_bool v = true) e.

Definition eq_pure {A} : sm A -> sm A -> pure := sm_bin (@eq A).

Definition not_pure (p: pure) : pure :=
  sm_un not p.

Infix "·=·" := eq_pure (at level 70, no associativity).

Instance : subrelation sm_eq bientails_pure.
Proof.
  intros p q H. unfold sm_eq, bientails_pure, entails_pure in *. simpl in H.
  intuition congruence.
Qed.


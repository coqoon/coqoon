Require Export SemRules.

Open Scope asn_scope.

Lemma not_true_eq_false : forall b,
  (b <> true) <-> (b = false).
Proof.
  destruct b; intuition.
Qed.

Hint Rewrite not_true_eq_false : unentail_post.

Ltac remem_stack s :=
  let kk := fresh "k" in let Heqk := fresh "Heq" kk in
    let nn := fresh "n" in let Heqn := fresh "Heq" nn in 
      let bb := fresh "b" in let Heqb := fresh "Heq" bb in 
        let pp := fresh "p" in let Heqp := fresh "Heq" pp in 
          match goal with
            | [ HH: context [ stack_lookup s ?n ] |- _ ] =>
              remember (stack_lookup s n) as kk; clear Heqk
            | [  |- context [ stack_lookup s ?n ]] =>
              remember (stack_lookup s n) as kk
          end.

Lemma stack_rewr_ne : forall (s : stack) x v y
  (HNe : x <> y),
  (SM.add x v s : stack) y = s y.
Proof.
  intros; unfold stack_lookup; rewrite SM'.add_o;
    destruct (SS'.Dec.F.eq_dec x y) as [ HEq | HNeq ];
      [ contradiction | reflexivity].
Qed.

Lemma stack_rewr_eq : forall (s : stack) x v y
  (HEq : x = y),
  (SM.add x v s : stack) y = v.
Proof.
  intros; unfold stack_lookup; rewrite SM'.add_o;
    destruct (SS'.Dec.F.eq_dec x y) as [HEq' | HNeq];
      [reflexivity | contradiction].
Qed.

Ltac stack_simp :=
  match goal with
    | [ H:context [stack_lookup (SM.add _ _ _) _ ] |- _ ] =>
      (rewrite stack_rewr_ne in H; [stack_simp | solve [ intro; discriminate ]])
        || (rewrite stack_rewr_eq in H; [stack_simp | solve [ f_equal; auto ]])
    | [ |- context [stack_lookup (SM.add _ _ _) _ ] ] =>
      (rewrite stack_rewr_ne; [stack_simp | solve [ intro; discriminate ]])
        || (rewrite stack_rewr_eq; [stack_simp | solve [ f_equal; auto ]])
    | [ |- _ ] => idtac
  end.

Ltac unentail :=
  let s := fresh "s" with h := fresh "h" with HT := fresh "HLeft"
    in unfold string_var, entails_asn, entails_up in *; intros s h HLeft;
      simpl in *; stack_simp; repeat remem_stack s; subst; eauto;
        autorewrite with unentail_post in *; clear s h.

Close Scope asn_scope.
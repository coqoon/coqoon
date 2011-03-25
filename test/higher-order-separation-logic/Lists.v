Require Import Tactics.
Require Import LiftOp.
Require Import SubstAbstractAsn.

Open Scope string_scope.

Module Lists <: PROGRAM.

  Definition NodeC :=
    Build_Class (SS.add "val" (SS.add "next" SS.empty)) (SM.empty _).

  Definition eeq_ptr : expr -> expr -> expr :=
    sm_bin (fun v1 v2 =>
      vbool (proj1_sig (Sumbool.bool_of_sumbool 
        (natstring_DT.eq_dec (val_to_ptr v1) (val_to_ptr v2))))).

  Definition IsEmptyM :=
    Build_Method nil (cread "lst" "this" "head") (eeq_ptr "lst" null).

  Definition AddM :=
    Build_Method ("n" :: nil)
      (cseq (calloc "tn" "NodeC")
        (cseq (cwrite "tn" "val" "n")
          (cseq (cread "lst" "this" "head")
            (cseq (cwrite "tn" "next" "lst")
              (cwrite "this" "head" "tn")))))
      0.

  Definition ReverseM :=
    Build_Method nil
    (cseq (cassign "old" null)
      (cseq (cread "lst" "this" "head")
        (cseq
          (cwhile (enot (eeq_ptr "lst" null))
            (cseq (cread "new" "lst" "next")
              (cseq (cwrite "lst" "next" "old")
                (cseq (cassign "old" "lst")
                  (cassign "lst" "new")))))
          (cwrite "this" "head" "old")))) 0.

  Definition HeadM :=
    Build_Method nil
      (cseq (cread "lst" "this" "head")
        (cread "hd" "lst" "val")) "hd".

  Definition ListC :=
    Build_Class (SS.singleton "head")
      (SM.add "isEmpty" IsEmptyM (SM.add "add" AddM (SM.add "head" HeadM
        (SM.add "reverse" ReverseM (SM.empty _))))).

  Definition Prog := Build_Program
    (SM.add "List" ListC (SM'.singleton "NodeC" NodeC)).

  Definition unique_names := option_proof (search_unique_names Prog).
  Opaque unique_names.

End Lists.

Module Lists_Spec.

  Import Lists.
  Module Import SC := Rules Lists.

  Fixpoint Node_list (p : ptr) (xs : list Z) : upred heap_alg :=
    match xs with
      | nil     => (<pure> (p = null))%upred
      | x :: xs => xist_up (\v:ptr, pointsto_up p "val" x <*>
        pointsto_up p "next" v <*> Node_list v xs)%upred
    end.

  Definition List_rep (p : ptr) (xs : list Z) : hasn :=
    sm_const (xist_up
      (\head:ptr, pointsto_up p "head" head <*> Node_list head xs)%upred).

  Definition isEmpty_spec (ListRep : ptr -> list Z -> hasn) : spec :=
    ([A] p, [A] xs, "List" :.: "isEmpty" |-> []
      {{ ListRep p xs </\> "this" == p }}-{{ "r", ListRep p xs </\> "r" ==
        (vbool (match xs with [] => true | _ => false end)) }})%asn%spec.

  Definition add_spec (ListRep : ptr -> list Z -> hasn) : spec :=
    ([A] xs, all_spec (\x:Z, [A] p, "List" :.: "add" |-> (["n":var])
      {{ ListRep p xs </\> "this" == p </\> "n" == x }}-{{ "",
        ListRep p (x :: xs)}}))%asn%spec.

  Definition head_spec (ListRep : ptr -> list Z -> hasn) : spec :=
    ([A] xs, [A] x, [A] p, "List" :.: "head" |-> []
      {{ ListRep p (x :: xs) </\> "this" == p }}-{{ "r",
         ListRep p (x :: xs) </\> "r" == x }})%asn%spec.

  Definition reverse_spec (ListRep : ptr -> list Z -> hasn) : spec :=
    ([A] xs, [A] p, "List" :.: "reverse" |-> []
      {{ListRep p xs </\> "this" == p}}-{{"", ListRep p (rev xs)}})%asn%spec.

  Definition List_spec : spec :=
    ([E] LR, isEmpty_spec LR [/\] add_spec LR [/\] head_spec LR [/\]
      reverse_spec LR)%spec.

  Open Scope spec_scope.
  Open Scope asn_scope.

  Lemma valid_isEmpty_List_rep : |= isEmpty_spec List_rep.
  Proof.
    unfold isEmpty_spec.
    spec_all p; spec_all xs.
    unfold_method_spec.
    unfold List_rep; rewrite const_xist, distr_xist_and_asnL,
      <- exists_into_precond_c, all_specEIR; intros head.
    forward.
    - instantiate (1 := head); rewrite and_asnC; asn_unify.
      rewrite const_sc; apply sc_asnEL; unentail.
    rewrite distr_xist_and_asnL; apply xist_asnIR; exists head; substitution.
    asn_solve.
    rewrite const_sc; apply and_asnEL; apply sc_asnER.
    destruct xs; simpl.
    - rewrite const_pure; unentail.
      intros HEq; subst; reflexivity.
    rewrite const_xist, xist_asnEIL; intros v; rewrite const_sc; apply sc_asnEL.
    unfold pointsto_up; rewrite const_and, const_pure; apply and_asnER.
    unentail; simpl; intro HNeq.
    destruct (natstring_DT.eq_dec head null); [contradiction | reflexivity].
  Qed.

  Lemma valid_add_List_rep : |= add_spec List_rep.
    unfold add_spec; spec_all xs; spec_all x; spec_all p; unfold_method_spec.
    forward.
    - rewrite true_asn_unitL; apply rule_frame.
      - intros v HF; apply modifies_syn_sem; simpl in *; intuition; subst;
        rewrite SS'.singleton_iff in *; discriminate.
      apply rule_alloc_ax with (fields := c_fields NodeC); red; simpl; SM'.simp.
      eexists; intuition try discriminate.
    simpl.
    rewrite distr_xist_sc_asnL, <- exists_into_precond_c; spec_all np.
    repeat rewrite SS'.fold_add with (eqA := @bientails_asn _);
      try rewrite SS'.fold_empty; eauto using sc_asnCA with typeclass_instances.
    forward; [apply rule_write_frame with (e := null:expr) |].
    - rewrite and_asnER with (p := "tn" == np); [| apply and_asnI; reflexivity].
      rewrite sc_asnC with (q := <true>), true_asn_unitR.
      do 2 rewrite <- and_asnA; do 4 rewrite pure_and_sc_asnR.
      rewrite sc_asnAC with (p := pointsto np "val" null), sc_asnA, sc_asnA,
        sc_asnC; apply sc_asnME; [reflexivity |].
      rewrite <- pure_and_sc_asnR, and_asnC; asn_unify; unentail.
    rewrite sc_asnC, <- sc_asnA, <- sc_asnA, sc_asnC; unfold List_rep;
      rewrite const_xist; do 3 rewrite distr_xist_sc_asnL;
        rewrite <- exists_into_precond_c, all_specEIR; intros head.
    rewrite sc_asnC; forward; [ eapply rule_read_fwd with (e := head : expr) |].
    - apply sc_asnER; do 2 (rewrite <- pure_and_sc_asnR, and_asnC; asn_unify).
      rewrite const_sc; apply sc_asnEL; unentail.
    rewrite <- exists_into_precond_c; spec_all v.
    forward; [apply rule_write_frame with (e := null:expr) |].
    - substitution; rewrite sc_asnC, and_asnC, pure_and_sc_asnR, sc_asnC.
      rewrite and_asnI with (p := "tn" == np); [| reflexivity | reflexivity].
      rewrite pure_and_sc_asnR, sc_asnCA with (q := "tn" == np).
      do 5 rewrite <- sc_asnA; rewrite sc_asnA; apply sc_asnME; [reflexivity |].
      rewrite <- pure_and_sc_asnR, and_asnC; asn_unify; reflexivity.
    eapply roc_post; [apply rule_write_frame with (e := head:expr) |].
    - rewrite const_sc, sc_asnAC with (r := "this" == p), sc_asnCA.
      rewrite and_asnI with (p := "this" == p);
        [rewrite pure_and_sc_asnR | reflexivity | reflexivity].
      do 6 rewrite sc_asnA; rewrite <- sc_asnA, sc_asnC; apply sc_asnME;
        [reflexivity | rewrite <- pure_and_sc_asnR, and_asnC].
      asn_unify; unentail.
    rewrite sc_asnA, sc_asnC, <- pure_and_sc_asnR, and_asnC; asn_unify.
    rewrite sc_asnA, sc_asnA, sc_asnC, <- pure_and_sc_asnR, and_asnC; asn_unify.
    rewrite sc_asnA, sc_asnCA, sc_asnC, <- pure_and_sc_asnR, and_asnC; asn_unify.
    rewrite sc_asnA, sc_asnCA, sc_asnME with (r := "tn" == np); [| reflexivity
      | reflexivity].
    rewrite sc_asnC, <- pure_and_sc_asnR, and_asnC; asn_unify.
    rewrite const_xist; apply xist_asnIR; exists np; rewrite const_sc.
    rewrite sc_asnC with (q := pointsto p "head" np), sc_asnCA; apply sc_asnME;
      [unentail |].
    rewrite const_xist; apply xist_asnIR; exists head; do 2 rewrite const_sc.
    rewrite sc_asnC, sc_asnA; unentail.
    (* boring properties *)
    - intros u v HEq h h' HB; subst; rewrite HB; reflexivity.
    - rewrite SS'.empty_iff; tauto.
    - intros u v HEq h h' HB; subst; rewrite HB; reflexivity.
    - rewrite SS'.add_iff, SS'.empty_iff; intuition discriminate.
  Qed.

  Lemma valid_head_List_rep : |= head_spec List_rep.
  Proof.
    unfold head_spec.
    spec_all xs; spec_all x; spec_all p; unfold_method_spec.
    rewrite const_xist, distr_xist_and_asnL, <- exists_into_precond_c,
      all_specEIR; intros head.
    forward; [ eapply rule_read_fwd with (e := head : expr) |].
    - rewrite and_asnC, const_sc; asn_unify.
      apply sc_asnEL; unentail.
    rewrite <- exists_into_precond_c, all_specEIR; intros head'.
    spec_substitution; clear head'.
    forward.
    - instantiate (1 := x); asn_unify; apply and_asnEL.
      rewrite const_sc, const_xist; apply sc_asnER; apply xist_asnEIL;
        intros nxt; rewrite const_sc; apply sc_asnEL; unentail.
    apply and_asnI; [| rewrite true_asnR; substitution; unentail].
    apply xist_asnIR; exists head; asn_unify; asn_solve.
  Qed.    

  Section Reversal_proof.

    Definition rev_inv xs : hasn := <E> q, <E> ys, <E> r, <E> zs,
      sm_const (Node_list q ys) <*> sm_const (Node_list r zs) </\>
      "old" == q </\> "lst" == r </\> <pure> (Prop_pure (xs = app (rev ys) zs)).

    Lemma reverse_body xs :
      |= c_triple (rev_inv xs </\> <pure> (enot (eeq_ptr "lst" null)))
        (rev_inv xs)
        (cseq (cread "new" "lst" "next")
          (cseq (cwrite "lst" "next" "old")
            (cseq (cassign "old" "lst")
              (cassign "lst" "new")))).
    Proof.
      unfold rev_inv at 1; unfold valid.
      repeat (setoid_rewrite distr_xist_and_asnL;
        setoid_rewrite <- exists_into_precond_c; setoid_rewrite all_specEIR).
      intros q ys r zs; destruct zs; simpl.
      - eapply roc_pre with <false>; [| apply c_triple_false].
        rewrite const_pure, <- pure_and_sc_asnR, and_asnER with
          (q := sm_const (Node_list q ys)); [| reflexivity].
        unentail; intuition; subst; simpl in *; discriminate.
      rewrite const_xist, sc_asnC, distr_xist_sc_asnL;
        repeat rewrite distr_xist_and_asnL; rewrite <- exists_into_precond_c.
      spec_all nxt; rewrite and_asnEL; [| reflexivity].
      forward; [apply rule_read_fwd with (e := nxt:expr) |].
      - repeat rewrite <- and_asnA; apply and_asnEL.
        rewrite and_asnC; asn_unify; apply and_asnEL.
        repeat rewrite const_sc; rewrite <- sc_asnCA; do 2 apply sc_asnEL;
          unentail.
      forward; [apply rule_write_frame with (e := nxt:expr) |].
      - apply xist_asnEIL; intro v; substitution; clear v.
        fold (<pure> Prop_pure (xs = app (rev ys) (z :: zs)) : hasn).
        do 2 rewrite const_sc.
        rewrite <- sc_asnCA, sc_asnC, <- sc_asnCA, and_asnC, sc_asnC.
        repeat rewrite <- and_asnA; repeat rewrite pure_and_sc_asnR.
        do 2 rewrite sc_asnAC with
          (q := <pure> (("lst":expr) ·=· sm_const (r : val))).
        rewrite and_asnI with (p := "lst" == r); [| reflexivity | reflexivity ].
        do 4 rewrite sc_asnAC with (q := sm_const (pointsto_up r "next" nxt)).
        rewrite pure_and_sc_asnR, <- sc_asnA, sc_asnA.
        repeat rewrite <- pure_and_sc_asnR; apply sc_asnME; [reflexivity |].
        rewrite sc_asnC, <- pure_and_sc_asnR, and_asnC; asn_unify; unentail.
      forward; [apply rule_assign_fwd; reflexivity |].
      rewrite <- exists_into_precond_c; spec_all q'; spec_substitution.
      forward.
      unfold rev_inv; substitution.
      fold (<pure> (("old":expr) ·=· sm_const (x:val)): hasn).
      apply xist_asnIR; exists r; substitution.
      apply xist_asnIR; exists (z :: ys); substitution.
      apply xist_asnIR; exists nxt; substitution.
      apply xist_asnIR; exists zs; substitution; simpl.
      do 2 rewrite const_eq_prop.
      repeat rewrite pure_and_sc_asnR.
      repeat rewrite <- sc_asnA.
      rewrite <- pure_and_sc_asnR, and_asnC; asn_unify.
      rewrite sc_asnAC, <- pure_and_sc_asnR; apply Prop_pure_EL; intro H; subst.
      rewrite sc_asnAC, <- pure_and_sc_asnR, and_asnC; asn_unify.
      rewrite sc_asnAC, <- pure_and_sc_asnR; apply Prop_pure_EL; intro H; subst.
      rewrite sc_asnAC, <- pure_and_sc_asnR; apply Prop_pure_EL; intro H; subst.
      repeat rewrite <- pure_and_sc_asnR; asn_solve.
      - rewrite const_xist, distr_xist_sc_asnL; apply xist_asnIR; exists q.
        repeat rewrite const_sc; rewrite sc_asnAC; apply sc_asnME;
          [|reflexivity].
        rewrite sc_asnAC, sc_asnC; apply sc_asnME; [unentail |].
        rewrite sc_asnC; apply sc_asnME; [unentail | reflexivity].
      rewrite true_asnR; unentail; intros _; rewrite <- app_assoc; reflexivity.
    Qed.

    Lemma valid_reverse_List_rep : |= reverse_spec List_rep.
    Proof.
      unfold reverse_spec.
      spec_all xs; spec_all p; unfold_method_spec.
      fold (rev xs).
      forward; [apply rule_assign_fwd |].
      - apply xist_asnEIL; intros v; substitution; clear v; reflexivity.
      unfold List_rep at 1; rewrite const_xist, distr_xist_and_asnL,
        distr_xist_and_asnL, <- exists_into_precond_c, all_specEIR; intros head.
      forward; [apply rule_read_fwd with (e := head : expr)|].
      - apply and_asnEL; rewrite and_asnC, const_sc; asn_unify; apply sc_asnEL;
        unentail.
      rewrite <- exists_into_precond_c; spec_all v; spec_substitution.
      clear v; fold (<pure> (("old" : expr) ·=· sm_const (null : val)) : hasn).
      forward.
      - eapply roc_pre; [| eapply rule_frame with
        (R := (sm_const (pointsto_up p "head" head) </\> "this" == p))].
        - rewrite and_asnC, const_sc; do 3 rewrite pure_and_sc_asnR.
          rewrite sc_asnAC with (r := "this" == p).
          rewrite sc_asnC with (q := sm_const (Node_list head xs)).
          rewrite <- sc_asnAC with (q := "old" == null).
          rewrite <- sc_asnAC with (q := "lst" == head).
          do 3 rewrite <- pure_and_sc_asnR; reflexivity.
        - simpl; intros; intuition; subst; apply modifies_syn_sem; simpl.
          repeat rewrite SS'.union_iff; repeat rewrite SS'.singleton_iff;
            repeat rewrite SS'.empty_iff; intuition discriminate.     
        eapply roc_pre; [|
          eapply rule_while with (P := rev_inv xs); apply reverse_body].
        unfold rev_inv.
        apply xist_asnIR; exists null; apply xist_asnIR; exists (@nil Z);
          apply xist_asnIR; exists head; apply xist_asnIR; exists xs; simpl.
        rewrite and_asnC; asn_unify.
        rewrite and_asnC; asn_unify.
        rewrite sc_asnC, const_pure, <- pure_and_sc_asnR; substitution.
        asn_solve; rewrite true_asnR with (p := sm_const (Node_list head xs));
          unentail.
      unfold rev_inv; repeat (setoid_rewrite distr_xist_and_asnL; setoid_rewrite
        distr_xist_sc_asnL; setoid_rewrite <- exists_into_precond_c).
      spec_all q; spec_all rxs; spec_all n; spec_all emptys.
      destruct emptys; simpl.
      - rewrite app_nil_r.
        eapply roc_post; [eapply rule_write_frame with (head : expr) |].
        - rewrite and_asnI with (p := "this" == p); [| reflexivity | reflexivity].
          rewrite <- and_asnA with (q := "this" == p),
            pure_and_sc_asnR with (Q := ("this" : expr) ·=· (p : expr)),
            <- sc_asnA, <- sc_asnAC.
          apply sc_asnME; [reflexivity |].
          rewrite and_asnC; asn_unify; unentail.
        unfold List_rep; rewrite const_xist; apply xist_asnIR; exists q.
        rewrite sc_asnA, const_sc; repeat rewrite <- and_asnA;
          repeat rewrite pure_and_sc_asnR.
        repeat rewrite sc_asnAC with (q := "old" == q).
        rewrite sc_asnA, sc_asnC with (p := "this" == p), sc_asnC.
        repeat rewrite <- pure_and_sc_asnR; apply sc_asnME.
        - do 2 (rewrite and_asnC; asn_unify); unentail.
        rewrite const_pure, <- pure_and_sc_asnR, and_asnEL
          with (q := <pure> Prop_pure (n = null)); [| reflexivity].
        apply and_asnEL; apply Prop_pure_EL; intros HRev; subst;
          rewrite rev_involutive; apply and_asnEL; reflexivity.
      apply roc_pre with <false>; [| apply c_triple_false].
      apply sc_asnEL.
      rewrite sc_asnER; [| reflexivity].
      rewrite const_xist; repeat rewrite distr_xist_and_asnL;
        apply xist_asnEIL; intro f; rewrite const_sc.
      rewrite sc_asnEL; [| reflexivity].
      unfold pointsto_up; simpl; rewrite const_and, const_pure, and_asnER
        with (p := <pure> Prop_pure (n <> null)); [| reflexivity].
      unentail; intuition; subst; simpl in *.
      destruct (natstring_DT.eq_dec n null); simpl in *;
        [apply H; apply e | discriminate].
    Qed.

  End Reversal_proof.

  Lemma valid_List : |= List_spec.
  Proof.
    unfold List_spec; apply xist_specIR; exists List_rep.
    repeat apply and_specI.
    - apply valid_isEmpty_List_rep.
    - apply valid_add_List_rep.
    - apply valid_head_List_rep.
    - apply valid_reverse_List_rep.
  Qed.

End Lists_Spec.

Require Import SemCmd.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Section Rules.
  Variable S : sep_alg.
  Implicit Types P Q : asn S.
  Implicit Type c : semCmd S.

  Open Scope spec_scope.
  Open Scope asn_scope.

  Section StructuralRules.

    Lemma rule_of_consequence : forall P P' Q Q' c
      (HPre  : P  |- P')
      (HPost : Q' |- Q ),
      {{P'}}c{{Q'}} |= {{P}}c{{Q}}.
    Proof.
      intros; unfold entails_spec; simpl.
      introv HT Hmn Hkm HP.
      destruct (HT _ _ s h Hmn Hkm) as [HSafe HQ]; clear HT;
        [ apply HPre; assumption | split ].
      assumption.
      intros ? ? HSem; apply HPost; auto.
    Qed.

    Definition not_modifies c x : Prop :=
      forall s s' h h' n (HSem : c n s h (Some (s', h'))),
        s x = s' x.

    Lemma frame_rule : forall P Q R c
      (HMod : forall x, free R x -> not_modifies c x),
      {{ P }} c {{ Q }} |= {{ P <*> R }} c {{ Q <*> R }}.
    Proof.
      intros; unfold entails_spec; simpl.
      intros ? HSpec ? ? ? ? Hmn Hkm [h1 [h2 [HSub [HP HR]]]]; split.
      (* safety *)
      destruct (HSpec m k s h Hmn Hkm) as [HS _]; [ | assumption].
      apply up_mono with h1 m; auto.
      rewrite sa_mulC in HSub; eapply sub_opt_mul_r; eassumption.
      (* correctness *)
      intros ? ? HSem.
      remember (h1 · h2)%sa as ht; destruct ht as [ht |]; [| contradiction ].
      destruct HSub as [hs HSub].
      replace (ht · hs)%sa with (Some ht ·· Some hs)%sa in HSub by reflexivity.
      rewrite Heqht, sa_mulA in HSub; clear Heqht ht.
      remember (h2 · hs)%sa as ht; destruct ht as [ht |]; [| contradiction ];
        simpl in HSub.
      destruct (@cmd_frame _ c s s' h h' ht h1 k) as [h3 [HS HSem']].
      - rewrite HSub; simpl; reflexivity.
      - intros l HLe; destruct (HSpec m l s h1 Hmn) as [HS _];
        assumption || omega.
      - assumption.
      exists h3 ht; repeat split.
      - rewrite HS; simpl; reflexivity.
      - destruct (HSpec m k s h1 Hmn Hkm HP) as [_ HC]; auto.
      destruct k as [| k]; simpl; [contradiction (cmd_zero HSem) |].
      apply up_mono with h2 m; [ | omega | ].
      - exists hs; rewrite Heqht; reflexivity.
      erewrite sm_prop; [ eassumption |].
      symmetry; intros x HF; eapply HMod; eassumption.
    Qed.

  End StructuralRules.

  Section PrimitivesRules.

    Lemma skip_rule : forall P, |= {{P}}skip_cmd _{{P}}.
    Proof.
      unfold valid, entails_spec; simpl.
      introv _ Hmn Hkm HP; split.
      intro H; inversion H.
      intros h' s' HSem; inversion HSem; subst; apply up_mono with h' m; auto; omega.
    Qed.

    Lemma seq_rule : forall c1 c2 P Q R,
      ({{P}}c1{{Q}} [/\] {{Q}}c2{{R}}) |= {{P}}seq_cmd c1 c2{{R}}.
    Proof.
      unfold entails_spec; simpl.
      introv [Hc1 Hc2] Hmn Hkm HP; split.
      - intro HFail; inversion HFail; subst; clear HFail.
        - destruct (Hc1 _ n0 s h Hmn) as [HS _];
          [omega | assumption | apply HS; exact H].
        destruct (Hc1 _ n0 s h Hmn) as [_ HC]; [omega | assumption |].
        specialize (HC _ _ H).
        destruct (Hc2 (m - n0) n' s' h') as [HS _]; [omega | omega | |].
        - eapply up_mono; [reflexivity | | eassumption]; omega.
        apply HS; exact H0.
      intros h1 s1 HSem; inversion HSem; subst; clear HSem.
      destruct (Hc1 _ n0 s h Hmn) as [_ HC1]; [omega | assumption |].
      specialize (HC1 _ _ H4).
      destruct (Hc2 (m - n0) n1 s2 h2) as [_ HC2]; [omega | omega | |].
      - eapply up_mono; [reflexivity | | eassumption]; omega.
      eapply up_mono; [reflexivity | | eapply HC2]; [omega | assumption].
    Qed.

    Lemma nondet_rule : forall c1 c2 P Q,
      ({{P}}c1{{Q}} [/\] {{P}}c2{{Q}}) |= {{P}}nondet_cmd c1 c2{{Q}}.
    Proof.
      unfold entails_spec; simpl.
      introv [Hc1 Hc2] Hmn Hkm HP; split.
      - intro HFail; inversion HFail; subst; clear HFail.
        - destruct (Hc1 _ n0 s h Hmn) as [HS _];
          [omega | assumption | apply HS; exact H].
        destruct (Hc2 _ n0 s h Hmn) as [HS _];
          [omega | assumption | apply HS; exact H].
      intros h1 s1 HSem; inversion HSem; subst; clear HSem.
      - destruct (Hc1 _ n0 s h Hmn) as [_ HC]; [omega | assumption |].
        eapply up_mono; [reflexivity | | apply HC; assumption]; omega.
      destruct (Hc2 _ n0 s h Hmn) as [_ HC]; [omega | assumption |].
      eapply up_mono; [reflexivity | | apply HC; assumption]; omega.
    Qed.

    Lemma kleene_rule : forall c P,
      {{P}}c{{P}} |= {{P}} kleene_cmd c {{P}}.
    Proof.
      unfold entails_spec; simpl; introv Hc Hmn Hkm Hp; split.
      - intro HFail; generalize dependent m; remember (@None (stack * S)) as cfg;
        induction HFail; inversion Heqcfg; subst; intros.
        - destruct (Hc m n0 s h Hmn) as [HS _];
          [omega | assumption | apply HS; exact H].
        destruct (Hc m n0 s h Hmn) as [_ HC]; [omega | assumption |].
        apply IHHFail with (m-n0); [reflexivity | omega | omega | ].
        eapply up_mono; [reflexivity | | apply HC; assumption]; omega.
      intros h0 s0 HSem; generalize dependent m; remember (Some (s0, h0)) as cfg;
        induction HSem; inversion Heqcfg; subst; intros.
      - eapply up_mono; [reflexivity | | eassumption]; omega.
      destruct (Hc m n0 s h Hmn) as [_ HC]; [omega | assumption |].
      specialize (HC _ _ H); clear Hp.
      eapply up_mono; [reflexivity | | apply (IHHSem (eq_refl _) (m-n0))];
        try omega.
      eapply up_mono; [reflexivity | | apply HC]; omega.
    Qed.

    Lemma assume_rule : forall P (t : pure),
      |= {{P}} assume_cmd _ t {{P </\> <pure> t}}.
    Proof.
      unfold valid, entails_spec; simpl; introv _ Hmn Hkm HP; split.
      - clear HP Hmn Hkm; induction k; introv HFail; inversion HFail; subst.
      intros h0 s0 HSem; remember (Some (s0, h0)) as cfg; induction HSem;
        inversion Heqcfg; subst; intros.
      split; [| assumption]; apply up_mono with h0 m; auto; omega.
    Qed.

    Lemma assert_rule : forall P p,
      P |- <pure> p ->
      |= {{P}} assert_cmd _ p {{P}}.
    Proof.
      unfold valid, entails_spec; simpl; introv HPe _ Hmn Hkm HP;
        specialize (HPe _ _ _ HP); simpl in HPe; split.
      - intro HF; inversion HF; subst; auto.
      intros h0 s0 HSem; inversion HSem; subst; eapply up_mono;
        [reflexivity | | eassumption]; omega.
    Qed.

  End PrimitivesRules.

  Section LaterRules.

    Lemma later_triple : forall p q c,
      (|> {{p}}c{{q}}) =|= {{<|>> p}} later_cmd c {{q}}.
    Proof.
      unfold bientails_spec, entails_spec. simpl. split.
      - introv H Hmn Hkm Hp.
        destruct (nat_dec k 0).
        - subst. split. 
          - unfold safe; simpl; apply safe_zero.
          introv Hcmd. apply cmd_zero in Hcmd. destruct Hcmd.
        destruct m.
        - intuition.
        specialize (H m (k - 1) s h); destruct H; try omega.
        - simpl in Hp; rewrite <- minus_n_O in Hp; assumption.
        split.
        - unfold safe; simpl; assumption.
        intros. specialize (H0 _ _ H1).
        eapply up_mono; try eassumption; [ reflexivity | omega ].
      introv H Hmn Hkm Hp.
      destruct (nat_dec n 0).
      - assert (k = 0) by omega. subst. split.
        - apply safe_zero.
        introv Hcmd. apply cmd_zero in Hcmd. destruct Hcmd.
      destruct (H (m + 1) (k + 1) s h); try omega.
      - eapply up_mono; try eassumption; [ reflexivity | omega].
      replace k with (k + 1 - 1) by omega. split.
      - unfold safe in H0; apply H0.
      intros. specialize (H1 _ _ H2).
      eapply up_mono; try eassumption; [ reflexivity | omega].
    Qed.

    Lemma later_triple_pre : forall p q c,
      ({{<|>> p}}c{{q}}) |= {{p}}c{{q}}.
    Proof.
      intros; eapply rule_of_consequence;
        [ apply later_entails_asn | reflexivity ].
    Qed.
 
    Lemma later_triple_post : forall p q c,
      ({{p}}c{{q}}) |= {{p}}c{{<|>> q}}.
    Proof.
      intros; apply rule_of_consequence;
        [ reflexivity | apply later_entails_asn ].
    Qed.

    Lemma later_triple_cmd : forall P Q c,
      (({{P}}c{{Q}}) |= {{P}}(later_cmd c){{Q}})%spec.
    Proof.
      unfold entails_spec. simpl. 
      introv H Hmn Hkm Hp.
      specialize (H m (k - 1) s h). destruct H; intuition.
      specialize (H0 _ _ H1).
      eapply up_mono; try eassumption; [ reflexivity | omega ].
    Qed.

  End LaterRules.

  Lemma exists_into_precond {A} (P: A -> asn S) c q :
    ([A]x, {{P x}} c {{q}}) =|= {{<E>x, P x}} c {{q}}.
  Proof.
    unfold triple, bientails_spec, entails_spec. simpl. split; [|eauto].
    introv H Hm Hk [x HP]. eauto.
  Qed.

  Lemma I_precond SP P Q c :
    (SP [->] {{ P }} c {{ Q }}) =|=
    {{ sm_const (FunI _ SP) </\> P }} c {{ Q }} %asn.
  Proof.
    split.
    - unfold entails_spec. simpl. introv H Hmn Hkm [HP Hp].
      edestruct H with (m:=m) (m0:=m) (k:=k); eauto.
    unfold entails_spec. simpl. intros.
    edestruct H with (m:=m0) (k:=k); eauto using spec_dc with arith.
  Qed.

  Close Scope asn_scope.

End Rules.

Add Parametric Morphism S : (@triple S) with signature
  (@entails_asn _) --> (@entails_asn _) ++> eq ==> entails_spec
  as triple_entails_m.
Proof.
  eauto using rule_of_consequence.
Qed.

Add Parametric Morphism S: (@triple S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> eq ==> bientails_spec
  as triple_bientails_m.
Proof.
  intros P P' HP Q Q' HQ c.
  split; (apply rule_of_consequence; [rewrite HP|rewrite HQ]); reflexivity.
Qed.

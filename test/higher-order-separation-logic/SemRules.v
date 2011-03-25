Require Export Semantics.
Require Export SemCmdRules.

Set Implicit Arguments.
Unset Strict Implicit.

Module SemRules (Import module_program: PROGRAM).

  Open Scope asn_scope.

  Module Export Sem := SemContents module_program.

  Definition c_not_modifies c x :=
    forall sc, semantics c sc -> not_modifies sc x.

  Lemma modifies_syn_sem c x :
     ~ SS.In x (modifies c) -> c_not_modifies c x.
  Proof.
    induction c; simpl in *; intros HNM; intros sc HSem; inversion_clear HSem.
    - rewrite SS'.singleton_iff in HNM; intros s s0 h h0 n HAsgn;
      simpl in *; inversion HAsgn; subst.
      rewrite stack_lookup_add2; trivial.
    - intros s s0 h h0 n HSkip; simpl in *; inversion HSkip; subst; trivial.
    - intros s s0 h h0 n HSeq; simpl in *; inversion HSeq; subst.
      transitivity (s2 x).
      - eapply IHc1; [| eassumption | eassumption]; intros HIn; apply HNM;
        rewrite SS'.union_iff; auto.
      eapply IHc2; [| eassumption | eassumption]; intros HIn; apply HNM;
        rewrite SS'.union_iff; auto.
    - intros s s0 h h0 n HND; simpl in *; inversion HND; subst; clear HND.
      - inversion H3; subst.
        apply assume_inv in H5; destruct H5; subst.
        eapply IHc1; [| eassumption | eassumption]; intros HIn; apply HNM;
          rewrite SS'.union_iff; auto.
      inversion H3; subst.
      apply assume_inv in H5; destruct H5; subst.
      eapply IHc2; [| eassumption | eassumption]; intros HIn; apply HNM;
          rewrite SS'.union_iff; auto.
    - intros s s0 h h0 n HKl; simpl in *; inversion HKl; subst; clear HKl.
      apply assume_inv in H5; destruct H5; subst; simpl in *.
      remember (Some (s0, h0)); induction H4; subst;
        [inversion Heqo; trivial | discriminate | discriminate |].
      transitivity (s1 x); simpl in *.
      - inversion H; subst; clear H.
        apply assume_inv in H6; destruct H6; subst.
        eapply IHc; eassumption.
      apply IHkleene_sem; assumption.
    - intros s s0 h h0 n HWr; simpl in *; inversion HWr; subst; reflexivity.
    - rewrite SS'.singleton_iff in HNM; intros s s0 h h0 n HRd; simpl in *;
      inversion HRd; subst; rewrite stack_lookup_add2; trivial.
    - rewrite SS'.singleton_iff in HNM; intros s s0 h h0 n HCl; simpl in *;
      inversion HCl; subst; rewrite stack_lookup_add2; trivial.
    - rewrite SS'.singleton_iff in HNM; intros s s0 h h0 n HCl; simpl in *;
      inversion HCl; subst; rewrite stack_lookup_add2; trivial.
    - intros s s0 h h0 n HAs; inversion HAs; subst; reflexivity.
  Qed.

  Add Morphism c_triple with signature
    (@entails_asn _) --> (@entails_asn _) ++> eq ==> entails_spec
    as c_triple_entails_m.
  Proof.
    intros p p' Hp q q' Hq c.
    unfold c_triple. setoid_rewrite <- Hp. setoid_rewrite Hq.
    reflexivity.
  Qed.

  Add Morphism c_triple with signature
    (@bientails_asn _) ==> (@bientails_asn _) ==> eq ==> bientails_spec
    as c_triple_bientails_m.
  Proof.
  intros P P' HP Q Q' HQ c. split.
  - rewrite HP. rewrite HQ. reflexivity.
  - rewrite <- HP. rewrite <- HQ. reflexivity.
  Qed.
  
  (* This lemma allows turning a theorem about [triple] into a theorem about
    [c_triple]. *)
  Lemma lift_to_c_triple: forall F: (hasn -> hasn -> spec) -> Prop,
    (* The "<->" part actually just has to be "<-", but this works better. *)
    (forall tr tr', (forall p q, tr p q =|= tr' p q)%spec ->
                    (F tr <-> F tr')) ->
    (forall T f, (forall t:T, F (\p,\q, f t p q)) ->
                 F (\p,\q, [A]t:T, f t p q)%spec ) ->
    (forall sc, F (\p,\q, triple p q sc)) ->
    (forall c, F (\p,\q, c_triple p q c)).
  Proof.
    introv Hbientails Hall Htriple.
    assert (HProper:
      Proper (pointwise_relation hasn (
              pointwise_relation hasn bientails_spec) ==> iff) F)
      by exact Hbientails.
    assert (Hall_rel:
      (forall T f, (flip impl)
                   (F (\p,\q, [A]t:T, f t p q))%spec
                   (forall t:T, F (\p,\q, f t p q)))) by exact Hall.
    unfold c_triple. rewrite Hall_rel. intros sc.
    setoid_rewrite pure_alt_impl. rewrite Hall_rel. intro Hsem. apply Htriple.
  Qed.

  Example exists_into_precond_c_alt {A} (P: A -> hasn) c q: (
      ([A]x, c_triple (P x) q c) =|= c_triple (<E>x, P x) q c
    )%asn%spec.
  Proof.
    revert A P q.
    apply (@lift_to_c_triple (\tr:hasn->hasn->spec, (
        forall {A} (P: A -> hasn) q,
        ([A]x, tr (P x) q) =|= tr (<E>x, P x) q
      )%asn%spec)).
    - introv H. setoid_rewrite H. reflexivity.
    - introv H. setoid_rewrite <- H.
      (* The [A]'s commute *)
      unfold bientails_spec, entails_spec. simpl; eauto.
    - eauto using exists_into_precond.
  Qed.

  Lemma exists_into_precond_c {A} (P: A -> hasn) c q : (
      ([A]x, c_triple (P x) q c) =|= c_triple (<E>x, P x) q c
    )%asn%spec.
  Proof.
    unfold c_triple. setoid_rewrite <- exists_into_precond.
    unfold bientails_spec, entails_spec. simpl; eauto.
  Qed.

  Lemma I_precond_c P p q c :
    ((P [->] c_triple p c q) =|=
    c_triple (sm_const (FunI _ P) </\> p) c q) %asn%spec.
  Proof.
    unfold c_triple. setoid_rewrite <- I_precond.
    unfold bientails_spec, entails_spec. simpl; split; intros.
    - edestruct H with (m:=m0) (m1:=m1); eauto; omega.
    - edestruct H with (m0:=m) (m1:=m1); eauto; omega.
  Qed.

  Lemma c_triple_false G c Q :
    (G |= c_triple <false> Q c)%spec.
  Proof.
    intros n; simpl; intuition.
  Qed.

  Lemma rule_call_basic : forall C m ps (es : list expr) (x y r : var) P Q,
    (|> (C :.: m |-> ps {{ P }}-{{ r, Q}}) |= [A] v:val,
      {[ x == v </\> <pure> y ::: C </\> P // zip ("this"%string :: ps)
          ((y:expr) :: es) ]}
      ccall x y m es
      {[Q // zip ("this"%string :: r :: ps)
        ((y:expr)[(v:expr)//x] :: (x:expr) :: map (\e, e[(v:expr)//x]) es) ]})
    %asn%spec.
  Proof.
    unfold method_spec, c_triple in *; intros.
    spec_all v.
    rewrite later_and_spec, later_pure_spec, and_specC.
    eapply impl_specE; [| reflexivity].
    apply pure_impl_specR; intros [HND [HFP HFQ]].
    do 3 (setoid_rewrite later_E_spec; setoid_rewrite xist_specEIL);
      intros ps' c re.
    rewrite later_and_spec, later_pure_spec, and_specC.
    eapply impl_specE; [| reflexivity]; apply pure_impl_specR;
      intros [HML [HLen HNMod]].
    rewrite all_specEIR. intros sc. apply impl_specI. rewrite and_specC.
    rewrite and_specC; eapply impl_specE; [| reflexivity];
      apply pure_impl_specR; intros HSem.
    unfold entails_spec, triple; simpl;
      intros n HLt j k s h HLej HLek [HVc [HClass HP]]; split.

    (* Safety part *)
    inversion HSem. subst x0 y0 m0 es0 sc. rename sc0 into sc.
    intro Hfail. inversion Hfail.
    - subst. apply HLFail in HML. contradiction.
    - subst s0 h0 k C. pose proof (method_lookup_function _ _ _ _ _ HLookup HML) as Hmeq.
      inversion Hmeq. subst. clear Hmeq. clear HLookup.
      destruct HLt with (u:=sc) (m:=n-1) (m0:=j-1) (k:=n0) (h:=h)
        (s := SM.add "this"%string (s y) (zip ps' es :@: s +:+ SM.empty _)) as [HS _];
        eauto; try omega; [].
      apply up_mono with h j; [apply subheap_refl | omega |].
      erewrite sm_prop; [eassumption |].
      rewrite NoDup_cons_iff in HND; destruct HND as [_ HND].
      change (let s0 := zip ("this"%string :: ps') ((y : expr) :: es) :@: s +:+
          SM.empty val in
          agree_on (free P) (zip ps (map var_expr ps') :@: s0 +:+ s0)
            (zip ("this"%string :: ps) ((y : expr) :: es) :@: s +:+ s));
          apply agree_on_pre_call; auto.
      apply unique_names in HML; rewrite NoDup_cons_iff in HML.
      rewrite NoDup_cons_iff; destruct HML as [HNin HML]; split;
          [intro HIn; apply HNin; assumption | clear HNin ].
      assumption.

    (* Postcondition part *)
    inversion HSem. subst x0 y0 m0 es0 sc. rename sc0 into sc.
    clear HSem. rename HSem0 into HSem.
    intros h' s' HSemB; inversion HSemB; subst; clear HSemB.
    assert (HPS := method_lookup_function _ _ _ _ _ HLookup HML); inversion HPS; subst;
      clear HPS HLookup.
    destruct HLt with (u:=sc) (m:=n-1) (m0:=j-1) (k:=n0) (h:=h)
      (s := SM.add "this"%string (s y) (zip ps' es :@: s +:+ SM.empty _)) as [_ HC];
        clear HLt; eauto; try omega.
    - apply up_mono with h j; [apply subheap_refl | omega |].
      erewrite sm_prop; [eassumption |].
      rewrite NoDup_cons_iff in HND; destruct HND as [_ HND].
      change (let s0 := zip ("this"%string :: ps') ((y : expr) :: es) :@: s +:+
        SM.empty val in
        agree_on (free P) (zip ps (map var_expr ps') :@: s0 +:+ s0)
          (zip ("this"%string :: ps) ((y : expr) :: es) :@: s +:+ s));
        apply agree_on_pre_call; auto.
      apply unique_names in HML; rewrite NoDup_cons_iff in HML.
      rewrite NoDup_cons_iff; destruct HML as [HNin HML]; split;
        [intro HIn; apply HNin; assumption | clear HNin ].
      assumption.
    replace (j - 1 - n0) with (j - S n0) in * by omega.
    erewrite sm_prop; [apply HC; eassumption |].
    (* agree_on part *)
    intros z HF; apply HFQ in HF.
    unfold stack_lookup at 1.
    rewrite SM'.add_o; destruct (SM'.eq_dec "this"%string z).
    - apply in_inv in HF; destruct HF as [HEq | HF].
      - rewrite NoDup_cons_iff in HND; destruct HND as [HND _]; subst.
        contradiction HND; simpl; left; reflexivity.
      rewrite stack_lookup_add2 with (y := z).
      rewrite <- stack_lookup_not_in.
      subst; specialize (HNMod "this"%string (in_eq _ _)).
      apply modifies_syn_sem in HNMod.
      specialize (HNMod _ HSem _ _ _ _ _ HSem0);
        rewrite <- HNMod, stack_lookup_add.
      destruct (SM'.eq_dec x y); [subst; rewrite stack_lookup_add |
        repeat rewrite stack_lookup_add2; [| assumption | assumption ]];
          reflexivity.
      (* boring side conditions *)
      repeat rewrite NoDup_cons_iff in HND; destruct HND as [_ [HND _]]; subst;
        assumption.
      rewrite map_length; assumption.
      rewrite NoDup_cons_iff in HND; destruct HND as [HND _]; subst.
      intro HEq; subst; apply HND; left; reflexivity.
      (* end boring side conds *)
    rewrite SM'.add_o; destruct (SM'.eq_dec r z).
    - subst; repeat rewrite stack_lookup_add; reflexivity.
    rewrite stack_lookup_add2; [| assumption ].
    destruct HF as [HF | [HF | HF]]; [contradiction n2 | contradiction n1 |];
      try assumption; fold (In z ps) in HF.
    apply unique_names in HML; clear HFP HFQ HND HP HC; simpl m_params in *.
    assert (HNM := fun x H => modifies_syn_sem (HNMod x H) HSem HSem0);
      clear HNMod HSem0.
    generalize dependent ps'; generalize dependent es.
    induction ps; [contradiction HF |]; intros.
    destruct ps'; [discriminate |]; destruct es; [discriminate |].
    simpl; rewrite SM'.add_o; destruct (SM'.eq_dec a z); [ subst |].
    - rewrite stack_lookup_add, <- HNM, stack_lookup_add2; simpl;
      [ rewrite stack_lookup_add | | auto; fail].
      - apply sm_prop; intros t HF'; destruct (SM'.eq_dec x t); [subst |].
        - rewrite stack_lookup_add; reflexivity.
        repeat rewrite stack_lookup_add2; reflexivity || assumption.
      rewrite NoDup_cons_iff in HML; destruct HML as [HT _].
      intro HEq; subst; apply HT; simpl; auto.
    rewrite stack_lookup_add2; [| assumption]; eapply IHps; clear IHps;
        eauto; [| |].
    - apply in_inv in HF; destruct HF as [HEq | HF];
      [subst; contradiction n3; reflexivity | assumption].
    - rewrite NoDup_cons_iff in HML; destruct HML as [HNin HND].
      rewrite NoDup_cons_iff; split; [| clear HNin].
      - intro HIn; apply HNin; simpl in *; intuition.
      rewrite NoDup_cons_iff in HND; destruct HND as [HNin HND].
      assumption.
    intros t HIn; specialize (HNM t); destruct (SM'.eq_dec "this"%string t);
      [subst; repeat rewrite stack_lookup_add in *; apply HNM; simpl; auto |].
    rewrite stack_lookup_add2 in HNM; [rewrite stack_lookup_add2 |];
      try assumption; simpl in HNM.
    rewrite stack_lookup_add2 in HNM; [apply HNM; destruct HIn; simpl; auto |].
    simpl in HIn; destruct HIn; [contradiction n4; subst; reflexivity |].
    rewrite NoDup_cons_iff in HML; destruct HML as [_ HND].
    inversion HND as [ | ? ? HNIn _]; intros HEq; subst;
      apply HNIn; assumption.
  Qed.

  Lemma xist_from_post U P c (Q : U -> hasn) :
    (([E] x:U, {[ P ]} c {[ Q x ]}) |= {[ P ]} c {[ <E> x, Q x ]})%spec.
  Proof.
    apply xist_specEIL; intros x n HP; simpl in *; intros.
    specialize (HP _ _ H H0 _ _ _ _ H1 H2 H3); intuition eauto.
  Qed.

  Lemma rule_call_old : forall C m ps (es : list expr) (x y r : var) (P Q : hasn),
    (|> (C :.: m |-> ps {{ P }}-{{ r, Q}}) |=
      c_triple (<pure> y ::: C </\> P // zip ("this"%string :: ps)
          ((y:expr) :: es))
        (<E> v:val, Q // zip ("this"%string :: r :: ps)
          ((y:expr)[(v:expr) // x] :: (x:expr) ::
            map (\e, e[(v:expr)//x]) es))
        (ccall x y m es))%asn%spec.
  Proof.
    intros; rewrite existentialize with (x := x).
    rewrite <- exists_into_precond_c; spec_all v.
    rewrite <- xist_from_post; apply xist_specIR; exists v.
    rewrite rule_call_basic; apply all_specEL with v; reflexivity.
  Qed.

  Lemma rule_frame_ax  P Q R c
    (HMod : forall x, free R x -> c_not_modifies c x) : (
    c_triple P Q c |= c_triple (P <*> R)%asn (Q <*> R)%asn c
  )%spec.
  Proof.
    unfold c_triple in *; intros.
    apply all_specEIR. intro sc. apply all_specEL with sc.
    apply pure_impl_specR. intro Hsem. apply pure_impl_specL; [assumption|].
    apply frame_rule. unfold c_not_modifies in HMod. auto.
  Qed.

  Lemma rule_frame P Q R c G
    (HMod : forall x, free R x -> c_not_modifies c x)
    (HPre : (G |= c_triple P Q c)%spec) :
    (G |= c_triple (P <*> R) (Q <*> R) c)%asn%spec.
  Proof.
    intros; rewrite <- rule_frame_ax; assumption.
  Qed.

  Lemma rule_frame_nt x m ps P Q R r: (
    free_in R nil ->
    x :..: m |-> ps {{P}}-{{r,Q}} |-
    x :..: m |-> ps {{P <*> R}}-{{r,Q <*> R}}
  )%asn.
  Proof.
    intros HfreeR. unfold expr_spec, method_spec.
    apply xist_asnEIL. intro C. apply xist_asnIR. exists C.
    apply and_asnI; [apply and_asnEL; reflexivity |]. apply and_asnER.
    assert (forall p q S, p |= q ->
                         (sm_const (FunI S p) |- sm_const (FunI S q))
                         )%asn%spec.
    - clear. introv H. unfold entails_asn, sm_rel, entails_up.
      unfold entails_spec in H. simpl in *. auto.
    apply H. clear H.
    apply and_specI; [apply and_specEL | apply and_specER].
    - unfold entails_spec; simpl. intros _ [Hdup [HP HQ]].
      split; [assumption|]. unfold free_in in *. simpl in *.
      split; (intros y [HyR | [HyP |?]];
          [contradiction (HfreeR _ HyR) | auto | contradiction]).
    apply xist_specEIL. intro ps'. apply xist_specIR. exists ps'.
    apply xist_specEIL. intro c. apply xist_specIR. exists c.
    apply xist_specEIL. intro re. apply xist_specIR. exists re.
    setoid_rewrite (rule_frame_ax (R:=R)) at 1.
    - apply and_specI; [apply and_specEL; reflexivity | apply and_specER].
      assert (forall PQ sub, PQ//sub <*> R -|- (PQ <*> R) // sub)%asn as Hsub.
      - intros. setoid_replace R with (R // sub)
                               using relation (@sm_eq (upred heap_alg)) at 1.
        - symmetry. unfold sc_asn, lift_up_bin. apply eq_sub_bient.
          apply subst_bin_pointwise.
        - unfold sm_eq, sm_bin. simpl. intros s. apply sm_prop.
          unfold agree_on. intros y HyR. contradiction (HfreeR _ HyR).
      repeat rewrite Hsub. reflexivity.
    - intros y HyR. contradiction (HfreeR _ HyR).
  Qed.

  Lemma rule_call_nt m ps (es : list expr) (x y r : var) (P Q : hasn):
    (|= {[ <|>> (y :..: m |-> ps {{ P }}-{{ r, Q}})
      </\> P // zip ("this"%string :: ps) ((y:expr) :: es)]}
        ccall x y m es
        {[ <E> v:val, Q // zip ("this"%string :: r :: ps)
          ((y:expr)[(v:expr) // x] :: (x:expr) ::
            map (fun e => e [(v:expr)//x]) es) ]})%asn%spec.
  Proof.
  unfold expr_spec. rewrite later_E.
  rewrite distr_xist_and_asnL. rewrite <- exists_into_precond_c. unfold valid.
  rewrite all_specEIR. intro C.
  rewrite later_and. rewrite and_asnA. rewrite and_asnC. rewrite and_asnA.
  rewrite later_sm_const. rewrite I_later.
  rewrite <- I_precond_c. apply impl_specI. apply and_specER.
  rewrite and_asnC. rewrite later_pure. apply rule_call_old.
  Qed.

  Lemma rule_seq_ax : forall c1 c2 (P Q R : hasn),
    ((c_triple P Q c1 [/\] c_triple Q R c2) |= c_triple P R (cseq c1 c2))%spec.
  Proof.
    unfold c_triple in *; intros.
    apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem. inversion Hsem. subst.
    rewrite distr_all_and_specL. eapply all_specEL. rewrite and_specC.
    rewrite distr_all_and_specL. eapply all_specEL.
    rewrite distr_impl_and_L. apply pure_impl_specL; [eassumption|].
    rewrite and_specC.
    rewrite distr_impl_and_L. apply pure_impl_specL; [eassumption|].
    apply seq_rule.
  Qed.

  Lemma rule_seq : forall c1 c2 (P Q R : hasn) G
    (Hc1 : (G |= c_triple P Q c1)%spec)
    (Hc2 : (G |= c_triple Q R c2)%spec),
    (G |= c_triple P R (cseq c1 c2))%spec.
  Proof.
    intros; rewrite <- rule_seq_ax; apply and_specI; eassumption.
  Qed.

  Lemma rule_if_ax : forall (e : expr) c1 c2 (P Q : hasn),
    ((c_triple (P </\> <pure> e) Q c1 [/\] c_triple (P </\> <pure> !e) Q c2)
    |= c_triple P Q (cif e c1 c2))%asn%spec.
  Proof.
    unfold c_triple in *; intros.
    apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem. inversion Hsem. subst.
    rewrite distr_all_and_specL. eapply all_specEL. rewrite and_specC.
    rewrite distr_all_and_specL. eapply all_specEL.
    rewrite distr_impl_and_L. apply pure_impl_specL; [eassumption|].
    rewrite and_specC.
    rewrite distr_impl_and_L. apply pure_impl_specL; [eassumption|].
    rewrite and_specC.
    rewrite <- nondet_rule; apply and_specI; rewrite <- seq_rule;
      [apply and_specER | apply and_specEL]; (apply and_specI;
        [rewrite true_specR | reflexivity]; apply assume_rule).
  Qed.

  Lemma rule_if : forall (e : expr) c1 c2 (P Q : hasn) G
    (Hc1 : (G |= c_triple (P </\> <pure> e)%asn Q c1)%spec)
    (Hc2 : (G |= c_triple (P </\> <pure> !e)%asn Q c2)%spec),
    (G |= c_triple P Q (cif e c1 c2))%spec.
  Proof.
    intros; rewrite <- rule_if_ax; apply and_specI; assumption.
  Qed.

  Lemma rule_while_ax : forall (e : expr) c (P : hasn),
    ((c_triple (P </\> (e : hasn)) P c) |=
      c_triple P (P </\> (! e : hasn)) (cwhile e c))%asn%spec.
  Proof.
    unfold c_triple in *; intros.
    apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem. inversion Hsem. subst.
    eapply all_specEL. apply pure_impl_specL; [eassumption|].
    rewrite <- seq_rule; apply and_specI;
      [| rewrite true_specR; apply assume_rule].
    rewrite <- kleene_rule, <- seq_rule; apply and_specI; [| reflexivity].
    rewrite true_specR; apply assume_rule.
  Qed.

  Lemma rule_while : forall (e : expr) c (P : hasn) G
    (Hc : (G |= c_triple (P </\> (e : hasn))%asn P c)%spec),
    (G |= c_triple P (P </\> (!e : hasn))%asn (cwhile e c))%spec.
  Proof.
    intros; rewrite <- rule_while_ax; assumption.
  Qed.

  Lemma rule_assert : forall (e : expr) P G
    (HPe : (P |- <pure> e)%asn),
    (G |= {[ P ]} cassert e {[ P ]})%spec.
  Proof.
    unfold c_triple; intros; spec_all sc.
    apply pure_impl_specR; intro HSem; inversion HSem; subst.
    rewrite true_specR; apply assert_rule.
    rewrite HPe; intros s; simpl.
    destruct (e s); simpl; intros h n; simpl; congruence.
  Qed.

  Lemma roc : forall (P P' Q Q' : hasn) c G
    (HPre  : (P  |- P')%asn)
    (HPost : (Q' |- Q )%asn)
    (Hc    : (G  |= c_triple P' Q' c)%spec),
    (G |= c_triple P Q c)%spec.
  Proof.  
    intros; unfold c_triple in *.
    rewrite all_specEIR in *; intros sc; specialize (Hc sc).
    apply impl_specI; rewrite <- rule_of_consequence;
      [| eassumption | eassumption].
    eapply impl_specE; [| reflexivity]; assumption.
  Qed.

  Lemma roc_pre : forall (P P' Q : hasn) c G
    (HPre : (P |- P')%asn)
    (Hc   : (G |= c_triple P' Q c)%spec),
    (G |= c_triple P Q c)%spec.
  Proof.
    intros; eapply roc; eassumption || reflexivity.
  Qed.

  Lemma roc_post : forall (P Q Q' : hasn) c G
    (Hc    : (G  |= c_triple P Q' c)%spec)
    (HPost : (Q' |- Q)%asn),
    (G |= c_triple P Q c)%spec.
  Proof.
    intros; eapply roc; eassumption || reflexivity.
  Qed.

  Lemma rule_skip_ax P :
    (|= c_triple P P cskip)%spec.
  Proof.
    unfold c_triple. apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem.
    inversion Hsem. subst. apply skip_rule.
  Qed.

  Lemma rule_skip P G :
    (G |= c_triple P P cskip)%spec.
  Proof.
    etransitivity.
    - apply true_specR.
    - apply rule_skip_ax.
  Qed.

  Lemma rule_skip_fwd P Q G
    (HPQ: P |- Q)
  : (G |= c_triple P Q cskip)%spec.
  Proof.
    eapply roc_post; [|eassumption].
    etransitivity.
    - apply true_specR.
    - apply rule_skip_ax.
  Qed.

  Lemma rule_assign : forall x e (Q : hasn) (G : spec),
    (G |= c_triple (Q [e // x]) Q (cassign x e))%spec.
  Proof.
    unfold c_triple in *; intros. apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem. inversion Hsem. subst.
    unfold entails_spec, triple; simpl; split; intros.
    intro HFail; inversion HFail.
    inversion H3; subst; apply up_mono with h' m;
      [apply subheap_refl | omega | assumption].
  Qed.

  Lemma rule_assign_fwd G P Q x e :
    (<E>v:val, P [(v:expr) // x] </\> x == e [(v:expr) // x] |- Q)%asn ->
    (G |= {[ P ]} cassign x e {[ Q ]})%spec.
  Proof.
    intros HEnt; rewrite true_specR.
    eapply roc_pre; [| eapply rule_assign].
    rewrite xist_asnEIL in HEnt; intros s.
    specialize (HEnt (s x) (SM.add x (e s) s)); simpl in *.
    rewrite <- HEnt; clear HEnt.
    apply and_upI.
    - erewrite sm_prop; [reflexivity |]; symmetry.
      etransitivity; [apply agree_on_overwrite | apply agree_on_add_id].
    unfold stack_lookup at 1; rewrite SM'.add_eq_o; [| reflexivity].
    rewrite true_upR.
    erewrite sm_prop at 1.
    intros h n _; reflexivity.
    symmetry; etransitivity; [apply agree_on_overwrite | apply agree_on_add_id].
  Qed.

  Lemma rule_read_fwd : forall (x y : var) f e (P : hasn)
    (HPT : (P |- y · f >> e)%asn),
    (|= {[ P ]} cread x y f
      {[ <E> v:val, x == e [(v:expr)//x] </\> P[(v: expr)//x]]})%asn%spec.
  Proof.
    unfold c_triple in *; intros. apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsem. inversion Hsem. subst.
    unfold entails_spec, triple; simpl; split; intros.
    - intro HFail; inversion HFail; subst; apply Snotin; clear Snotin HFail.
      specialize (HPT _ _ _ H2); unfold pointsto, pointsto_up in HPT;
        simpl in HPT; destruct HPT as [[h' HPT] HNN].
      Lemma in_dot_in : forall (h0 h1 h : heap_alg) k
        (HDot : lift_option_rel PSM.Equal (dot h0 h1) (Some h))
        (HIn  : PSM.In k h0),
        PSM.In k h.
      Proof.
        intros h0 h1; remember (dot h0 h1) as oh; generalize dependent oh;
          unfold dot; apply PSM'.fold_rec_bis; intros; subst.
        setoid_rewrite H in H0; eauto.
        rewrite PSM'.empty_in_iff in HIn; contradiction.
        destruct (monAddSome HDot) as [h2 [HE1 HE2]].
        rewrite PSM'.in_find_iff, PSM'.add_o in HIn.
        destruct (PSM'.eq_dec k k0); [inversion e0; subst |].
        rewrite <- HE2, PSM'.add_in_iff; auto.
        rewrite <- HE2, PSM'.add_in_iff; right; eapply H1; eauto.
        rewrite PSM'.in_find_iff; assumption.
      Qed.
      eapply in_dot_in; eauto.
      unfold PSM'.singleton; rewrite PSM'.add_in_iff; auto.
    inversion H3; subst; clear H3.
    specialize (HPT _ _ _ H2); simpl in HPT; destruct HPT as [[ht HPT] HNN].
    exists (s x); split.
    - rewrite stack_lookup_add, <- (sm_prop _ s).
      unfold sa_mul, sa_op, heap_alg, dot, PSM'.singleton in HPT; simpl in HPT.
      rewrite PSM'.fold_add in HPT; simpl in HPT.
      destruct (PSM.find (val_to_ptr (s y), f) ht);
        [contradiction|]; simpl in HPT;
          rewrite <- HPT, PSM'.find_mapsto_iff, PSM'.add_eq_o in Rmaps;
            [ injection Rmaps; auto | reflexivity ].
      (* boring, again *)
      split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
        [reflexivity | symmetry | etransitivity]; eauto.
      unfold Proper, respectful, flip; simpl; intros.
      apply mon_Add_Eq_m; auto.
      unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
      (* end boring *)
      rewrite PSM'.empty_in_iff; intros HC; contradiction.
      intros z HF; unfold stack_lookup at 2; rewrite SM'.add_o;
        destruct (SM'.eq_dec x z); [inversion e0; subst; reflexivity |].
      rewrite SM'.add_neq_o; [reflexivity | assumption].
    erewrite sm_prop; [eapply up_mono; [reflexivity | | eassumption]; omega |]; eauto.
    intros z HF; unfold stack_lookup at 1; rewrite SM'.add_o;
      destruct (SM'.eq_dec x z); [inversion e0; subst; reflexivity |].
    rewrite SM'.add_neq_o; [reflexivity | assumption].
  Qed.

  Lemma rule_read_fwd2 : forall (x y : var) f e (P Q : hasn) G
    (HPT : P |- y · f >> e)
    (HQT: <E> v:val, x == e[(v:expr)//x] </\> P [(v:expr)//x] |- Q),
    (G |= {[ P ]} cread x y f {[ Q ]})%asn%spec.
  Proof.
    intros. etransitivity; [apply true_specR|].
    eapply roc_post; [| eassumption].
    eapply rule_read_fwd. assumption.
  Qed.

  Lemma rule_read_nofv : forall (x y : var) f e (P : hasn)
    (HPT : (P |- y · f >> e)%asn)
    (HPx : ~ free P x)
    (Hex : ~ free e x),
    (|= {[ P ]} cread x y f {[ x == e </\> P ]})%asn%spec.
  Proof.
    intros.
    eapply roc_post; [eapply rule_read_fwd2 |];
      [ eassumption | reflexivity |].
    apply xist_asnEIL. intro v.
    assert (sm_eq e[(v:expr)//x] e) as He.
    - red. simpl. intros s. symmetry. auto using @sm_prop, agree_on_add.
    assert (sm_eq P[(v:expr)//x] P) as HP.
    - red. simpl. intros s. symmetry. auto using @sm_prop, agree_on_add.
    setoid_rewrite HP at 1.
    setoid_rewrite He at 1.
    reflexivity.
  Qed.

  (* No built-in frame rule *)
  Lemma rule_call_fwd : forall C m ps (es : list expr) (x y r : var) G
  (P Q Pm Qm : hasn), (
    G |= |> C :.: m |-> ps {{ Pm }}-{{ r, Qm }} ->
    P |- <pure> y ::: C </\> Pm // zip ("this"%string :: ps) ((y:expr) :: es) ->
    (<E> v:val, Qm // zip ("this"%string :: r :: ps)
          ((y:expr)[(v:expr) // x] :: (x:expr) ::
            map (\e, e[(v:expr)//x]) es)) |- Q ->
    G |= c_triple P Q (ccall x y m es)
  )%asn%spec.
  Proof.
    introv Hmspec HP HQ. eapply roc; try eassumption; [].
    etransitivity; [eassumption|]. apply rule_call_old.
  Qed.

  Lemma sc_idempotent_pure S (P : pure) :
    <pure> P <*> <pure> P -|- (<pure> P : asn S).
  Proof.
    rewrite <- pure_and_sc_asnL; apply and_idempotent_asn.
  Qed.

  Lemma rule_call_complete : forall C m ps (es : list expr) (x y r : var) G
  (P Q F Pm Qm : hasn), (
    G |= |> C :.: m |-> ps {{ Pm }}-{{ r, Qm }} ->
    P |- (<pure> y ::: C </\> Pm // zip ("this"%string :: ps) ((y:expr) :: es))
      <*> F ->
    <E> v:val, <pure> (y:expr)[(v:expr)//x] ::: C </\>
      Qm // zip ("this"%string :: r :: ps) ((y:expr)[(v:expr)//x] :: (x:expr) ::
        map (\e, e[(v:expr)//x]) es) <*> F[(v:expr)//x] |- Q ->
    G |= {[ P ]} ccall x y m es {[ Q ]}
  )%asn%spec.
  Proof.
    introv HC HP HQ; rewrite <- HQ, HP, HC.
    rewrite existentialize with (x := x), <- exists_into_precond_c; spec_all v.
    rewrite <- xist_from_post; apply xist_specIR; exists v.
    eapply roc_pre.
    - rewrite pure_and_sc_asnL with (P := y:::C),
      <- sc_idempotent_pure with (P:= y:::C), sc_asnAC with (q:= <pure> y:::C),
      <- pure_and_sc_asnL, sc_asnA.
      rewrite pure_and_sc_asnL, <- sc_idempotent_pure, <- sc_asnA,
        sc_asnAC with (p := x == v), <- pure_and_sc_asnL, sc_asnA.
      apply sc_asnME; [reflexivity |].
      rewrite <- pure_and_sc_asnL; apply var_eq_substL.
    eapply roc_post; [apply rule_frame |].
    - intros z [[v' [e HF]] | HF]; apply modifies_syn_sem; simpl in *;
      rewrite SS'.singleton_iff; intro HEq; subst; intuition.
      - inversion H0; subst; contradiction.
      - inversion H0; subst; contradiction.
    - rewrite rule_call_basic with (x := x) (y := y); apply all_specEL with v;
      reflexivity.
    rewrite pure_and_sc_asnL, sc_asnCA; apply sc_asnME; [reflexivity |].
    substitution; reflexivity.
  Qed.

  Lemma rule_alloc_ax : forall (x : var) C fields,
    field_lookup C fields ->
    (|= {[ <true> ]} calloc x C {[ <E> p:ptr,
      SS.fold (fun f Q => p · f >> null <*> Q) fields
        <true> </\> <pure> x ::: C </\> x == p]})%spec%asn.
  Proof.
    intros ? ? ? HLU.
    unfold c_triple; spec_all sc.
    apply pure_impl_specR; intros HSem; inversion HSem; subst; clear HSem.
    intros n _; unfold triple; simpl; introv Hm Hk _; split.
    - intro HS; inversion HS.
    intros h0 s0 HSem; inversion HSem; subst; clear HSem; exists ref.
    rewrite stack_lookup_add; split; [| split; reflexivity].
    assert (fields0 = fields) by (eapply field_lookup_function; eauto); subst.
    clear Sfields HLU.
    strengthen (forall f, PSM.In (ref, f) h0 -> In f (SS.elements fields)).
    rewrite SS.fold_1 in *; assert (HDup := SS.elements_3w fields).
    remember (SS.elements fields) as l; clear Heql.
    generalize dependent h0; induction l as [|f fs] using rev_ind; intros.
    - rewrite sa_mulC in Sh0; simpl in *; setoid_rewrite <- Sh0 in Sfresh_h;
      intuition.
    apply NoDupA_swap in HDup; [| auto]; rewrite <- app_nil_end in HDup.
    rewrite fold_left_app in *; simpl fold_left in *.
    remember (fold_left (\a:PSM.t val, \e:SS.elt, PSM.add (ref, e) (null:val) a)
      fs (PSM.empty val)) as hh.
    unfold sa_mul in Sh0; simpl sa_op in Sh0; unfold dot in Sh0.
    rewrite <- NIn_monAdd_Some in Sh0. rewrite PSM'.fold_commutes in Sh0.
    replace (PSM.fold monAdd h (Some hh)) with ((dot (h:heap_alg) hh)%sa) in Sh0;
      [| reflexivity].
    symmetry in Sh0; apply monAddSome in Sh0; destruct Sh0 as [hr [HEq1 HEq2]].
    inversion_clear HDup; symmetry in HEq1.
    specialize (IHfs H0 _ HEq1).
    simpl; destruct IHfs as [HHeap HIn]; split.
    - exists (PSM'.singleton (ref, f) (null:val)) hr; repeat split.
      assert (@sa_eq_l heap_alg (dot (PSM'.singleton (ref, f) (null:val) : heap_alg) hr)
        (Some h0))%sa as HQ; [| rewrite HQ; eexists; apply sa_unitI].
        unfold PSM'.singleton, sa_mul; simpl; unfold dot; simpl.
        rewrite PSM'.fold_add; simpl.
        - destruct (PSM'.In_dec hr (ref, f)).
          - apply HIn in i; contradiction H; apply In_InA; [|assumption].
            split; [intro; reflexivity | intros ? ? ?; symmetry; assumption
              | intros ? ? ? ? ?; etransitivity; eassumption].
          rewrite PSM'.not_find_in_iff in n0; rewrite n0, HEq2; reflexivity.
        (* boring, again *)
        split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
          [reflexivity | symmetry | transitivity y]; auto.
        unfold Proper, respectful, flip; simpl; intros.
        apply mon_Add_Eq_m; auto.
        unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
        (* end boring *)
        rewrite PSM'.empty_in_iff; tauto.
      - exists ((@sa_unit _):heap_alg)%sa; apply sa_unitI.
      - assumption.
      assumption.
    intros ff HffIn.
    rewrite <- HEq2 in HffIn.
    rewrite PSM'.add_in_iff in HffIn.
    apply in_or_app; destruct HffIn; [| left; apply HIn; assumption].
    inversion H1; subst; right; intuition.
    (* boring stuff *)
    - split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
      [reflexivity | symmetry | transitivity y]; auto.
    - unfold Proper, respectful, flip; simpl; intros.
    - apply mon_Add_Eq_m; auto.
      unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
    (* end boring *)
    - apply Sfresh_h.
    - inversion_clear HDup; subst hh; intro HIn.
      apply H; apply In_InA; [auto with typeclass_instances |].
      clear H H0 IHfs Sh0.
      induction fs using rev_ind; simpl in *.
      - rewrite PSM'.empty_in_iff in HIn; assumption.
      rewrite fold_left_app in HIn; simpl in *; rewrite PSM'.add_in_iff in HIn;
        destruct HIn as [HL | HIn]; [inversion HL; subst |]; apply in_or_app.
      - simpl; auto.
      left; apply IHfs; exact HIn.
  Qed.

  Lemma rule_alloc_fwd : forall (x : var) C fields P G,
    field_lookup C fields ->
    (G |= {[ P ]} calloc x C {[ (<E>v:val, P[(v:expr)//x]) <*> (<pure> x ::: C
      </\> SS.fold (fun f Q => x · f >> null <*> Q) fields <true>) ]})%spec%asn.
  Proof.
    introv HLU. rewrite sc_asnC at 1.
    apply roc_pre with (<true> <*> <E>v:val, P[(v:expr)//x]).
    - rewrite <- true_asn_unitL.
      apply existentialize_subst.
    apply rule_frame.
    - intros x' Hfree. simpl in Hfree. apply modifies_syn_sem. simpl.
      intro HIn. apply SS'.singleton_iff in HIn.
      destruct Hfree as [v [[y [e HF]] | HF]]; intuition.
      inversion H0; subst; contradiction.
    apply roc_post with (
        xist_asn (\p:ptr, SS.fold (fun f Q => p · f >> null <*> Q) fields
        <true> </\> <pure> x ::: C </\> x == p)
      ).
    - etransitivity.
      - apply true_specR.
      - apply rule_alloc_ax; assumption.
    apply xist_asnEIL. intro p. rewrite and_asnC at 1. rewrite and_asnA at 1.
    apply and_asnI.
    - apply and_asnEL. reflexivity.
    - apply and_asnER. apply SS'.fold_rel with (R := \fold1, \fold2,
        <pure> (var_expr x ·=· val_expr (vptr p)) </\> fold1 |- fold2).
      - apply true_asnR.
      - intros f R R' HIn HR. rewrite <- HR.
        simpl. eapply unify; [reflexivity|]. substitution.
        destruct (string_dec x x); [|congruence]. simpl. substitution.
        apply sc_asnME; [reflexivity|].
        substitution. apply and_asnI; [|reflexivity].
        unfold entails_asn, sm_rel, entails_up. simpl. reflexivity.
  Qed.

  Lemma rule_write : forall (x : var) f e e',
    (|= {[ x · f >> e ]} cwrite x f e' {[ x · f >> e']})%spec.
  Proof.
    intros. unfold c_triple. apply all_specEIR. intro sc.
    apply pure_impl_specR. intro Hsemantics.
    inversion Hsemantics. subst. clear Hsemantics.
    intros n _. unfold triple; simpl. introv Hm Hk [HSub Hnotnull].
    assert (PSM.MapsTo (val_to_ptr (s x), f) (e s) h) as Hh.
    - apply heap_sub_alt in HSub. red in HSub. apply HSub. PSM'.simp. auto.
    clear HSub.
    split.
    - intro Hfail. inversion Hfail. subst. firstorder.
    - introv Hsem. inversion Hsem. subst. split; [|assumption].
      apply heap_sub_alt. unfold PSM'.Sub. PSM'.simp. firstorder.
  Qed.

  Lemma rule_write_frame G P Q (x : var) f e e' :
    (P |- Q <*> x · f >> e)%asn ->
    (G |= {[ P ]} cwrite x f e' {[ Q <*> x · f >> e']})%spec%asn.
  Proof.
    intros HP; rewrite HP.
    rewrite sc_asnC; eapply roc_post;
      [eapply rule_frame | rewrite sc_asnC; reflexivity].
    - intros y _; apply modifies_syn_sem; simpl; rewrite SS'.empty_iff;
      tauto.
    rewrite true_specR; apply rule_write.
  Qed.

  Ltac solve_free :=
    match goal with
      | |- free_in ?f _ => 
        let r1 := gethead f in 
          unfold free_in, r1; simpl; intros; destruct_conjs; intuition
    end.

  Ltac asn_unify :=
    match goal with
      | |- pure_hasn ((var_expr ?va) ·=· ?ea) </\> ?ra |- ?p =>
        apply (@unify heap_alg) with (v:=va) (e:=ea) (r:=ra);
          [reflexivity|substitution]
      | |- pure_asn ?S ((var_expr ?va) ·=· ?ea) </\> ?ra |- ?p =>
        apply (@unify S) with (v:=va) (e:=ea) (r:=ra);
          [reflexivity|substitution]
      | |- pure_asn ?S ((var_expr ?va) ·=· ?ea) |- ?p =>
        transitivity (((var_expr va) ·=· ea) </\> <true>);
          [apply and_asnI; [reflexivity; apply true_asnR] | asn_unify]
    end.

  Ltac lob :=
    apply lob; [intros _| apply and_specER].
  
  Ltac spec_substitution :=
    match goal with 
      | |- (?r |= c_triple ?p ?q ?c)%spec =>
        eapply roc; [
          substitution; reflexivity |
          substitution; reflexivity |
        ]
    end.

  Lemma roc_mspec : forall C m_name ps G (P P' Q Q' : hasn) (r : var)
    (HPre  : (P  |- P')%asn)
    (HFP   : forall x, free P x -> free P' x)
    (HPost : (Q' |- Q)%asn)
    (HFQ   : forall x, free Q x -> free Q' x)
    (HMS   : (G  |= C :.: m_name |-> ps {{ P' }}-{{ r, Q' }})%spec),
    (G |= C :.: m_name |-> ps {{ P }}-{{ r , Q }})%spec.
  Proof.
    unfold method_spec; intros.
    rewrite HMS; clear HMS; apply and_specI;
      [apply and_specEL | apply and_specER].
    - intros n; simpl; clear n; unfold free_in; intuition.
    apply xist_specEIL; intros ps'; apply xist_specEIL; intros c;
      apply xist_specEIL; intros re; apply xist_specIR; exists ps';
        apply xist_specIR; exists c; apply xist_specIR; exists re.
    apply and_specI; [apply and_specEL; reflexivity | apply and_specER].
    eapply roc; [| | reflexivity]; intro s; simpl; [apply HPre | apply HPost].
  Qed.
    
  Lemma frame_rule_mspec : forall G C m_name ps P Q R r
    (HFree : free_in R ("this"%string :: ps))
    (HMS   : (G  |= C :.: m_name |-> ps {{ P }}-{{ r, Q }})%spec),
    (G |= C :.: m_name |-> ps {{ P <*> R }}-{{ r , Q <*> R }})%asn%spec.
  Proof.
    unfold method_spec; intros.
    rewrite HMS; clear HMS; apply and_specI; [apply and_specEL |].
    - intros n; simpl; clear n; unfold free_in in *; simpl; intuition.
    rewrite and_specC; eapply impl_specE; [| reflexivity];
      apply pure_impl_specR; intros [HND [HFP HFQ]].
    apply xist_specEIL; intros ps'; apply xist_specEIL; intros c;
      apply xist_specEIL; intros re; apply xist_specIR; exists ps';
        apply xist_specIR; exists c; apply xist_specIR; exists re.
    apply and_specI; [apply and_specEL; reflexivity |].
    rewrite and_specC; eapply impl_specE; [| reflexivity];
      apply pure_impl_specR; intros [HML [HLen HNMod]].
    spec_substitution.
    inversion_clear HND as [|? ? HNIn HF].
    assert (HNr : ~free R r) by (intros HFr; apply HNIn; exact (HFree _ HFr)).
    rewrite (subst_expr_cons_ineq _ re HNr).
    assert (HUN := unique_names C m_name _ HML); simpl in HML.
    apply rule_frame_ax.
    clear HNr HNIn; intros x HFR; apply modifies_syn_sem.
    apply HNMod.
    destruct (string_dec "this"%string x); [left; assumption |]; right.
    assert (HT : exists qs, free_in R (qs ++ ps) /\ ~ In x qs).
    - exists ["this"%string]; split; simpl; [apply HFree |]; intuition congruence.
    clear HFP HFQ HF HUN HML HFree HNMod n.
    generalize dependent ps'; induction ps; destruct ps'; intros;
      inversion HLen; subst; simpl in *.
    - destruct HFR as [[v [e [HF _]]] | [_ HFR]]; [contradiction |].
      destruct HT as [qs [HF HNIn]]; rewrite app_nil_r in HF; apply HF in HFR;
        contradiction.
    destruct (string_dec v x); [left; assumption |]; right.
    destruct HT as [qs [HFIn HNIn]].
    destruct HFR as [[v0 [e [HIn [HFR HFe]]]] | [HNIn' HFR]].
    - destruct HIn as [HEq | HIn]; [inversion HEq; subst |].
      - simpl in HFe; congruence.
      apply in_zip in HIn; [| rewrite map_length; assumption].
      apply proj2 in HIn; rewrite in_map_iff in HIn;
        destruct HIn as [q [HEq HIn]]; subst; simpl in HFe; subst; assumption.
    apply IHps; [| assumption |].
    - exists (qs ++ [a]); split; [rewrite <- app_assoc; assumption |].
      intros HIn; apply in_app_or in HIn; destruct HIn as [HC | [HE | HC]];
        [contradiction | intuition | contradiction].
    right; split; [|assumption].
    intros HIn; apply HNIn'; right; assumption.
  Qed.

  Ltac check_not_modifies :=
    let x := fresh "y" in let HC := fresh "HC" in let HIn := fresh "HIn" in
      intros x HC HIn;
        repeat match goal with
                 | [ HIn: context [SS.In x (SS.union ?S1 ?S2)] |- _] =>
                   rewrite SS'.union_iff in HIn
                 | [ HIn: context [SS.In x (SS.add ?e ?S)] |- _] =>
                   rewrite SS'.add_iff in HIn
                 | [ HIn: context [SS.In x (SS.singleton ?e)] |- _] =>
                   rewrite SS'.singleton_iff in HIn
                 | [ HIn: context [SS.In x SS.empty] |- _] =>
                   rewrite SS'.empty_iff in HIn
               end; intuition (subst; discriminate).

  Ltac unfold_method_spec :=
    apply and_specI; [
      apply pure_specIR; split; [
        search_NoDup string_dec|split; solve_free
      ] |
      do 3 spec_eexists; spec_split; [
        apply pure_specIR; split; [
          eexists; split; SM'.mapsto_tac
          | simpl; split; [reflexivity | check_not_modifies]
        ] | spec_substitution
      ]
    ].

  Ltac unfold_method_spec_zero :=
    split; [
      simpl; split; [
        search_NoDup string_dec|split; solve_free
      ] |
      do 3 (reduce; eexists); split; [
        split; [
          eexists; split; SM'.mapsto_tac
          | simpl; split; [reflexivity | check_not_modifies]
        ] | apply c_triple_zero
      ]
    ].

  Ltac frame_free :=
    intros; apply modifies_syn_sem; simpl in *; unfold string_var in *; 
      clear_disj; rewrite SS'.singleton_iff; congruence.

  Lemma rule_assign_fwd2 : forall (x : var) e (P Q : hasn) (G : spec)
    (HPT: (<E> v:val, (x == e [(v:expr) // x]) </\> P [(v:expr) // x]) |- Q),
    (G |= {[ P ]} cassign x e {[ Q ]})%asn%spec.
  Proof.
    intros.
    eapply roc_post; [| eassumption].
    eapply rule_assign_fwd.
    eapply xist_asnEIL; intro v; eapply xist_asnIR; exists v; rewrite and_asnC;
      reflexivity.
  Qed.

  Ltac read_forward :=
    eapply rule_read_fwd2; [
      | apply xist_asnEIL; intro; substitution; asn_unify
    ].
      
  Ltac assign_forward :=
    eapply rule_assign_fwd2; [apply xist_asnEIL; intro; substitution; asn_unify].

  Ltac alloc_forward :=
    apply rule_alloc_fwd; eexists; split; [SM'.mapsto_tac | simpl; reflexivity].

  Ltac forward :=
    match goal with
      | |- (_ |= c_triple _ _ cskip)%spec => apply rule_skip_fwd
      | |- (_ |= c_triple _ _ (cread _ _ _))%spec => read_forward
      | |- (_ |= c_triple _ _ (cif _ _ _))%spec => apply rule_if
      | |- (_ |= c_triple _ _ (cseq _ _))%spec => eapply rule_seq
      | |- (_ |= c_triple _ _ (cassign _ _))%spec => assign_forward
      | |- (_ |= c_triple _ _ (calloc _ _))%spec => alloc_forward
      | |- (_ |= c_triple _ _ (ccall _ _ _ _))%spec =>
        eapply roc; [| | apply rule_call_old]; [
          substitution |
            apply xist_asnEIL; intro; substitution
        ]
    end.

  Close Scope asn_scope.

End SemRules.

Module Type RULES (P: PROGRAM).
  Include SemRules P.
End RULES.

Module Rules (P: PROGRAM) : RULES P.
  Include SemRules P.
End Rules.

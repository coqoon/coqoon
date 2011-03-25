Require Export Spec.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Section Definitions.
  Variable S : sep_alg.

  Definition semCmdType := nat -> stack -> S -> option (stack * S) -> Prop.
  Definition safe (cmd : semCmdType) s t n := ~(cmd s t n None).
  Definition frame_property (cmd : semCmdType) :=
    forall s s' (big big' frame h:S) n
      (HFrame : (h · frame == Some big)%sa)
      (HSafe  : forall m, m <= n -> safe cmd m s h)
      (HSem   : cmd n s big (Some (s', big'))),
      exists h', (h' · frame == Some big')%sa /\ cmd n s h (Some (s', h')).

  Record semCmd := {
    cmd_rel   :> nat -> stack -> S -> option (stack * S) -> Prop;
    cmd_zero  :  forall s h cfg, ~(cmd_rel 0 s h cfg);
    cmd_frame :  frame_property cmd_rel}.

  Lemma safe_zero : forall (cmd : semCmd) s t, safe cmd 0 s t.
  Proof.
    intros; apply cmd_zero.
  Qed.

  Program Definition later_cmd (c : semCmd) : semCmd :=
    @Build_semCmd (fun n => c (n - 1)) _ _.
  Next Obligation.
    simpl; exact cmd_zero.
  Qed.
  Next Obligation.
    unfold frame_property; intros.
    destruct n as [| n]; unfold safe in HSafe; simpl in *;
      [contradiction (cmd_zero HSem) | rewrite <- minus_n_O in *].
    eapply cmd_frame; eauto.
    intros m HLe HFail; apply HSafe with (1 + m); [omega | simpl].
    rewrite <- minus_n_O; assumption.
  Qed.

  (* Commands *)

  (* Skip *)
  Inductive skip_sem : semCmdType :=
  | s_ok : forall s h, skip_sem 1 s h (Some (s, h)).
  Program Definition skip_cmd := @Build_semCmd skip_sem _ _.
  Next Obligation.
    intros s h cfg H; inversion H.
  Qed.
  Next Obligation.
    unfold frame_property; intros.
    inversion HSem; subst; clear HSem.
    exists h; auto using skip_sem.
  Qed.

  (* Seq *)
  Inductive seq_sem (c1 c2 : semCmd) : semCmdType :=
  | seq_failL : forall n s h,
      c1 n s h None ->
      seq_sem c1 c2 (1 + n) s h None
  | seq_failR : forall n n' s s' h h',
      c1 n s h (Some (s', h')) ->
      c2 n' s' h' None ->
      seq_sem c1 c2 (1 + n + n') s h None
  | seq_ok    : forall n n0 s s0 s1 h h0 h1,
      c1 n  s  h  (Some (s0, h0)) ->
      c2 n0 s0 h0 (Some (s1, h1)) ->
      seq_sem c1 c2 (1 + n + n0) s h (Some (s1, h1)).
  Program Definition seq_cmd c1 c2 := @Build_semCmd (seq_sem c1 c2) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using seq_sem.
    unfold frame_property; intros.
    inversion HSem; subst; clear HSem.
    edestruct (@cmd_frame c1) as [h0 [HFrame0 HSem0]]...
    - intros m HLe HFail; apply HSafe with (1 + m); [omega | ]...
    edestruct (@cmd_frame c2) as [hr [HFrame1 HSem1]]...
    - intros m HLe HFail; apply HSafe with (1 + n0 + m); [omega | ]...
  Qed.

  (* Nondet *)
  Inductive nondet_sem (c1 c2 : semCmd) : semCmdType :=
  | nondet_failL : forall n s h,
      c1 n s h None ->
      nondet_sem c1 c2 (1 + n) s h None
  | nondet_okL   : forall n s s' h h',
      c1 n s h (Some (s', h')) ->
      nondet_sem c1 c2 (1 + n) s h (Some (s', h'))
  | nondet_failR : forall n s h,
      c2 n s h None ->
      nondet_sem c1 c2 (1 + n) s h None
  | nondet_okR   : forall n s s' h h',
      c2 n s h (Some (s', h')) ->
      nondet_sem c1 c2 (1 + n) s h (Some (s', h')).
  Program Definition nondet_cmd c1 c2 := @Build_semCmd (nondet_sem c1 c2) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using nondet_sem.
    unfold frame_property; intros.
    inversion HSem; subst; clear HSem.
    - edestruct (@cmd_frame c1) as [h0 [HFrame0 HSem0]]...
      intros m HLe HFail; apply HSafe with (1 + m); [omega |]...
    edestruct (@cmd_frame c2) as [h0 [HFrame0 HSem0]]...
    intros m HLe HFail; apply HSafe with (1 + m); [omega |]...
  Qed.

  (* Kleene star *)
  Inductive kleene_sem (c : semCmd) : semCmdType :=
  | kleene_emp       : forall s h,
      kleene_sem c 1 s h (Some (s, h))
  | kleene_fail_one  : forall n s h,
      c n s h None ->
      kleene_sem c (1 + n) s h None
  | kleene_fail_many : forall n n0 s s0 h h0,
      c n s h (Some (s0, h0)) ->
      kleene_sem c n0 s0 h0 None ->
      kleene_sem c (1 + n + n0) s h None
  | kleene_step_ok   : forall n n0 s s0 s1 h h0 h1,
      c n s h (Some (s0, h0)) ->
      kleene_sem c n0 s0 h0 (Some (s1, h1)) ->
      kleene_sem c (1 + n + n0) s h (Some (s1, h1)).
  Program Definition kleene_cmd c := @Build_semCmd (kleene_sem c) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using kleene_sem.
    unfold frame_property; intros.
    generalize dependent h; remember (Some (s', big')) as cfg; induction HSem;
      inversion Heqcfg; subst; intros; clear Heqcfg...
    edestruct (@cmd_frame c) as [h' [HFrame0 HSem0]]...
    - intros m HLe HFail; apply HSafe with (1 + m); [omega |]...
    destruct (IHHSem (eq_refl _) h' HFrame0) as [hr [HFrame1 HSem1]]...
    intros m HLe HFail; apply HSafe with (1 + n + m); [omega |]...
  Qed.

  (* assume command -- this command is a bit of a hack that just skips if
     a check holds and loops otherwise, this way allowing to encode if/while
     using nondeterministic choice/Kleene star *)
  Inductive assume_sem (P : pure) : semCmdType :=
  | assume_ok   : forall s h, P s -> assume_sem P 1 s h (Some (s, h)).
  Program Definition assume_cmd P := @Build_semCmd (assume_sem P) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation.
    unfold frame_property; simpl; intros.
    remember (Some (s', big')) as cfg; induction HSem;
      inversion Heqcfg; subst; eauto using assume_sem.
  Qed.

  (* assert command -- basically mimics C/Java assert: skips if the argument
     evaluates to true, faults otherwise *)
  Inductive assert_sem (P : pure) : semCmdType :=
  | assert_ok   : forall s h (HP : P s),
    assert_sem P 1 s h (Some (s, h))
  | assert_fail : forall s h (HNP : ~ P s),
    assert_sem P 1 s h None.
  Program Definition assert_cmd e := @Build_semCmd (assert_sem e) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation.
    unfold frame_property; simpl; intros.
    inversion HSem; subst; eauto using assert_sem.
  Qed.  

  (* |= {p}c{q} *)
  Program Definition triple (p q : asn S) (c : semCmd) : spec :=
    Build_spec (fun n => forall m k s h, m <= n -> k <= m -> p s h m ->
      safe c k s h /\
      forall h' s', c k s h (Some(s', h')) -> q s' h' (m - k)) _.

End Definitions.

Lemma assume_inv : forall S e n s s0 h h0
  (HSem : assume_cmd S e n s h (Some (s0, h0))),
  s = s0 /\ h = h0.
Proof.  
  intros; remember (Some (s0, h0)); induction HSem; subst;
    try inversion Heqo; intuition.
Qed.

Notation "{{ p }} c {{ q }}" := (triple p q c) (at level 89,
  format "{{ p }} '/ '  c '/ '  {{ q }}").

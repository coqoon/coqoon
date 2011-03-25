Require Export Pwf.
Require Export SemCmd.
Require Export HeapAsn.
Require Export SubstAbstractAsn.
Require Export AbstractAsn.
Require Import Quote.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Definition heap := PSM.t val.

Module SemContents (Import module_program: PROGRAM).

  Implicit Type s : stack.
  Notation field_lookup := (field_lookup Prog).
  Notation method_lookup := (method_lookup Prog).

  Reserved Notation "cfg '~>' cfg'" (at level 40).

  Inductive assign_sem (x : var) (e : expr) : semCmdType heap_alg :=
  | assign_ok : forall s h,
      assign_sem x e 1 s h (Some (SM.add x (e s) s, h)).
  Program Definition assign_cmd x e := @Build_semCmd _ (assign_sem x e) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using assign_sem.
    unfold frame_property; intros.
    inversion HSem; subst; clear HSem; exists h...
  Qed.

  Inductive read_sem (x y : var) (f : field) : semCmdType heap_alg :=
  | read_ok : forall ref v s h
      (Rref  : ref = val_to_ptr (s y))
      (Rmaps : PSM.MapsTo (ref,f) v h),
      read_sem x y f 1 s h (Some (SM.add x v s, h))
  | read_fail : forall ref s h
      (Sref   : ref = val_to_ptr (s y))
      (Snotin : ~ PSM.In (ref,f) h ),
      read_sem x y f 1 s h None.
  Program Definition read_cmd x y f := @Build_semCmd _ (read_sem x y f) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using read_sem.
    unfold frame_property; intros.
    inversion HSem; subst n s0 h0 s' big'; clear HSem; exists h; intuition.
    apply read_ok with ref; [assumption |]; specialize (HSafe _ (le_n _)).
    destruct (mapsto_dot Rmaps HFrame); [assumption|].
    contradiction HSafe; apply read_fail with ref; [assumption |].
    intro HIn.
    rewrite in_dot_fail in HFrame; [contradiction | eassumption | ].
    rewrite PSM'.find_mapsto_iff in H; rewrite PSM'.in_find_iff, H;
      discriminate.
  Qed.

  Inductive alloc_sem (x : var) (C : class) : semCmdType heap_alg :=
  | alloc_ok : forall s s0 (h h0 : heap_alg) ref fields
      (Snotnull : ref <> null)
      (Seq      : val_class ref = C)
      (Sfresh_h : forall f, ~ PSM.In (ref, f) h)
      (Sfields  : field_lookup C fields)
      (Sh0      : (Some h0 == h ·
        (SS.fold (fun f h' => PSM.add (ref, f) (null : val) h') fields (PSM.empty _)))%sa)
      (Ss0      : s0 = SM.add x (ref : val) s),
      alloc_sem x C 1 s h (Some (s0, h0)).
  Program Definition alloc_cmd x C := @Build_semCmd _ (alloc_sem x C) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation.
    unfold frame_property; intros.
    inversion HSem; subst n s0 h0 s' big'; clear HSem.
    remember (h · SS.fold (\f, \h, PSM.add (ref, f) (null : val) h) fields
      (PSM.empty _))%sa as oh; destruct oh as [h0 |].
    - exists h0; split.
      - change (Some h0 ·· Some frame == Some h1)%sa; rewrite Heqoh, sa_mulC,
        sa_mulA, HFrame, Sh0, sa_mulC; simpl; reflexivity.
      eapply alloc_ok; [eassumption | assumption | | eassumption | |
        reflexivity].
      - assert (HLT : (h <= big)%sa) by (exists frame; assumption).
        rewrite heap_sub_alt in HLT; unfold PSM'.Sub in HLT.
        intros f; remember (PSM.find (ref, f) h) as ot; destruct ot;
          symmetry in Heqot.
        - intros HIn; apply (Sfresh_h f).
          rewrite <- PSM'.find_mapsto_iff in Heqot.
          apply HLT in Heqot; rewrite PSM'.find_mapsto_iff in Heqot.
          rewrite PSM'.in_find_iff, Heqot; discriminate.
        rewrite PSM'.not_find_in_iff; assumption.
      rewrite Heqoh; reflexivity.
    remember (SS.fold (\f, \h', PSM.add (ref, f) (null:val) h') fields
      (PSM.empty _)) as hn.
    assert (HT : ((Some frame ·· None) == Some frame ·· (h · hn))%sa)
      by (rewrite Heqoh; reflexivity).
    rewrite <- sa_mulA in HT; rewrite sa_mulC in HT; rewrite HFrame in HT;
      rewrite <- Sh0 in HT; contradiction.
  Qed.

  Inductive write_sem (x:var) (f:field) (e:expr) : semCmdType heap_alg :=
  | write_ok : forall (s: stack) h h' ref
      (Sref: ref = val_to_ptr (s x) )
      (Sin:  PSM.In (ref,f) h )
      (Sadd: h' = PSM.add (ref,f) (e s) h ),
      write_sem x f e 1 s h (Some (s,h'))
  | write_fail : forall (s: stack) h ref
      (Sref:   ref = val_to_ptr (s x) )
      (Snotin: ~ PSM.In (ref,f) h ),
      write_sem x f e 1 s h None.
  Program Definition write_cmd x f e := @Build_semCmd _ (write_sem x f e) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation.
    unfold frame_property; intros.
    inversion HSem. subst. remember (val_to_ptr (s' x)) as ref. clear HSem.
    exists (PSM.add (ref, f) (e s') h). split.
    - specialize (HSafe _ (le_n _)).
      unfold safe in HSafe.
      destruct (PSM'.In_dec frame (ref, f)).
      - destruct (PSM'.In_dec h (ref, f)).
        - rewrite (in_dot_fail i0 i) in HFrame; contradiction.
        contradiction HSafe; apply write_fail with ref; assumption.
      rewrite heap_dot_update in *; split.
      - unfold PSM'.Disjoint; intros k [HInl HInr]; destruct
        (PSM'.In_dec (PSM.add (ref, f) (e s') h) k); [| contradiction].
        rewrite PSM'.add_in_iff in HInl; destruct HInl; subst;
          [ apply n; assumption |].
        destruct HFrame as [HL HR]; apply HL with k; split; assumption.
      rewrite PSM'.Equal_mapsto_iff; intros k v.
      rewrite PSM'.update_mapsto_iff; split.
      - intros HMT; rewrite PSM'.add_mapsto_iff in HMT; destruct HMT as
        [[HEq HEq'] | [HNeq HMT]]; [subst; rewrite PSM'.add_mapsto_iff; auto |].
        destruct HFrame as [HL HR]; rewrite PSM'.Equal_mapsto_iff in HR;
          specialize (HR k v); rewrite PSM'.update_mapsto_iff in HR.
        apply -> HR in HMT; rewrite PSM'.add_neq_mapsto_iff; [| assumption].
        destruct HMT as [HMT | [HMT HNin]]; [left; assumption |
          right; split; [assumption | intros HIn; apply HNin]].
        rewrite PSM'.add_neq_in_iff in HIn; assumption.
      destruct HFrame as [HDj HEq]; rewrite PSM'.Equal_mapsto_iff in HEq;
        specialize (HEq k v); rewrite PSM'.update_mapsto_iff in HEq;
          intros [HMT | [HMT HNin]].
      - rewrite PSM'.add_mapsto_iff in HMT; destruct HMT as
        [[HE1 HE2] | [HNe HMT]]; subst; [rewrite PSM'.add_mapsto_iff; simpl; auto
          |].
        rewrite PSM'.add_neq_mapsto_iff; [| assumption]; apply <- HEq; auto.
      destruct (PSM'.eq_dec (ref, f) k); [inversion e0; subst k |].
      - rewrite PSM'.not_find_in_iff in n; rewrite PSM'.find_mapsto_iff in HMT;
        rewrite HMT in n; discriminate.
      rewrite PSM'.add_neq_mapsto_iff; [| assumption];
        rewrite PSM'.add_neq_in_iff in HNin; [apply <- HEq; auto | assumption].
    - econstructor; eauto; []. assert (h <= big)%sa as HSub by (red; eauto).
       apply heap_sub_alt in HSub.
       edestruct PSM'.In_dec as [HIn|HnotIn]; [eassumption|].
       unfold safe, not in HSafe.
       contradiction (HSafe 1); eauto using write_sem.
  Qed.

  Inductive call_sem (rvar cvar : var) m es (c : cmd) (sc : semCmd _)
    : semCmdType heap_alg :=
  | call_failL : forall s h
      (HLFail  : forall mrec, ~ method_lookup (val_class (s cvar)) m mrec),
      call_sem rvar cvar m es c sc 1 s h None
  | call_failC : forall ps rexpr (s : stack) h n
      (HLookup : method_lookup (val_class (s cvar)) m (Build_Method ps c rexpr))
      (HLen    : length ps = length es)
      (HFail   : sc n (SM.add "this"%string (s cvar)
        (zip ps es :@: s +:+ SM.empty _)) h None),
      call_sem rvar cvar m es c sc (S n) s h None
  | call_ok    : forall ps rexpr (s sr : stack) h hr n
      (HLookup : method_lookup (val_class (s cvar)) m (Build_Method ps c rexpr))
      (HLen    : length ps = length es)
      (HSem    : sc n (SM.add "this"%string (s cvar)
        (zip ps es :@: s +:+ SM.empty _)) h (Some (sr, hr))),
      call_sem rvar cvar m es c sc (S n) s h
        (Some (SM.add rvar (rexpr sr) s, hr)).
  Program Definition call_cmd rvar cvar m es c sc :=
    @Build_semCmd _ (call_sem rvar cvar m es c sc) _ _.
  Next Obligation.
    introv H; inversion H.
  Qed.
  Next Obligation with eauto using call_sem.
    unfold frame_property; intros.
    inversion HSem; subst; clear HSem.
    edestruct (@cmd_frame _ sc) as [h1 [HFrame1 HSem1]]...
    intros k HLe HFail; apply HSafe with (S k); [omega |]...
  Qed.

  Inductive semantics : cmd -> (semCmd heap_alg) -> Prop :=
  | semassign : forall x e,
      semantics (cassign x e) (assign_cmd x e)
  | semread   : forall x y f,
      semantics (cread x y f) (read_cmd x y f)
  | semalloc  : forall x C,
      semantics (calloc x C) (alloc_cmd x C)
  | semwrite  : forall x f e,
      semantics (cwrite x f e) (write_cmd x f e)
  | semskip   : semantics cskip (skip_cmd _)
  | semseq    : forall c1 c2 sc1 sc2
      (HL : semantics c1 sc1)
      (HR : semantics c2 sc2),
      semantics (cseq c1 c2) (seq_cmd sc1 sc2)
  | semif     : forall e cl cr scl scr
      (HL : semantics cl scl)
      (HR : semantics cr scr),
      semantics (cif e cl cr) (nondet_cmd
        (seq_cmd (assume_cmd _ e) scl)
        (seq_cmd (assume_cmd _ (! e)) scr))
  | semwhile  : forall e c sc
      (HS : semantics c sc),
      semantics (cwhile e c) (seq_cmd (kleene_cmd
        (seq_cmd (assume_cmd _ e) sc)) (assume_cmd _ (! e)))
  | semcall   : forall x y m es c sc
      (HSem     : semantics c sc),
      semantics (ccall x y m es) (call_cmd x y m es c sc)
  | semassert : forall e,
      semantics (cassert e) (assert_cmd _ e).

  Definition c_triple (P Q : hasn) (c : cmd) :=
    ([A] sc, [pure] semantics c sc [->] triple P Q sc)%spec.

  Definition method_spec C m (ps : list var) (rn : var) (P Q : hasn) :=
    ([pure] (NoDup (rn :: "this"%string :: ps) /\
      free_in P ("this"%string :: ps) /\
      free_in Q (rn :: "this"%string :: ps)) [/\]
    [E] ps', [E] c, [E] re,
      [pure] (method_lookup C m (Build_Method ps' c re)
        /\ length ps = length ps' /\
        (forall x, In x ("this"%string :: ps') -> ~ SS.In x (modifies c)))
      [/\] c_triple (P // zip ps (map var_expr ps'))
        (Q // zip (rn :: ps) (re :: map var_expr ps')) c)%spec.
  Notation " C ':.:' m |-> ps {{ P }}-{{ r , Q }} " :=
    (method_spec C m ps r P Q) (at level 60).
  Notation " '{[' P ']}' c '{[' Q ']}' " := (c_triple P Q c) (at level 89,
    format " {[ P ]} '/' c '/' {[ Q ]} ").

  Lemma c_triple_zero (p q : hasn) (c : cmd) :
    {[ p ]} c {[ q ]} 0.
  Proof.
    simpl.
    introv H H1 H2 H3 Hp.
    assert (m = 0) by omega.
    assert (k = 0) by omega.
    subst. split.
    - apply safe_zero.
    - introv H4. apply cmd_zero in H4. destruct H4.
  Qed.

  Definition typeof (x : expr) (C : class) : pure :=
    sm_un (fun v => val_class v = C) x.

  Notation " x ':::' C " := (typeof x C) (at level 60).

  Definition expr_spec x m (ps: list var) (r: var) (P Q: hasn) : hasn :=
    (<E> C, <pure> x:::C </\> sm_const (FunI _ (C:.:m |-> ps {{P}}-{{r,Q}})))
    %asn.

  Arguments Scope expr_spec [_ _ _ _ asn_scope asn_scope].

  Notation " x ':..:' m |-> ps {{ P }}-{{ r , Q }} " :=
    (expr_spec x m ps r P Q) (at level 60).

End SemContents.

Module Type SEM (P: PROGRAM).
  Include SemContents P.
End SEM.

Module Sem (P: PROGRAM) : SEM P.
  Include SemContents P.
End Sem.

Open Scope asn_scope.

Ltac solve_free :=
  match goal with
    | |- free_in ?f _ => 
      let r1 := gethead f in 
        unfold free_in, r1; simpl; intuition
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

Close Scope asn_scope.
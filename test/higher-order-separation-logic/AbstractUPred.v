Require Export AbstractSepAlg.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Open Scope sa_scope.

Hint Extern 3 ((_ <= _)%sa) => reflexivity.

Section Defs.
  Variable S : sep_alg.

  Record upred := PackUpred {
    up_prop :> S -> nat -> Prop;
    up_mono : forall s s0 n n0, s <= s0 ->  (n0 <= n)%nat ->
      up_prop s n -> up_prop s0 n0}.
  Implicit Arguments PackUpred [].

  (* Definitions of BI connectives *)

  Program Definition sc_up (p q : upred) : upred :=
    PackUpred (fun s n => exists s1 s2,
      s1 · s2 <=| Some s /\ p s1 n /\ q s2 n) _.
  Next Obligation.
    intros p q s s0 n n0 HSub HLe [s1 [s2 [HS [HP HQ]]]]; exists s1 s2;
      intuition eauto using up_mono.
    rewrite HS; simpl; auto.
  Qed.

  Program Definition si_up (p q : upred) : upred :=
    PackUpred (fun s n => forall s0 st, s · s0 <=| Some st ->
      forall n0, (n0 <= n)%nat -> p s0 n0 -> q st n0) _.
  Next Obligation.
    intros p q s s0 n n0 HSub HLe HS s1 st HC n1 HLe0 HP.
    apply HS with s1; [| eauto with arith | assumption].
    transitivity (s0 · s1); [apply cancel_leq | assumption]; [ assumption |].
    apply sub_opt_mul_compat with st; assumption.
  Qed.

  Program Definition all_up {T : Type} (P : T -> upred) : upred :=
    PackUpred (fun s n => forall (x : T), P x s n) _.
  Next Obligation.
    eauto using up_mono.
  Qed.

  Program Definition xist_up {T : Type} (P : T -> upred) : upred :=
    PackUpred (fun s n => exists x, P x s n) _.
  Next Obligation.
    intros T P s s0 n n0 HSub HLe [x HP]; eauto using up_mono.
  Qed.

  Program Definition impl_up (p q : upred) : upred :=
    PackUpred (fun s n => forall s', s <= s' -> forall n', (n' <= n)%nat ->
      p s' n' -> q s' n') _.
  Next Obligation.
    intros p q s s0 n n0 HSub HLe HImp s1 HSub1 n1 HLe1 HP.
    eapply HImp; [transitivity s0 | | assumption]; eauto with arith.
  Qed.

  Program Definition and_up (p q : upred) : upred :=
    PackUpred (fun s n => p s n /\ q s n) _.
  Next Obligation.
    intuition eauto using up_mono.
  Qed.

  Program Definition or_up (p q : upred) : upred :=
    PackUpred (fun s n => p s n \/ q s n) _.
  Next Obligation.
    intuition eauto using up_mono.
  Qed.

  Program Definition pure_upred (P : Prop) : upred :=
    PackUpred (fun _ _ => P) _.

  Definition true_up  := pure_upred True.
  Definition false_up := pure_upred False.

  Definition entails_up (p q : upred) := forall s n, p s n -> q s n.

  Program Definition singleton_upred s : upred :=
    PackUpred (fun s0 _ => s <= s0) _.
  Next Obligation.
    intros; transitivity s0; auto.
  Qed.

  Program Definition later_up (p : upred) : upred :=
    PackUpred (fun s n => p s (n-1)) _.
  Next Obligation.
    intros p s s0 [|n] [|n0] HSub HLe HP; simpl in *;
      try rewrite <- minus_n_O in *; eauto using up_mono;
        inversion HLe; subst; clear HLe; eauto using up_mono with arith.
  Qed.

End Defs.
Implicit Arguments PackUpred [].

Instance entails_up_preo (S : sep_alg) : PreOrder (@entails_up S).
Proof. split; unfold entails_up; intuition. Qed.

Notation "a '<*>' b"   := (@sc_up _ a b)
  (at level 79, right associativity) : upred_scope.
Notation "a '<-*>' b"  := (@si_up _ a b)
  (at level 88, right associativity) : upred_scope.
Notation "a '</\>' b"  := (@and_up _ a b)
  (at level 80, right associativity) : upred_scope.
Notation "a '<\/>' b"  := (@or_up _ a b)
  (at level 85, right associativity) : upred_scope.
Notation "a '<->>' b"  := (@impl_up _ a b)
  (at level 88, right associativity) : upred_scope.
Notation "a '|-' b"    := (@entails_up _ a b)
  (at level 90, no associativity) : upred_scope.
Notation "'<A>' x , p" := (@all_up _ _ (fun x => p))
  (at level 89, x ident) : upred_scope.
Notation "'<A>' x : T , p" := (@all_up _ _ (fun x:T => p))
  (at level 89, x ident) : upred_scope.
Notation "'<E>' x , p" := (@xist_up _ _ (fun x => p))
  (at level 89, x ident) : upred_scope.
Notation "'<E>' x : T , p" := (@xist_up _ _ (fun x:T => p))
  (at level 89, x ident) : upred_scope.
Notation "'<true>'"    := (@true_up _) : upred_scope.
Notation "'<false>'"   := (@false_up _) : upred_scope.
Notation "'<|>>' p"    := (@later_up _ p) (at level 70) : upred_scope.
Notation "'<pure>' P"   := (@pure_upred _ P) (at level 65) : upred_scope.

Delimit Scope upred_scope with upred.

Definition bientails_up (S : sep_alg) (p q : upred S) := ((p |- q) /\ (q |- p))%upred.
Notation "a '-|-' b" := (@bientails_up _ a b)
  (at level 90, no associativity) : upred_scope.

Instance bientails_up_equiv (S : sep_alg) : Equivalence (@bientails_up S).
Proof.
  split.
  intros x; split; reflexivity.
  intros x y [Hl Hr]; split; assumption.
  intros x y z [Hxy Hyx] [Hyz Hzy]; split; transitivity y; assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@entails_up S) with signature
  (@entails_up _) --> (@entails_up _) ++> impl
as entails_up_m.
Proof.
  intros x y HEntxy u t HEnttu HEntxu.
  transitivity x; [| transitivity u]; assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@bientails_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> iff
as bientails_up_m.
Proof.
  intros x y HBixy u t HBiut; split; [intros HBixu | intros HBiyt].
  transitivity x; [symmetry | transitivity u]; assumption.
  transitivity t; [transitivity y | symmetry]; assumption.
Qed.

Instance bient_ent_subrel S : subrelation (@bientails_up S) (@entails_up S).
Proof.
  intros p q [Hp _]; assumption.
Qed.

Instance bient_inverse_ent_subrel S :
  subrelation (@bientails_up S) (inverse (@entails_up S)).
Proof.
  intros p q [_ Hp]; assumption.
Qed.

Section Properties.
  Variable S : sep_alg.
  Implicit Types p q r : upred S.

  Open Scope upred_scope.

  (* Properties of Heyting part of BI *)
  (* top *)
  Lemma true_upR : forall p, p |- <true>.
  Proof.
    intros p h n Hp; exact I.
  Qed.

  (* bottom *)
  Lemma false_upL : forall p, <false> |- p.
  Proof.
    intros p h n Hp; contradiction Hp.
  Qed.

  (* and introduction *)
  Lemma and_upI : forall p q r (Hp_q : p |- q) (Hp_r : p |- r), p |- q </\> r.
  Proof.
    unfold entails_up; simpl; eauto.
  Qed.

  (* and eliminations *)
  Lemma and_upEL : forall p q r (Hp_qr : p |- r), p </\> q |- r.
  Proof.
    intros p q r Hp_r h n [Hp Hq]; apply Hp_r; trivial.
  Qed.

  Lemma and_upER : forall p q r (Hp_r : p |- r), q </\> p |- r.
  Proof.
    intros p q r Hp_r h n [Hq Hp]; apply Hp_r; trivial.
  Qed.

  (* or introductions *)
  Lemma or_upIL : forall p q r (Hp_q : p |- q), p |- q <\/> r.
  Proof.
    unfold entails_up; simpl; eauto.
  Qed.

  Lemma or_upIR : forall p q r (Hp_q : p |- r), p |- q <\/> r.
  Proof.
    unfold entails_up; simpl; eauto.
  Qed.

  (* or elimination *)
  Lemma or_upE : forall p q r (Hp_r : p |- r) (Hq_r : q |- r), p <\/> q |- r.
  Proof.
    unfold entails_up; simpl; intuition.
  Qed.

  (* impl introduction *)
  Lemma impl_upI : forall p q r (Hpq_r : p </\> q |- r), p |- q <->> r.
  Proof.
    unfold entails_up; simpl; eauto using up_mono.
  Qed.

  (* impl elimination *)
  Lemma impl_upE : forall p q r s (Hp_r_s : p |- r <->> s) (Hq_r : q |- r),
    p </\> q |- s.
  Proof.
    intros p q r s Hp_r_s Hq_r h n [Hp Hq]; apply (Hp_r_s h n); auto.
  Qed.

  (* Quantifier properties *)

  Lemma all_upEIR : forall T p (f : T -> upred S),
    p |- <A> x, f x <-> forall x, p |- f x.
  Proof.
    unfold entails_up; simpl; intuition.
  Qed.

  Lemma xist_upEIL : forall T p (f : T -> upred S),
    <E> x, f x |- p <-> forall x, f x |- p.
  Proof.
    split; [intros Hp x s n Hfx | intros Hfx s n [x Hex]].
    apply Hp; exists x; assumption.
    apply Hfx with x; assumption.
  Qed.

  Lemma xist_upIR : forall T p (f : T -> upred S)
    (HEx : exists x, p |- f x), p |- <E> x, f x.
  Proof.
    intros T p f [x Hpf] s Hp; exists x; apply Hpf; auto.
  Qed.

  (* Properties of separating part of BI *)
  Lemma sc_upCL : forall p q, p <*> q |- q <*> p.
  Proof.
    intros p q s n [s1 [s2 PP]]; exists s2 s1; intuition.
    rewrite sa_mulC; assumption.
  Qed.

  Lemma sc_upAL : forall p q r, (p <*> q) <*> r |- p <*> q <*> r.
  Proof.
    intros p q r s n; simpl.
    intros [s1 [s2 [HSub12 [[s3 [s4 [HSub34 [Hp Hq]]]] Hr]]]].
    remember (s1 · s2) as s12; destruct s12 as [s12 |];
      [symmetry in Heqs12 | contradiction].
    remember (s3 · s4) as s34; destruct s34 as [s34 |];
      [symmetry in Heqs34 | contradiction].
    simpl in *.
    assert (Hc42 : s4 # s2).
    - apply compat_subheap with s1; [| exists s12; rewrite Heqs12; reflexivity].
      transitivity s34; [|assumption].
      exists s3; rewrite sa_mulC; rewrite Heqs34; reflexivity.
    destruct Hc42 as [s42 Heqs42].
    exists s3 s42; intuition.
    erewrite mul_some_right; [|eassumption].
    rewrite <- sa_mulA; rewrite Heqs34; simpl.
    apply sub_opt_mul_t with s1; [| rewrite Heqs12]; assumption.
    exists s4 s2; intuition.
    rewrite Heqs42; simpl; reflexivity.
  Qed.

  Lemma true_unitL : forall p, p |- <true> <*> p.
  Proof.
    intros p s Hp; simpl; eexists ^; exists s.
    intuition; rewrite sa_mulC; rewrite sa_unitI; simpl; reflexivity.
  Qed.

  Lemma true_unitR : forall p, <true> <*> p |- p.
  Proof.
    intros p s n [s1 [s2 [Hs [_ Hp]]]]; simpl in *.
    eapply up_mono; [| eauto | eassumption].
    apply (sub_opt_mul_r Hs).
  Qed.

  Lemma sc_upME : forall p q r s (Hp_r : p |- r) (Hq_s : q |- s),
    p <*> q |- r <*> s.
  Proof.
    intros p q r s n Hpr Hqs h [h1 [h2 Hpq]]; simpl; exists h1 h2; intuition.
  Qed.

  Lemma si_upI : forall p q r (Hpq_r : p <*> q |- r), p |- q <-*> r.
  Proof.
    intros p q r He h n Hp h0 ht Hst n0 HLe Hq.
    apply He; exists h h0; intuition eauto using up_mono.
  Qed.

  Lemma si_upE : forall p q r s (Hp_r_s : p |- r <-*> s) (Hq_r : q |- r),
    p <*> q |- s.
  Proof.
    intros p q r s Hprs Hqr h n [h1 [h2 [Hs12 [Hp Hq]]]].
    eapply Hprs; [eassumption | eassumption | eauto |].
    apply Hqr; assumption.
  Qed.

  Lemma later_entails_up : forall p, p |- <|>> p.
  Proof.
    unfold entails_up; simpl; intros.
    eapply up_mono; [reflexivity | | eassumption]; omega.
  Qed.

  (* Properties of pure injection *)

  Lemma pure_upEL p q (P : Prop) :
    ((P -> p |- q) -> <pure> P </\> p |- q)%upred.
  Proof.
    intros HPE h n [HP HG]; apply HPE; assumption.
  Qed.

  Lemma pure_upIR p q (P : Prop) :
    (P -> (p |- q) -> p |- <pure> P </\> q)%upred.
  Proof.
    intros HP HE h n HG; split; [| apply HE]; assumption.
  Qed.

  Close Scope upred_scope.

End Properties.

Add Parametric Morphism (S : sep_alg) : (@sc_up S) with signature
  (@entails_up _) ==> (@entails_up _) ==> (@entails_up _)
as sc_entails_up_m.
Proof.
  intros x y HEntxy u t HEntut; apply sc_upME; assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@sc_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> (@bientails_up _)
as sc_bientails_up_m.
Proof.
  intros x y HEntxy u t HEntut; split;
    (apply sc_upME; [rewrite HEntxy | rewrite HEntut]); reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@si_up S) with signature
  (@entails_up _) --> (@entails_up _) ++> (@entails_up _)
as si_entails_up_m.
Proof.
  intros x y HEntyx u t HEntut.
  apply si_upI; rewrite HEntyx; rewrite <- HEntut.
  eapply si_upE; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@si_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> (@bientails_up _)
as si_bientails_up_m.
Proof.
  intros x y HBixy u t HBiut; split.
  rewrite HBiut; rewrite HBixy; reflexivity.
  rewrite <- HBixy; rewrite <- HBiut; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@and_up S) with signature
  (@entails_up _) ==> (@entails_up _) ==> (@entails_up _)
as and_entails_up_m.
Proof.
  intros x y HEntxy u t HEntut.
  apply and_upI; [apply and_upEL | apply and_upER]; trivial.
Qed.

Add Parametric Morphism (S : sep_alg) : (@and_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> (@bientails_up _)
as and_bientails_up_m.
Proof.
  intros x y HEntxy u t HEntut; split.
  rewrite HEntxy; rewrite HEntut; reflexivity.
  rewrite <- HEntxy; rewrite <- HEntut; reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@or_up S) with signature
  (@entails_up _) ==> (@entails_up _) ==> (@entails_up _)
as or_entails_up_m.
Proof.
  intros x y HEntxy u t HEntut.
  apply or_upE; [apply or_upIL | apply or_upIR]; trivial.
Qed.

Add Parametric Morphism (S : sep_alg) : (@or_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> (@bientails_up _)
as or_bientails_up_m.
Proof.
  intros x y HEntxy u t HEntut; split.
  rewrite HEntxy; rewrite HEntut; reflexivity.
  rewrite <- HEntxy; rewrite <- HEntut; reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@impl_up S) with signature
  (@entails_up _) --> (@entails_up _) ++> (@entails_up _)
as impl_entails_up_m.
Proof.
  intros x y HEntyx u t HEntut.
  apply impl_upI; rewrite HEntyx; rewrite <- HEntut.
  eapply impl_upE; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@impl_up S) with signature
  (@bientails_up _) ==> (@bientails_up _) ==> (@bientails_up _)
as impl_bientails_up_m.
Proof.
  intros x y HBixy u t HBiut; split.
  rewrite HBiut; rewrite HBixy; reflexivity.
  rewrite <- HBixy; rewrite <- HBiut; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@later_up S) with signature
  (@entails_up _) ==> (@entails_up _)
as later_entails_up_m.
Proof.
  unfold entails_up; simpl; eauto.
Qed.

Add Parametric Morphism (S : sep_alg) : (@later_up S) with signature
  (@bientails_up _) ==> (@bientails_up _)
as later_bientails_up_m.
Proof.
  intros x y [Hxy Hyx]; split; [ rewrite Hxy | rewrite Hyx ]; reflexivity.
Qed.

Section AdditionalProperties.
  Variable S : sep_alg.
  Implicit Types p q r : upred S.

  Open Scope upred_scope.

  Lemma sc_upA : forall p q r, (p <*> q) <*> r -|- p <*> q <*> r.
  Proof.
    intros; split; [apply sc_upAL |].
    do 2 (rewrite sc_upCL, sc_upAL). rewrite sc_upCL. reflexivity.
  Qed.

  Lemma sc_upC : forall p q, p <*> q -|- q <*> p.
  Proof.
    intros; split; apply sc_upCL.
  Qed.

  Lemma sc_upCA : forall p q r, p <*> q <*> r -|- q <*> p <*> r.
  Proof.
    intros; rewrite <- sc_upA, (sc_upC p), sc_upA; reflexivity.
  Qed.

  Lemma sc_upAC : forall p q r, (p <*> q) <*> r -|- (p <*> r) <*> q.
  Proof.
    intros; rewrite sc_upA, (sc_upC q), <- sc_upA; reflexivity.
  Qed.

  (* <*> and </\> preserve joins *)

  Lemma sc_false_up : forall p, <false> <*> p -|- <false>.
  Proof.
    split.
    rewrite sc_upME; [rewrite sc_upC, true_unitR | | apply true_upR ];
      reflexivity.
    apply false_upL.
  Qed.

  Lemma sc_or_up : forall p q r,
    (p <\/> q) <*> r -|- (p <*> r) <\/> (q <*> r).
  Proof.
    split.
    - unfold entails_up; simpl; firstorder.
    apply or_upE; (apply sc_upME; [| reflexivity]);
      [apply or_upIL | apply or_upIR]; reflexivity.
  Qed.  

  Lemma sc_xist_up : forall U (F : U -> upred S) p,
    (<E> x, F x) <*> p -|- <E> x, F x <*> p.
  Proof.
    split.
    - unfold entails_up; simpl; intros s n [s1 [s2 [HSub [[u HFu] Hp]]]];
      exists u s1 s2; auto.
    apply <- xist_upEIL; intros u; apply sc_upME; [| reflexivity ].
    apply xist_upIR; exists u; reflexivity.
  Qed.

  Lemma and_false_up : forall p, <false> </\> p -|- <false>.
  Proof.
    split; [ apply and_upEL |]; apply false_upL.
  Qed.

  Lemma and_or_up : forall p q r,
    (p <\/> q) </\> r -|- (p </\> r) <\/> (q </\> r).
  Proof.
    split.
    - unfold entails_up; simpl; intuition.
    apply or_upE; (apply and_upI; [ apply and_upEL
      | apply and_upER; reflexivity]); [apply or_upIL | apply or_upIR];
    reflexivity.
  Qed.

  Lemma and_xist_up : forall U (F : U -> upred S) p,
    (<E> x, F x) </\> p -|- <E> x, F x </\> p.
  Proof.
    split.
    - unfold entails_up; simpl; firstorder.
    apply <- xist_upEIL; intros u; apply and_upI;
      [ apply and_upEL | apply and_upER; reflexivity ].
    apply xist_upIR; exists u; reflexivity.
  Qed.

  (* Later commutes with other operators *)

  Lemma later_and_up : forall p q, <|>> (p </\> q) -|- <|>> p </\> <|>> q.
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_or_up : forall p q, <|>> (p <\/> q) -|- <|>> p <\/> <|>> q.
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_sc_up : forall p q, <|>> (p <*> q) -|- <|>> p <*> <|>> q.
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_A_up T (P : T -> upred S) : <|>> (<A> x, P x) -|- <A> x,
    (<|>> P x).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_A'_up T (P : T -> upred S) : <|>> (all_up P) -|- <A> x,
    (<|>> P x).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_E_up T (P : T -> upred S) : <|>> (<E> x, P x) -|- <E> x,
    (<|>> P x).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_E'_up T (P : T -> upred S) : <|>> (xist_up P) -|- <E> x,
    (<|>> P x).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_pure_up : forall P : Prop, @later_up S (<pure> P) -|- <pure> P.
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma later_impl_up : forall p q, <|>> (p <->> q) -|- <|>> p <->> <|>> q.
  Proof.
    split; unfold entails_up; simpl; intros.
    eapply H; eauto; omega.
    destruct n as [|n]; [inversion H1; clear H1 |]; simpl in *; subst.
    specialize (H s' H0 0); simpl in H; apply H; auto.
    specialize (H s' H0 (Datatypes.S n')); simpl in *;
      rewrite <- minus_n_O in *; apply H; auto with arith.
  Qed.

  Lemma later_si_up : forall p q, <|>> (p <-*> q) -|- <|>> p <-*> <|>> q.
  Proof.
    split; unfold entails_up; simpl; intros.
    apply H with s0; auto; omega.
    destruct n as [| n]; [inversion H1; clear H1 |]; simpl in *; subst.
    specialize (H s0 st H0 0); apply H; auto.
    specialize (H s0 st H0 (Datatypes.S n0)); simpl in *;
      rewrite <- minus_n_O in *; apply H; auto with arith.
  Qed.

End AdditionalProperties.

Section Supported.

  Variable S : sep_alg.
  Implicit Type s : S.

  Definition supported (p : upred S) : Prop :=
    forall n s_big, p s_big n ->
      exists s_min, s_min <= s_big /\ p s_min n /\
        forall s, s <= s_big -> p s n -> s_min <= s.

  Lemma supported_singleton s : supported (singleton_upred s).
  Proof.
    unfold supported; intros.
    exists s; intuition; simpl in *.
    reflexivity.
  Qed.

  Open Scope upred_scope.

  Lemma supported_all_up : forall U (F : U -> upred S) (p : upred S)
    (Hsup: supported p), (exists u : U, True) ->
    all_up F <*> p -|- <A> x, F x <*> p.
  Proof.
    intros U F p Hsup [u _]; split.
    - unfold entails_up; simpl; firstorder.
    unfold entails_up; simpl; intros.
    destruct (Hsup n s) as [s_min [HSub [HP HAS]]].
    - destruct (H u) as [s1 [s2 [HSub [HF HP]]]];
      apply up_mono with s2 n; auto; eapply sub_opt_mul_r; eassumption.
    destruct HSub as [s_F HSub];
      exists s_F s_min; split.
    - rewrite sa_mulC, HSub; simpl; reflexivity.
    split; [intros u' | assumption ].
    destruct (H u') as [s1 [s2 [HSub' [HF' HP']]]].
    remember (s1 · s2) as st; destruct st as [st |]; [| contradiction].
    destruct HSub' as [ss HSub'].
    replace (st · ss) with (Some st ·· Some ss) in HSub' by reflexivity.
    rewrite Heqst in HSub'; clear Heqst st; rewrite sa_mulA in HSub'.
    remember (s2 · ss) as st; destruct st as [st |]; [| contradiction].
    destruct (HAS st) as [sb HS].
    - exists s1; rewrite <- HSub', sa_mulC; reflexivity.
    - apply up_mono with s2 n; auto; exists ss; rewrite Heqst; reflexivity.
    apply up_mono with s1 n; auto.
    rewrite <- HS, sa_mulC, <- sa_mulA in HSub'.
    remember (s1 · sb) as sf; destruct sf as [sf |]; [| contradiction].
    simpl in HSub'; rewrite sa_mulC in HSub';
      assert (HEq := sa_cancel HSub HSub'); rewrite HEq; exists sb;
        rewrite Heqsf; reflexivity.
  Qed.

  Lemma supported_sc_up (p q: upred S)
    (Hp: supported p) (Hq: supported q) : supported (p <*> q).
  Proof.
    unfold supported in *. simpl. introv [s1 [s2 [Hsub [Hp_s1 Hq_s2]]]].
    destruct (Hp n s_big) as [sp [Hsub_sp [Hp_sp Hmin_sp]]]; clear Hp.
    - eauto using up_mono, sub_opt_mul_l.
    destruct (Hq n s_big) as [sq [Hsub_sq [Hp_sq Hmin_sq]]]; clear Hq.
    - eauto using up_mono, sub_opt_mul_r.
    assert (sp · sq <=| Some s_big) as Hsub_spq.
    - apply sub_opt_mul_lr with s1 s2;
        eauto using sub_opt_mul_l, sub_opt_mul_r.
    assert (sp # sq) as [spq Hspq] by eauto using sub_opt_mul_compat.
    exists spq. split; [|split].
    - rewrite Hspq in Hsub_spq. auto.
    - exists sp sq. rewrite Hspq. unfold sub_opt. simpl. intuition reflexivity.
    - intros scand Hcand [sp' [sq' [Hspq' [Hsp' Hsq']]]].
      assert (Hsub_sp': sp <= sp').
      - apply Hmin_sp; eauto. transitivity scand; eauto using sub_opt_mul_l.
      assert (Hsub_sq': sq <= sq').
      - apply Hmin_sq; eauto. transitivity scand; eauto using sub_opt_mul_r.
      eapply sub_opt_mul_lr in Hspq'; eauto.
      rewrite Hspq in Hspq'. auto.
  Qed.

  Close Scope upred_scope.

End Supported.

Close Scope sa_scope.

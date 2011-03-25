Require Export AbstractUPred.
Require Export Pure.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Definition sm_rel (T : Type) (R : relation T) : relation (sm T) :=
  fun t0 t1 : sm T => forall s, R (t0 s) (t1 s).

Section Defs.
  Variable S : sep_alg.

  Definition asn  := sm (upred S).
  Definition pure_asn (p : pure) : asn :=
    sm_un (fun P => pure_upred _ P) p.

  Definition lift_up_bin (op : upred S -> upred S -> upred S) :
    asn -> asn -> asn := sm_bin op.

  Definition true_asn  : asn := pure_asn (Prop_pure True).
  Definition false_asn : asn := pure_asn (Prop_pure False).
  Definition sc_asn   := lift_up_bin (@sc_up S).
  Definition si_asn   := lift_up_bin (@si_up S).
  Definition and_asn  := lift_up_bin (@and_up S).
  Definition impl_asn := lift_up_bin (@impl_up S).
  Definition or_asn   := lift_up_bin (@or_up S).
  Definition entails_asn   : relation asn := sm_rel (@entails_up S).
  Definition bientails_asn : relation asn := sm_rel (@bientails_up S).
  Definition singleton_asn : sm S -> asn := sm_un (@singleton_upred S).
  Definition later_asn := sm_un (@later_up S).
  Definition all_asn {T} := sm_bind (@all_up S T).
  Definition xist_asn {T} := sm_bind (@xist_up S T).
End Defs.

Instance entails_asn_preo (S : sep_alg) : PreOrder (@entails_asn S).
Proof.
  split.
  intros p s; reflexivity.
  intros x y z Hxy Hxz s; transitivity (y s); [apply Hxy | apply Hxz].
Qed.

Instance bientails_asn_equiv (S : sep_alg) : Equivalence (@bientails_asn S).
Proof.
  split.
  intros x; split; reflexivity.
  intros x y HS s; symmetry; apply HS.
  intros x y z Hxy Hxz s; transitivity (y s); [apply Hxy | apply Hxz].
Qed.

Instance eq_sub_bient (S: sep_alg) : subrelation sm_eq (@bientails_asn S).
Proof.
  unfold subrelation, predicate_implication, impl, sm_eq, bientails_asn, sm_rel.
  simpl. intros p q H s. rewrite H. reflexivity.
Qed.

Delimit Scope asn_scope with asn.

Notation " ! p " := (not_pure p) (at level 50).
Notation "a '<*>' b"  := (@sc_asn _ a b)
  (at level 79, right associativity) : asn_scope.
Notation "a '<-*>' b" := (@si_asn _ a b)
  (at level 88, right associativity) : asn_scope.
Notation "a '</\>' b" := (@and_asn _ a b)
  (at level 80, right associativity) : asn_scope.
Notation "a '<\/>' b" := (@or_asn _ a b)
  (at level 85, right associativity) : asn_scope.
Notation "a '<->>' b" := (@impl_asn _ a b)
  (at level 88, right associativity) : asn_scope.
Notation "a '|-' b"   := (@entails_asn _ a b)
  (at level 90, no associativity) : asn_scope.
Notation "a '-|-' b" := (@bientails_asn _ a b)
  (at level 90, no associativity) : asn_scope.
Notation "'<true>'"     := (@true_asn _) : asn_scope.
Notation "'<false>'"    := (@false_asn _) : asn_scope.
Notation "'<A>' x , p":= (@all_asn _ _ (fun x => p)) (at level 89, x ident) : asn_scope.
Notation "'<A>' x : T , p":= (@all_asn _ _ (fun x : T => p))
  (at level 89, x ident) : asn_scope.
Notation "'<E>' x , p":= (@xist_asn _ _ (fun x => p)) (at level 89, x ident) : asn_scope.
Notation "'<E>' x : T , p":= (@xist_asn _ _ (fun x : T => p))
  (at level 89, x ident) : asn_scope.
Notation "'<|>>' p"    := (@later_asn _ p) (at level 70) : asn_scope.
Notation "'<pure>' P"   := (@pure_asn _ P) (at level 65) : asn_scope.
Notation "e1 == e2" := (<pure> ((e1:expr) ·=· (e2:expr)))%asn
  (at level 70, no associativity) : asn_scope.  

Open Scope asn_scope.

Add Morphism expr_pure with signature
sm_eq ==> sm_eq
as sm_eq_expr_pure_m.
Proof.
  unfold sm_eq. simpl. intros. rewrite H. reflexivity.
Qed.

Add Parametric Morphism A : (@eq_pure A) with signature
  sm_eq ==> sm_eq ==> sm_eq
  as sm_eq_eq_expr_m.
Proof.
  unfold sm_eq. simpl. intros. rewrite H. rewrite H0. reflexivity.
Qed.

Add Morphism not_pure with signature
  sm_eq ==> sm_eq
  as pure_eq_not_pure_m.
Proof.
  unfold sm_eq. simpl. intros. rewrite H. reflexivity.
Qed.

Lemma bientails_asn_alt S (a b: asn S) :
  a -|- b <-> (a |- b) /\ (b |- a).
Proof. firstorder. Qed.

Add Parametric Morphism (S : sep_alg) : (@entails_asn S) with signature
  (@entails_asn _) --> (@entails_asn _) ++> impl
as entails_asn_m.
Proof.
  intros x y HEntxy u t HEnttu HEntxu.
  transitivity x; [| transitivity u]; assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@entails_asn S) with signature
  (@bientails_asn S) ==> (@bientails_asn S) ==> iff
  as entails_bientails.
Proof.
  intros p q H r t H1. 
  split.
  - intros H2 s S1 n H3. apply H1. apply H2. apply H. assumption.
  - intros H2 s S1 n H3. apply H1. apply H2. apply H. assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@bientails_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> iff
as bientails_asn_m.
Proof.
  intros x y HBixy u t HBiut; split; [intros HBixu | intros HBiyt].
  transitivity x; [symmetry | transitivity u]; assumption.
  transitivity t; [transitivity y | symmetry]; assumption.
Qed.

Add Parametric Morphism S: sm_const with signature
  (@entails_up S) ==> (@entails_asn S)
  as sm_const_entails.
Proof.
  intros p q H. firstorder.
Qed.

Add Parametric Morphism S: sm_const with signature
  (@bientails_up S) ==> (@bientails_asn S)
  as sm_const_bientails.
Proof.
  intros p q H. rewrite bientails_asn_alt. firstorder.
Qed.

Instance bient_ent_asn_subrel S :
  subrelation (@bientails_asn S) (@entails_asn S).
Proof.
  intros p q Hpq s; rewrite (Hpq s); reflexivity.
Qed.

Lemma sm_eq_bient (S: sep_alg) (p q: asn S) : impl (sm_eq p q) (p -|- q)%asn.
Proof. apply eq_sub_bient. Qed.

Instance bient_inverse_ent_asn_subrel S :
  subrelation (@bientails_asn S) (inverse (@entails_asn S)).
Proof.
  intros p q Hpq s; rewrite (Hpq s); reflexivity.
Qed.

Close Scope asn_scope.

Section Properties.
  Variable S : sep_alg.
  Implicit Types p q r : asn S.

  Open Scope asn_scope.

  (* Properties of Heyting part of BI *)
  (* top *)
  Lemma true_asnR : forall p, p |- <true>.
  Proof.
    intros p s; apply true_upR.
  Qed.

  (* bottom *)
  Lemma false_asnL : forall p, <false> |- p.
  Proof.
    intros p s; apply false_upL.
  Qed.

  (* and introduction *)
  Lemma and_asnI : forall p q r (Hp_q : p |- q) (Hp_r : p |- r), p |- q </\> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using and_upI.
  Qed.

  (* and eliminations *)
  Lemma and_asnEL : forall p q r (Hp_qr : p |- r), p </\> q |- r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using and_upEL.
  Qed.

  Lemma and_asnER : forall p q r (Hp_r : p |- r), q </\> p |- r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using and_upER.
  Qed.

  (* or introductions *)
  Lemma or_asnIL : forall p q r (Hp_q : p |- q), p |- q <\/> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using or_upIL.
  Qed.

  Lemma or_asnIR : forall p q r (Hp_q : p |- r), p |- q <\/> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using or_upIR.
  Qed.

  (* or elimination *)
  Lemma or_asnE : forall p q r (Hp_r : p |- r) (Hq_r : q |- r), p <\/> q |- r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using or_upE.
  Qed.

  (* impl introduction *)
  Lemma impl_asnI : forall p q r (Hpq_r : p </\> q |- r), p |- q <->> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using impl_upI.
  Qed.

  (* impl elimination *)
  Lemma impl_asnE : forall p q r s
    (Hp_r_s : p |- r <->> s) (Hq_r : q |- r), p </\> q |- s.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using impl_upE.
  Qed.

  (* Quantifier properties *)

  Lemma all_asnEIR : forall T p (f : T -> asn S),
    p |- <A> x, f x <-> forall x, p |- f x.
  Proof.
    unfold entails_asn, sm_rel; simpl; intuition.
    revert x; rewrite <- all_upEIR; auto.
    rewrite all_upEIR; auto.
  Qed.

  Lemma xist_asnEIL : forall T p (f : T -> asn S),
    <E> x, f x |- p <-> forall x, f x |- p.
  Proof.
    unfold entails_asn, sm_rel; simpl; intuition.
    revert x; rewrite <- xist_upEIL; auto.
    rewrite xist_upEIL; auto.
  Qed.

  Lemma xist_asnIR : forall T p (f : T -> asn S)
    (HEx : exists x, p |- f x), p |- <E> x, f x.
  Proof.
    unfold entails_asn, sm_rel; simpl; intros T p f [x HEx] s.
    eauto using xist_upIR.
  Qed.

  (* Properties of separating part of BI *)
  Lemma sc_asnCL : forall p q, p <*> q |- q <*> p.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using sc_upCL.
  Qed.

  Lemma sc_asnAL : forall p q r, (p <*> q) <*> r |- p <*> q <*> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using sc_upAL.
  Qed.

  Lemma true_asn_unitL : forall p, p |- <true> <*> p.
  Proof.
    unfold entails_asn, sm_rel; simpl; fold (true_up S);
      eauto using true_unitL.
  Qed.

  Lemma true_asn_unitR : forall p, <true> <*> p |- p.
  Proof.
    unfold entails_asn, sm_rel; simpl; fold (true_up S);
      eauto using true_unitR.
  Qed.

  Lemma true_asn_eq_unitL : forall p, <true> <*> p -|- p.
  Proof.
    split; [apply true_asn_unitR| apply true_asn_unitL].
  Qed.

  Lemma sc_asnC : forall p q, p <*> q -|- q <*> p.
  Proof.
    intros; split; apply sc_asnCL.
  Qed.

  Lemma true_asn_eq_unitR : forall p, p <*> <true> -|- p.
  Proof.
    intros. rewrite sc_asnC. apply true_asn_eq_unitL.
  Qed.

  Lemma sc_asnME : forall p q r s
    (Hp_r : p |- r) (Hq_s : q |- s), p <*> q |- r <*> s.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using sc_upME.
  Qed.

  Lemma si_asnI : forall p q r (Hpq_r : p <*> q |- r), p |- q <-*> r.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using si_upI.
  Qed.

  Lemma si_asnE : forall p q r s
    (Hp_r_s : p |- r <-*> s) (Hq_r : q |- r), p <*> q |- s.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using si_upE.
  Qed.

  Lemma later_entails_asn : forall p, p |- <|>> p.
  Proof.
    unfold entails_asn, sm_rel; simpl; eauto using later_entails_up.
  Qed.

  Lemma eq_pure_refl {A} (p: asn S) (a : sm A) :
    p |- <pure>(a ·=· a).
  Proof.
    intros s S' n H.
    simpl; reflexivity.
  Qed.

  Close Scope asn_scope.

End Properties.

Open Scope asn_scope.

Add Parametric Morphism (S : sep_alg) : (@sc_asn S) with signature
  (@entails_asn _) ==> (@entails_asn _) ==> (@entails_asn _)
as sc_entails_asn_m.
Proof.
  intros x y HEntxy u t HEntut; apply sc_asnME; assumption.
Qed.

Add Parametric Morphism (S : sep_alg) : (@sc_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> (@bientails_asn _)
as sc_bientails_asn_m.
Proof.
  intros x y HEntxy u t HEntut; split;
    (apply sc_asnME; [rewrite HEntxy | rewrite HEntut]); reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@si_asn S) with signature
  (@entails_asn _) --> (@entails_asn _) ++> (@entails_asn _)
as si_entails_asn_m.
Proof.
  intros x y HEntyx u t HEntut.
  apply si_asnI; rewrite HEntyx; rewrite <- HEntut.
  eapply si_asnE; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@si_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> (@bientails_asn _)
as si_bientails_asn_m.
Proof.
  intros x y HBixy u t HBiut; split; simpl.
  rewrite (HBiut s); rewrite (HBixy s); reflexivity.
  rewrite <- (HBixy s); rewrite <- (HBiut s); reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@and_asn S) with signature
  (@entails_asn _) ==> (@entails_asn _) ==> (@entails_asn _)
as and_entails_asn_m.
Proof.
  intros x y HEntxy u t HEntut.
  apply and_asnI; [apply and_asnEL | apply and_asnER]; trivial.
Qed.

Add Parametric Morphism (S : sep_alg) : (@and_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> (@bientails_asn _)
as and_bientails_asn_m.
Proof.
  intros x y HEntxy u t HEntut; split; simpl.
  rewrite (HEntxy s); rewrite (HEntut s); reflexivity.
  rewrite <- (HEntxy s); rewrite <- (HEntut s); reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@or_asn S) with signature
  (@entails_asn _) ==> (@entails_asn _) ==> (@entails_asn _)
as or_entails_asn_m.
Proof.
  intros x y HEntxy u t HEntut.
  apply or_asnE; [apply or_asnIL | apply or_asnIR]; trivial.
Qed.

Add Parametric Morphism (S : sep_alg) : (@or_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> (@bientails_asn _)
as or_bientails_asn_m.
Proof.
  intros x y HEntxy u t HEntut; split; simpl.
  rewrite (HEntxy s); rewrite (HEntut s); reflexivity.
  rewrite <- (HEntxy s); rewrite <- (HEntut s); reflexivity.
Qed.  

Add Parametric Morphism (S : sep_alg) : (@impl_asn S) with signature
  (@entails_asn _) --> (@entails_asn _) ++> (@entails_asn _)
as impl_entails_asn_m.
Proof.
  intros x y HEntyx u t HEntut.
  apply impl_asnI; rewrite HEntyx; rewrite <- HEntut.
  eapply impl_asnE; reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@impl_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _) ==> (@bientails_asn _)
as impl_bientails_asn_m.
Proof.
  intros x y HBixy u t HBiut; split; simpl.
  rewrite (HBiut s); rewrite (HBixy s); reflexivity.
  rewrite <- (HBixy s); rewrite <- (HBiut s); reflexivity.
Qed.

Add Parametric Morphism (S : sep_alg) : (@later_asn S) with signature
  (@entails_asn _) ==> (@entails_asn _)
as later_entails_up_m.
Proof.
  unfold entails_asn, sm_rel, entails_up; simpl; eauto.
Qed.

Add Parametric Morphism (S : sep_alg) : (@later_asn S) with signature
  (@bientails_asn _) ==> (@bientails_asn _)
as later_bientails_up_m.
Proof.
  intros x y Hxy; split; simpl; rewrite (Hxy s); reflexivity.
Qed.

Instance xist_asn_entails_m (A : Type) S :
  Proper (pointwise_relation A (@entails_asn S) ++> (@entails_asn S))
         (@xist_asn S A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold entails_asn, sm_rel, entails_up in *.
  simpl. firstorder.
Qed.

Instance xist_asn_bientails_m (A : Type) S :
  Proper (pointwise_relation A (@bientails_asn S) ==> (@bientails_asn S))
         (@xist_asn S A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold bientails_asn, sm_rel, bientails_up, entails_up in *.
  simpl. firstorder.
Qed.

Instance all_asn_entails_m (A : Type) S :
  Proper (pointwise_relation A (@entails_asn S) ++> (@entails_asn S))
         (@all_asn S A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold entails_asn, sm_rel, entails_up in *.
  simpl. firstorder.
Qed.

Instance all_asn_bientails_m (A : Type) S :
  Proper (pointwise_relation A (@bientails_asn S) ==> (@bientails_asn S))
         (@all_asn S A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold bientails_asn, sm_rel, bientails_up, entails_up in *.
  simpl. firstorder.
Qed.

Close Scope asn_scope.

Section DerivedProperties.
  Variable S : sep_alg.
  Implicit Types p q r : asn S.

  Open Scope asn_scope.

  Hint Extern 1 (?p |- ?p) => reflexivity.

  Lemma and_asnCL : forall p q, p </\> q |- q </\> p.
  Proof.
    auto using and_asnI, and_asnEL, and_asnER.
  Qed.

  Lemma and_asnC : forall p q, p </\> q -|- q </\> p.
  Proof.
    intros. rewrite bientails_asn_alt. auto using and_asnCL.
  Qed.

  Lemma and_asnA : forall p q r, (p </\> q) </\> r -|- p </\> q </\> r.
  Proof.
    intros. rewrite bientails_asn_alt.
    split; auto using and_asnI, and_asnEL, and_asnER.
  Qed.

  Lemma and_asnCA : forall p q r, p </\> q </\> r -|- q </\> p </\> r.
  Proof.
    intros; rewrite <- and_asnA, (and_asnC p), and_asnA; reflexivity.
  Qed.

  Lemma and_asnAC : forall p q r, (p </\> q) </\> r -|- (p </\> r) </\> q.
  Proof.
    intros; rewrite and_asnA, (and_asnC q), <- and_asnA; reflexivity.
  Qed.

  Lemma sc_asnA : forall p q r, (p <*> q) <*> r -|- p <*> q <*> r.
  Proof.
    intros; split; [apply sc_asnAL |].
    simpl; rewrite sc_upA; reflexivity.
  Qed.

  Lemma sc_asnA2 : forall p q r, p <*> q <*> r -|- (p <*> q) <*> r.
  Proof.
    intros; split; [|apply sc_asnAL ].
    simpl; rewrite sc_upA; reflexivity.
  Qed.

  Lemma sc_asnCA : forall p q r, p <*> q <*> r -|- q <*> p <*> r.
  Proof.
    intros; rewrite <- sc_asnA, (sc_asnC p), sc_asnA; reflexivity.
  Qed.

  Lemma sc_asnAC : forall p q r, (p <*> q) <*> r -|- (p <*> r) <*> q.
  Proof.
    intros; rewrite sc_asnA, (sc_asnC q), <- sc_asnA; reflexivity.
  Qed.

  Lemma and_idempotent_asn p : p </\> p -|- p.
  Proof.
    apply bientails_asn_alt.
    split; [apply and_asnEL | apply and_asnI]; reflexivity.
  Qed.

  Lemma later_and : forall p q, <|>> (p </\> q) -|- <|>> p </\> <|>> q.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_and_up.
  Qed.

  Lemma later_or : forall p q, <|>> (p <\/> q) -|- <|>> p <\/> <|>> q.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_or_up.
  Qed.

  Lemma later_sc : forall p q, <|>> (p <*> q) -|- <|>> p <*> <|>> q.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_sc_up.
  Qed.

  Lemma later_A T (P : T -> asn S) : <|>> (<A> x, P x) -|- <A> x,
    (<|>> P x).
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_A_up.
  Qed.

  Lemma later_A' T (P : T -> asn S) : <|>> (all_asn P) -|- <A> x,
    (<|>> P x).
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_A'_up.
  Qed.

  Lemma later_E T (P : T -> asn S) : <|>> (<E> x, P x) -|- <E> x,
    (<|>> P x).
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_E_up.
  Qed.

  Lemma later_E' T (P : T -> asn S) : <|>> (xist_asn P) -|- <E> x,
    (<|>> P x).
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_E'_up.
  Qed.

  Lemma later_pure : forall P : pure, @later_asn S (<pure> P) -|- <pure> P.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_pure_up.
  Qed.

  Lemma later_impl : forall p q, <|>> (p <->> q) -|- <|>> p <->> <|>> q.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_impl_up.
  Qed.

  Lemma later_si : forall p q, <|>> (p <-*> q) -|- <|>> p <-*> <|>> q.
  Proof.
    unfold bientails_asn, sm_rel; simpl; eauto using later_si_up.
  Qed.

  Lemma later_sm_const : forall P: upred S,
    <|>> sm_const P -|- sm_const (<|>> P)%upred.
  Proof. firstorder. Qed.

  Lemma pure_and_sc_asnR (P : asn S) Q :
    P </\> <pure> Q -|- P <*> <pure> Q.
  Proof.
    split.
    - intros h n [HP HQ]; simpl in *.
    exists h (sa_unit S); intuition.
    rewrite sa_unitI; simpl; reflexivity.
    intros h n [h1 [h2 [HLe [HP HQ]]]]; simpl in *; intuition.
    eapply up_mono; [| constructor | eassumption].
    remember (h1 · h2)%sa as oh; destruct oh as [ht |]; [| contradiction].
    transitivity ht; [exists h2; rewrite Heqoh; reflexivity | assumption].
  Qed.

  Lemma pure_and_sc_asnL (Q : asn S) P :
    <pure> P </\> Q -|- <pure> P <*> Q.
  Proof.
    rewrite sc_asnC. rewrite and_asnC. apply pure_and_sc_asnR.
  Qed.

  Lemma sc_asnEL (P Q R : asn S) :
    P |- R ->
    P <*> Q |- R.
  Proof.
    intros; rewrite sc_asnC, sc_asnME; [rewrite true_asn_unitR
      | apply true_asnR | reflexivity]; assumption.
  Qed.

  Lemma sc_asnER (P Q R : asn S) :
    Q |- R ->
    P <*> Q |- R.
  Proof.
    intros; rewrite sc_asnME; [rewrite true_asn_unitR | apply true_asnR
      | reflexivity]; assumption.
  Qed.

  Lemma distr_xist_and_asnL A (p: A -> asn S) q :
    (<E>x, p x) </\> q -|- <E>x, p x </\> q.
  Proof.
  - rewrite bientails_asn_alt. split.
    - apply impl_asnE with q; [|reflexivity]. apply xist_asnEIL. intros x.
      apply impl_asnI. apply xist_asnIR. exists x. reflexivity.
    - apply xist_asnEIL. intros x. eapply impl_asnE; [|reflexivity]. revert x.
      rewrite <- xist_asnEIL. apply impl_asnI. reflexivity.
  Qed.

  Lemma distr_xist_sc_asnL A (p: A -> asn S) q :
    (<E>x, p x) <*> q -|- <E>x, p x <*> q.
  Proof.
    rewrite bientails_asn_alt; split.
    - apply si_asnE with q; [| reflexivity]; apply xist_asnEIL; intros x.
      apply si_asnI; apply xist_asnIR; exists x; reflexivity.
    apply xist_asnEIL; intros x; eapply si_asnE; [| reflexivity].
    revert x; rewrite <- xist_asnEIL; apply si_asnI; reflexivity.
  Qed.

  Lemma distr_xist_and_asnR A (q: A -> asn S) p :
    p </\> (<E>x, q x) -|- <E>x, p </\> q x.
  Proof.
    setoid_rewrite and_asnC. apply distr_xist_and_asnL.
  Qed.

  Lemma distr_xist_sc_asnR A (q: A -> asn S) p :
    p <*> (<E>x, q x) -|- <E>x, p <*> q x.
  Proof.
    setoid_rewrite sc_asnC. apply distr_xist_sc_asnL.
  Qed.

  Lemma Prop_pure_EL (G Q : asn S) (P : Prop) :
    (P -> (G |- Q)) ->
    (G </\> <pure> Prop_pure P |- Q).
  Proof.
    intros HImp s h n [HG HP]; apply HImp; tauto.
  Qed.

  Lemma Prop_pure_IR (G Q : asn S) (P : Prop) :
    P -> (G |- Q) -> (G |- Q </\> <pure> Prop_pure P).
  Proof.
    intros; apply and_asnI; [assumption | intros ? ? ? _; assumption]. 
  Qed.

  Lemma const_xist T (P : T -> upred S) :
    (sm_const (xist_up P) -|- <E> x, sm_const (P x))%asn.
  Proof.
    split; intros h n HEq; simpl in *; assumption.
  Qed.

  Lemma const_sc (P Q : upred S) :
    (sm_const (P <*> Q)%upred -|- sm_const P <*> sm_const Q)%asn.
  Proof.
    split; intros h n HEq; simpl in *; assumption.
  Qed.
  
  Lemma const_si (P Q : upred S) :
    (sm_const (P <-*> Q)%upred -|- sm_const P <-*> sm_const Q)%asn.
  Proof.
    split; intros h n HEq; simpl in *; assumption.
  Qed.

  Lemma const_and (P Q : upred S) :
    (sm_const (P </\> Q)%upred -|- sm_const P </\> sm_const Q)%asn.
  Proof.
    split; intros h n HEq; simpl in *; assumption.
  Qed.

  Lemma const_pure (P : Prop) :
    ((sm_const (<pure> P)%upred : asn S) -|- <pure> (Prop_pure P))%asn.
  Proof.
    split; intros h n HEq; simpl in *; assumption.
  Qed.

  Lemma const_eq_prop (v v' : val) :
    (v == v' : asn S) |- <pure> Prop_pure (v = v').
  Proof.
    intros s h n HEq; simpl in *; assumption.
  Qed.

  Lemma existentialize_base (x : var) : (<true> : asn S) |- <E> v:val, x == v.
  Proof.
    intros s; simpl; apply xist_upIR; exists (s x).
    intros h n _; simpl; reflexivity.
  Qed.

  Lemma existentialize (P: asn S) (x : var) : P |- <E>v:val, x==v </\> P.
  Proof.
    rewrite and_asnI; [| apply true_asnR | reflexivity].
    rewrite existentialize_base with (x := x).
    rewrite distr_xist_and_asnL; reflexivity.
  Qed.

Lemma pure_and_sc_asnI (P : asn S) Q :
  P </\> <pure> Q |- P <*> <pure> Q.
Proof.
  rewrite pure_and_sc_asnR; reflexivity.
Qed.

Lemma pure_and_sc_asnE (P : asn S) Q :
  P <*> <pure> Q |- P </\> <pure> Q.
Proof.
  rewrite pure_and_sc_asnR; reflexivity.
Qed.

End DerivedProperties.

  Hint Rewrite
    distr_xist_and_asnR
    distr_xist_and_asnL
    distr_xist_sc_asnR
    distr_xist_sc_asnL
    : pullout_xist_asn.


Open Scope asn_scope.

Ltac asn_solve_singleton :=
  first [apply eq_pure_refl |
  match goal with
    | |- ?p |- true_asn ?S => apply true_asnR
    | |- ?p </\> ?q |- ?r =>
      (apply and_asnEL; asn_solve_singleton) || 
      (apply and_asnER; asn_solve_singleton)
    | _ => (apply entails_asn_preo) (* reflexivity *)
  end].

Lemma aux {S} : forall p, 
  true_asn S <->> p |- p.
Proof.
  intros.
  unfold entails_asn, sm_rel; simpl. intuition.
  unfold entails_up. intros. simpl in *. intuition.
Qed.

Ltac asn_solve :=
  match goal with
    | |- ?p |- ?q <->> true_asn ?S => apply impl_asnI; apply true_asnR
    | |- ?p |- true_asn ?S <->> ?q => apply impl_asnI; apply and_asnEL
    | |- ?p <->> true_asn ?S |- ?q => transitivity (true_asn S); [apply true_asnR|]
    | |- true_asn ?S <->> ?p |- ?q => transitivity p; [apply aux|]
    | |- ?p |- ?q <*> <true> =>
      rewrite (sc_asnC p (<true>));
      apply true_asn_unitL; asn_solve
    | |- ?p |- <true> <*> ?q =>
      apply true_asn_unitL; asn_solve
    | |- ?p <*> <true> |- ?q =>
      rewrite (sc_asnC p (<true>));
      transitivity p; [apply true_asn_unitR | asn_solve]
    | |- <true> <*> ?p |- ?q =>
      transitivity p; [apply true_asn_unitR | asn_solve]
    | |- ?p |- ?q <*> <pure>?r => transitivity (q </\> <pure>r); [asn_solve | apply pure_and_sc_asnI]
    | |- ?p |- <pure>?q <*> ?r => rewrite (sc_asnC (<pure>q) r); 
      transitivity (r </\> <pure>q); [asn_solve | apply pure_and_sc_asnI]
    | |- ?p <*> <pure>?q |- ?r => transitivity (p </\> <pure>q); [apply pure_and_sc_asnE | asn_solve]
    | |- <pure>?p <*> ?q |- ?r => rewrite (sc_asnC (<pure>p) q); 
      transitivity (q </\> <pure>p); [apply pure_and_sc_asnE | asn_solve]
    | |- ?p |- ?q </\> ?r =>
      apply and_asnI; [asn_solve | asn_solve]
    | |- ?p -|- ?q => apply bientails_asn_alt; split; [asn_solve | asn_solve]
    | _ => try asn_solve_singleton
  end.

Close Scope asn_scope.
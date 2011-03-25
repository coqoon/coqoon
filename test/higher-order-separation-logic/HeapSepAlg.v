Require Export AbstractSepAlg.
Require Export Lang.
Import SepAlgNotations.

Set Implicit Arguments.
Unset Strict Implicit.

Definition heap := PSM.t val.

Definition monAdd (k : PSM.key) (v : val) (oh : option heap) : option heap :=
  match oh with
    | Some h => match PSM.find k h with
                  | Some _ => None
                  | None   => Some (PSM.add k v h)
                end
    | None   => None
  end.
Definition dot (h1 h2 : heap) : option heap :=  PSM.fold monAdd h1 (Some h2).
Definition emp := PSM.empty val.
Lemma dot_emp : forall h, lift_option_rel PSM.Equal (dot h emp) (Some h).
Proof.
  intro h; symmetry; unfold dot; revert h.
  apply PSM'.fold_rec_weak with
    (P := fun h f => lift_option_rel PSM.Equal (Some h) f).
  (* EQ *)
  intros h h0 [h1 |] HEq0 HEq1; simpl in *; [transitivity h | contradiction];
    [symmetry|]; assumption.
  (* Empty *)
  simpl; reflexivity.
  (* Add *)
  intros k v [h0 |] h1 HNIn HInd; [| contradiction HInd]; simpl in *.
  rewrite HInd in HNIn; apply PSM'.not_find_in_iff in HNIn; rewrite HNIn; simpl.
  rewrite HInd; reflexivity.
Qed.

Add Morphism monAdd with signature
(@eq _) ==> (@eq _) ==> lift_option_rel PSM.Equal ==> lift_option_rel PSM.Equal
as mon_Add_Eq_m.
Proof.
  intros x v [h1 |] [h2 |] HEq; simpl in *; try discriminate || tauto.
  rewrite HEq; destruct (PSM.find x h2); [reflexivity | simpl].
  rewrite HEq; reflexivity. 
Qed.

Lemma trneq_monAdd : PSM'.transpose_neqkey (lift_option_rel PSM.Equal) monAdd.
Proof.
  intros k0 k1 v0 v1 [h |] HNEq; simpl; [|trivial].
  remember (PSM.find k1 h) as hk1; remember (PSM.find k0 h) as hk0;
    destruct hk0 as [hk0 |]; destruct hk1 as [hk1 |]; simpl.
  trivial.
  rewrite PSM'.add_neq_o; [rewrite <- Heqhk0; reflexivity | congruence].
  rewrite PSM'.add_neq_o; [rewrite <- Heqhk1; trivial | congruence].
  repeat rewrite PSM'.add_neq_o; [ | congruence | congruence ].
  rewrite <- Heqhk1; rewrite <- Heqhk0; simpl.
  intro k.
  repeat rewrite PSM'.add_o.
  destruct (PSM'.eq_dec k0 k); destruct (PSM'.F.eq_dec k1 k); try reflexivity.
  destruct e; destruct e0; contradiction HNEq; reflexivity.
Qed.

Lemma trneq_add: PSM'.transpose_neqkey PSM.Equal (@PSM.add val).
Proof.
  clear. red. intros. apply PSM'.Equal_mapsto_iff. PSM'.simp.
  firstorder congruence.
Qed.

Add Parametric Morphism T k (v : T) : (PSM'.Add k v) with signature
PSM.Equal ==> PSM.Equal ==> impl
as Add_Equal_m.
Proof.
  intros g h HEqgh g0 h0 HEqgh0 HAg k0.
  rewrite <- HEqgh0. rewrite <- HEqgh.
  apply HAg.
Qed.

Add Morphism (PSM.fold monAdd) with signature
PSM.Equal ==> lift_option_rel PSM.Equal ==> lift_option_rel PSM.Equal
as fold_monAdd_Eq_m.
Proof.
  intros; transitivity (PSM.fold monAdd x y0).
  (* snd *)
  apply PSM'.fold_rel with (R := fun x y => lift_option_rel PSM.Equal x y);
    [trivial |].
  intros k e [h0 |] [h1 |] HMT HInd; simpl in *; try contradiction || tauto.
  rewrite HInd; destruct (PSM.find k h1); simpl;
    [exact I | rewrite HInd; reflexivity].
  (* fst *)
  clear H0 x0; generalize dependent y.
  induction x using PSM'.map_induction.
  (* Empty *)
  intros.
  assert (Hx :=
    @PSM'.fold_Empty _ _ (lift_option_rel PSM.Equal) _ monAdd x y0 H).
  rewrite H0 in H.
  assert (Hy :=
    @PSM'.fold_Empty _ _ (lift_option_rel PSM.Equal) _ monAdd y y0 H).
  unfold dot; rewrite Hx; rewrite Hy; reflexivity.
  (* Add *)
  intros.
  assert (Hx := @PSM'.fold_Add _ _ (lift_option_rel PSM.Equal) _
    monAdd _ trneq_monAdd x1 x2 x3 e y0 H H0).
  setoid_rewrite H1 in H0.
  assert (Hy := @PSM'.fold_Add _ _ (lift_option_rel PSM.Equal) _
    monAdd _ trneq_monAdd x1 y x3 e y0 H H0).
  rewrite Hx; rewrite Hy; reflexivity.
Qed.

Add Morphism dot with signature
PSM.Equal ==> PSM.Equal ==> lift_option_rel PSM.Equal
as dot_Equal_mL.
Proof.
  intros; unfold dot; apply fold_monAdd_Eq_m; simpl; auto.
Qed.

Lemma fold_monAdd_None : forall h,
  lift_option_rel PSM.Equal (PSM.fold monAdd h None) None.
Proof.
  induction h using PSM'.map_induction_bis; intros.
  rewrite <- H; assumption.
  simpl; trivial.
  rewrite PSM'.fold_add; unfold Proper, respectful;
    auto using mon_Add_Eq_m, trneq_monAdd, opt_eq_rel, PSM'.EqualSetoid.
  rewrite IHh; simpl; trivial.
Qed.

Lemma dot_In_NoneR : forall k v h1 h2 (HIn : PSM.In (elt := val) k h1),
  lift_option_rel PSM.Equal (dot h1 (PSM.add k v h2)) None.
Proof.
  induction h1 using PSM'.map_induction_bis; intros.
  rewrite <- H in *; auto.
  PSM'.simp; contradiction.
  unfold dot; rewrite PSM'.fold_add; unfold Proper, respectful;
    auto using mon_Add_Eq_m, trneq_monAdd, opt_eq_rel, PSM'.EqualSetoid.
  rewrite <- PSM'.fold_commutes; auto.
  PSM'.simp; destruct HIn as [HEq | HIn].
  simpl; rewrite PSM'.add_eq_o; auto using fold_monAdd_None.
  simpl; rewrite PSM'.add_o; destruct (PSM'.eq_dec k x); simpl;
    auto using fold_monAdd_None.
  destruct (PSM.find x h2); simpl; auto using fold_monAdd_None.
  assert (HEQ : lift_option_rel PSM.Equal (Some (PSM.add x e (PSM.add k v h2)))
    (Some (PSM.add k v (PSM.add x e h2)))).
  simpl; intro y; repeat rewrite PSM'.add_o.
  destruct (PSM'.eq_dec x y); destruct (PSM'.eq_dec k y); simpl; auto.
  destruct e0; destruct e1; contradiction n; reflexivity.
  setoid_rewrite HEQ; apply IHh1; trivial.
  (* boring stuff *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.
  
Lemma monAdd_In_None : forall k v h (HIn : PSM.In (elt := val) k h),
  lift_option_rel PSM.Equal (monAdd k v (Some h)) None.
Proof.
  unfold monAdd; intros.
  rewrite PSM'.in_find_iff in HIn.
  destruct (PSM.find k h); [ | contradiction HIn]; reflexivity.
Qed.

Lemma NIn_monAdd_Some : forall k v h (HNIn : ~ PSM.In (elt:=val) k h),
  lift_option_rel PSM.Equal (monAdd k v (Some h)) (Some (PSM.add k v h)).
Proof.
  simpl; intros; rewrite PSM'.not_find_in_iff in HNIn; rewrite HNIn;
    reflexivity.
Qed.

Lemma dot_com : forall h1 h2, lift_option_rel PSM.Equal (dot h1 h2) (dot h2 h1).
Proof.
  induction h1 using PSM'.map_induction_bis; intro h2.
  (* EQ *)
  setoid_rewrite H in IHh1_1; apply IHh1_1.
  (* emp *)
  rewrite dot_emp; simpl; reflexivity.
  (* add *)
  unfold dot.

  rewrite PSM'.fold_add; [ | apply opt_eq_rel; apply PSM'.EqualSetoid
    | unfold Proper, respectful; auto using mon_Add_Eq_m | apply trneq_monAdd
    | apply H ].
  rewrite <- PSM'.fold_commutes; auto.

  destruct (PSM'.In_dec h2 x).

  rewrite monAdd_In_None; auto; rewrite fold_monAdd_None; symmetry;
    apply dot_In_NoneR; auto.
    
  rewrite NIn_monAdd_Some; auto; rewrite <- (NIn_monAdd_Some _ H).
  rewrite PSM'.fold_commutes; [ rewrite <- PSM'.fold_add | | | |];
    unfold Proper, respectful;
      auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid;
        apply IHh1.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.

Lemma dot_mon : forall h h0 h1 (HEq : PSM.Equal h0 h1),
  lift_option_rel PSM.Equal (dot h h0) (dot h h1).
Proof.
  intros; rewrite HEq; reflexivity.
Qed.

Lemma bin_add_lr : forall k v h oh (HNIn : ~ PSM.In k h),
  lift_option_rel PSM.Equal (bin_lift_option dot (Some (PSM.add k v h)) oh)
    (bin_lift_option dot (Some h) (monAdd k v oh)).
Proof.
  intros k v h [h0 |] HNIn; [| simpl; trivial].
  simpl bin_lift_option at 1; unfold dot at 1.
  rewrite PSM'.fold_add; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  symmetry; rewrite <- PSM'.fold_commutes; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid;
      symmetry; simpl; destruct (PSM.find k h0);
        [ apply fold_monAdd_None | reflexivity ].
Qed.

Lemma monAdd_In_add : forall k v h0 h1 (HIn : PSM.In k h0),
  lift_option_rel PSM.Equal (monAdd k v (dot h0 h1)) None.
Proof.
  induction h0 using PSM'.map_induction_bis; intros.
  rewrite <- H in *; apply IHh0_1; trivial.
  PSM'.simp; contradiction.
  unfold dot; rewrite PSM'.fold_add; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  PSM'.simp; destruct HIn as [HEq | HIn].
  fold (dot h0 h1); destruct (dot h0 h1); simpl; auto.
  destruct (PSM.find x h); simpl; auto.
  rewrite PSM'.add_eq_o; simpl; auto.
  rewrite <- PSM'.fold_commutes; auto.
  destruct (PSM'.In_dec h1 x).
  rewrite monAdd_In_None; simpl; auto.
  rewrite fold_monAdd_None; simpl; auto.
  rewrite NIn_monAdd_Some; auto; apply IHh0; auto.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.

Lemma dot_assoc : forall a b c,
  lift_option_rel PSM.Equal (bin_lift_option dot (dot a b) (Some c))
    (bin_lift_option dot (Some a) (dot b c)).
Proof.
  induction a using PSM'.map_induction_bis; intros.
  setoid_rewrite <- H; apply IHa1.
  simpl; destruct (dot b c); simpl; [reflexivity | trivial].
  (* add *)
  unfold dot; rewrite PSM'.fold_add; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite <- PSM'.fold_commutes; auto.
  rewrite bin_add_lr; auto.
  
  destruct (PSM'.In_dec b x).

  fold (dot b c); rewrite monAdd_In_add; auto.
  rewrite monAdd_In_None; auto.
  rewrite fold_monAdd_None; simpl; trivial.

  rewrite <- PSM'.fold_add; auto.
  rewrite NIn_monAdd_Some; auto.
  apply IHa.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.

Lemma not_in_ok : forall a b c k v (HNa : ~ PSM.In k a) (HNb : ~ PSM.In k b)
  (HEq : lift_option_rel PSM.Equal (dot a b) (Some c)),
  lift_option_rel PSM.Equal (monAdd k v (dot a b)) (Some (PSM.add k v c)).
Proof.
  induction a using PSM'.map_induction_bis; intros.
  rewrite <- H in *; apply IHa1; auto.
  replace (dot (PSM.empty val) b) with (Some b) in * by reflexivity.
  simpl in HEq; rewrite HEq in *.
  repeat rewrite NIn_monAdd_Some; auto; reflexivity.
  PSM'.simp.
  destruct (PSM'.In_dec b x).
  unfold dot in HEq; rewrite PSM'.fold_add in HEq; try tauto.
  rewrite <- PSM'.fold_commutes in HEq; try tauto;  unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite monAdd_In_None in HEq; [| assumption ];
    rewrite fold_monAdd_None in HEq; contradiction HEq.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  (* end boring *)
  unfold dot; rewrite PSM'.fold_add; try tauto; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite <- PSM'.fold_commutes; try tauto.
  rewrite NIn_monAdd_Some; auto; fold (dot a (PSM.add x e b)).
  apply IHa; PSM'.simp; try tauto.
  unfold dot in *; rewrite PSM'.fold_add in HEq; try tauto.
  rewrite <- PSM'.fold_commutes in HEq; try tauto; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite NIn_monAdd_Some in HEq; auto.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.

Lemma monAddSome : forall oh h k v
  (HEq : lift_option_rel PSM.Equal (monAdd k v oh) (Some h)),
  exists h0, lift_option_rel PSM.Equal oh (Some h0) /\
    PSM.Equal (PSM.add k v h0) h.
Proof.
  intros [h0 |] h k v HEq; [|simpl in *; contradiction].
  destruct (PSM'.In_dec h0 k).
  rewrite monAdd_In_None in HEq; [contradiction | assumption].
  rewrite NIn_monAdd_Some in HEq; [| assumption]; simpl in *.
  exists h0; split; [ reflexivity | assumption ].
Qed.

Lemma NIn_dot : forall a b c k
  (HEq : lift_option_rel PSM.Equal (dot a b) (Some c))
  (HNa : ~ PSM.In k a)
  (HNb : ~ PSM.In k b),
  ~ PSM.In k c.
Proof.  
  induction a using PSM'.map_induction_bis; intros.
  rewrite <- H in *; eapply IHa1; eauto.
  simpl in *; rewrite <- HEq; assumption.
  PSM'.simp.
  unfold dot in HEq; rewrite PSM'.fold_add in HEq; try tauto.
  fold (dot a b) in HEq; apply monAddSome in HEq.
  destruct HEq as [d [HEq HEq0]].
  rewrite <- HEq0; PSM'.simp.
  intros [HEqk | HIn]; [tauto |].
  revert HIn; apply IHa with b; tauto.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.

Lemma NIn_eq : forall a b k (v:val)
  (HNa : ~ PSM.In k a)
  (HNb : ~ PSM.In k b)
  (HEq : PSM.Equal (PSM.add k v a) (PSM.add k v b)),
  PSM.Equal a b.
Proof.
  induction a using PSM'.map_induction_bis; intros.
  rewrite <- H in *; eapply IHa1; eauto.
  intro m; specialize (HEq m); simpl in *.
  repeat rewrite PSM'.add_o in HEq.
  destruct (PSM'.eq_dec k m); simpl in *; [|assumption].
  rewrite PSM'.not_find_in_iff in HNb.
  rewrite PSM'.empty_o; destruct e; rewrite HNb; reflexivity.
  intro m; specialize (HEq m); simpl in *.
  repeat rewrite PSM'.add_o in HEq.
  destruct (PSM'.eq_dec k m); simpl in *.
  rewrite PSM'.not_find_in_iff in HNa; rewrite PSM'.not_find_in_iff in HNb;
    destruct e0; rewrite HNa; rewrite HNb; reflexivity.
  rewrite PSM'.add_o; destruct (PSM'.eq_dec x m); simpl in *; auto.
Qed.

Lemma dot_cancel : forall a b c d
  (Hab : lift_option_rel PSM.Equal (dot a b) (Some d))
  (Hac : lift_option_rel PSM.Equal (dot a c) (Some d)),
  PSM.Equal b c.
Proof.
  induction a using PSM'.map_induction_bis; intros.
  apply IHa1 with d; rewrite H; auto.
  simpl in *; rewrite Hab; rewrite Hac; reflexivity.
  destruct (PSM'.In_dec b x).
  (* dot (x,v):a b = None *)
  unfold dot in Hab.
  rewrite PSM'.fold_add in Hab; auto.
  rewrite <- PSM'.fold_commutes in Hab; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite monAdd_In_None in Hab; [| assumption];
    rewrite fold_monAdd_None in Hab; contradiction Hab.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  (* end boring *)
  destruct (PSM'.In_dec c x).
  (* dot (x,v):a c = None *)
  unfold dot in Hac.
  rewrite PSM'.fold_add in Hac; auto.
  rewrite <- PSM'.fold_commutes in Hac; unfold Proper, respectful;
    auto using mon_Add_Eq_m, opt_eq_rel, trneq_monAdd, PSM'.EqualSetoid.
  rewrite monAdd_In_None in Hac; [| assumption];
    rewrite fold_monAdd_None in Hac; contradiction Hac.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  (* end boring *)
  (* inductive case *)
  unfold dot in Hab, Hac.
  rewrite PSM'.fold_add in Hab; auto.
  rewrite PSM'.fold_add in Hac; auto.
  fold (dot a b) in Hab; fold (dot a c) in Hac.
  apply monAddSome in Hab; apply monAddSome in Hac.
  destruct Hab as [h0 [HEq0 HAdd0]]; destruct Hac as [h1 [HEq1 HAdd1]].
  apply IHa with h0; [assumption |].
  rewrite HEq1; apply NIn_eq with x e;
    [ eapply NIn_dot | eapply NIn_dot | rewrite <- HAdd0 in HAdd1 ]; eauto.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
Qed.


Definition heap_alg := SepAlg  (SepAlgMixin (PSM'.EqualSetoid val)
  dot_com dot_assoc dot_emp dot_mon dot_cancel).

Lemma in_dot_fail : forall (h1 h2 : heap_alg) k
  (HMT1 : PSM.In k h1)
  (HMT2 : PSM.In k h2),
  (h1 · h2 == None)%sa.
Proof.
  intros h1 h2 k; unfold sa_mul, sa_op, heap_alg, dot in *; simpl in *;
    remember (PSM.fold monAdd h1 (Some h2))%sa as oh; generalize dependent oh;
      apply PSM'.fold_rec_bis; intros; subst.
  setoid_rewrite H in H0; eauto.
  rewrite PSM'.empty_in_iff in HMT1; contradiction.
  rewrite PSM'.add_in_iff in HMT1; destruct HMT1 as [HEq | HMT1]; [subst |].
  fold (dot (PSM.add k e m') h2); rewrite dot_com; apply dot_In_NoneR; auto.
  rewrite PSM'.fold_add, H1; simpl; eauto.
  (* boring, again *)
  split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
    [reflexivity | symmetry | transitivity y]; auto.
  unfold Proper, respectful, flip; simpl; intros.
  apply mon_Add_Eq_m; auto.
  unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
  (* end boring *)
Qed.

Lemma heap_monAdd_add k v (h h': heap_alg) :
  (((monAdd k v (Some h)): option heap_alg) == Some h')%sa <->
  ~PSM.In k h /\ PSM.Equal h' (PSM.add k v h).
Proof.
split.
- intros H. simpl in H. remember (PSM.find (elt:=val) k h) as v'.
  destruct v'; [inversion H|]. simpl in H. rewrite <- H. split; [|reflexivity].
  symmetry in Heqv'. rewrite <- PSM'.not_find_in_iff in Heqv'. assumption.
- intros [HnotIn HEqual]. rewrite HEqual. apply NIn_monAdd_Some. assumption.
Qed.

Lemma heap_In_dot k (h1 h2 h: heap_alg) :
  (h1 · h2 == Some h)%sa -> PSM.In k h2 -> PSM.In k h.
Proof.
  intros Hdot HIn_h2. unfold sa_mul in Hdot. simpl in Hdot. revert Hdot.
  unfold dot. revert h.
  pattern h1. apply PSM'.map_induction_bis.
  - introv HEqual P Hdot. apply P. rewrite HEqual. assumption.
  - simpl. introv Heq. rewrite <- Heq. assumption.
  - intros k' v m HnotIn IH h HmonAdd.
    rewrite PSM'.fold_add in HmonAdd; eauto with typeclass_instances.
    - destruct (PSM.fold monAdd m (@Some heap h2)) as [h'|].
      - simpl in HmonAdd. destruct (PSM.find k' h'); [contradiction|].
        simpl in HmonAdd. rewrite <- HmonAdd. apply PSM'.add_in_iff.
        right. apply IH. reflexivity.
      - contradiction HmonAdd.
    (* boring stuff *)
    - split; unfold Reflexive, Symmetric, Transitive, flip; simpl; intros;
      [reflexivity | symmetry | transitivity y]; auto.
    - unfold Proper, respectful, flip; simpl; intros.
    - apply mon_Add_Eq_m; auto.
      unfold flip, PSM'.transpose_neqkey; intros; apply trneq_monAdd; intuition.
    (* end boring *)
Qed.

Lemma heap_dot_update (h h' hh': heap_alg) : (h · h' == Some hh')%sa <->
  PSM'.Disjoint h h' /\ PSM.Equal hh' (PSM'.update h' h).
Proof.
  split.
  (* Left to right *)
  - unfold sa_mul, PSM'.update; simpl. unfold dot. revert hh'.
    apply (PSM'.map_induction (P := fun h =>
        forall hh' : heap_alg,
        ((PSM.fold monAdd h (Some h') : option heap_alg) == Some hh')%sa ->
        PSM'.Disjoint h h' /\ PSM.Equal hh' (PSM.fold (@PSM.add _) h h')
      )).
    - introv HEmpty.
      repeat rewrite PSM'.fold_Empty; eauto with typeclass_instances.
      firstorder.
    - intros m m' IH k v HnotIn HAdd hh'.
      rewrite PSM'.fold_Add;
        eauto using trneq_monAdd with typeclass_instances.
      setoid_rewrite PSM'.fold_Add at 2;
        eauto using trneq_add with typeclass_instances.
      intros H.
      assert (exists m'',
              ((PSM.fold monAdd m (Some h')): option heap_alg) == Some m'')%sa
        as [m'' Hm''].
      - destruct (PSM.fold monAdd m (Some h')) as [m''|]; [|contradiction H].
        exists m''. reflexivity.
      destruct (IH _ Hm'') as [IHDisjoint IHEqual]. clear IH.
      pose proof (PSM'.Add_in_iff HAdd) as HAdd'. clear HAdd.
      rewrite Hm'' in H. rewrite heap_monAdd_add in H.
      destruct H as [HnotIn_m'' Hhh']. rewrite IHEqual in Hhh'.
      split; [|assumption]. intros k' [Hm' Hh'].
      unfold PSM'.Disjoint, not in IHDisjoint. eapply IHDisjoint.
      split; [|apply Hh']. destruct HAdd' with k' as [HAdd _].
      destruct HAdd as [?|Hkeq]; try assumption; []. subst.
      specialize (IHDisjoint k).
      assert (PSM.fold monAdd m (@Some heap_alg h') = (dot m h'))
        as Heq by reflexivity.
      rewrite Heq in Hm''. apply False_ind.
      eapply heap_In_dot in Hh'; [|apply Hm'']. contradiction.
  (* Right to left *)
  - unfold sa_mul, PSM'.update; simpl. unfold dot. revert hh'.
    apply (PSM'.map_induction (P := fun h =>
        forall hh' : heap_alg,
        PSM'.Disjoint h h' /\ PSM.Equal hh' (PSM.fold (@PSM.add _) h h') ->
        ((PSM.fold monAdd h (Some h') : option heap_alg) == Some hh')%sa
      )).
    - introv HEmpty.
      repeat rewrite PSM'.fold_Empty; eauto with typeclass_instances.
      firstorder.
    - intros m m' IH k v HnotIn HAdd hh'.
      rewrite PSM'.fold_Add;
        eauto using trneq_add with typeclass_instances.
      setoid_rewrite PSM'.fold_Add at 2;
        eauto using trneq_monAdd with typeclass_instances.
      intros [HDisjoint HEqual]. rewrite (IH (PSM.fold (@PSM.add _) m h')).
      - assert (~PSM.In k h') as HnotIn_h'.
        - intro HIn_h'. unfold PSM'.Disjoint, not in HDisjoint.
          apply HDisjoint with k. split; [|assumption].
          rewrite (PSM'.Add_in_iff HAdd). auto.
        simpl.
        replace (PSM.fold (@PSM.add _) m h') with (PSM'.update h' m) in *
          by reflexivity.
        remember (PSM.find k (PSM'.update h' m)) as find_k.
        destruct find_k as [v'|].
        - symmetry in Heqfind_k. apply PSM'.find_mapsto_iff in Heqfind_k.
          apply PSM'.update_mapsto_iff in Heqfind_k. contradict Heqfind_k.
          firstorder.
        - congruence.
      - split; [|reflexivity]. pose proof (PSM'.Add_in_iff HAdd) as HAdd'.
        clear - HnotIn HDisjoint HAdd'. intros k' [Hk'm Hk'h'].
        specialize (HDisjoint k'). specialize (HAdd' k'). tauto.
Qed.

Lemma heap_sub_alt (h h': heap_alg): (h <= h')%sa <-> PSM'.Sub h h'.
Proof.
  unfold subheap. setoid_rewrite heap_dot_update.
  unfold PSM'.Sub. unfold PSM'.Disjoint. setoid_rewrite PSM'.Equal_mapsto_iff.
  setoid_rewrite PSM'.update_mapsto_iff. split; [firstorder|].
  intros H. exists (PSM'.diff h' h).
  setoid_rewrite PSM'.diff_in_iff. setoid_rewrite PSM'.diff_mapsto_iff.
  split; [firstorder|]. intros k v. split; [|firstorder].
  intros Hh'. destruct (PSM'.In_dec h k) as [HIn|?]; [|firstorder].
  left. destruct HIn as [v' Hv']. change (PSM.MapsTo k v' h) in Hv'.
  destruct (val_dec v v') as [|Hne]; [congruence|].
  contradict Hne. eauto using PSM'.MapsTo_fun.
Qed.

Lemma mapsto_dot : forall (h1 h2 h : heap_alg) k v
  (HMT  : PSM.MapsTo k v h)
  (HDot : (h1 · h2 == Some h)%sa),
  PSM.MapsTo k v h1 \/ PSM.MapsTo k v h2.
Proof.
  intros.
  rewrite heap_dot_update in HDot; destruct HDot as [HDisj HUpd].
  rewrite HUpd in HMT.
  rewrite PSM'.update_mapsto_iff in HMT; intuition.
Qed.
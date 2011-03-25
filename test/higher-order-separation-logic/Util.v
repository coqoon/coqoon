(* General utility definitions *)

Require Export Setoid RelationClasses Morphisms.
Require Export List.
Require Export ZArith.
Require Export Omega.
Require Export String.
Require Export DecidableType.
Require Export DecidableTypeEx.
Require Export FSetWeakList FSetFacts FSetProperties FSetDecide. 
Require Export FMapWeakList FMapFacts.
Require Export Bool.
Require Export Program.Basics Program.Tactics Program.Syntax.

(* Needed with current definition of stacks and stack monads *)
Require Export FunctionalExtensionality.

(* If obligations cannot be discharged completely, backtrack and leave goal
   untouched. *)
Obligation Tactic := try solve [program_simpl].

(* For readability of proof scripts *)
Tactic Notation "-" tactic(t) := t.

(* Like intros, but keeping the names of already-named hypotheses. *)
Ltac intro_named :=
  match goal with
  | |- ?P -> ?Q => let tmp := fresh "tmp" in
                   intro tmp; intro_named; revert tmp
  | |- ?P -> _ => intro; intro_named
  | _ => idtac
  end.
Tactic Notation "introv" simple_intropattern(i1) := intro_named; intros i1.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) := intro_named; intros i1 i2.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) simple_intropattern(i3) :=
  intro_named; intros i1 i2 i3.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) simple_intropattern(i3)
  simple_intropattern(i4) := intro_named; intros i1 i2 i3 i4.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) simple_intropattern(i3)
  simple_intropattern(i4) simple_intropattern(i5) := intro_named; intros i1 i2 i3 i4 i5.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) simple_intropattern(i3)
  simple_intropattern(i4) simple_intropattern(i5) simple_intropattern(i6) :=
  intro_named; intros i1 i2 i3 i4 i5 i6.
Tactic Notation "introv" simple_intropattern(i1) simple_intropattern(i2) simple_intropattern(i3)
  simple_intropattern(i4) simple_intropattern(i5) simple_intropattern(i6) simple_intropattern(i7)
  := intro_named; intros i1 i2 i3 i4 i5 i6 i7.

(* setoid_rewrite in goal and all hypotheses. *)
Ltac sr lem := try match goal with H : _ |- _ => setoid_rewrite lem in H end;
               try setoid_rewrite lem.

(*Decision procedures for pairs and natural numbers. 
  This should be in the standard library somewhere *)

Lemma pair_dec : forall {A B},
  (forall a b : A, {a = b} + {a <> b}) ->
  (forall c d : B, {c = d} + {c <> d}) ->
  forall e f : (A * B), {e = f} + {e <> f}.
Proof.
  decide equality.
Defined.

Lemma nat_dec : forall (n m :nat), {n = m} + {n <> m}.
Proof.
  decide equality.
Defined.

(*Tactics that clears assumptions with several disjunctions of False *)
Ltac clear_disj :=
  match goal with
    | [H : False |- _] => destruct H
    | [H : ?P \/ False |- _] => destruct H as [H | H]; [clear_disj | destruct H]
    | [H : False \/ ?P |- _] => destruct H as [H | H]; [destruct H | clear_disj]
    | [H : ?P \/ ?Q |- _] => destruct H as [H | H]; [clear_disj | clear_disj]
    | _ => idtac
  end.

(* Conjoin any proposition to the goal. Useful before induction. *)
Ltac strengthen P :=
  let tmp := fresh "tmp" in
  match goal with |- ?g =>
    assert (tmp: g /\ P); [|exact (proj1 tmp)]
  end.

Notation "\ x , a" := (fun x => a) (at level 200, x ident, right associativity,
  format "\ x , '/ '  a").
Notation "\ x : T , a" := (fun x:T => a) (at level 200, x ident, right associativity,
  format "\ x :  T , '/ '  a").
Notation "\_ , a" := (fun _ => a) (at level 200, right associativity,
  format "\_ ,  '/ '  a").

(* A scary general fold function that facilitates writing functions that take a variable
   number of curried arguments. *)
Fixpoint lfoldl_dep (R: Type) (F: Type -> Type) (G: Type -> Type)
    (f: forall A As', G A -> F (A -> arrows As' R) -> F (arrows As' R))
    (As: list Type) : forall acc: F (arrows As R), arrows (map G As) (F R) :=
  match As with
  | nil => fun acc => acc
  | A::As' => fun acc: F (A -> arrows As' R) =>
              fun x: G A =>
                lfoldl_dep _ _ _ f As' (f _ _ x acc)
  end.

Definition search_cast {P': Prop} (P: Prop) (imp: P -> P') (search: option P)
    : option P' :=
  match search with
  | Some pf => Some (imp pf)
  | None => None
  end.

Definition sumbool_to_option {P Q} (sb: {P}+{Q}) : option P :=
  match sb with
  | left pf => Some pf
  | right _ => None
  end.

Program Definition list_search_forall
      {elt} {P': elt -> Prop} (search: forall x, option (P' x)) (s: list elt)
      : option (forall x, In x s -> P' x) :=
    match forallb (fun x => if search x then true else false) s with
    | true => Some _
    | false => None
    end.
  Next Obligation.
  introv ? Hall. introv HIn. subst.
  symmetry in Hall. rewrite forallb_forall in Hall.
  specialize (Hall _ HIn).
    remember (search x) as pf'. destruct pf' as [pf|]; [auto | discriminate].
  Qed.

Lemma opt_dec {A} : forall
  (HDec: forall (e1 e2:A), {e1 = e2} + {e1 <> e2})
  (o1 o2: option A),
  {o1 = o2} + {o1 <> o2}.
Proof.
  decide equality.
Defined.

Ltac mapsto_element := constructor; simpl; constructor.

Ltac mapsto_list := 
  match goal with 
    | |- InA _ _ (_::_) =>
      rewrite InA_cons; first [left; mapsto_element | right; mapsto_list]
    | |- InA _ _ ([]) => fail
  end.

Module WMapMore_fun (E: UsualDecidableType) (M: FMapInterface.WSfun E).
  Module P := FMapFacts.WProperties_fun E M.
  Include P.F.
  Include P.
  Import M.

  Definition Sub {elt} m m' := forall k (e:elt), MapsTo k e m -> MapsTo k e m'.

  Lemma Sub_reflexive {T}: reflexive _ (@Sub T). Proof. firstorder. Qed.
  Lemma Sub_transitive {T}: transitive _ (@Sub T). Proof. firstorder. Qed.

  Add Parametric Relation T : _ (@Sub T)
    reflexivity proved by Sub_reflexive
    transitivity proved by Sub_transitive
    as Sub_r.

  Add Parametric Morphism A: (@Disjoint A) with signature
    Sub --> Sub --> impl
    as Disjoint_Sub.
  unfold Disjoint. unfold not, impl. intros m1 m1' H1 m2 m2' H2 H k HIn.
  assert (In k m1) by firstorder.
  assert (In k m2) by firstorder.
  firstorder.
  Qed.

  Add Parametric Morphism A: (@In A) with signature
    eq ==> Sub ++> impl
    as In_impl_m.
  Proof. firstorder. Qed.

  Definition singleton {elt} k (e: elt) := add k e (empty elt).

  Lemma singleton_mapsto_iff : forall {elt} k k' (e e': elt),
    MapsTo k e (singleton k' e') <-> k=k' /\ e=e'.
  Proof. intros. unfold singleton. map_iff. intuition. Qed.

  Lemma singleton_in_iff : forall {elt} k k' (e': elt),
    In k (singleton k' e') <-> k=k'.
  Proof. intros. unfold singleton. map_iff. intuition. Qed.

  Lemma Add_in_iff : forall {elt} {k} {v:elt} {m m'}, Add k v m m' -> forall k',
    (In k' m' <-> (In k' m \/ k' = k)).
  Proof.
    unfold Add. introv HAdd. split.
    - intros HIn. destruct (eq_dec k' k) as [Heq|Hne].
      - right. assumption.
      - left. specialize (HAdd k'). rewrite add_neq_o in HAdd; [|intuition].
        rewrite in_find_iff in *. congruence.
    - intros [HIn|Heq].
      - specialize (HAdd k'). rewrite in_find_iff in *. rewrite HAdd.
        rewrite add_o. destruct (eq_dec k k'); congruence.
      - subst. specialize (HAdd k). rewrite in_find_iff. rewrite HAdd.
        rewrite add_eq_o; congruence.
  Qed.

  Ltac simp :=
    repeat progress (
      sr diff_mapsto_iff
    ; sr diff_in_iff
    ; sr empty_mapsto_iff
    ; sr empty_in_iff
    ; sr add_mapsto_iff
    ; sr add_in_iff
    ; sr singleton_mapsto_iff
    ; sr singleton_in_iff
    ).

  Program Definition search_exists_val
    {elt} {P': elt -> Prop} (k : key) (search: forall e, option (P' e))
      (m: t elt) : option (exists e, MapsTo k e m /\ P' e) :=
      match find k m with
        | Some e => if search e then Some _ else None
        | None => None
      end.
  Next Obligation.
    simpl; introv search Heq Hex; symmetry in Heq.
    rewrite <- find_mapsto_iff in Heq; eauto.
  Qed.

  Program Definition search_forall
      {elt} {P': key -> elt -> Prop} (search: forall k e, option (P' k e))
                                     (m: t elt)
      : option (forall k e, MapsTo k e m -> P' k e) :=
    match for_all (fun k e => if search k e then true else false) m with
    | true => Some _
    | false => None
    end.
  Next Obligation.
  introv ? Hall. subst. introv Hmaps.
  symmetry in Hall. eapply (proj1 (for_all_iff _ _)) in Hall; [|eassumption].
  simpl in Hall. remember (search k e) as pf'.
  destruct pf' as [pf|]; [auto | discriminate].
  Qed.

  Program Definition search_cond_forall
    {elt} {P': key -> elt -> Prop} {Q: key -> elt -> Prop} 
    (dec: forall k e, {Q k e} + {~(Q k e)})
    (search : forall k e, option (P' k e))
    (m: t elt) : option (forall k e, MapsTo k e m -> Q k e -> P' k e) :=
    search_cast (forall k e, MapsTo k e m -> Q k e -> P' k e) _
    (search_forall (fun k e => match dec k e with
                                 | left pf => match search k e with
                                                | Some pf' => Some _
                                                | None => None
                                              end
                                 | right pf => Some _
                               end) m).
Next Obligation.
intros. contradiction pf.
Qed.

Lemma dec_mapsto {elt}: forall 
  (HDec: forall (e1 e2:elt), {e1 = e2} + {e1 <> e2})
  k (e:elt) m,
  (Decidable.decidable (MapsTo k e m)).
Proof.
  unfold Decidable.decidable. intros.
  repeat rewrite find_mapsto_iff.
  destruct (@opt_dec elt HDec (find k m) (Some e)); intuition.
Qed.

Ltac mapsto_tac :=
  match goal with
    | |- MapsTo _ _ _ => rewrite elements_mapsto_iff; simpl; mapsto_list
  end.

End WMapMore_fun.

Module WSetMore_fun (E : UsualDecidableType) (M : FSetInterface.WSfun E).
  Include FSetFacts.WFacts_fun E M.
  Include FSetProperties.WProperties_fun E M.
  Include FSetDecide.WDecide_fun E M.
  Include FSetDecideAuxiliary.

  Import M.

  Program Definition search_forall
      {P': elt -> Prop} (search: forall x, option (P' x)) (s: t)
      : option (forall x, In x s -> P' x) :=
    match for_all (\x, if search x then true else false) s with
    | true => Some _
    | false => None
    end.
  Next Obligation.
  introv ? Hall. introv HIn. subst.
  symmetry in Hall. rewrite <- for_all_iff in Hall; [|].
  - unfold For_all in Hall. specialize (Hall _ HIn).
    remember (search x) as pf'. destruct pf' as [pf|]; [auto | discriminate].
  - intros a b Heq. rewrite Heq. reflexivity.
  Qed.

Lemma diff_ineq: forall s s' s''
  (Hineq: ~ Equal s s')
  (HSub1: Subset s s'')
  (HSub2: Subset s' s''),
  ~ Equal (diff s'' s) (diff s'' s').
Proof.
  intros.
  intro Heq. apply Hineq. clear Hineq. unfold Equal in *. 
  unfold Subset in *. intro x.
  specialize (Heq x). specialize (HSub1 x). specialize (HSub2 x). 
  repeat rewrite diff_iff in Heq. destruct Heq. split.
  - intros. specialize (HSub1 H1). 
    destruct (dec_In x s'). assumption. intuition.
  - intros. specialize (HSub2 H1). 
    destruct (dec_In x s). assumption. intuition.
Qed.

Lemma subset_diff2: forall s s' s''
  (HSub: Subset s s'),
  Subset (diff s'' s') (diff s'' s).
Proof.
  unfold Subset. intros. rewrite diff_iff in *; intuition.
Qed.

Lemma card_diff_le: forall s s',
  cardinal (diff s s') <= cardinal s.
Proof.
  intros.
  assert (Subset (diff s s') s) by apply diff_subset.
  apply subset_cardinal in H. assumption.
Qed.

Lemma subset_cardinal2: forall s' s
  (Hneq: ~ Equal s s')
  (Hsub: Subset s s'),
  cardinal s < cardinal s'.
Proof.
  intro s'. induction s' using set_induction_bis.
  - intros. rewrite <- H in Hneq. rewrite <- H in Hsub. rewrite <- H. intuition.
  - intros. contradict Hneq. unfold Equal. unfold Subset in Hsub.
    intros x. specialize (Hsub x). rewrite empty_iff in *. intuition.
  - intros. 
    destruct (dec_In x s). 
    assert (~ Equal (remove x s) s'). intro Heq. apply Hneq.
    - unfold Equal in Heq. unfold Equal. intro y. specialize (Heq y).
      rewrite add_iff. rewrite remove_iff in Heq.
      destruct Heq as [Hl Hr]. split.
      - intros. destruct (dec_eq x y) as [Heq | Hneq2].
        - subst. intuition.
        - intuition.
      - intros. destruct H1 as [Heq | HIn].
        - subst. intuition.
        - intuition.
    assert (Subset (remove x s) s') as HSub.
    - unfold Subset. intros y HIn. 
      rewrite remove_iff in HIn. destruct HIn as [HIn Hneq2]. 
      unfold Subset in Hsub. specialize (Hsub y). rewrite add_iff in Hsub.
      intuition.
    specialize (IHs' _ H1 HSub).
    assert (S (cardinal (remove x s)) < S (cardinal s')) by omega.
    rewrite remove_cardinal_1 in H2; try assumption.
    rewrite add_cardinal_2; assumption.
    - eapply subset_cardinal_lt; try eassumption; 
                                     try (rewrite add_iff; intuition).
      firstorder.
Qed.

Lemma diff_subset_zero : forall s s'
  (Hcard: cardinal (diff s s') = 0),
  Subset s s'.
Proof.
  intros. rewrite <- cardinal_Empty in Hcard.
  unfold Empty in Hcard. unfold Subset. intros x HIn.
  specialize (Hcard x). rewrite diff_iff in Hcard.
  destruct (dec_In x s'). assumption. intuition.
Qed.

Lemma dec_Empty: forall s, (Decidable.decidable (Empty s)).
Proof.
  unfold Decidable.decidable. intros.
  rewrite is_empty_iff.
  destruct (bool_dec (is_empty s) true); intuition.
Qed.

Lemma dec_Equal: forall s s', (Decidable.decidable (Equal s s')).
Proof.
  intros. unfold Decidable.decidable. rewrite equal_iff.
  destruct (bool_dec (equal s s') true); intuition.
Qed.

End WSetMore_fun.

Program Fixpoint list_dec {A} (A_dec: forall a b : A, {a=b}+{a<>b}) (xs ys : list A) :
  {xs=ys}+{xs<>ys} :=
  match xs, ys with
    | [], [] => left _
    | x::xs', y::ys' => match list_dec A_dec xs' ys' with
                          | left pft => match A_dec x y with
                                          | left pfh => left _
                                          | right pf => right _
                                        end
                          | right pf => right _
                        end
    | _, _ => right _
  end.
Next Obligation.
  simpl; intros.
  intro HW; subst.
  destruct xs; destruct ys; try discriminate.
  destruct H; contradiction H0; auto.
  injection HW; intros; subst.
  destruct H as [H _]; eapply H; eauto.
Qed.
Next Obligation.
  simpl; intros; split; intros.
  intros [Hd _]; discriminate.
  intros [_ Hd]; discriminate.
Defined.
Next Obligation.
  simpl; intros; split; intros.
  intros [_ Hd]; discriminate.
  intros [Hd _]; discriminate.
Defined.

Definition search_dec_eq {A}
  (A_dec : forall a b : A, {a=b}+{a<>b}) (a b : A) : option (a = b) :=
  sumbool_to_option (A_dec a b).

Program Fixpoint search_NoDup
    {A} (A_dec: forall a b: A, {a=b}+{a<>b}) (l: list A) : option (NoDup l) :=
  match l with
  | [] => Some _
  | a::l' =>
      match search_NoDup A_dec l' with
      | Some nodup =>
          match In_dec A_dec a l' with
          | left isin => None
          | right notin => Some _
          end
      | None => None
      end
  end.
Next Obligation.
intros. constructor.
Qed. Next Obligation.
intros. subst. constructor; assumption.
Defined.

(* Funny function borrowed from Chlipala's book *)
Definition option_proof {P: Prop} (pf': option P) :=
  match pf' return (match pf' with Some _ => P | None => True end) with
  | Some pf => pf
  | None => I
  end.

(* Tactic for applying a proof search procedure *)
Ltac search pf' := exact (option_proof pf') || fail "No proof found".

Ltac search_NoDup dec :=
  match goal with
  |- NoDup ?l => search (search_NoDup dec l)
  end.

Fixpoint zip {A} {B} (lst1 : list A) (lst2 : list B) : list (A * B) :=
  match (lst1, lst2) with
    | (nil, nil) => nil
    | (x::xs, nil) => nil
    | (nil, y::ys) => nil
    | (x::xs, y::ys) => (x, y) :: zip xs ys
  end.

Program Fixpoint zipLen {A} {B} (xs : list A) (ys : list B) (P : List.length xs = List.length ys) :
  list (A * B) :=
  match xs, ys with
    | [], [] => []
    | x::xs', y::ys' => (x, y) :: zipLen xs' ys' _
    | _, _ => _
  end.
Next Obligation.
  intros; subst; destruct xs; destruct ys; simpl in *; try discriminate.
  destruct H as [_ H]; contradiction H; split; reflexivity.
  destruct H as [H _]; contradiction H with a xs b ys; split; reflexivity.
Defined.
Next Obligation.
  split; intros; intro; destruct H; discriminate.
Defined.
Next Obligation.
  split; intros; intro; destruct H; discriminate.
Defined.

Lemma zip_in : forall {A} {B} (x:A) (xs:list A) (ys:list B)
  (HIn: In x xs)
  (HLength: List.length xs = List.length ys),
  exists y, In (x, y) (zip xs ys) .
Proof.
  induction xs. 
  - simpl. intros. intuition.
  - intros. destruct ys. simpl in HLength. assert False as H. omega. destruct H.
    assert (List.length xs = List.length ys) by (simpl in *; omega).
    simpl in HIn. destruct HIn as [Heq | HIn].
    - subst. exists b. simpl. firstorder.
    - firstorder.
Qed.

Lemma in_zip : forall {A} {B} (x:A) xs (y:B) ys
  (HIn: In (x, y) (zip xs ys))
  (HLength: List.length xs = List.length ys),
  In x xs /\ In y ys.
Proof.
  induction xs. 
  - simpl. intros. destruct ys. intuition. simpl in HIn. destruct HIn.
  - intros. destruct ys. simpl in HLength. assert False as H. omega. destruct H.
    assert (List.length xs = List.length ys) by (simpl in *; omega).
    simpl in HIn. destruct HIn as [Heq | HIn].
    - inversion Heq. subst. simpl. intuition.
    - firstorder.
Qed.

Lemma map_fst_zip_id : forall {A} {B} (xs : list A) (ys : list B),
  List.length xs = List.length ys -> map (fst (B:=B)) (zip xs ys) = xs.
Proof.
  induction xs; destruct ys; intros; try discriminate; simpl; auto.
  inversion H; subst; f_equal; auto.
Qed.

Lemma NoDup_concat : forall {A} x (xs ys : list A),
  NoDup (xs ++ ys) -> In x xs -> ~ In x ys.
Proof.
  induction ys; introv Hdup HIn.
  - auto.
  - intro Hys; destruct Hys; subst.
  eapply List.NoDup_remove_2; eauto using in_or_app.
  intuition eauto using NoDup_remove_1.
Qed.

Lemma NoDup_cons_iff : forall {A} x (xs : list A),
  NoDup(x::xs) <-> (~ In x xs) /\ NoDup xs.
Proof.
  intros. split. 
  - intros. inversion H. intuition.
  - intros [HIn HDup]. apply NoDup_cons; assumption.
Qed.

Lemma NoDup_concat_left : forall {A} (ys xs : list A)
  (HDup: NoDup (xs ++ ys)),
  (forall x, In x xs -> ~ In x ys) /\ NoDup xs /\ NoDup ys.
Proof.
  induction ys. 
  - intros. repeat split.
    - intros HIn Hnin. intuition.
    - assert (xs ++ [] = xs) as Heq by intuition. rewrite Heq in HDup. assumption.
      - apply NoDup_nil.
  - intros. 
      assert (H := List.NoDup_remove_1 _ _ _ HDup).
      eapply List.NoDup_remove_2 in HDup; eauto using in_or_app.
      specialize (IHys _ H). destruct IHys as [HIn [HDupx HDupy]]. 
      repeat split.
      - intros x HIn1 HNin. specialize (HIn x). simpl in HNin. destruct HNin as [Heq | HIn2].
        - subst. intuition.
        - intuition.
      - assumption.
      - rewrite NoDup_cons_iff. intuition.
Qed.

Lemma NoDup_concat_right : forall {A} (ys xs : list A)
  (HIn: forall x, In x xs -> ~ In x ys)
  (HDupxs: NoDup xs)
  (HDupys: NoDup ys),
  NoDup (xs ++ ys).
Proof.
  induction xs.
  - intros.
    assert ([] ++ ys = ys) as Heq by intuition. rewrite Heq. assumption.
  - intros. inversion HDupxs; subst. clear HDupxs.
    assert ((a :: xs) ++ ys = (a :: (xs ++ ys))) as Heq 
      by intuition. 
    rewrite Heq. clear Heq.
    apply NoDup_cons. intro Happ. apply in_app_or in Happ. destruct Happ.
    - intuition.
    - specialize (HIn a). simpl in HIn. intuition.
    apply IHxs. intros x HInxs HInys.
    specialize (HIn x). apply HIn. simpl. intuition.
    assumption.
    assumption.
    assumption.
Qed.

Lemma NoDup_concat_iff : forall {A} (ys xs : list A),
  NoDup (xs ++ ys) <->
  (forall x, In x xs -> ~ In x ys) /\ NoDup xs /\ NoDup ys.
Proof.
  split. 
  apply NoDup_concat_left. 
  intros. apply NoDup_concat_right; intuition.
Qed.

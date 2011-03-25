Require Export AbstractAsn.
Require Import Setoid.
Require Import Program.
Require Import String.
Require Import Omega.

(* Specifications are interpreted into downwards-closed subsets of
   natural numbers. *)
Record spec := { spec_rel :> nat -> Prop;
                 spec_step : forall n, spec_rel (S n) -> spec_rel n}.

Hint Resolve spec_step.

Section Connectives.

  Lemma spec_dc: forall (P: spec) n n', n <= n' -> P n' -> P n.
  Proof.
    induction 1; eauto.
  Qed.

  Program Definition pure_spec (P: Prop) : spec :=
    Build_spec (fun n => P) _.

  (* Definitions of intuitionistic logic connectives *)

  Definition true_spec  : spec := pure_spec True.
  Definition false_spec : spec := pure_spec False.

  Program Definition and_spec (P Q : spec) : spec :=
    Build_spec (fun n => P n /\ Q n) _.

  Program Definition or_spec (P Q : spec) : spec :=
    Build_spec (fun n => P n \/ Q n) _.
  Next Obligation.
    intuition eauto.
  Qed.

  Program Definition impl_spec (P Q : spec) : spec :=
    Build_spec (fun n => forall m, m <= n -> P m -> Q m) _.

  Program Definition all_spec  {U : Type} (F : U -> spec) : spec :=
    Build_spec (fun n => forall u, F u n) _.

  Program Definition xist_spec {U : Type} (F : U -> spec) : spec :=
    Build_spec (fun n => exists u, F u n) _.
  Next Obligation.
    intros U F n [u HFu]; eauto.
  Qed.

  (* Later operator : this operator is borrowed from
     D. Dreyer, A. Ahmed, and L. Birkedal. Logical step-indexed logical
     relations. January 2010. Submitted for publication.
     http://itu.dk/people/birkedal/papers/lslr-journal.pdf *)
  Program Definition later_spec (P: spec) : spec :=
    Build_spec (fun n => P (n-1)) _.
  Next Obligation.
    destruct n; [auto|]. simpl in *. rewrite <- minus_n_O. auto.
  Qed.

End Connectives.

Add Parametric Morphism (s: spec) : s with signature
  le --> impl
  as spec_dc_m.
Proof.
  unfold impl. eauto using spec_dc.
Qed.

Definition entails_spec (P Q: spec) : Prop := forall n, P n -> Q n.

Instance entails_spec_preo : PreOrder entails_spec.
Proof. split; unfold entails_spec; intuition. Qed.

Notation " P '[/\]' Q "  := (and_spec P Q)
  (at level 80, right associativity) : spec_scope.
Notation " P '[\/]' Q "  := (or_spec P Q)
  (at level 85, right associativity) : spec_scope.
Notation " P '[->]' Q "  := (impl_spec P Q)
  (at level 88, right associativity) : spec_scope.
Notation " P '|=' Q "    := (entails_spec P Q)
  (at level 90, no associativity) : spec_scope.
Notation " '[A]' x , P " := (all_spec (fun x => P))
  (at level 89, x ident) : spec_scope.
Notation " '[A]' x : T , P " := (all_spec (fun x:T => P))
  (at level 89, x ident) : spec_scope.
Notation " '[E]' x , P " := (xist_spec (fun x => P))
  (at level 89, x ident) : spec_scope.
Notation " '[E]' x : T , P " := (xist_spec (fun x:T => P))
  (at level 89, x ident) : spec_scope.
Notation "'[true]'"      := true_spec : spec_scope.
Notation "'[false]'"     := false_spec : spec_scope.
Notation "'|>' P"        := (later_spec P) (at level 70) : spec_scope.
Notation "'[pure]' P"    := (pure_spec P) (at level 65) : spec_scope.

Delimit Scope spec_scope with spec.

Definition bientails_spec (P Q : spec) := ((P |= Q) /\ (Q |= P))%spec.
Notation " P '=|=' Q "   := (bientails_spec P Q)
  (at level 90, no associativity) : spec_scope.

Definition valid (P: spec) : Prop := ([true] |= P)%spec.
Hint Unfold valid.
Notation " '|=' P " := (valid P) (at level 90) : spec_scope.

Instance bientails_spec_equiv : Equivalence bientails_spec.
Proof.
  split.
  intros x; split; reflexivity.
  intros x y [Hl Hr]; split; assumption.
  intros x y z [Hxy Hyx] [Hyz Hzy]; split; etransitivity; eassumption.
Qed.

Add Morphism entails_spec with signature
entails_spec --> entails_spec ++> impl
as entails_spec_m.
Proof.
  intros x y HEntxy u t HEnttu HEntxu.
  transitivity x; [| transitivity u]; assumption.
Qed.

Add Morphism entails_spec with signature
  bientails_spec ==> bientails_spec ==> iff
  as entails_spec_iff_m.
Proof.
  unfold bientails_spec. intros P P' [? ?] Q Q' [? ?].
  split; intro.
  - transitivity P; [| transitivity Q]; assumption.
  - transitivity P'; [| transitivity Q']; assumption.
Qed.

Add Morphism bientails_spec with signature
bientails_spec ==> bientails_spec ==> iff
as bientails_spec_m.
Proof.
  intros x y HBixy u t HBiut; split; [intros HBixu | intros HBiyt].
  etransitivity; [symmetry | etransitivity]; eassumption.
  etransitivity; [etransitivity | symmetry]; eassumption.
Qed.

Add Morphism valid with signature
  bientails_spec ==> iff
  as valid_iff_m.
Proof.
  intros P Q H. unfold valid. rewrite H. reflexivity.
Qed.

Instance bient_ent_spec_subrel : subrelation bientails_spec entails_spec.
Proof. intros p q [Hp _]; assumption. Qed.

Instance bient_inv_ent_spec_subrel :
  subrelation bientails_spec (inverse entails_spec).
Proof. intros p q [_ Hq]; assumption. Qed.

Section Defs.
  Variable S : sep_alg.

  (* Connections between assertion and specification logics *)

  Program Definition FunI (T:spec) :=
    (PackUpred S (fun _ n => T n)) _.
  Next Obligation.
    intros. firstorder. eapply spec_dc; eassumption.
  Qed.

  (* Existance of this connection depends on the subheap relation being a
     partial order (which is not strictly necessary, but should hold for
     most models) *)
  Program Definition FunH (T:spec) :=
    (PackUpred S (fun _ n => T n)) _.
  Next Obligation.
    intros. firstorder. eapply spec_dc; eassumption.
  Qed.

  Program Definition FunL (p:upred S) :=
    Build_spec (fun n => exists s, p s n) _.
  Next Obligation.
    intros. destruct H. exists x. 
    eapply up_mono; try eassumption; intuition.
  Qed.

  Program Definition FunR (p:upred S) :=
    Build_spec (fun n => forall s, p s n) _.
  Next Obligation.
    intros. specialize (H s).
    eapply up_mono; try eassumption; intuition.
  Qed.

  Open Scope spec_scope.
  Open Scope upred_scope.

  Lemma LIAdj: forall p T, FunL p |= T <-> p |- FunI T.
  Proof.
    unfold entails_spec, entails_up.
    split.
    - intros H s n Hp. specialize (H n). simpl in *. apply H. firstorder.
    - intros H n HLp. simpl in *. destruct HLp as [s HLp]. firstorder.
  Qed.

  Lemma IRAdj: forall p T, FunI T |- p <-> T |= FunR p.
  Proof.
    unfold entails_spec, entails_up; split.
    - intros H n HLp. simpl in *. firstorder.
    - intros H s n Hp. specialize (H n). simpl in *. apply H. assumption.
  Qed.

  (* A collection of preservation lemmas about FunI and FunR *)

  Lemma I_true : FunI [true] -|- <true>.
  Proof.
    split; unfold entails_up; simpl; intros; exact I.
  Qed.

  Lemma I_and : forall P Q, FunI (P [/\] Q) -|- FunI P </\> FunI Q.
  Proof.
    split; unfold entails_up; simpl; intros; assumption.
  Qed.

  Lemma I_all : forall U (P : U -> spec),
    FunI (all_spec P) -|- <A> x, FunI (P x).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma I_false : FunI [false] -|- <false>.
  Proof.
    split; unfold entails_up; simpl; intros; assumption.
  Qed.

  Lemma I_or : forall P Q, FunI (P [\/] Q) -|- FunI P <\/> FunI Q.
  Proof.
    split; unfold entails_up; simpl; intros; assumption.
  Qed.

  Lemma I_xist : forall U (P : U -> spec),
    FunI (xist_spec P) -|- <E> x, FunI (P x).
  Proof.
    split; unfold entails_up; simpl; intros; assumption.
  Qed.

  Lemma I_impl : forall P Q, FunI (P [->] Q) -|- FunI P <->> FunI Q.
  Proof.
    split; unfold entails_up; simpl; intros; eauto.
  Qed.

  Lemma I_later : forall P, <|>> FunI P -|- FunI (|> P).
  Proof.
    split; unfold entails_up; simpl; auto.
  Qed.

  Lemma R_true : FunR <true> =|= [true].
  Proof.
    split; unfold entails_spec; simpl; intros; exact I.
  Qed.

  Lemma R_and : forall P Q, FunR (P </\> Q) =|= FunR P [/\] FunR Q.
  Proof.
    split; unfold entails_spec; simpl; intros; firstorder.
  Qed.

  Lemma R_all : forall U (P : U -> upred S),
    FunR (all_up P) =|= [A] x, FunR (P x).
  Proof.
    split; unfold entails_spec; simpl; intros; auto.
  Qed.

  Lemma R_false : FunR <false> =|= [false].
  Proof.
    split; unfold entails_spec; simpl; intros; apply H; apply sa_unit.
  Qed.

  Lemma R_or : forall P Q, FunR (P <\/> Q) =|= FunR P [\/] FunR Q.
  Proof.
    split; unfold entails_spec; simpl; intros; intuition.
    destruct (H (@sa_unit _)); [left | right]; intros s;
      (eapply up_mono; [ | auto | eassumption]); exists s;
        rewrite sa_mulC; apply sa_unitI.
  Qed.

  Lemma R_xist : forall U (P : U -> upred S),
    FunR (xist_up P) =|= [E] x, FunR (P x).
  Proof.
    split; unfold entails_spec; simpl; intros; firstorder.
    destruct (H (@sa_unit _)) as [u HP]; exists u; intro s.
    apply up_mono with (@sa_unit _) n; auto.
    exists s; rewrite sa_mulC; apply sa_unitI.
  Qed.

  (* Lemmas connecting the images of spatial and intuitionistic connectives
     under FunR only hold if the subheap relation is a partial order *)

  Close Scope upred_scope.
  Close Scope spec_scope.

End Defs.

Section Properties.

  Open Scope spec_scope.

  (* Defining properties of intuitionistic logic *)
  (* top *)
  Lemma true_specR : forall p, p |= [true].
  Proof.
    intros P n _; exact I.
  Qed.

  (* bottom *)
  Lemma false_specL : forall p, [false] |= p.
  Proof.
    intros P n Hp; contradiction Hp.
  Qed.

  (* and introduction *)
  Lemma and_specI : forall p q r (Hp_q : p |= q) (Hp_r : p |= r), p |= q [/\] r.
  Proof.
    unfold entails_spec; simpl; eauto.
  Qed.

  (* and eliminations *)
  Lemma and_specEL : forall p q r (Hp_r : p |= r), p [/\] q |= r.
  Proof.
    unfold entails_spec; simpl; intuition.
  Qed.

  Lemma and_specER : forall p q r (Hq_r : q |= r), p [/\] q |= r.
  Proof.
    unfold entails_spec; simpl; intuition.
  Qed.

  (* or introductions *)
  Lemma or_specIL : forall p q r (Hp_q : p |= q), p |= q [\/] r.
  Proof.
    unfold entails_spec; simpl; eauto.
  Qed.

  Lemma or_specIR : forall p q r (Hp_q : p |= r), p |= q [\/] r.
  Proof.
    unfold entails_spec; simpl; eauto.
  Qed.

  (* or elimination *)
  Lemma or_specE : forall p q r (Hp_r : p |= r) (Hq_r : q |= r), p [\/] q |= r.
  Proof.
    unfold entails_spec; simpl; intuition.
  Qed.

  (* impl introduction *)
  Lemma impl_specI : forall p q r (Hpq_r : p [/\] q |= r), p |= q [->] r.
  Proof.
    unfold entails_spec; simpl; eauto using spec_dc.
  Qed.

  (* impl elimination *)
  Lemma impl_specE : forall p q r s (Hp_r_s : p |= r [->] s) (Hq_r : q |= r),
    p [/\] q |= s.
  Proof.
    intros p q r s Hp_r_s Hq_r n [Hp Hq]; apply (Hp_r_s n); auto.
  Qed.

  (* Quantifier properties *)

  Lemma all_specEIR : forall T p (f : T -> spec),
    p |= all_spec f <-> forall x, p |= f x.
  Proof.
    unfold entails_spec; simpl; intuition.
  Qed.

  Lemma all_specEL : forall T p (f: T -> spec) a,
    (f a |= p -> [A]x, f x |= p) %spec.
  Proof.
    introv H. unfold entails_spec in *. eauto.
  Qed.

  Lemma xist_specEIL : forall T p (f : T -> spec),
    xist_spec f |= p <-> forall x, f x |= p.
  Proof.
    split; [intros Hp x n Hfx | intros Hfx n [x Hex]].
    apply Hp; exists x; assumption.
    apply Hfx with x; assumption.
  Qed.

  Lemma xist_specIR : forall T p (f : T -> spec)
    (HEx : exists x, p |= f x), p |= xist_spec f.
  Proof.
    intros T p f [x Hpf] Hp; exists x; apply Hpf; auto.
  Qed.

  (* Other operators *) 

  Lemma later_entails_spec : forall S, S |= |> S.
  Proof.
    intros S n HS; simpl; eapply spec_dc; [ |eassumption]; omega.
  Qed.

  Lemma pure_specIR : forall (p: spec) (Q: Prop),
    Q -> p |= [pure] Q.
  Proof. firstorder. Qed.
  
  Lemma pure_specEIL : forall (P: Prop) (q: spec),
    P -> |= q <->
    [pure] P |= q.
  Proof.
  intros P q. split.
  - unfold valid, entails_spec. simpl. auto.
  - unfold valid, entails_spec. simpl. auto.
  Qed.

  Lemma pure_impl_specR  P p q :
    p |= [pure] P [->] q <->
    P -> p |= q.
  Proof.
    unfold entails_spec. simpl. split; eauto using spec_dc.
  Qed.

  Lemma pure_impl_specL  (P: Prop) p q :
    P -> (p |= q) -> [pure] P [->] p |= q.
  Proof.
    unfold entails_spec. simpl. eauto.
  Qed.

  Lemma distr_impl_and_L  P Q R:
    (P [->] Q) [/\] R |=
    P [->] (Q [/\] R).
  Proof.
    unfold entails_spec. simpl. introv H Hm HP. split.
    - apply H; assumption.
    - rewrite Hm. apply H.
  Qed.

  (* Holds in the other direction only for non-empty types *)
  Lemma distr_all_and_specL : forall U (F : U -> spec) p,
    (all_spec F) [/\] p |= [A] x, F x [/\] p.
  Proof.
    intros. intros n H; simpl in *. split; apply H.
  Qed.

  Lemma distr_xist_and_spec : forall U (F : U -> spec) p,
    ((xist_spec F) [/\] p =|= [E] x, F x [/\] p)%spec.
  Proof.
    split.
    - unfold entails_spec; simpl; firstorder.
    apply <- xist_specEIL; intros u; apply and_specI;
      [ apply and_specEL | apply and_specER; reflexivity ].
    apply xist_specIR; exists u; reflexivity.
  Qed.

  Close Scope spec_scope.

End Properties.

Add Morphism and_spec with signature
entails_spec ==> entails_spec ==> entails_spec
as and_entails_spec_m.
Proof.
  intros x y HEntxy u t HEntut.
  auto using and_specI, and_specEL, and_specER.
Qed.

Add Morphism and_spec with signature
bientails_spec ==> bientails_spec ==> bientails_spec
as and_bientails_spec_m.
Proof.
  intros x y HEntxy u t HEntut; split.
  rewrite HEntxy; rewrite HEntut; reflexivity.
  rewrite <- HEntxy; rewrite <- HEntut; reflexivity.
Qed.  

Add Morphism or_spec with signature
entails_spec ==> entails_spec ==> entails_spec
as or_entails_spec_m.
Proof.
  intros x y HEntxy u t HEntut.
  auto using or_specE, or_specIL, or_specIR.
Qed.

Add Morphism or_spec with signature
bientails_spec ==> bientails_spec ==> bientails_spec
as or_bientails_spec_m.
Proof.
  intros x y HEntxy u t HEntut; split.
  rewrite HEntxy; rewrite HEntut; reflexivity.
  rewrite <- HEntxy; rewrite <- HEntut; reflexivity.
Qed.  

Add Morphism impl_spec with signature
entails_spec --> entails_spec ++> entails_spec
as impl_entails_spec_m.
Proof.
  intros x y HEntyx u t HEntut.
  apply impl_specI; rewrite HEntyx; rewrite <- HEntut.
  eapply impl_specE; reflexivity.
Qed.

Add Morphism impl_spec with signature
bientails_spec ==> bientails_spec ==> bientails_spec
as impl_bientails_spec_m.
Proof.
  intros x y HBixy u t HBiut; split.
  rewrite HBiut; rewrite HBixy; reflexivity.
  rewrite <- HBixy; rewrite <- HBiut; reflexivity.
Qed.

Add Morphism later_spec with signature
  entails_spec ==> entails_spec
  as later_entails_spec_m.
Proof.
  unfold entails_spec; simpl; eauto.
Qed.

Add Morphism later_spec with signature
bientails_spec ==> bientails_spec
as later_bientails_spec_m.
Proof.
  intros x y [Hxy Hyx]; split; [ rewrite Hxy | rewrite Hyx ]; reflexivity.
Qed.

Add Morphism pure_spec with signature
  iff ==> bientails_spec
  as pure_spec_bientails_m.
Proof.
  unfold bientails_spec, entails_spec. simpl. intuition.
Qed.

Add Morphism pure_spec with signature
  impl ++> entails_spec
  as pure_spec_entails_m.
Proof.
  unfold entails_spec. simpl. intuition.
Qed.

Instance xist_spec_entails_m (A : Type) :
  Proper (pointwise_relation A entails_spec ++> entails_spec) (@xist_spec A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold entails_spec in *. simpl. introv [a Hp]. exists a. eauto.
Qed.

Instance xist_spec_bientails_m (A : Type) :
  Proper (pointwise_relation A bientails_spec ==> bientails_spec)
         (@xist_spec A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold bientails_spec, entails_spec in *. simpl. firstorder.
Qed.

Instance all_spec_entails_m (A : Type) :
  Proper (pointwise_relation A entails_spec ++> entails_spec) (@all_spec A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold entails_spec in *. simpl. eauto.
Qed.

Instance all_spec_bientails_m (A : Type) :
  Proper (pointwise_relation A bientails_spec ==> bientails_spec) (@all_spec A).
Proof.
  intros p q H. unfold pointwise_relation in H.
  unfold bientails_spec, entails_spec in *. simpl. firstorder.
Qed.

Section DerivedProperties.
  Implicit Types P Q R : spec.
  Open Scope spec_scope.

  (* associativity and commutativity of [/\] *)
  Lemma and_specA : forall P Q R, (P [/\] Q) [/\] R =|= P [/\] Q [/\] R.
  Proof.
    unfold bientails_spec, entails_spec; simpl; intuition.
  Qed.

  Lemma and_specC : forall P Q, P [/\] Q =|= Q [/\] P.
  Proof.
    unfold bientails_spec, entails_spec; simpl; intuition.
  Qed.

  Lemma and_specCA : forall P Q R, P [/\] Q [/\] R =|= Q [/\] P [/\] R.
  Proof.
    intros; rewrite <- and_specA, (and_specC P), and_specA; reflexivity.
  Qed.

  Lemma and_specAC : forall P Q R, (P [/\] Q) [/\] R =|= (P [/\] R) [/\] Q.
  Proof.
    intros; rewrite and_specA, (and_specC Q), <- and_specA; reflexivity.
  Qed.

  (* later commutes with operators *)

  Lemma later_and_spec P Q : |> (P [/\] Q) =|= |> P [/\] |> Q.
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_or_spec P Q : |> (P [\/] Q) =|= |> P [\/] |> Q.
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_A'_spec T (P : T -> spec) : |> (all_spec P) =|= [A] x, (|> P x).
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_A_spec T (P : T -> spec) : |> ([A] x, P x) =|= [A] x, (|> P x).
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_E_spec T (P : T -> spec) : |> ([E] x, P x) =|= [E] x, (|> P x).
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_E'_spec T (P : T -> spec) : |> (xist_spec P) =|= [E] x, (|> P x).
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_pure_spec : forall P : Prop, |> [pure] P =|= [pure] P.
  Proof.
    split; unfold entails_spec; simpl; auto.
  Qed.

  Lemma later_impl_spec P Q : |> (P [->] Q) =|= |> P [->] |> Q.
  Proof.
    split.
    - unfold entails_spec; simpl. introv H Hmn.
      eapply H; eauto; omega. 
    - unfold entails_spec; simpl. introv H Hmn. 
      destruct n as [|n]; [inversion Hmn; clear Hmn |]; simpl in *; subst.
      specialize (H 0); simpl in H; apply H; auto.
      specialize (H (Datatypes.S m)); simpl in *;
      rewrite <- minus_n_O in *; apply H; auto with arith.
  Qed.

  Lemma later_entails : forall (P Q: spec),
    |>P |= Q <-> forall n, P n -> Q (Datatypes.S n).
  Proof.
    unfold entails_spec. simpl. intros. split; intro H.
    intros. apply H. simpl. rewrite <- minus_n_O. assumption.
    intros n Hr. destruct n; [auto|]. apply H. simpl in *.
    rewrite <- minus_n_O in *. assumption.
  Qed.

  Lemma pure_alt_impl (P : Prop) Q:
    ([pure]P [->] Q =|= [A]pf:P, Q)%spec.
  Proof.
    split.
    - unfold entails_spec. simpl. auto.
    - unfold entails_spec. simpl. eauto using spec_dc.
  Qed.

  (* Loeb rule (O case needed due to injection of Prop) *)
  Lemma lob : forall (G P : spec)
    (HO : G 0 -> P 0)
    (HL : G [/\] |> P |= P),
    G |= P.
  Proof.
    intros; unfold entails_spec; induction n; [assumption | intros HGS].
    apply HL; simpl; rewrite <- minus_n_O; auto.
  Qed.

  (* Simple composition of specifications (for a single program) *)
  Lemma compose : forall (G R S:spec)
    (HR    : G [/\] |> S |= R)
    (HS    : G [/\] |> R |= S)
    (HPure : G 0 -> R 0 /\ S 0),
    G |= R [/\] S.
  Proof.
    intros; apply lob; simpl; [assumption |].
    apply and_specI; rewrite later_and_spec;
      [ rewrite (and_specER (|> R)) | rewrite (and_specEL (|> R)) ];
        eassumption || reflexivity.
  Qed.

  Lemma compose_nonmutual : forall (G R S : spec)
    (HR : G           |= R)
    (HS : G [/\] |> R |= S),
    G |= R [/\] S.
  Proof.
    intros; eapply compose; [apply and_specEL; assumption | assumption |].
    intro HG0; split; [apply HR; assumption | apply HS].
    split; [assumption | apply later_entails_spec; apply HR; assumption].
  Qed.

  Close Scope spec_scope.

End DerivedProperties.

Hint Rewrite later_and_spec
             later_or_spec
             later_A_spec
             later_E_spec
             later_pure_spec
             later_impl_spec : pushin_later.

Add Parametric Morphism S : (FunI S) with signature
  entails_spec ++> (@entails_up S)
  as FunI_entails_m.
Proof.
  unfold entails_up, entails_spec. simpl. auto.
Qed.

Add Parametric Morphism S : (FunI S) with signature
  bientails_spec ++> (@bientails_up S)
  as FunI_bientails_m.
Proof.
  unfold bientails_up, bientails_spec, entails_up, entails_spec. simpl.
  intros p q [Hpq Hqp]. auto.
Qed.

Ltac spec_exists t :=
  apply xist_specIR; exists t.

Ltac spec_eexists :=
  apply xist_specIR; eexists.

Ltac spec_all v :=
  apply all_specEIR; intro v.

Ltac spec_split :=
  match goal with
    | |- (?p |= ?q [/\] ?r)%spec => apply and_specI
  end.

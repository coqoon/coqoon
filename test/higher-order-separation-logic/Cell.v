Require Import Tactics.
Require Import LiftOp.
Require Import SubstAbstractAsn.

Open Scope string_scope.
Open Scope list_scope.

Module CellP <: PROGRAM.

(* class Cell *)

  Definition cinitm :=
    Build_Method nil cskip "".

  Definition cgetm :=
    Build_Method nil (cread "x" "this" "value") "x".

  Definition csetm :=
    Build_Method (["x"]) (cwrite "this" "value" "x") 0.

  Definition CellC :=
    Build_Class (SS.singleton "value")
      (SM.add "init" cinitm (SM.add "get" cgetm
        (SM.add "set" csetm (SM.empty _)))).

(* class ProxySet *)

  Definition proxySetm :=
    Build_Method (["c"; "x"]) (ccall "" "c" "set" (["x":expr])) 0.

  Definition ProxySetC :=
    Build_Class SS.empty (SM.add "proxySet" proxySetm (SM.empty _)).

(* class Recell *)

  Definition rinitm :=
    Build_Method nil (cseq (calloc "cell" "Cell") (
                      cseq (ccall "" "cell" "init" nil)
                           (cwrite "this" "cell" "cell"))) 0.

  Definition rgetm :=
    Build_Method nil (cseq (cread "cell" "this" "cell")
                           (ccall "x" "cell" "get" nil)) "x".

  Definition rsetm :=
    Build_Method (["x"]) (cseq (cread "cell" "this" "cell") (
                          cseq (ccall "bak" "cell" "get" nil) (
                          cseq (cwrite "this" "bak" "bak") (
                                ccall "" "cell" "set" (["x":expr]))))) 0.

  Definition rundom :=
    Build_Method nil (cseq (cread "cell" "this" "cell") (
                      cseq (cread "bak" "this" "bak") (
                            ccall "" "cell" "set" (["bak":expr])))) 0.

  Definition RecellC :=
    Build_Class (SS.add "bak" (
                 SS.add "cell" (
                 SS.empty)))
                (SM.add "init" rinitm (
                 SM.add "get" rgetm (
                 SM.add "set" rsetm (
                 SM.add "undo" rundom (
                (SM.empty _)))))).

  (* class World *)

  Definition makem :=
    Build_Method nil
    (cseq (calloc "r" "Recell")
      (cseq (ccall "" "r" "init" nil)
        (cseq (ccall "" "r" "set" ([5:expr]))
          (cseq (calloc "ps" "ProxySet")
            (cseq (ccall "" "ps" "proxySet" (["r":expr; 3:expr]))
              (ccall "" "r" "undo" nil)))))) "r".

  Definition mainm :=
    Build_Method nil
    (cseq (calloc "w" "World")
      (cseq (ccall "c" "w" "make" nil)
        (cseq (ccall "v" "c" "get" nil)
          (cassert (eeq "v" 5))))) "".

  Definition WorldC :=
    Build_Class SS.empty
      (SM.add "make" makem (SM.add "main" mainm (SM.empty _))).

  Definition Prog := Build_Program (SM.add "Cell" CellC (
                                    SM.add "ProxySet" ProxySetC (
                                    SM.add "Recell" RecellC (
                                    SM.add "World" WorldC (
                                    SM.empty _))))).

  Lemma unique_names :
    forall C m mrec,
      method_lookup Prog C m mrec ->
      NoDup ("this" :: m_params mrec).
  Proof.
    search (search_unique_names Prog).
  Qed.

End CellP.

Import CellP.

Module Import SC := Rules CellP.

Open Scope spec_scope.
Open Scope asn_scope.

Definition ICell (C:class)
                   (T: Type)
                   (cval: val -> T -> upred heap_alg)
                   (cget: T -> val)
                   (cset: T -> val -> T)
                   : spec :=
      ([A]t:T, C :.: "get" |-> nil
        {{ sm_bin cval ("this":expr) (sm_const t) }}-{{
        "r", sm_bin cval ("this":expr) (sm_const t) </\> "r" == cget t }})
    [/\]
      ([A]t:T, C :.: "set" |-> (["x"])
        {{ sm_bin cval ("this":expr) (sm_const t) }}-{{
        "", sm_bin cval ("this":expr) (sm_bin cset (sm_const t) ("x":expr)) }})
    [/\]
      [pure](forall t v, cget (cset t v) = v).

Section ProxySet.

  Definition proxySet_spec : spec :=
    [A] C:class, [A] T:Type, [A] cv, [A] cg, [A] cs,
      ICell C T cv cg cs [->] [A]t:T, "ProxySet" :.: "proxySet" |-> ["x"; "v"]
        {{ <pure> "x" ::: C </\> sm_bin cv ("x":expr) (sm_const t) }}-{{
          "", <pure> "x" ::: C </\>
          sm_bin cv ("x":expr) (sm_bin cs (sm_const t) ("v":expr)) }}.

  Lemma valid_proxySet : |= proxySet_spec.
  Proof.
    unfold proxySet_spec; spec_all C; spec_all T; spec_all cv; spec_all cg;
      spec_all cs; apply impl_specI; spec_all t; apply and_specER.
    unfold_method_spec.
    unfold ICell; apply and_specER; apply and_specEL.
    eapply rule_call_complete.
    - apply all_specEL with t; apply later_entails_spec.
    - substitution; rewrite sc_asnC; apply true_asn_unitL.
    apply xist_asnEIL; intros v.
    apply and_asnI; [apply and_asnEL | apply and_asnER; simpl].
    - substitution; reflexivity.
    rewrite sc_asnME; [| substitution; reflexivity | reflexivity].
    substitution; rewrite sc_asnC; apply true_asn_unitR.
  Qed.

End ProxySet.

(* This use of sections is a way to control visibility of abstract predicates.
 * The only thing visible outside the section is Cell_spec and its validity. *)
Section CellInternal.

  Let VC_internal c v := (c · "value" >> v)%upred.

  Definition Cell_class VC :=
    let VC' : expr -> expr -> hasn := sm_bin VC in
    "Cell" :.: "init" |-> nil
      {{ "this" · "value" >> null <*> <true> }}-{{ "",
         <E>v:val, VC' "this" v }} [/\]
    ([A] v:val, "Cell" :.: "get" |-> nil
      {{ VC' "this" v }}-{{ "r", VC' "this" v </\> "r" == v }}) [/\]
    ([A] v:val, "Cell" :.: "set" |-> (["x"])
      {{ VC' "this" v }}-{{ "", VC' "this" "x" }}).

  Let Cell_class_valid : |= Cell_class VC_internal.
  Proof.
    apply and_specI; [|apply and_specI]. subst VC_internal.

    (* Proof of constructor *)
    unfold_method_spec. eapply roc_post; [apply rule_skip |].
    rewrite sc_asnC. rewrite true_asn_unitR.
    apply xist_asnIR. exists null. substitution. reflexivity.

    (* Proof of get method *)
    spec_all v. unfold_method_spec. eapply rule_read_fwd2.
    - instantiate (1:=v). reflexivity.
    - apply xist_asnEIL; intros v0. substitution. asn_solve.

    (* Proof of set method *)
    spec_all v_old. unfold_method_spec.
    eapply roc; [| |apply rule_write].
    - instantiate (1:=v_old). reflexivity.
    - reflexivity.
  Qed.

  Definition Cell_spec :=  [E] VC : val -> val -> upred heap_alg,
    ICell "Cell" val VC (\v,v) (\_,\v,v) [/\]
    Cell_class VC.

  Lemma Cell_spec_valid:
    |= Cell_spec.
  Proof.
    spec_exists VC_internal. spec_split.
    - unfold ICell. subst VC_internal.
      rewrite <- and_specA at 1. spec_split.
        assert (forall t: val, sm_eq
                (sm_bin (\_,  \v: val, v) (sm_const t) ("x":expr))
                ("x":expr)) as Hbin
                by (unfold sm_eq; simpl; reflexivity).
        etransitivity; [apply Cell_class_valid|]. apply and_specER.
        spec_split.
        - apply and_specEL. reflexivity.
        - apply and_specER. spec_all t. apply all_specEL with t.
          eapply roc_mspec.
          - reflexivity.
          - auto.
          - rewrite Hbin. reflexivity.
          - simpl. intuition.
          - reflexivity.
      - apply pure_specIR. auto.
    - apply Cell_class_valid.
  Qed.

End CellInternal.

Definition IRecell (C: class)
                   (T: Type)
                   (rval: val -> T -> upred heap_alg)
                   (rget: T -> val)
                   (rset: T -> val -> T)
                   (rundo: T -> T)
                   : spec :=
      ICell C T rval rget rset
    [/\]
      ([A]t:T, C:.:"undo" |-> nil
        {{ sm_bin rval ("this":expr) (sm_const t) }}-{{
           "", sm_bin rval ("this":expr) (sm_const (rundo t)) }})
    [/\]
      [pure](forall t v, rget (rundo (rset t v)) = rget t).

Ltac substitution' :=
  match goal with
  | |- xist_asn _ |- _ =>
    let x := fresh in
    apply xist_asnEIL; intro x;
    substitution';
    (clear x || (revert x; apply xist_asnEIL))
  | |- _ => substitution
  end.

Section RecellInternal.

  Let VR'_type := expr -> expr -> expr -> hasn.

  Let mkVR (VC : val -> val -> upred _) (r v b: val) : upred _ := (
    <E> cell,
      r · "bak" >> b <*>
      r · "cell" >> cell <*>
      (<pure>(val_class cell = "Cell") </\> VC cell v))%upred.

  Let Hlift VC (er ev eb : expr) : sm_tern
        (\r: val, \v: val, \b: val,
           <E> cell, r · "bak" >> b <*> r · "cell" >> cell <*>
             (<pure>(val_class cell = "Cell") </\> VC cell v))%upred
            er ev eb -|-
        <E> cell:val, er · "bak" >> eb <*> er · "cell" >> cell <*>
        (<pure>(cell ::: "Cell") </\> sm_bin VC (cell:expr) ev).
  Proof.
    unfold bientails_asn, entails_asn, sm_rel, entails_up. reflexivity.
  Qed.

  Definition Recell_init (VR': VR'_type) :=
    "Recell" :.: "init" |-> nil
      {{ "this" · "cell" >> null <*> "this" · "bak" >> null <*>
         <true> }}-{{ "", <E>v:val, <E>b:val, VR' "this" v b }}.

  Definition Recell_get (VR': VR'_type) :=
    [A]v:val, [A]b:val, "Recell" :.: "get" |-> nil
      {{ VR' "this" v b }}-{{ "r", VR' "this" v b </\> "r" == v }}.

  Definition Recell_set (VR': VR'_type) :=
    [A]v:val, [A]b:val, "Recell" :.: "set" |-> ["x"]
      {{ VR' "this" v b }}-{{ "", VR' "this" "x" v }}.

  Definition Recell_undo (VR': VR'_type) :=
    [A]v:val, [A]b:val, "Recell" :.: "undo" |-> nil
      {{ VR' "this" v b }}-{{ "", VR' "this" b b }}.

  Let Recell_init_valid VC :
    |> Cell_class VC |= Recell_init (sm_tern (mkVR VC)).
  Proof.
    unfold Cell_class. autorewrite with pushin_later.
    apply and_specEL. unfold_method_spec.
    forward; [forward|]. forward.
    - eapply roc_pre.
      - (* The substitution tactic doesn't go under binders. *)
        assert (forall v:val,
          (("this"·"cell">>null <*> "this"·"bak">>null <*> <true>)[(v:expr)//"cell"]) |-
          ("this"·"cell">>null <*> "this"·"bak">>null)) as Hsubst.
        - intros. substitution. apply sc_asnME; [reflexivity|].
          rewrite sc_asnC. apply true_asn_unitR.
        setoid_rewrite Hsubst. clear Hsubst.
        assert (forall A (P:hasn), <E>x:A, P |- P) as Hex.
        - intros. apply xist_asnEIL. reflexivity.
        rewrite Hex. clear Hex.
        rewrite sc_asnC. reflexivity.
      eapply rule_call_complete with (F :=
          "this"·"cell">>null <*> "this"·"bak">>null).
      - reflexivity.
      - (* Normal substitution damages the lhs. *)
        etransitivity; [|substitution;reflexivity]. reflexivity.
      - apply xist_asnEIL. intro v. clear Hlift.
        apply and_entails_asn_m.
        - substitution. reflexivity.
        - apply sc_asnME.
          - substitution. substitution'. substitution'. reflexivity.
          - substitution. reflexivity.
    - eapply roc_post.
      - eapply rule_write_frame. rewrite sc_asnC. rewrite and_asnC.
        rewrite pure_and_sc_asnR. rewrite sc_asnA. rewrite sc_asnA.
        rewrite sc_asnC. reflexivity.
      etransitivity.
      - setoid_rewrite sc_asnC at 1. rewrite <- sc_asnA.
        setoid_rewrite sc_asnC at 2. rewrite sc_asnA.
        rewrite <- pure_and_sc_asnR. rewrite and_asnC.
        autorewrite with pullout_xist_asn.
        reflexivity.
      apply xist_asnEIL. intro v. rewrite existentialize_subst with (x:="cell").
      substitution'.
      rewrite <- Hlift. apply xist_asnIR. exists v. substitution.
      apply xist_asnIR. exists null. substitution. reflexivity.
  Qed.

  Let Recell_get_valid VC :
    |> Cell_class VC |= Recell_get (sm_tern (mkVR VC)).
  Proof.
    clear Recell_init_valid.
    unfold Cell_class, Recell_get. autorewrite with pushin_later.
    apply and_specER. apply and_specEL. spec_all v. spec_all b. subst mkVR.
    unfold_method_spec. rewrite Hlift. clear Hlift.
    rewrite <- exists_into_precond_c. spec_all cell. forward.
    - apply rule_read_fwd2 with (cell:expr).
      - apply sc_asnER. apply sc_asnEL. reflexivity.
      substitution'. reflexivity.
    apply all_specEL with v.
    eapply rule_call_complete with (F :=
        "this"·"bak">>b <*> "this"·"cell" >> sm_const cell </\>
        "cell" == cell).
    - reflexivity.
    - substitution. asn_unify. substitution. rewrite pure_and_sc_asnR.
      setoid_rewrite <- sc_asnA at 2. rewrite <- pure_and_sc_asnR.
      apply and_asnI; [|unentail].
      rewrite <- sc_asnA. rewrite sc_asnC. reflexivity.
    apply xist_asnEIL. intro dummy. etransitivity.
    - apply and_entails_asn_m.
      - substitution. reflexivity.
      - apply sc_asnME.
        - substitution. substitution. reflexivity.
        - substitution. reflexivity.
    autorewrite with pullout_xist_asn. apply xist_asnIR. exists cell.
    etransitivity.
    - setoid_rewrite pure_and_sc_asnR at 2. rewrite <- !sc_asnA.
      rewrite sc_asnC. rewrite and_asnC. rewrite <- pure_and_sc_asnL.
      rewrite and_asnA. reflexivity.
    asn_unify. substitution. etransitivity.
    - rewrite sc_asnA. rewrite sc_asnC. do 2 setoid_rewrite pure_and_sc_asnR.
      rewrite sc_asnA. setoid_rewrite sc_asnC at 3. rewrite !sc_asnA.
      reflexivity.
    rewrite pure_and_sc_asnR, pure_and_sc_asnL. rewrite !sc_asnA.
    reflexivity.
  Qed.

  Let Recell_set_valid VC :
    |> Cell_class VC |= Recell_set (sm_tern (mkVR VC)).
  Proof.
    clear Recell_init_valid Recell_get_valid.
    unfold Cell_class, Recell_set. autorewrite with pushin_later.
    apply and_specER. spec_all v. spec_all b. subst mkVR.
    unfold_method_spec. rewrite Hlift. rewrite <- exists_into_precond_c.
    spec_all cell. forward.
    - apply rule_read_fwd2 with (cell:expr).
      - apply sc_asnER. apply sc_asnEL. reflexivity.
      substitution'. reflexivity.
    forward.
    - eapply rule_call_complete with (F :=
          "this"·"bak">>b <*> "this"·"cell" >> sm_const cell </\>
          "cell" == cell).
      - apply and_specEL. apply all_specEL with v.
        reflexivity.
      - substitution. asn_unify. substitution. rewrite pure_and_sc_asnR.
        setoid_rewrite <- sc_asnA at 2. rewrite <- pure_and_sc_asnR.
        apply and_asnI; [|unentail].
        rewrite <- sc_asnA. rewrite sc_asnC. reflexivity.
      apply xist_asnEIL. intro dummy. apply and_entails_asn_m.
      - substitution. reflexivity.
      - apply sc_asnME.
        - substitution. substitution. reflexivity.
        - substitution. reflexivity.
    forward.
    - etransitivity; [apply true_specR|]. eapply rule_write_frame.
      setoid_rewrite pure_and_sc_asnR at 2. rewrite pure_and_sc_asnL.
      rewrite sc_asnC. setoid_rewrite sc_asnC at 2.
      do 3 rewrite sc_asnA.
      etransitivity; [|rewrite sc_asnC; reflexivity].
      reflexivity.
    eapply rule_call_complete with (F :=
        "this"·"bak">>v <*> "this"·"cell" >> sm_const cell </\>
        "cell" == cell).
    - apply and_specER. apply all_specEL with v.
      reflexivity.
    - substitution. setoid_rewrite sc_asnC at 2. do 2 rewrite sc_asnA.
      rewrite <- pure_and_sc_asnL. asn_unify. substitution.
      rewrite and_asnC. rewrite sc_asnA.
      rewrite pure_and_sc_asnL. rewrite sc_asnA.
      rewrite <- pure_and_sc_asnL. asn_unify. substitution.
      setoid_rewrite sc_asnC at 3.
      rewrite pure_and_sc_asnL. setoid_rewrite sc_asnC at 5.
      rewrite pure_and_sc_asnR.
      setoid_rewrite <- sc_asnA at 3. rewrite <- pure_and_sc_asnR.
      apply and_asnI; [|unentail].
      rewrite !sc_asnA. reflexivity.
    rewrite Hlift.
    etransitivity.
    - apply xist_asnEIL. intro dummy. apply and_entails_asn_m.
      - substitution. reflexivity.
      - apply sc_asnME.
        - substitution. substitution. reflexivity.
        - substitution. reflexivity.
    apply xist_asnIR. exists cell. rewrite pure_and_sc_asnL.
    rewrite <- sc_asnA. rewrite <- pure_and_sc_asnL.
    rewrite sc_asnC. rewrite pure_and_sc_asnR. setoid_rewrite sc_asnC at 2.
    rewrite sc_asnA. rewrite <- pure_and_sc_asnL. asn_unify. substitution.
    rewrite sc_asnA. reflexivity.
  Qed.

  Let Recell_undo_valid VC :
    |> Cell_class VC |= Recell_undo (sm_tern (mkVR VC)).
  Proof.
    clear Recell_init_valid Recell_get_valid Recell_set_valid.
    unfold Cell_class, Recell_undo. autorewrite with pushin_later.
    apply and_specER. apply and_specER. spec_all v. spec_all b.
    subst mkVR.
    unfold_method_spec. rewrite Hlift. rewrite Hlift. clear Hlift.
    rewrite <- exists_into_precond_c. spec_all cell.
    forward.
    - apply rule_read_fwd2 with (cell:expr).
      - apply sc_asnER. apply sc_asnEL. reflexivity.
      substitution'. reflexivity.
    forward.
    - apply rule_read_fwd2 with (b:expr).
      - apply and_asnER. apply sc_asnEL. reflexivity.
      substitution'. reflexivity.
    eapply rule_call_complete with (F :=
        "bak" == b <*> "cell" == cell <*>
        "this"·"bak">>b <*> "this"·"cell" >> cell).
    - apply all_specEL with v. reflexivity.
    - substitution. asn_unify. asn_unify. substitution.
      setoid_rewrite sc_asnC at 3. do 2 rewrite sc_asnA.
      do 2 rewrite <- pure_and_sc_asnL. do 2 (apply and_asnI; [unentail|]).
      rewrite sc_asnA. reflexivity.
    apply xist_asnEIL. intro dummy. etransitivity.
    - apply and_entails_asn_m.
      - substitution. reflexivity.
      - apply sc_asnME.
        - substitution. substitution. reflexivity.
        - substitution. reflexivity.
    apply xist_asnIR. exists cell.
    etransitivity.
    - rewrite pure_and_sc_asnL. rewrite <- !sc_asnA.
      setoid_rewrite sc_asnC at 3. setoid_rewrite sc_asnC at 4.
      setoid_rewrite <- pure_and_sc_asnL at 3. rewrite !sc_asnA.
      rewrite <- !pure_and_sc_asnL. reflexivity.
    asn_unify. asn_unify. substitution. rewrite sc_asnC, sc_asnA. reflexivity.
  Qed.

  Definition Recell_class VR :=
    let VR' := sm_tern VR in
    Recell_init VR' [/\]
    Recell_get VR' [/\]
    Recell_set VR' [/\]
    Recell_undo VR'.

  Definition Recell_spec :=
    [E] VR : val -> val -> val -> upred heap_alg,
    IRecell "Recell"
            (val*val)%type
            (\this,\t, VR this (fst t) (snd t))
            fst
            (\t,\v, (v,fst t))
            (\t, (snd t, snd t)) [/\]
    Recell_class VR.

  Lemma Recell_spec_valid:
    |> Cell_spec |= Recell_spec.
  Proof.
    clear Hlift.
    unfold Cell_spec; rewrite later_E_spec; setoid_rewrite later_and_spec.
    apply xist_specEIL. intro VC. apply and_specER.
    spec_exists (mkVR VC). spec_split.
    - unfold IRecell, ICell. repeat spec_split.

      (* get *)
      - spec_all vb. destruct vb as [v b]. simpl.
        rewrite Recell_get_valid.
        apply all_specEL with v. apply all_specEL with b.
        match goal with
          |- _ :.: _ |-> _ {{ ?P' }}-{{ _, ?Q' }} |= _ =>
            apply roc_mspec with P' Q'
        end.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - reflexivity.

      (* set *)
      - spec_all vb. destruct vb as [v b]. simpl. rewrite Recell_set_valid.
        apply all_specEL with v. apply all_specEL with b.
        match goal with
          |- _ :.: _ |-> _ {{ ?P' }}-{{ _, ?Q' }} |= _ =>
            apply roc_mspec with P' Q'
        end.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - reflexivity.

      (* ICell pure part *)
      - apply pure_specIR. auto.

      (* undo *)
      - spec_all vb. destruct vb as [v b]. simpl. rewrite Recell_undo_valid.
        apply all_specEL with v. apply all_specEL with b.
        match goal with
          |- _ :.: _ |-> _ {{ ?P' }}-{{ _, ?Q' }} |= _ =>
            apply roc_mspec with P' Q'
        end.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - unfold entails_asn, sm_rel. simpl. intro; reflexivity.
        - simpl. intuition.
        - reflexivity.

      (* IRecell pure part *)
      - apply pure_specIR. auto.

    - unfold Recell_class; repeat spec_split; auto using
      Recell_init_valid, Recell_get_valid, Recell_set_valid, Recell_undo_valid.
  Qed.

End RecellInternal.

Section World.

  Definition make_spec : spec :=
    "World" :.: "make" |-> nil
    {{ <pure> "this" ::: "World" }}-{{ "r",
      <E> C, <E> T, <E> VR, <E> cg, <E> cs,
      sm_const (FunI _ (ICell C T VR cg cs)) </\> <E> t:T,
      sm_bin VR ("r":expr) (sm_const t) </\> cg t == 5 </\> <pure> "r" ::: C}}.

  Lemma I_postcond P Q SC c :
    (|>SC [/\] {[ P ]} c {[ Q ]}) |= {[ P ]} c {[ sm_const (FunI _ SC) </\> Q ]}.
  Proof.
    unfold entails_spec; simpl; intros n [HSC HT]; intros;
      edestruct HT; intuition eauto.
    eapply spec_dc; [| eassumption].
    destruct k; [contradiction (cmd_zero H6) |].
    omega.
  Qed.

  Lemma valid_make : |> proxySet_spec [/\] |> Recell_spec |= make_spec.
  Proof.
    unfold make_spec; unfold_method_spec.
    - destruct H5; intuition.
    forward; [forward |].
    unfold Recell_spec; rewrite later_E_spec, and_specC, distr_xist_and_spec.
    apply xist_specEIL; intros VR; unfold Recell_class;
      repeat rewrite later_and_spec.
    forward; [eapply rule_call_complete |].
    - apply and_specEL; apply and_specER; apply and_specEL.
      unfold Recell_init; reflexivity.
    - rewrite distr_xist_sc_asnL; apply xist_asnEIL; intros v; substitution.
      rewrite sc_asnC; apply sc_asnME; [| reflexivity].
      repeat rewrite SS'.fold_add with (eqA := @bientails_asn _);
        try rewrite SS'.fold_empty; eauto using sc_asnCA with typeclass_instances.
      substitution. rewrite sc_asnCA; reflexivity.
      - intros ? ? HEq h h' HB; subst; rewrite HB; reflexivity.
      - intros ? ? P; apply sc_asnCA.
      - rewrite SS'.empty_iff; tauto.
      - intros ? ? HEq h h' HB; subst; rewrite HB; reflexivity.
      - intros ? ? ?; apply sc_asnCA.
      - rewrite SS'.add_iff, SS'.empty_iff; intuition discriminate.
    - reflexivity.
    rewrite <- exists_into_precond_c; spec_all v.
    eapply roc_pre; [rewrite sc_asnME; substitution;
      [rewrite sc_asnC | |]; reflexivity |].
    rewrite distr_xist_sc_asnR, distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all x; spec_substitution.
    rewrite distr_xist_sc_asnR, distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all b; spec_substitution.
    spec_substitution; clear v.
    forward; [eapply rule_call_complete |].
    - apply and_specEL; do 3 apply and_specER; apply and_specEL;
      unfold Recell_set; rewrite later_A_spec; apply all_specEL with x;
        rewrite later_A_spec; apply all_specEL with b; reflexivity.
    - substitution.
      rewrite pure_and_sc_asnL, sc_asnCA, sc_asnC, <- pure_and_sc_asnL;
        reflexivity.
    - apply xist_asnEIL; intros v; rewrite sc_asnME; substitution; [|reflexivity
      | reflexivity].
      substitution; clear v; rewrite sc_asnC; reflexivity.
    forward; [forward |].
    rewrite SS'.fold_empty, and_asnEL with (q := <true>); [|reflexivity].
    rewrite distr_xist_sc_asnL, <- exists_into_precond_c; spec_all v.
    spec_substitution; clear v.
    forward; [eapply rule_call_complete |].
    - rewrite and_specEL with (p := |> IRecell _ _ _ _ _ _); [| reflexivity].
      rewrite and_specC; unfold proxySet_spec.
      rewrite later_A_spec, distr_all_and_specL; apply all_specEL with "Recell".
      rewrite later_A_spec, distr_all_and_specL;
        apply all_specEL with (val*val)%type.
      rewrite later_A_spec, distr_all_and_specL; apply all_specEL with
        (\this,\t, VR this (fst t) (snd t)).
      rewrite later_A_spec, distr_all_and_specL; apply all_specEL with fst.
      rewrite later_A_spec, distr_all_and_specL; apply all_specEL with
        (\t, \v, (v, fst t)).
      rewrite later_impl_spec, impl_specE; [| reflexivity |].
      - rewrite later_A_spec; apply all_specEL with (5:val, x); reflexivity.
      unfold IRecell; rewrite later_and_spec; apply and_specEL; reflexivity.
    - rewrite <- pure_and_sc_asnR, <-pure_and_sc_asnL, sc_asnC; etransitivity;
      [| apply true_asn_unitL].
      substitution.
      asn_solve.
      apply and_asnEL; do 2 apply and_asnER; unentail.
    - apply xist_asnEIL; intros v.
      rewrite sc_asnME; substitution; [| reflexivity | reflexivity].
      rewrite sc_asnC, true_asn_unitR; substitution.
      reflexivity.
    rewrite <- xist_from_post; apply xist_specIR; exists "Recell";
      spec_substitution.
    rewrite <- xist_from_post; apply xist_specIR; exists (val*val)%type;
      spec_substitution.
    rewrite <- xist_from_post; apply xist_specIR;
      exists (\this,\t, VR this (fst t) (snd t)); spec_substitution.
    rewrite <- xist_from_post; apply xist_specIR; eexists fst;
      spec_substitution.
    rewrite <- xist_from_post; apply xist_specIR; eexists (\t, \v, (v, fst t));
      spec_substitution.
    rewrite <- I_postcond; apply and_specI.
    - apply and_specEL; apply and_specEL; unfold IRecell;
      rewrite later_and_spec; apply and_specEL; reflexivity.
    eapply rule_call_complete.
    - apply and_specEL; do 4 apply and_specER; unfold Recell_undo.
      rewrite later_A_spec; apply all_specEL with 3; rewrite later_A_spec;
        apply all_specEL with 5; reflexivity.
    - apply and_asnER; substitution.
      rewrite sc_asnC; etransitivity; [| apply true_asn_unitL].
      unentail.
    apply xist_asnEIL; intros v; rewrite sc_asnME; substitution; [| reflexivity
      | reflexivity].
    substitution; rewrite sc_asnC, true_asn_unitR.
    apply xist_asnIR; exists (5:val, 5:val).
    substitution.
    apply and_asnI; [| asn_solve]; apply and_asnER.
    unentail.
  Qed.

  Definition main_spec : spec :=
    "World" :.: "main" |-> [] {{ <true> }}-{{"", <true>}}.

  Lemma valid_main :
    |> make_spec |= main_spec.
  Proof.
    unfold main_spec; unfold_method_spec.
    forward; [forward |].
    rewrite sc_asnER; [| reflexivity].
    rewrite SS'.fold_empty.
    forward; [eapply rule_call_complete |].
    - unfold make_spec; reflexivity.
    - substitution; apply and_asnEL; rewrite and_idempotent_asn, sc_asnC;
      apply true_asn_unitL.
    - reflexivity.
    rewrite <- exists_into_precond_c; spec_all v; spec_substitution.
    rewrite sc_asnC, true_asn_unitR, distr_xist_and_asnR.
    rewrite <- exists_into_precond_c; spec_all C; spec_substitution.
    rewrite distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all T; spec_substitution.
    rewrite distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all RC; spec_substitution.
    rewrite distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all cg; spec_substitution.
    rewrite distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all cs; spec_substitution.
    rewrite and_asnCA, <- I_precond_c; apply impl_specI.
    rewrite distr_xist_and_asnR, <- exists_into_precond_c;
      spec_all t; spec_substitution.
    forward; [eapply rule_call_complete |].
    - apply and_specER; unfold ICell; apply and_specEL; apply all_specEL with t.
      apply later_entails_spec.
    - apply and_asnER; rewrite and_asnC with (p := cg t == 5%Z), <- and_asnA,
      const_eq_prop, pure_and_sc_asnR.
      rewrite and_asnC; substitution; reflexivity.
    - clear v; apply xist_asnEIL; intros v; substitution.
      rewrite <- pure_and_sc_asnR; substitution; reflexivity.
    eapply roc_post; [apply rule_assert | apply true_asnR].
    apply and_asnER; apply Prop_pure_EL; intros HEq; rewrite HEq.
    clear HEq; apply and_asnER; unentail.
    destruct k0; intros HEq; inversion HEq; subst; intuition.
  Qed.

End World.

Definition prog_spec : spec :=
  Cell_spec [/\] Recell_spec [/\] proxySet_spec [/\] make_spec [/\] main_spec.

Lemma program_valid : |= prog_spec.
Proof.
  unfold prog_spec.
  repeat rewrite <- and_specA.
  eapply compose_nonmutual; [| apply and_specER; rewrite later_and_spec;
    apply and_specER; apply valid_main].
  eapply compose_nonmutual; [| apply and_specER; rewrite and_specA;
    do 2 rewrite later_and_spec; apply and_specER; rewrite and_specC;
      apply valid_make].
  apply and_specI; [| apply valid_proxySet].
  eapply compose_nonmutual; [apply Cell_spec_valid |
    apply and_specER; apply Recell_spec_valid].
Qed.

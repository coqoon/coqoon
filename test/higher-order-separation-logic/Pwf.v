Require Export Lang.

Record Method := {
  m_params: list var;
  m_body: cmd;
  m_ret: expr
}.

Record Class := {
  c_fields: SS.t;
  c_methods: SM.t Method
}.

Record Program := {
  p_classes: SM.t Class
}.

Section Pwf.

  Variable Prog: Program.

  Definition field_lookup (C:class) (fields:SS.t) :=
    exists Crec, SM.MapsTo C Crec (p_classes Prog) /\
      fields = c_fields Crec.

  Lemma field_lookup_function: forall C f f',
    field_lookup C f -> field_lookup C f' -> f = f'.
  Proof.
    unfold field_lookup.
    intros C f f' HM1 HM2. destruct HM1 as [m HM1]. destruct HM2 as [m' HM2].
    assert (m = m') by firstorder using SM'.MapsTo_fun. intuition congruence.
  Qed.

  Definition method_lookup (C:class) m mrec :=
    exists Crec, SM.MapsTo C Crec (p_classes Prog) /\
      SM.MapsTo m mrec (c_methods Crec).

  Lemma method_lookup_function : forall C m mr0 mr1
    (HML0 : method_lookup C m mr0)
    (HML1 : method_lookup C m mr1),
    mr0 = mr1.
  Proof.
    unfold Pwf.method_lookup; intros; destruct HML0 as [C0 [HC0 HM0]];
      destruct HML1 as [C1 [HC1 HM1]].
    rewrite SM'.find_mapsto_iff in *; rewrite HC0 in HC1; inversion HC1; subst.
    rewrite HM0 in HM1; inversion HM1; subst; reflexivity.
  Qed.

  Program Definition search_method_lookup
      {P': method -> Method -> Prop} (search: forall m mrec, option (P' m mrec))
      : option (forall C m mrec, method_lookup C m mrec -> P' m mrec) :=
    search_cast _ _ (
      SM'.search_forall (fun _ Crec =>
        SM'.search_forall (fun m mrec =>
          search m mrec
        ) (c_methods Crec)
      ) (p_classes Prog)
    ).
  Next Obligation.
    introv HoptP' Hcast [Crec [HC Hm]]. firstorder.
  Qed.

  Program Definition search_method_class_lookup
    {P' : class -> method -> Method -> Prop} (search: forall C m mrec, option
      (P' C m mrec)) : option (forall C m mrec, method_lookup C m mrec ->
        P' C m mrec) :=
      search_cast _ _ (
        SM'.search_forall (fun C Crec =>
          SM'.search_forall (fun m mrec =>
            search C m mrec) (c_methods Crec)) (p_classes Prog)).
  Next Obligation.
    introv H1 H2 [Crec [HC Hm]]. eauto.
  Qed.

Definition search_unique_names : option (
      forall C m mrec,
      method_lookup C m mrec ->
      NoDup ("this"%string :: m_params mrec)) :=
  search_method_lookup (fun _ mrec =>
    search_NoDup string_dec ("this"%string :: m_params mrec)
  ).

End Pwf.

Module Type PROGRAM.

  Parameter Prog: Program.

  Axiom unique_names :
      forall C m mrec,
      method_lookup Prog C m mrec ->
      NoDup ("this"%string :: m_params mrec).

End PROGRAM.

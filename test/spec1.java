final class Coq {
  static enum M { PROGRAM, BEFORESPEC, AFTERSPEC, TOP, PRELUDE };
  static void def (M m, String s) { }
  static void requires (String s) { }
  static void ensures (String s) { }
}

class FacC {
  static {
    Coq.def(Coq.M.PRELUDE,
            "Add LoadPath \"/Users/hannes/tomeso/git/semantics/Coq\"." +
            "Require Import Tactics." +
            "Require Import Ascii." +
            "Require Import LiftOp." +
            "Open Scope string_scope." +
            "Open Scope list_scope.");
    Coq.def(Coq.M.PROGRAM,
            "Lemma unique_names :" +
            "      forall C m mrec," +
            "      method_lookup P C m mrec ->" +
            "      NoDup (\"this\" :: \"ret\" :: m_params mrec ++ m_locals mrec)." +
            "Proof." +
            "  search (search_unique_names P)." +
            "Qed.");
  }

  static {
    Coq.def(Coq.M.BEFORESPEC,
            "Fixpoint mfac n :=" +
            "  match n with" +
            "      | S n => (S n) * mfac n" +
            "      | 0 => 1" +
            "  end.");
    Coq.def(Coq.M.BEFORESPEC,
            "Lemma fac_step : forall n," +
            "  n > 0 ->" +
            "  (n * (mfac (n - 1))) = mfac n." +
            "Proof." +
            "  induction n." +
            "  - intuition." +
            "  - simpl; intuition." +
            "Qed.");

    Coq.def(Coq.M.BEFORESPEC,
            "Definition facZ := fun (n:Z) =>" +
            "  match ((n ?= 0)%Z) with" +
            "      | Lt => 0" +
            "      | _ => Z_of_nat (mfac (Zabs_nat n))" +
            "  end.");
    Coq.def(Coq.M.BEFORESPEC, "Definition fac_Z := ge_un (fun v => vint (facZ (val_to_int v))).");

    Coq.def(Coq.M.BEFORESPEC,
            "Lemma fac_Z_nat : forall n," +
            "  (n >= 0)%Z ->" +
            "  facZ n = Z_of_nat(mfac(Zabs_nat n))." +
            "Proof." +
            "  destruct n. simpl." +
            "  - unfold facZ. simpl. reflexivity." +
            "  - unfold facZ. simpl. reflexivity." +
            "  - firstorder." +
            "Qed.");

    Coq.def(Coq.M.BEFORESPEC,
            "Lemma Zabs_nat_shrink : forall n," +
            "  (n > 0)%Z ->" +
            "  Zabs_nat (n - 1) = (Zabs_nat n) - 1." +
            "Proof." +
            "  intros." +
            "  rewrite Zabs_nat_Zminus. firstorder." +
            "  firstorder." +
            "Qed.");

    Coq.def(Coq.M.BEFORESPEC,
            "Lemma Zabs_nat_scope_mult : forall n m," +
            "  (n >= 0)%Z ->" +
            "  ((n * Z_of_nat m)%Z) = Z_of_nat((Zabs_nat n) * m)." +
            "Proof." +
            "  intros." +
            "  rewrite <- (Zabs_nat_Z_of_nat m)." +
            "  rewrite <- Zabs_nat_mult." +
            "  rewrite Zabs_nat_Z_of_nat." +
            "  rewrite inj_Zabs_nat." +
            "  rewrite Zabs_eq; [|intuition]. reflexivity." +
            "Qed.");

    Coq.def(Coq.M.BEFORESPEC,
            "Lemma facZ_step : forall (n:Z)," +
            "  (n > 0)%Z ->" +
            "  ((n * (facZ (n - 1)))%Z) = facZ n." +
            "Proof." +
            "  intros." +
            "  repeat (rewrite fac_Z_nat; [|omega])." +
            "  rewrite Zabs_nat_shrink; [|omega]." +
            "  rewrite Zabs_nat_scope_mult; [|omega]." +
            "  rewrite fac_step. reflexivity." +
            "  assert ((Z_of_nat (Zabs_nat n) > 0)%Z)." +
            "  rewrite inj_Zabs_nat. rewrite Zabs_eq; [|firstorder]." +
            "  assumption." +
            "  intuition." +
            "Qed.");

//inside Fac_spec
    Coq.def(Coq.M.AFTERSPEC,
            "Lemma specs_params_eq : spec_params_eq_type P Spec." +
            "Proof." +
            "  search (test_spec_params_eq P Spec)." +
            "Qed.");

    Coq.def(Coq.M.AFTERSPEC,
            "Lemma static_spec : static_spec_type P Spec." +
            "Proof." +
            "  search (test_static_spec P Spec)." +
            "Qed.");

    Coq.def(Coq.M.AFTERSPEC,
            "Lemma free_pre_params : forall I m parI specI," +
            "  mspec Spec I m parI specI ->" +
            "  forall u y," +
            "  free (spec_p specI u) y ->" +
            "  In y (\"this\"%string :: parI)." +
            "Proof." +
            "  intros. mspec_backwards_compat." +
            "  reduce_tm_context; simpl in *; subst; try contradiction." +
            "  reduce_sm_context; simpl in *; subst; try contradiction; simpl in *." +
            "  inversion Hval; subst; simpl in *." +
            "  intuition (try (symmetry; tauto))." +
            "Qed.");

    Coq.def(Coq.M.AFTERSPEC,
            "Lemma free_post_params : free_post_params_type Spec." +
            "Proof." +
            "  unfold free_post_params_type." +
            "  intros. mspec_backwards_compat." +
            "  reduce_tm_context; simpl in *; subst; try contradiction." +
            "  reduce_sm_context; simpl in *; subst; try contradiction; simpl in *." +
            "  inversion Hval; subst; simpl in *." +
            "  intuition (try (symmetry; tauto))." +
            "Qed.");

    Coq.def(Coq.M.AFTERSPEC,
            "Lemma params_not_modified : params_not_modified_type P Spec." +
            "Proof." +
            "  search (test_params_not_modified P Spec)." +
            "Qed.");

  }

  static {
    //on top level
    Coq.def(Coq.M.TOP, "Module Import V := MVerify Fac Fac_spec.");

    Coq.def(Coq.M.TOP,
            "Lemma sg : specs_grow." +
            "  unfold specs_grow; intros. mspec_backwards_compat." +
            "  reduce_tm_context; try contradiction; simpl in *; subst; simpl in *." +
            "  reduce_sm_context; try contradiction; simpl in *; subst; try discriminate." +
            "  red; inversion Hval0; inversion Hval; subst; clear Hval Hval0; auto." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Lemma substs_fac : forall {e:expr} {es}," +
            "  ge_eq ((Fac_spec.fac_Z e) // es) (Fac_spec.fac_Z (e // es))." +
            "Proof." +
            "  split;  intros s h HPre; simpl in *; auto." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Add Morphism Fac_spec.fac_Z with signature" +
            "  ge_eq ==> ge_eq" +
            "  as ge_eq_fac_Z_m." +
            "Proof." +
            "  unfold ge_eq; simpl; intros x y H s. rewrite H. reflexivity." +
            "Qed.");

    Coq.def(Coq.M.TOP, "Hint Rewrite @substs_fac : subst_rewrites.");

    Coq.def(Coq.M.TOP,
            "Lemma unify : forall (p : asn) (v : var) (e : expr) (q:asn)" +
            "  (Heq: p |= (v:expr) ·=· e)" +
            "  (Hsubst: ge_subst_expr p e v |= ge_subst_expr q e v)," +
            "  p |= q." +
            "Proof." +
            "  intros." +
            "  introv s h Hp." +
            "  unfold entails in *." +
            "  specialize (Heq _ _ Hp). simpl in *." +
            "  specialize (Hsubst s h)." +
            "  assert (p s =  p (SM.add v (e s) s))." +
            "  apply ge_prop. unfold agree_on. intros." +
            "  destruct (string_dec x v)." +
            "  subst." +
            "  rewrite stack_lookup_add. intuition." +
            "  rewrite stack_lookup_add2. reflexivity. intuition." +
            "  rewrite <- H in Hsubst. specialize (Hsubst Hp)." +
            "  assert (q s =  q (SM.add v (e s) s))." +
            "  apply ge_prop. unfold agree_on. intros." +
            "  destruct (string_dec x v)." +
            "  subst." +
            "  rewrite stack_lookup_add. intuition." +
            "  rewrite stack_lookup_add2. reflexivity. intuition." +
            "  rewrite H0. assumption." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Lemma unify2 : forall (p : asn) (v : var) (e : expr) (q r:asn)" +
            "  (Heq: p |= r //\\ (v:expr) ·=· e)" +
            "  (Hsubst: ge_subst_expr r e v |= ge_subst_expr q e v)," +
            "  p |= q." +
            "Proof." +
            "  intros." +
            "  introv s h Hp." +
            "  unfold entails in *." +
            "  specialize (Heq _ _ Hp). simpl in *." +
            "  specialize (Hsubst s h)." +
            "  assert (r s =  r (SM.add v (e s) s))." +
            "  apply ge_prop. unfold agree_on. intros." +
            "  destruct (string_dec x v)." +
            "  subst." +
            "  rewrite stack_lookup_add. intuition." +
            "  rewrite stack_lookup_add2. reflexivity. intuition." +
            "  assert (q s =  q (SM.add v (e s) s))." +
            "  apply ge_prop. unfold agree_on. intros." +
            "  destruct (string_dec x v)." +
            "  subst." +
            "  rewrite stack_lookup_add. intuition." +
            "  rewrite stack_lookup_add2. reflexivity. intuition." +
            "  rewrite H0. apply Hsubst. rewrite <- H. intuition." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Ltac assign_forward := " +
            "  eapply roc_post'; [apply rule_assign_forward| " +
            "    apply xist_left; intros; substitution;" +
            "      (eapply unify2; [reflexivity|substitution])].");

    Coq.def(Coq.M.TOP,
            "Ltac forward :=" +
            "  match goal with" +
            "    | |- |=G {{_}} cif _ _ _ {{_}} => apply rule_if'" +
            "    | |- |=G {{_}} cseq _ _ {{_}} => eapply rule_seq'" +
            "    | |- |=G {{_}} cassign _ _ {{_}} => assign_forward" +
            "  end.");

    Coq.def(Coq.M.TOP,
            "Lemma fac_valid : |=G {{spec_p Fac_spec.fac_spec ()}}Fac.fac_body" +
            "  {{spec_qret Fac_spec.fac_spec () \"x\"}}." +
            "Proof." +
            "  unfold_valid." +
            "  forward. forward." +
            "  call_rule (TClass \"FacC\") ()." +
            "  - substitution. unentail. intuition." +
            "  - reflexivity. substitution." +

            "  forward." +
            "    unentail. intuition. subst. simpl." +
            "    rewrite Fac_spec.facZ_step; [reflexivity | omega]." +

            "  forward." +
            "    unentail. intuition. subst. destruct (Z_dec (val_to_int k) 0)." +
            "    assert False; [|intuition]. destruct s; intuition." +
            "    rewrite e. intuition." +

            "  Existential 1:=()." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Lemma vg : valid_G." +
            "  unfold valid_G. introv Hlookup Hmspec. mspec_backwards_compat." +
            "  destruct Hlookup as [Crec [HClass HMeth]]." +
            "  reduce_tm_context; try contradiction; simpl in *; subst." +
            "  inversion Hkey; subst; clear Hkey." +
            "  reduce_sm_context; try contradiction; simpl in *; subst." +
            "  inversion Hval0; subst; clear Hval0 Hkey; simpl in *." +
            "  repeat rewrite InA_cons in *; rewrite InA_nil in *; destruct_sm_eqs;" +
            "    simpl in *; subst; [ | contradiction]; clear Hkey H0." +
            "  apply fac_valid." +
            "Qed.");

    Coq.def(Coq.M.TOP,
            "Theorem Fac_valid : program_valid." +
            "  split." +
            "  apply sg." +
            "  apply vg." +
            "Qed.");
  }

  public int fac (int n) {
    Coq.requires("ege \"n\" 0 //\\ !((\"this\":expr) ·=· (null:expr))");
    Coq.ensures("(((\"ret\":expr) ·=· (fac_Z (\"n\":expr))):asn) //\\ ege \"n\" 0");
    int x;
    if (n > 0)
      x = n * fac(n - 1);
    else
      x = 1;
    return x;
  }
}

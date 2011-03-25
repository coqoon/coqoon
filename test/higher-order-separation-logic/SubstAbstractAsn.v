Definition deep_label : nat -> Prop := fun x, True.

Ltac revert_substs lp lq :=
  match goal with
    | [H : (_ /\ deep_label _) |- _] => destruct H as [H _]; 
      try rewrite H in lp; try rewrite H in lq; clear H
  end.

Require Export Subst.
Require Export AbstractAsn.

Open Scope asn_scope.

Lemma var_eq_substL S (P : asn S) (x : var) e :
  x == e </\> P |- P [e // x].
Proof.
  intros s; simpl; apply pure_upEL; intros HEq; rewrite <- HEq; clear HEq.
  erewrite <- sm_prop; [reflexivity | apply agree_on_add_id].
Qed.  

Lemma var_eq_substR S (P Q : asn S) (x : var) e :
  (P |- Q [e // x]) -> x == e </\> P |- Q.
Proof.
  intros HS s; specialize (HS s); simpl in *; apply pure_upEL; intros HEq.
  erewrite HS, <- HEq, sm_prop; [reflexivity | apply agree_on_add_id].
Qed.

Lemma var_eq_subst S (P Q : asn S) (x : var) e :
  (P [e // x] |- Q [e // x]) -> x == e </\> P |- Q.
Proof.
  intros HS; rewrite and_asnI with (p := x == e); [|reflexivity |reflexivity].
  rewrite and_asnA; apply var_eq_substR; rewrite var_eq_substL; exact HS.
Qed.

Lemma existentialize_subst S (P : asn S) x : P |- <E> v:val, P[(v:expr) // x].
Proof.
  rewrite existentialize with (x := x).
  apply xist_asnEIL; intros v; apply xist_asnIR; exists v.
  apply var_eq_substL.
Qed.  

Lemma unify {S} : forall (p : asn S) (v : var) (e : expr) (q r:asn S)
  (Heq: p |- <pure>((v:expr) ·=· e) </\> r)
  (Hsubst: r[e//v] |- q[e//v]),
  p |- q.
Proof.
  intros.
  introv s S' n Hp.
  specialize (Heq _ _ _ Hp). simpl in Heq.
  destruct Heq as [Heq Hr].
  assert (r s =  r (SM.add v (e s) s)).
  apply sm_prop. unfold agree_on. intros.
  destruct (string_dec x v).
  subst.
  rewrite stack_lookup_add. intuition.
  rewrite stack_lookup_add2. reflexivity. intuition.
  assert (q s =  q (SM.add v (e s) s)).
  apply sm_prop. unfold agree_on. intros.
  destruct (string_dec x v).
  subst.
  rewrite stack_lookup_add. intuition.
  rewrite stack_lookup_add2. reflexivity. intuition.
  rewrite H0. apply Hsubst. simpl. rewrite <- H. assumption.
Qed.

Lemma true_subst {S} : forall es : substs,
  sm_eq (<true> // es) (true_asn S).
Proof.  
  intros; unfold sm_eq; reflexivity.
Qed.

Lemma const_subst {A} (c:A) (es : substs) :
  sm_eq ((sm_const c) // es) (sm_const c).
Proof.  
  unfold sm_eq; reflexivity.
Qed.

Definition pfun A := forall B, (A -> B) -> Prop.

Ltac gethead t :=
  match t with
    | ?h _ => gethead h
    | _ => t
  end.

Ltac getdephead t n :=
  match t with
  | ?h _ => match type of t with
              | forall x : _, _ => constr:(h, n + 1)
              | _ => getdephead h (n + 1)
            end
  | _ => constr:(t, n)
  end.

Definition base_dep Y (a: forall S: sep_alg, (Y S)) : forall S: sep_alg, pfun (Y S) := fun S _ f => f (a S) = f (a S).
Definition step_dep X Y (prev:  forall S : sep_alg, pfun ((\T: sep_alg, X T -> Y T) S)) : forall S: sep_alg, pfun (Y S) := fun S _ f => forall x, prev _ _ (fun a => f (a x)).

Definition base Y (a: Y) : pfun Y:= fun _ f => f a = f a.
Definition step X Y (prev: pfun (X -> Y)) : pfun Y := fun _ f => forall x, prev _ (fun a => f (a x)).

Ltac steps n x :=
  match n with
  | 0 => x
  | S ?n' => let sn' := steps n' x in constr:(step _ _ sn')
  end.

Ltac apparity t acc :=
  match t with
  | ?h ?t => apparity h (S acc)
  | _ => acc
  end.

Ltac geneq term :=
  let hd := gethead term in
  let n := apparity term 0 in
  let P := steps n (base hd) in
  let P := constr:(P _ id) in
  let P := (eval unfold id in P) in
  let P := (eval unfold base in P) in
  let P := (eval unfold step in P) in
  P.

Ltac generate_subst t n :=
  let hd := gethead t in
  let P := steps n (base _ t) in
  let P := constr:(P _ id) in
  let P := (eval unfold id in P) in
  let P := (eval unfold base in P) in
  let P := (eval unfold step in P) in
  let H := fresh in
    unfold hd at 1;
    assert (P /\ deep_label 0) as H 
      by (split; [reflexivity| unfold deep_label; intuition ]);
    unfold hd at 1 in H.


Ltac apply_subst t n :=
 let n := (eval cbv in n) in 
 match t with
   | ?h // ?es => remember es; apply_subst h n
   | ?h[?e//?x] => remember e; apply_subst h n
   | var_expr _ => idtac
   | <true> => idtac 
   | sm_const => idtac
   | sm_un => idtac
   | sm_bin => idtac
   | sm_tern => idtac
   | sm_const _ => idtac 
   | sm_un _ => idtac
   | sm_bin _ => idtac
   | sm_tern _ => idtac
   | ?h ?p => 
     apply_subst p 0;
     match type of h with
       | ?A -> ?B => apply_subst h (n + 1)
       | _ => generate_subst t n
     end
   | _ => 
       match type of t with
         | Type => idtac
         | sep_alg => idtac
         | sm ?A => idtac
         | _ => try
           let hd := gethead t in
             let p := (eval unfold hd in t) in
               generate_subst t n;
               match p with
                 | (fun x => ?f x) => apply_subst f 1
                 | _ => idtac
               end
       end
 end.

Inductive deep_sm : Type -> Type :=
| dTrue : forall {A:sep_alg}, deep_sm (upred A)
| dConst : forall {A: Type}, A -> deep_sm A
| dUn : forall (A C: Type), (A -> C) -> (@deep_sm A) -> deep_sm C
| dBin : forall (A B C: Type), (A -> B -> C) ->
          (@deep_sm A) -> (@deep_sm B) -> deep_sm C
| dTern : forall (A B C D: Type), (A -> B -> C -> D) ->
          deep_sm A -> deep_sm B -> deep_sm C -> deep_sm D
| dBind : forall {A B}, ((A -> B) -> B) -> (A -> sm B) -> deep_sm B
| dVar : var -> deep_sm val
| dBase : forall C, sm C -> deep_sm C.

Fixpoint deep_var_subst v (es:substs) :=
  match es with
    | [] => dVar v
    | (x, e)::es =>
      match string_dec x v with
        | left _ => dBase _ e
        | right _ => deep_var_subst v es
      end
  end.

Fixpoint deep_sm_substs {D} (dsm: deep_sm D) (es:substs) : deep_sm D :=
  match dsm in deep_sm D return deep_sm D with
    | dTrue _ => dTrue
    | dConst _ c => dConst c
    | dUn A C op a => dUn A C op (deep_sm_substs a es)
    | dBin A B C op a b => dBin A B C op (deep_sm_substs a es) (deep_sm_substs b es)
    | dTern A B C D op a b c =>
        dTern A B C D op (deep_sm_substs a es) (deep_sm_substs b es) (deep_sm_substs c es)
    | dBind _ _ f g => dBind f (fun x => (g x) // es)
    | dVar x => deep_var_subst x es
    | dBase _ a => dBase _ (a // es)
  end.

Fixpoint deep_sm_eval {A} (dsm: deep_sm A) :=
  match dsm in deep_sm A return sm A with
    | dTrue B => true_asn B
    | dConst _ c => sm_const c
    | dUn _ _ op a => sm_un op (deep_sm_eval a)
    | dBin _ _ _ op a b => sm_bin op (deep_sm_eval a) (deep_sm_eval b)
    | dTern _ _ _ _ op a b c =>
        sm_tern op (deep_sm_eval a) (deep_sm_eval b) (deep_sm_eval c)
    | dBind _ _ f g => sm_bind f g
    | dVar v => var_expr v
    | dBase _ a => a
  end.

Inductive deep_sm_subst : Type -> Type :=
| dsTrue : forall {S:sep_alg}, deep_sm_subst (upred S)
| dsConst : forall {A}, A -> deep_sm_subst A
| dsUn : forall {A C}, (A -> C) -> (deep_sm_subst A) -> (deep_sm_subst C)
| dsBin : forall {A B C}, (A -> B -> C) -> (deep_sm_subst A) -> (deep_sm_subst B) -> (deep_sm_subst C)
| dsTern : forall {A B C D}, (A -> B -> C -> D) ->
    (deep_sm_subst A) -> (deep_sm_subst B) -> (deep_sm_subst C) -> (deep_sm_subst D)
| dsBind : forall {A B}, ((A -> B) -> B) -> (A -> sm B) -> deep_sm_subst B
| dsSubst : forall {C}, deep_sm_subst C -> substs -> deep_sm_subst C
| dsVar : var -> deep_sm_subst val
| dsBase : forall {C}, sm C -> deep_sm_subst C.

Fixpoint deep_sm_subst_eval {A} (dsm: deep_sm_subst A) :=
  match dsm in deep_sm_subst A return sm A with
    | dsTrue B => true_asn B
    | dsConst _ c => sm_const c
    | dsUn _ _ op a => sm_un op (deep_sm_subst_eval a)
    | dsBin _ _ _ op a b => sm_bin op (deep_sm_subst_eval a) (deep_sm_subst_eval b)
    | dsTern _ _ _ _ op a b c =>
        sm_tern op (deep_sm_subst_eval a) (deep_sm_subst_eval b) (deep_sm_subst_eval c)
    | dsBind _ _ f g => sm_bind f g
    | dsSubst  _ c es => (deep_sm_subst_eval c) // es
    | dsVar v => var_expr v
    | dsBase _ a => a
  end.

Fixpoint apply_substs {D} dssm :=
  match dssm in deep_sm_subst D return deep_sm D with
    | dsTrue A => dTrue
    | dsConst _ c => dConst c
    | dsUn _ _ op a => dUn _ _ op (apply_substs a)
    | dsBin _ _ _ op a b => dBin _ _ _ op (apply_substs a) (apply_substs b)
    | dsTern _ _ _ _ op a b c =>
        dTern _ _ _ _ op (apply_substs a) (apply_substs b) (apply_substs c)
    | dsBind _ _ f g => dBind f g
    | dsSubst _ p es => deep_sm_substs (apply_substs p) es
    | dsVar v => dVar v
    | dsBase _ a => dBase _ a
  end.

Lemma deep_var_subst_sound (v: var) (es : substs) :
  sm_eq ((v:expr) // es) (deep_sm_eval (deep_var_subst v es)).
Proof.
  induction es.
  - simpl; rewrite subst_sm_expr_nil; reflexivity.
  - simpl. destruct a as [x e].
    destruct (string_dec x v).
    - subst; rewrite subst_var_cons_eq; simpl; reflexivity.
    - rewrite subst_var_cons_ineq; [assumption|intuition].
Qed.

Lemma deep_sm_sound_subst {A} (t : deep_sm A) (es : substs) :
  sm_eq (deep_sm_eval t // es) (deep_sm_eval (deep_sm_substs t es)).
Proof.
  intros; induction t.
  - simpl; rewrite true_subst; reflexivity.
  - simpl; rewrite const_subst; reflexivity.
  - simpl; rewrite subst_un_pointwise; rewrite IHt; reflexivity.
  - simpl; rewrite subst_bin_pointwise; rewrite IHt1, IHt2; reflexivity.
  - simpl; rewrite subst_tern_pointwise; rewrite IHt1, IHt2, IHt3; reflexivity.
  - simpl; rewrite subst_sm_bind; reflexivity.
  - simpl; apply deep_var_subst_sound.
  - simpl; reflexivity.
Qed.

Lemma deep_sm_subst_sound {A} : forall t : deep_sm_subst A,
  sm_eq (deep_sm_subst_eval t) (deep_sm_eval(apply_substs t)).
Proof.
  intros. induction t.
  - simpl; reflexivity.
  - simpl; reflexivity.
  - simpl; rewrite IHt; reflexivity.
  - simpl; rewrite IHt1, IHt2; reflexivity.
  - simpl; rewrite IHt1, IHt2, IHt3; reflexivity.
  - simpl; reflexivity.
  - simpl; rewrite IHt; apply deep_sm_sound_subst.
  - simpl; reflexivity.
  - simpl; reflexivity.
Qed.

Ltac sm_reflect t :=
  match t with
    | sm_const ?c => constr:(dsConst c)
    | sm_un ?op ?p => let r1 := sm_reflect p in
                          constr:(dsUn op r1)
    | sm_bin ?op ?p ?q => let r1 := sm_reflect p
                          with r2 := sm_reflect q in
                            constr:(dsBin op r1 r2)
    | sm_tern ?op ?p ?q ?r => let r1 := sm_reflect p
                              with r2 := sm_reflect q
                              with r3 := sm_reflect r in
                                constr:(dsTern op r1 r2 r3)
    | sm_bind ?f ?g => constr:(dsBind f g)
    | ?p // ?es => let r1 := sm_reflect p in
                   constr:(dsSubst r1 es)
    | ?p[?e//?x] => let r1 := sm_reflect p in
                    constr:(dsSubst r1 ([(x, e)]))
    | true_asn ?S => constr:(@dsTrue S)
    | var_expr ?v => constr:(dsVar v)
    | _ => constr:(dsBase t)
  end.

Ltac subst_sound :=
  setoid_rewrite <- deep_sm_subst_sound; simpl;
    first [reflexivity | setoid_rewrite subst_singleton; reflexivity].

Ltac restore_env :=
  match goal with
    | |- ?p |- ?p =>
      let lp := fresh "lp" in
        remember p as lp;
        match goal with 
          | Heqlp : context[lp = _] |- _ =>
            repeat revert_substs Heqlp Heqlp
        end
    | |- ?p |- ?q =>
      let lp := fresh "lp" in
        remember p as lp;
        match goal with 
          | |- ?p |- ?q =>
            let lq := fresh "lq" in 
              remember q as lq;
              match goal with 
                | Heqlp : context[lp = _] |- _ =>
                  match goal with 
                    | Heqlq : context [lq = _] |- _ =>
                      repeat revert_substs Heqlp Heqlq
                  end
              end
        end
  end.

Ltac reflect_substitution :=
  match goal with
    | |- ?p |- ?q => 
      let r1 := sm_reflect p in
      let r2 := sm_reflect q in 
          
        transitivity(deep_sm_eval(apply_substs r1)); 
          [subst_sound |transitivity (deep_sm_eval(apply_substs r2)); 
            [simpl; restore_env; (repeat subst); simpl | subst_sound]]
  end.

Ltac generalize_substitution :=
  match goal with 
    | |- ?p |- ?q => 
      apply_subst p 0; 
      match goal with
        |- ?p |- ?q => 
          apply_subst q 0; unfold lift_up_bin in *
      end
  end.

Ltac substitution := generalize_substitution; reflect_substitution.


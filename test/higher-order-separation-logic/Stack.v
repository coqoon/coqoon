Require Export Util.

Module string_DT' <: MiniDecidableType.
  Definition t := string.
  Definition eq_dec := string_dec.
End string_DT'.
Module string_DT <: UsualDecidableType := Make_UDT string_DT'.

Module SS  := FSetWeakList.Make(string_DT).
Module SS'  := WSetMore_fun (string_DT) SS.

Module SM  := FMapWeakList.Make(string_DT).
Module SM'  := WMapMore_fun (string_DT) SM.

Definition class := string.
Definition ptr := (nat * class)%type.
Definition var := string.

Inductive val :=
| vint :> Z -> val
| vbool :> bool -> val
| vptr :> ptr -> val
.

Definition stack := SM.t val.
Definition null := ((0, EmptyString) : ptr).

Definition val_to_int  (v:val) := match v with vint v => v  | _ => 0%Z end.
Definition val_to_bool (v:val) := match v with vbool b => b | _ => false end.
Definition val_to_ptr  (v:val) := match v with vptr p => p  | _ => null end.

Definition stack_lookup (s:stack) x :=
  match SM.find x s with
  | None => null
  | Some v => v
  end.
Coercion stack_lookup : stack >-> Funclass.

Lemma stack_lookup_add: forall (s: stack) (x: var) (v: val),
  ((SM.add x v s):stack) x = v.
Proof.
intros. unfold stack_lookup.
rewrite SM'.add_o. destruct_calls SS'.Dec.F.eq_dec; congruence.
Qed.

Lemma stack_lookup_add2 : forall  (x:var) y v s
  (Hneq: x <> y),
  stack_lookup (SM.add x v s) y = stack_lookup s y.
Proof.
  intros. unfold stack_lookup.
  remember (SM.find (elt:=val) y (SM.add x v s)) as f. destruct f.
  - apply symmetry in Heqf. rewrite <- SM'.find_mapsto_iff in Heqf.
    rewrite SM'.add_mapsto_iff in Heqf. destruct Heqf as [[Heqx Heqy] | [Hneqx HMap]].
    - subst. intuition.
    - rewrite SM'.find_mapsto_iff in HMap. rewrite HMap. reflexivity.
  - apply symmetry in Heqf. rewrite <- SM'.not_find_in_iff in Heqf.
    rewrite SM'.add_in_iff in Heqf. assert (not (SM.In y s)) as Hnin by intuition.
    rewrite SM'.not_find_in_iff in Hnin. rewrite Hnin. reflexivity.
Qed.

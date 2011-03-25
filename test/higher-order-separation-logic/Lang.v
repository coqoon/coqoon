(* Programming language *)

Require Export Expr.
Require Coq.Program.Wf.

Module natstring_DT := PairUsualDecidableType Nat_as_DT string_DT.
Module ptrstring_DT := PairUsualDecidableType natstring_DT string_DT.

Module NM  := FMapWeakList.Make(Nat_as_DT).
Module PSM := FMapWeakList.Make(ptrstring_DT).

Module NM'  := WMapMore_fun (Nat_as_DT) NM.
Module PSM' := WMapMore_fun (ptrstring_DT) PSM.

Coercion string_var (x: string) : var := x.
Coercion Z_of_nat : nat >-> Z.

Definition val_dec (v v': val) : { v = v' } + { v <> v' }.
Proof. repeat decide equality. Defined.

Definition val_class : val -> class := fun v => snd(val_to_ptr v).

Definition field  := string.
Definition method := string.

Inductive cmd :=
| cassign : var -> expr -> cmd
| cskip   : cmd
| cseq    : cmd -> cmd -> cmd
| cif     : expr -> cmd -> cmd -> cmd
| cwhile  : expr -> cmd -> cmd
| cwrite  : var -> field -> expr -> cmd
| cread   : var -> var -> field -> cmd
| calloc  : var -> class -> cmd
| ccall   : var -> var -> method -> list expr -> cmd
| cassert : expr -> cmd
.

(* The set of stack variables potentially modified by a command *)
Fixpoint modifies (c: cmd) :=
  match c with
  | cassign x _   => SS.singleton x
  | cskip         => SS.empty
  | cseq c1 c2    => SS.union (modifies c1) (modifies c2)
  | cif _ c1 c2   => SS.union (modifies c1) (modifies c2)
  | cwhile _ c    => modifies c
  | cwrite _ _ _  => SS.empty
  | cread x _ _   => SS.singleton x
  | calloc x _    => SS.singleton x
  | ccall x _ _ _ => SS.singleton x
  | cassert _     => SS.empty
  end.

Require Export AbstractAsn.
Require Export HeapSepAlg.

Set Implicit Arguments.
Unset Strict Implicit.


Definition hasn := asn heap_alg.

Coercion pure_hasn (p : pure) : hasn := pure_asn heap_alg p.

Definition pointsto_up (x : val) (f : field) (v : val) :=
  (@singleton_upred heap_alg (PSM'.singleton (val_to_ptr x, f) v)
    </\> <pure> (val_to_ptr x <> null))%upred.

Notation "e1 '·' f '>>' e2" := (pointsto_up e1 f e2) (at level 60) : upred_scope.

Definition pointsto (e : expr) (f : field) (e' : expr) : hasn :=
  sm_bin (fun v v' => pointsto_up v f v') e e'.

Notation "e1 '·' f '>>' e2" := (pointsto e1 f e2) (at level 60) : asn_scope.

Open Scope asn_scope.

Example example_asn_setoid: forall p q q' r t t' : hasn,
  q |- q' -> t |- t' -> (p <*> q) <*> (r <\/> t) |- (p <*> q') <*> (r <\/> t').
Proof.
  introv Hq Ht. rewrite Hq. rewrite Ht. reflexivity.
Qed.

Add Morphism pure_hasn with signature
  entails_pure ==> (@entails_asn heap_alg)
  as pure_hasn_entails.
Proof.
  intros p p' H. unfold entails_pure in H.
  unfold entails_asn, sm_rel, entails_up. simpl. intuition.
Qed.
  
Add Morphism pure_hasn with signature
  bientails_pure ==> (@bientails_asn heap_alg)
  as pure_hasn_bientails.
Proof.
  intros p p' H. apply bientails_asn_alt.
  unfold bientails_pure, entails_pure in H.
  unfold entails_asn, sm_rel, entails_up. simpl. intuition.
Qed.

Close Scope asn_scope.
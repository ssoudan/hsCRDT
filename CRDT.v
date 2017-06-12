Require Import Coq.Unicode.Utf8.

Set Implicit Arguments.

  
Module Type JoinSemiLattice.
  Parameter A : Type.
  Parameter le : A -> A -> Prop.

  Notation "x ≤ y" := (le x y) (at level 70, no associativity).
  
  Axiom le_refl : forall x, x ≤ x.
  (* Axiom le_antisym : forall x y, x ≤ y -> y ≤ x -> x = y. *)
  (* Axiom le_trans : forall x y z, x ≤ y -> y ≤ z -> x ≤ z. *)

  (* This is a join semilattice so lub exists for all pair of element *)
  Parameter lub : A -> A -> A.
  Axiom lub_def : forall x y,  x ≤ lub x y /\ y ≤ lub x y /\ (¬ (exists e : A, e ≤ lub x y /\ x ≤ e /\ y ≤ e)).

  Lemma lub_sym: forall x y, lub x y = lub y x.
  Proof.
    intros.
    pose lub_def as H.
    specialize (H x y).
    destruct H, H0.
    elim H1.
    eexists (lub x y).
    split.
    apply le_refl.
    split.
    assumption.
    assumption.
  Qed.
  
  Lemma lub_assoc: forall x y z, lub (lub x y) z = lub x (lub y z).
  Proof.
    intros.
    pose lub_def as H.
    specialize (H x (lub y z)).
    destruct H, H0.
    elim H1.
    eexists (lub x (lub y z)).
    split.
    apply le_refl.    
    split.
    assumption.
    assumption.
  Qed.

  Lemma lub_refl: forall x, lub x x = x.
  Proof.
    intros.
    pose lub_def as H.
    specialize (H x x).
    destruct H, H0.
    elim H1.
    eexists x.
    split.
    apply H.
    split.
    apply le_refl.
    apply le_refl.
  Qed.
      
End JoinSemiLattice.

Module CRDT (Carrier : JoinSemiLattice).
  Definition merge a b : Carrier.A := Carrier.lub a b.
    
  Parameter query : Carrier.A -> nat.
  Parameter update: nat -> Carrier.A -> Carrier.A.
  Parameter init : Carrier.A.

  Axiom init_low: forall a, Carrier.le init a.
  
  Definition compare: Carrier.A -> Carrier.A -> Prop := Carrier.le.
  Axiom update_monotonic: forall n s, Carrier.le s (update n s).
  Definition equal (a : Carrier.A) (b : Carrier.A) : Prop := Carrier.le a b /\ Carrier.le b a.

  Lemma equal_refl : forall a, equal a a.
  Proof.
    unfold equal.
    split.
    apply Carrier.le_refl.
    apply Carrier.le_refl.
  Qed.
  
  Lemma merge_idemp : forall x, equal (merge x x) x.
  Proof.
    intros.
    unfold merge.
    rewrite Carrier.lub_refl.
    apply equal_refl.
  Qed.

  Lemma merge_comm : forall x y, equal (merge x y) (merge y x).
  Proof.
    intros.
    unfold merge.
    rewrite <- Carrier.lub_sym.
    apply equal_refl.
  Qed.
    
  Lemma merge_assoc : forall x y z,
      equal (merge x (merge y z)) (merge (merge x y) z).
  Proof.
    intros.
    unfold merge.
    rewrite <- Carrier.lub_assoc.
    apply equal_refl.
  Qed.

  Lemma merge_mono : forall x y, compare x (merge x y).
  Proof.
    intros.
    unfold merge.
    unfold compare.
    pose Carrier.lub_def as H.
    specialize (H x y).
    destruct H, H0.
    assumption.
  Qed.

  Lemma convergence_weak: forall a s,
      update a s = merge (update a s) init.
  Proof.
    intros.
    unfold merge.
    pose Carrier.lub_def as H1.
    specialize (H1 (update a s) s).
    destruct H1, H0.
    elim H1.
    eexists (update a s).

    split.
    assumption.

    split.
    apply Carrier.le_refl.
    
    apply update_monotonic.
  Qed.

  Lemma convergence_as_long_as_we_get_a_message: forall a s s1 s2,
      s = merge s1 s2 -> 
      update a s = merge (update a s) init /\ update a s = merge (update a s) s1 /\ update a s = merge (update a s) s2.
  Proof.
    intros.
    split.
    apply convergence_weak.
    split.

    unfold merge.
    pose Carrier.lub_def as H3.
    specialize (H3 (update a s) s).
    destruct H3, H1.
    elim H2.
    eexists (update a s).
    split.
    assumption.
    split.
    apply Carrier.le_refl.

    apply update_monotonic.

    unfold merge.
    pose Carrier.lub_def as H3.
    specialize (H3 (update a s) s).
    destruct H3, H1.
    elim H2.
    eexists (update a s).
    split.
    assumption.
    split.
    apply Carrier.le_refl.
    apply update_monotonic.
  Qed.

  Lemma convergence_update_and_merge_commute: forall a s s1 s2,
      s = merge s1 s2 -> 
      update a s = merge (update a s1) s.
  Proof.
    intros.
    unfold merge.
    pose Carrier.lub_def as H3.
    specialize (H3 (update a s1) s).
    destruct H3, H1.
    elim H2.
    eexists (update a s1).
    split.
    assumption.
    split.
    apply Carrier.le_refl.
    
    unfold merge in H.
    pose Carrier.lub_def as H5.
    specialize (H5 s1 s2).
    destruct H5, H4.
    elim H5.
    eexists s.
    split.
    rewrite <- H.
    apply Carrier.le_refl.
    split.
    rewrite H.
    assumption.
    rewrite H.
    assumption.
  Qed.
  
End CRDT.


(* can't prove that. do we?
   Lemma update_mono: forall x y, compare x (update y x).
   Admitted.
*)
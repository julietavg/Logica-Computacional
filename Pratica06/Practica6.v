Module Props .

Notation "x :: l" := (cons x l)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Fixpoint map {A B : Type} (F : A -> B) (l : list A) : list B :=
  match l with
  | [] => []
  | x :: l' => F x :: map F l'
  end.

Fixpoint In {A : Type} (x : A) (l : list A) : Prop :=
  match l with
  | [] => False
  | y :: l' => x = y \/ In x l'
  end.


Example In_example_1 : In 4 [1; 2; 3; 4; 5].
Proof.
  simpl. right. right. right. left. reflexivity.
Qed.
                     
                     
Lemma In_map :
  forall (A B : Type) (f : A -> B) (l : list A) (x : A),
    In x l -> In (f x) (map f l).
Proof.
  intros A B f l x H.
  induction l as [| y l' IH].
  - inversion H.
  - simpl in H.
    destruct H as [Hx | Hl].
    + rewrite Hx. left. reflexivity.
    + right. apply IH. assumption.
Qed.
                     
Fixpoint All {T : Type} (P : T -> Prop) (l : list T) : Prop :=
  match l with
  | [] => True
  | x :: l' => P x /\ All P l'
  end.
  
Lemma All_In :
  forall T (P : T -> Prop) (l : list T),
    (forall x, In x l -> P x) <-> All P l.
Proof.
  intros T P l.
  split.
  - intros H. induction l as [| x l' IH].
    + simpl. trivial.
    + simpl. split.
      * apply H. left. reflexivity.
      * apply IH. intros x' H'. apply H. right. assumption.
  - intros H x Hx. induction l as [| x' l' IH].
    + inversion Hx.
    + simpl in H. simpl in Hx. destruct H as [Hx' Hl].
      destruct Hx as [Heq | Hin].
      * rewrite Heq. assumption.
      * apply IH; assumption.
Qed.

End Props .

ule Streams .

Variable L : Type.

Inductive TStream :=
   TScons : L -> TStream -> TStream.

Definition TShead (s : TStream) : L :=
  match s with
  | TScons x _ => x
  end.

Definition TStail (s : TStream) : TStream :=
  match s with
  | TScons _ xs => xs
  end.
  
Fixpoint TSnth (n : nat) (s : TStream) : L :=
  match n, s with
  | 0, TScons x _ => x
  | S m, TScons _ xs => TSnth m xs
  end.

Fixpoint TSnth_tail (n : nat) (s : TStream) : TStream :=
  match n, s with
  | 0, _ => s
  | S m, TScons _ xs => TSnth_tail m xs
  end.

Lemma one_step_nth_tail :
  forall (n : nat) (s : TStream),
  TStail (TSnth_tail n s) = TSnth_tail (S n) s.
Proof.
  intros n s.
  induction n as [|m IH] in s |- *.
  - reflexivity.
  - destruct s as [x xs]. simpl. apply IH.
Qed.
                
                            
Lemma multi_step_nth_tail : 
  forall (n:nat) (s:TStream) ,
  TSnth_tail (S n) s = TSnth_tail n (TStail s).
Proof.
  intros n s.
  destruct s as [x xs].
  simpl.
  reflexivity.
Qed.

Fixpoint TSnth_conc (n : nat) (s1 s2 : TStream) : TStream :=
 match n, s1 with
 | 0, _ => s2
 | S m, TScons x xs => TScons x (TSnth_conc m xs s2)
 end.

Lemma cons_head_tail :
  forall (s : TStream),
  TScons (TShead s) (TStail s) = s.
Proof.
  intros s.
  destruct s as [x xs].
  simpl.
  reflexivity.
Qed.
                
Lemma multi_step_nth_conc : 
  forall (n:nat) (s1 s2:TStream) ,
  (TSnth_conc (S n) s1 s2) =
  (TSnth_conc n s1 (TScons (TSnth n s1) s2)).
Proof.
  intros n s1 s2.
  induction n as [|m IH] in s1, s2 |- *.
  - destruct s1 as [x xs]. simpl. reflexivity.
  - destruct s1 as [x xs]. simpl. rewrite <- IH.
    rewrite <- cons_head_tail. reflexivity.
Qed.

Lemma nth_tail_with_nth_conc :
  forall (n : nat) (s1 s2 : TStream),
  TSnth_tail n (TSnth_conc n s1 s2) = s2.
Proof.
  intros n s1 s2.
  induction n as [|m IH] in s1, s2 |- *.
  - simpl. reflexivity.
  - destruct s1 as [x xs]. simpl. rewrite IH. reflexivity.
Qed.
                                 
Lemma nth_conc_with_nth_tail :
  forall (n:nat)(s:TStream) ,
  (TSnth_conc n s (TSnth_tail n s)) = s.
Proof.
  intros n s.
  induction n as [|m IH] in s |- *.
  - destruct s as [x xs]. simpl. reflexivity.
  - destruct s as [x xs]. simpl. rewrite IH. reflexivity.
Qed.

End Streams .
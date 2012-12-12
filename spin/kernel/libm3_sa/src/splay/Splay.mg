GENERIC MODULE Splay(Key, Value);

(* HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	This is an implementation of Splay trees copied from C code by Sleator.
 *
 *)

(*
  Comment from the original file "top-down-size-splay.c".

           An implementation of top-down splaying with sizes
             D. Sleator <sleator@cs.cmu.edu>, January 1994.

  This extends top-down-splay.c to maintain a size field in each node.
  This is the number of nodes in the subtree rooted there.  This makes
  it possible to efficiently compute the rank of a key.  (The rank is
  the number of nodes to the left of the given key.)  It it also
  possible to quickly find the node of a given rank.  Both of these
  operations are illustrated in the code below.  The remainder of this
  introduction is taken from top-down-splay.c.

  "Splay trees", or "self-adjusting search trees" are a simple and
  efficient data structure for storing an ordered set.  The data
  structure consists of a binary tree, with no additional fields.  It
  allows searching, insertion, deletion, deletemin, deletemax,
  splitting, joining, and many other operations, all with amortized
  logarithmic performance.  Since the trees adapt to the sequence of
  requests, their performance on real access patterns is typically even
  better.  Splay trees are described in a number of texts and papers
  [1,2,3,4].

  The code here is adapted from simple top-down splay, at the bottom of
  page 669 of [2].  It can be obtained via anonymous ftp from
  spade.pc.cs.cmu.edu in directory /usr/sleator/public.

  The chief modification here is that the splay operation works even if the
  item being splayed is not in the tree, and even if the tree root of the
  tree is NULL.  So the line:

                              t = splay(i, t);

  causes it to search for item with key i in the tree rooted at t.  If it's
  there, it is splayed to the root.  If it isn't there, then the node put
  at the root is the last one before NULL that would have been reached in a
  normal binary search for i.  (It's a neighbor of i in the tree.)  This
  allows many other operations to be easily implemented, as shown below.

  [1] "Data Structures and Their Algorithms", Lewis and Denenberg,
       Harper Collins, 1991, pp 243-251.
  [2] "Self-adjusting Binary Search Trees" Sleator and Tarjan,
       JACM Volume 32, No 3, July 1985, pp 652-686.
  [3] "Data Structure and Algorithm Analysis", Mark Weiss,
       Benjamin Cummins, 1992, pp 119-130.
  [4] "Data Structures, Algorithms, and Performance", Derick Wood,
       Addison-Wesley, 1993, pp 367-375
*)

(* In Sleator's original code, each splay tree procedure returns the
   new root of the tree. The caller of the procedure was responsible 
   for remembering the root. In this implementation, the N class behaves
   in exactly this way, but it is wrapped up in a T object which always
   remembers the root of the tree. *)

TYPE 
  N = OBJECT 
      key: Key.T;
      value: Value.T;
      left, right: N := NIL;
      size  : CARDINAL := 0;
    METHODS
      splay(key: Key.T): N := SplayNode;
      insert(new: N; VAR existed: BOOLEAN): N := InsertNode;
      delete(key: Key.T; VAR elem: N): N := DeleteNode;
      findRank(rank: INTEGER): N := FindRankNode;
      visit(Pre, In, Post: Proc) := VisitNode;
      init(key: Key.T; value: Value.T): N := InitNode;
    END;

REVEAL
  T = Public BRANDED OBJECT
      root : N := NIL;
    OVERRIDES
      splay := SplayTree;
      insert := InsertTree;
      delete := DeleteTree;
      find := FindTree;
      findRank := FindRankTree;
      rootRank := RootRank;
      getRoot := GetRootTree;
      visit := VisitTree;
    END;

(* Splay using the key (which may or may not be in the tree.) *)

PROCEDURE SplayNode(root: N; key: Key.T): N =
  VAR
    Dummy := NEW(N);
    l := Dummy;
    r := Dummy;
    y : N;
    l_size := 0;
    r_size := 0;
  BEGIN

    LOOP 
      WITH comp = Key.Compare(key, root.key) DO
        IF comp < 0 THEN
          IF root.left = NIL THEN EXIT; END;
          IF Key.Compare(key, root.left.key) < 0 THEN 
            y := root.left;
            root.left := y.right;
            y.right := root;
            root.size := 1;
            IF root.left # NIL THEN INC(root.size, root.left.size); END;
            IF root.right # NIL THEN INC(root.size, root.right.size); END;
            root := y;
            IF root.left = NIL THEN EXIT; END;
          END;
          r.left := root;
          r := root;
          root := root.left;
          INC(r_size);
          IF r.right # NIL THEN INC(r_size, r.right.size); END;
        ELSIF comp > 0 THEN
          IF root.right = NIL THEN EXIT; END;
          IF Key.Compare(key, root.right.key) > 0 THEN 
            y := root.right;
            root.right := y.left;
            y.left := root;
            root.size := 1;
            IF root.left # NIL THEN INC(root.size, root.left.size); END;
            IF root.right # NIL THEN INC(root.size, root.right.size); END;
            root := y;
            IF root.right = NIL THEN EXIT; END;
          END;
          l.right := root;
          l := root;
          root := root.right;
          INC(l_size);
          IF l.left # NIL THEN INC(l_size, l.left.size); END;
        ELSE
          EXIT;
        END;
      END;
    END;
    
    (* Now l_size and r_size are the sizes of the left and right
       trees we just built. *)
    IF root.left # NIL THEN INC(l_size, root.left.size); END;
    IF root.right # NIL THEN INC(r_size, root.right.size); END;
    root.size := l_size + r_size + 1;
    
    l.right := NIL;
    r.left := NIL;
    
    (* The following two loops correct the size fields of the right path 
       from the left child of the root and the right path from the left 
       child of the root. *)
    y := Dummy.right;
    WHILE y # NIL DO
      y.size := l_size;
      DEC(l_size);
      IF y.left # NIL THEN DEC(l_size, y.left.size); END;
      y := y.right;
    END;
    
    y := Dummy.left;
    WHILE y # NIL DO
      y.size := r_size;
      DEC(r_size);
      IF y.right # NIL THEN DEC(r_size, y.right.size); END;
      y := y.left;
    END;
    
    l.right := root.left;
    r.left := root.right;
    root.left := Dummy.right;
    root.right := Dummy.left;
    
    RETURN root;
  END SplayNode; 


(* Insert key i into the tree t, if it is not already there. 
   Return a pointer to the resulting tree. *)
PROCEDURE InsertNode(root: N; new: N; VAR existed: BOOLEAN): N =
  BEGIN
    existed := FALSE;

    root := root.splay(new.key);

    IF Key.Compare(new.key, root.key) = 0 THEN
      existed := TRUE;
      RETURN root;
    END;

    IF Key.Compare(new.key, root.key) < 0 THEN
      new.left := root.left;
      new.right := root;
      root.left := NIL;
      root.size := 1;
      IF root.right # NIL THEN INC(root.size, root.right.size); END;
    ELSE
      new.right := root.right;
      new.left := root;
      root.right := NIL;
      root.size := 1;
      IF root.left # NIL THEN INC(root.size, root.left.size); END;
    END;
    new.size := 1;
    IF new.left # NIL THEN INC(new.size, new.left.size); END;
    IF new.right # NIL THEN INC(new.size, new.right.size); END;

    RETURN new;
  END InsertNode;


(* Deletes i from the tree if it's there.          
   Return a pointer to the resulting tree. *)
PROCEDURE DeleteNode(root: N; key: Key.T; VAR elem: N): N =
  VAR
    x : N;
    tsize : INTEGER;
  BEGIN
    tsize := root.size;
    root := root.splay(key);
    IF Key.Compare(key, root.key) = 0 THEN
      IF root.left = NIL THEN
        x := root.right;
      ELSE
        x := root.left.splay(key);
        x.right := root.right;
      END;
      elem := root;
      IF x # NIL THEN
        x.size := tsize - 1;
      END;
      RETURN x;
    ELSE
      elem := root;
      RETURN root;
    END;
  END DeleteNode;

(* Returns a pointer to the node in the tree with the given rank.  
   Returns NULL if there is no such node.                          
   Does not change the tree.  To guarantee logarithmic behavior,   
   the node found here should be splayed to the root. *)
PROCEDURE FindRankNode(root: N; rank: INTEGER): N =
  VAR
    lsize: INTEGER;
  BEGIN
    IF rank < 0 OR rank >= root.size THEN
      RETURN NIL;
    END;

    LOOP
      IF root.left # NIL THEN lsize := root.left.size; ELSE lsize := 0; END;
      IF rank < lsize THEN
        root := root.left;
      ELSIF rank > lsize THEN
        rank := rank - lsize - 1;
        root := root.right;
      ELSE
        RETURN root;
      END;
    END;
  END FindRankNode;

(* Call the three functions before, during and after traversing the
   children of this node. *)
PROCEDURE VisitNode(node: N; Pre, In, Post: Proc) =
  BEGIN
    IF Pre # NIL THEN Pre(node.key, node.value); END;
    IF node.left # NIL THEN node.left.visit(Pre, In, Post); END;
    IF In # NIL THEN In(node.key, node.value); END;
    IF node.right # NIL THEN node.right.visit(Pre, In, Post); END;
    IF Post # NIL THEN Post(node.key, node.value); END;
  END VisitNode;

PROCEDURE InitNode(node: N; key: Key.T; value: Value.T): N =
  BEGIN
    node.key := key;
    node.value := value;
    RETURN node;
  END InitNode;

(* These are the member functions for the Splay.T object which contains
   the root of a splay tree. *)

PROCEDURE SplayTree(tree: T; key: Key.T) =
  BEGIN
    IF tree.root # NIL THEN
      tree.root := tree.root.splay(key);
    END;
  END SplayTree;

PROCEDURE InsertTree(tree: T; key: Key.T; value: Value.T): BOOLEAN =
  VAR
    existed := FALSE;
  BEGIN
    IF tree.root # NIL THEN
      tree.root := tree.root.insert(NEW(N).init(key, value), existed);
      RETURN existed;
    ELSE
      tree.root := NEW(N).init(key, value);
      RETURN FALSE;
    END;
  END InsertTree;

PROCEDURE DeleteTree(tree: T; key: Key.T; VAR elem_val: Value.T): BOOLEAN =
  VAR
    node: N;
  BEGIN
    IF tree.root # NIL THEN
      tree.root := tree.root.delete(key, node);
      IF Key.Compare(key, node.key) = 0 THEN
        elem_val := node.value;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END DeleteTree;

PROCEDURE FindTree(tree: T; key: Key.T; VAR value: Value.T): BOOLEAN =
  BEGIN
    IF tree.root # NIL THEN
      tree.root := tree.root.splay(key);
      IF Key.Compare(key, tree.root.key) = 0 THEN
        value := tree.root.value;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FindTree;

PROCEDURE FindRankTree(tree: T; rank: INTEGER; VAR elem_key: Key.T; 
                       VAR elem_val: Value.T): BOOLEAN =
  VAR
    node: N;
  BEGIN
    IF tree.root # NIL THEN
      node := tree.root.findRank(rank);
      IF node # NIL THEN
        elem_key := node.key;
        elem_val := node.value;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FindRankTree;

PROCEDURE RootRank(tree: T): CARDINAL =
  BEGIN
    IF tree.root # NIL AND tree.root.left # NIL THEN
      RETURN tree.root.left.size;
    ELSE
      RETURN 0;
    END;
  END RootRank; 

PROCEDURE GetRootTree(tree: T; VAR key: Key.T; VAR value: Value.T): BOOLEAN =
  BEGIN
    IF tree.root # NIL THEN
      key := tree.root.key;
      value := tree.root.value;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END GetRootTree;

PROCEDURE VisitTree(tree: T; Pre, In, Post: Proc) =
  BEGIN
    IF tree.root # NIL THEN tree.root.visit(Pre, In, Post); END;
  END VisitTree;


BEGIN
END Splay.

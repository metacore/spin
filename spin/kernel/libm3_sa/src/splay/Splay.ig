GENERIC INTERFACE Splay(Key, Value);

(* HISTORY
 * 18-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	This is an implementation of Splay trees copied from C code by Sleator.
 *
 *)

(* This interface defines a Splay tree type which enables efficient insertion,
   access and deletion in a binary tree. *)

(* This code is translated from the C source code of Dan Sleator. *)

(* Use splay to bring a particular node to the root of the tree and then
   getRootValue to read the value at the root. *)

TYPE
  T <: Public;

  Proc = PROCEDURE (key: Key.T; value: Value.T);

  Public = OBJECT 
  METHODS
    splay(key: Key.T);
    insert(key: Key.T; value: Value.T): BOOLEAN;
    delete(key: Key.T; VAR elem_val: Value.T): BOOLEAN;
    find(key: Key.T; VAR value: Value.T): BOOLEAN;
    findRank(rank: INTEGER; VAR elem_key: Key.T; VAR elem_val: Value.T): BOOLEAN;
    rootRank(): CARDINAL;
    getRoot(VAR key: Key.T; VAR value: Value.T): BOOLEAN;
    visit(Pre, In, Post: Proc);
  END;

END Splay.

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Removed MoveToHead. Added Remove{Head,Tail}Safe.
 * 07-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "Dequeue" implements a double ended queue.

   The type "Elem.T" has to include two fields "prev" and "next", both
   of the type "Elem.T". They are used as the dequeue links.

   Dequeue is not mutexed. Do it by yourself if you need to.
 *)
GENERIC INTERFACE Dequeue(Elem);
CONST Brand = "(Dequeue "& Elem.Brand & ")";

TYPE
  T = Elem.T;

  Iterator = RECORD
    head: T;
    cur: T;
  END;
  (* Iterator without memory allocation overhead. *)

PROCEDURE NewHeader() : T;
(* A T is represented by a header. All the members can be
  reached from the header. NewHeader creates a null T.
 *)

PROCEDURE InsertHead(head : T; t : T);
(* Insert "data" at the head of the dequeue. "head.next" becomes the
   newly inserted item.
 *)

PROCEDURE InsertTail(head: T; t: T);
(* Insert "data" at the tail of the dequeue. "head.prev" becomes the
   newly inserted item. *)

PROCEDURE Member(head: T; t: T): BOOLEAN;
(* Find an entry that has same value as "data". Uses "Elem.Equal" for
   the comparison. If the item is not found, returns DALSE.
 *)

PROCEDURE Empty(head: T): BOOLEAN;
(* See if the Tueue is empty or not. *)

PROCEDURE Remove(t: T);
(* Remove the element "t" from the deTueue *)

PROCEDURE RemoveHead(head: T): T;
(* Remove the element that is at the head of the Tueue, and store
   the element in "Value". If "head" is empty, then this proc will crash. *)
  
PROCEDURE RemoveHeadSafe(head: T): T;
(* Remove the element at the head of the queue "head". If the queue is
   empty, then the proc returns NIL. *)
  
PROCEDURE RemoveTail(head: T): T;
(* Remove the entry that is at the tail of the "head".
   If "head" is empty, then this proc will crash. *)
  
PROCEDURE Size(T: T): INTEGER;
(* Get the number of elements in the Tueue. *)

  
(*
 Iterator stuff
 *)
  
PROCEDURE Iterate(head: T): Iterator;
(* Returns a iterator that traverses all the Valueents in the deTueue
   in head-to-tail order. The iterator is not an object like in other
   libraries to gain performance. *)
  
PROCEDURE NextItr(VAR itr: Iterator; VAR t: T): BOOLEAN;
(* Get the next item from the iterator and store it on "t".
   Returns "FALSE" if the iterator
   reached the end of the dequeue, otherwise it returns "TRUE".
   You can call "Remove" on "t", but nothing else that modifies the
   links. *)

   
  

(* "Allocate" and "Free" provides fast allocation and deallocation of
   "T"s. This is done by pooling the freed objects in a list, and
   reusing the carcass in the list on allocation. *)
PROCEDURE Allocate() : T;
PROCEDURE Free(t: T);
  
END Dequeue.


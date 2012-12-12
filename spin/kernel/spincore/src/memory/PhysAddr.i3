(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 14-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made PhysAddr.T an object, and allowed subtyping of it.
 * 28-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Reorganized documents.
 *	
 * 25-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added zombie state. This is a desperate attempt to get rid of all
 *	potential deadlocks.
 * 29-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added protection stuff - copyonwrite preparation.
 *
 * 13-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added copy, read, write.
 *	
 * 7-Apr-96 oystr at the University of Washington
 *	Added AllocateIOSpace.
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *  html
 *)

(* 

 PhysAddr actually defines the interface to physical page frames.
 "PhysAddr.T" is an opaque handle to a frame.

<a name="states"></a>   
** State of Pages

The spincore currently uses FIFO with second chance scheme to choose
victims for page replacement.  Each physical frame is in one of the
below states.

<dl>
  <dt> <b>Free</b>
  <dd> The page is free. 
  <dt> <b>Active</b>
  <dd> The page is now mapped on virtual address spaces.
       
       Note: the pmap module doesn't guarantee that
       the page entered in pmap is always mapped.
       Therefore, an active page may cause a page fault.
       
  <dt> <b>Reclaimed</b>
  <dd> The page is valid, but is unmapped from all the address spaces.
	    
  <dt> <b>Pinned</b>
  <dd> The page is mapped, and it will be never chosen as a pageout
       victim. You can set a page this state only through the
       [PhysAddrPrivate] interface.

  <dt> <b>Transient, Zombie</b>
  <dd> These states should not be visible to clients. A page is transient
       when it is being manipulated by the PhysAddr procedures.
       Page is zombie when it is being paged out. When the page is zombie,
       most of the procedures that refers to the page raise an exception.
       Exceptions are the following:
      <ul>
      <li>all FUNCTIONAL procedures can work on zombie pages.
      <li><code>Deallocate, Read, Write, Access</code> can work
   on zombie pages.
      </ul>
</dl>

** Page Reclamation

   The policy deployed by spincore is FIFO with second chance, 
   similar to the mechanism used in VMS and Windows NT.

   Spincore keeps a FIFO queue of pages for each of the states.
   When there is an insufficient
   number of Free frames, spincore begins requeueing Active pages into
   the Reclaimed queue and raises the "Reclaimed" event.
   If the number of Free frames becomes really low,
   spincore begins to put Reclaimed pages into Zombie queue and
   raise the "Repossess" event. When the reposses handler returns,
   it puts back the Zombie frames into Free queue.

   The Reclaimed queue serves two purposes. First, it is used to make sure that
   the pages being purged are not mapped anywhere (if the page is modified
   by an application while it is written out to the swap disk,
   you will see corrupted memory next time the page is brought in). Next,
   it serves as the <em>second chance</em> queue. By placing certain number
   of pages in the reclaimed queue, VM system can take page faults on reclaimed
   pages and put it back on the front of Active queue. 
   Below is the summary of page state transition.

<ul>
  <li> Free -> Active when a page is allocated.
  <li> Active -> Reclaimed in FIFO order when the # of free pages
       falls below certain limit. This threshold is called
       <dfn>reclaim threshold</dfn>.<br>

       The page that became active first is chosen and is reclaimed.
       The "Reclaim" event is raised.
  <li> Reclaimed -> Active when page fault happens.
  <li> Reclaimed -> Free when the # of free pages falls below a
       fixed threshold. This threshold is called
       <dfn>reposses threshold</dfn>.<br>
       The "Repossess" event is raised.
</ul>

 *)

INTERFACE PhysAddr;
IMPORT Protection;
IMPORT CPU;
IMPORT VMError;

TYPE 
  T		<: ROOT;
  Size		= CPU.PhysAddress;
  Address	= CPU.PhysAddress;
  State         = {Transient, Dead, Zombie, Free, Reclaimed, Active, 
		   StronglyPinned};
  
  Tag = RECORD
    obj: REFANY;
    off: INTEGER;
  END;
  (*
     Tag is an additional information attached to each frame. It is not
     used by spincore, but extensions can store any value here. "obj" is
     also used as an authentication key for "Reclaimed" and "Reposses" event.

     The VMcore extension uses "obj" to hold the memory object that owns the
     page, and "off" to hold the offset of the page within the memory object.
     
     Note: the structure of "Tag" reflects the way it is used in vmcore
     extension. This may not be clean, but I traded the efficiency
     with aethetics. -- yas
  *)

  Content = ARRAY [0 .. CPU.PAGESIZE-1] OF CHAR;
  (* Content of a frame is represented by "Content" type. *)
  
PROCEDURE Allocate(tag: Tag): T  RAISES {VMError.E};
(* Allocate a page frame. "tag" provides an additional tag attached
   to the frame. This procedures is considered obsolete. Use
   Init(NEW(T), tag) instead. *)
  
PROCEDURE Initialize(t: T; tag: Tag): T RAISES {VMError.E};
(* Initialize the fields of "t" and assign a physical frame to "t".*)
  
FUNCTIONAL PROCEDURE GetTag(t: T): Tag;
FUNCTIONAL PROCEDURE GetState(t: T): State;
  
PROCEDURE ChangeState(t: T; state: State) RAISES {VMError.E};
(* This proc is used for several purposes.
   <ul>
   <li>If "state" = Active, then the page "t" will moved to the front of the
   active page list, meaning that the page is unlikely to be chosen as a
   victim in the near future.

   <li>If "state" = Reclaimed, then the page is likely to be chosen as a
   victim in the near future.

   <li>All other "state" values result in exception.
   </ul>
*)
  
EPHEMERAL
PROCEDURE ChangeTag(t: T; tag: Tag) RAISES {VMError.E};
(* Set the tag of the page. *)
  
FUNCTIONAL
PROCEDURE GetProtection(t: T): Protection.T RAISES {VMError.E};
(* Each frame also has a [Protection] attribute.
   access mode for that frame; the access mode in particular [Translation]
   may be weaker than this.
   Initially, each page has RWX protection, and the "other" is 0.
  *)

PROCEDURE ChangeProtection(t: T; prot: Protection.T)
  RAISES {VMError.E};
(* You can also change the protection bits here. The call will modify
   the prot modes of all the pmaps the frame is mapped onto. 
   Exception is raised if the page state is "Zombie". *)
  
PROCEDURE Deallocate(p: T);
(* Deallocate the page frame. This procedure is idempotent, i.e., calling
   Deallocate on the same frame twice is same as calling it once. *)
  
PROCEDURE Print(p: T): TEXT;
(* XXX this really has to be FUNCTIONAL EPHEMERAL *)

<*OBSOLETE*>FUNCTIONAL EPHEMERAL  
PROCEDURE FreeMem(): Size;
(* Returns the number of frames in "Free" state.
   Note: superseded by "GetStat"*)
  
PROCEDURE Copy(dest, src: T) RAISES {VMError.E};
(* Copy the contents from "src" to "dest". *)

PROCEDURE Read(p: T; off: CARDINAL; VAR buf: ARRAY OF CHAR)
  RAISES{VMError.E};
(* Read the contents of the frame "p", from offset "off" into "buf". *)

PROCEDURE Write(p: T; off: CARDINAL; READONLY buf: ARRAY OF CHAR)
  RAISES{VMError.E};
(* Write into the page "p" from "buf". *)

PROCEDURE Access(p: T; callback: PROCEDURE (VAR buf: Content))
  RAISES{VMError.E};
(* Call the procedure "callback" with the content of this frame *)

TYPE
  Params = RECORD
    requestThreshold: INTEGER;
    reclaimThreshold: INTEGER;
    repossesThreshold: INTEGER;
  END;
  
  Stat = RECORD
    sizes: ARRAY State OF CARDINAL;
    (* # of pages in each state *)
  END;

PROCEDURE GetParams(VAR p: Params);
PROCEDURE GetStat(VAR p: Stat);

(* Below three are events that default to nop *)

(* XXX these events do not provide a way to
   purge an alternative page. This is because I haven't yet come up with
   reliable mutual exclusion strategy. Please wait -- yas *)

PROCEDURE Reclaim(p: T);
(* This is an event. This event is raised when the frame "p"
   is moved from active list to reclaimed list.
   By default, PhysAddr module does nothing in response.
   See <a href="#states">page states</a> also. *)
  
PROCEDURE Repossess(p: T);
(* This is an asynchronous event. This event is raised when the frame "p"
   is moved from reclaimed list to free list.
   This is usually the only event external vm implementors have to
   listen to. Typically, one has to write out the frame contents
   when this event is raised. After the handlers for this event finish,
   the frame is reclaimed and reused for another purpose.
   By default, PhysAddr module does nothing in response. *)

PROCEDURE GetVictims(tag: REFANY; VAR (*OUT*)p: ARRAY OF T): CARDINAL;
(* Remove the frames that are at the tail of the reclaimed list, and
   put them into the Zombie state. Returns the # frames evicted from
   the reclaimed list. *)

CONST Brand = "PhysAddr";
END PhysAddr.


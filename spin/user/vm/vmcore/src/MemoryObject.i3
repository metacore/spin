(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added space and virtAddr to "request" args.
 * 24-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added stat
 * 02-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	COW support.
 * 16-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added Destroyed event.
 *
 * 20-Feb-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

(*

 Memory object is a conainer of page frames.

Internally, a memory object consists of two objects,
<a href="CacheObject.html">cache</a> and
<a href="PagerObject.html">pager</a>.
Cache holds a set of page frames. All the frames
in a memory object is recorded in the cache object.

Pager implements a mechanism to transfer pages to and from
memory and external I/O device. Pager is expected to be
subtyped. Currently, two types of pagers are provided, namely, 
<em>Bogopager</em>
and <!--href="filepager"--><em>Filepager</em></a>
pager</dfn>).

<ul>
  <li> Bogopager always provides a zero filled page on page fault.
       It doesn't provide the page out operation.
       Bogopager comes with the vmcore extension.
  <li> File pager provides a zero filled page on the first fault.
       It supports page-out on generic
       <!--href="fs"-->file system</a>.
       When loaded into memory, the default pager replaces the
       <a href="../src/DefaultPager.i3">public procedures</a>
       to call bogopager. Thus, the bogopager and the file pager
       are used in the same way.
</ul>

<bq>
<em>Note:</em>
The reason the bogopager is there is that the default pager depends on
the file system, and file system depends on vmcore. Therefore, we
can't put the default pager in vmcore, or we have circular dependency.
</bq>

*)
INTERFACE MemoryObject;

IMPORT VMError, VMTypes;
IMPORT PhysAddr;
IMPORT PagerObject;
IMPORT CacheObject;
IMPORT Protection;

TYPE
  PageNumber = VMTypes.PageNumber;
  PageCount = VMTypes.PageCount;
  
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(size: PageCount;
	 pager: PagerObject.T := NIL;
	 cache: CacheObject.T := NIL;
	 name: TEXT := NIL): T RAISES {VMError.E};
    (* This proc initializes the memory object. If "pager" is NIL, then
     the <!--href="filepager"-->file pager</a> will be newly created.
     If "cache" is NIL, then the default cache will be newly created.
     "size" specifies the maximum number of pages that this memory object
      will occupy. "name" is used just for debugging. *)
    
    request(offset: PageNumber; type: INTEGER;
	    VAR frame: PhysAddr.T; VAR prot: Protection.T): BOOLEAN
      RAISES {VMError.E};
    (* This is called by AddressSpace page fault handler. Request
       returns the physical frame that backs the location "offset", and
       the new protection bits that should be applied when mapping
       "frame" onto the faulted address.

       "type" is one of Trap.Read/Write/Execute that indicates
       how the fault happened.

     *)

    fork(): T RAISES {VMError.E};
    (* Create an eager copy of the object.
       THIS WILL BE REMOVED IN THE FUTURE. *)

    copyOnWrite(from := 0; len: PageCount := LAST(INTEGER)): T
        RAISES {VMError.E};
    (* Create a memory object that clones the region
       "from".."from+len" of this memobject.

       XXX need to change the interface so that we can turn
       existing mobj into cow'ed one.*)

    isMapped(): BOOLEAN;
    (* Returns true is the memory object it mapped on any addrspace.
       We don't return where it is mapped for safety reason. *)

    print(): TEXT;
    (* Returns some human-understandable descriptive string. *)
    
    stat(): Stat;
    
    destroy();
  END;

  Stat = RECORD
    virtualSize, residentSize: PageCount;
  END;

CONST
  Brand = "MemoryObject";

PROCEDURE Read(memObj: T; from: CARDINAL; VAR buf: ARRAY OF CHAR)
  RAISES {VMError.E};
PROCEDURE Write(memObj: T; from: CARDINAL; READONLY buf: ARRAY OF CHAR)
  RAISES {VMError.E};
PROCEDURE Access(memObj: T; from, len: CARDINAL;
		 proc: PROCEDURE (pos: CARDINAL;
				  VAR buf: ARRAY OF CHAR)) RAISES {VMError.E};
  
PROCEDURE Destroyed(memObj: T);
(* This is an event. Called when "memObj" is going to be destroyed.
   The auth key is the memory object.*)

END MemoryObject.




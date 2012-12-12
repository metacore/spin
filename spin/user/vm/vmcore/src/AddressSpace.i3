(*
 * Copyright 1995,1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added stat.
 * 22-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made T a subtype of Translation.
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Switch to be page oriented (instead of address oriented) to avoid
 *	all the truncs and rounds. 
 *
 * 13-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Changed to object representation
 *
 * 20-Dec-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *
 *)

(* 
The VM extension provides two types of address spaces,
"AddressSpace.T" and <!--ref="space"-->"Space.T"</a>.

"AddressSpace.T" provides the reservation of
an address range, and mapping and unmapping of a memory object on
the address space.
 
"Space.T" is a subtype of "AddressSpace.T".
Thus, it supports all the services that "AddressSpace.T"
provides. In addition to them, "Space.T" provides reading and
writing on it.

*)

INTERFACE AddressSpace;

IMPORT Translation, MemoryObject;
IMPORT VMError;
IMPORT VMTypes;
IMPORT ExternalRef;

TYPE
  T <: Public;
  Public = Translation.T OBJECT
  METHODS
    init(name: TEXT): T;
    (* This is the constructor. "name" is used only for debugging purposes. *)
    
    destroy();
    (* Frees up all the resources held by the address space. *)
    
    allocate(VAR pageNum: VMTypes.PageNumber;
	     numPages: VMTypes.PageCount; anyWhere: BOOLEAN)
             RAISES {VMError.E};
    deallocate(pageNum: VMTypes.PageNumber; numPages: VMTypes.PageCount)
               RAISES {VMError.E};
    (* These methods reserve or unreserve
       the virtual address region without allocating
       memory. 
       They are used to reserve a certain region for later use.
       If "anyWhere" is true, then "allocate" first tries the address
       "pageNum". If it is occupied, then it chooses a free region by
       its own. If "anyWhere" is false and the address "pageNum" is
       occupied, then "allocate" raises an exception.

       <bq><em>Note:</em> The semantics of when "anyWhere" is false is
       different from UNIX "mmap". This has to be fixed soon!</bq>
     
       "pageNum" and "numPages" are counted in the MMU page size unit.       
     *)

    map(pageNum: VMTypes.PageNumber; numPages: VMTypes.PageCount;
	mObj: MemoryObject.T;
	mOff: VMTypes.PageNumber; lazy := FALSE) RAISES {VMError.E};
    (* "map" maps the region
       "mOff" to "mOff+numPages"
       of the <a href="memobj.html">memory object</a> "mObj"
       on the virtual address "pageNum" to
       "pageNum + numPages". "mOff", "pageNum",
       and "numPages" are all specified in machine MMU page
       unit. On Alpha, the page size is 8192 bytes.
       
       The region "pageNum" to "pageNum + numPages" must
       be reserved beforehand using "allocate".

       If "lazy" is true, then the pages is the region is not
       immediately paged in. Instead, they are brought in as the
       user app page faults in the region. If "lazy" is false, then
       the pages are eager copied.
     
     *)

    unmap(pageNum: VMTypes.PageNumber; numPages: VMTypes.PageCount)
          RAISES {VMError.E};
    (* Unmaps the memory object mapped on the space.
     Currently, the region "pageNum .. pageNum+numPages" must be
     <em>exactly</em> same as the region specified in previous "mmap" call.
     In other words, you can not unmap a subregion(or superregion) of
     a region created by mmap. *)

    stat(VAR (*OUT*) buf: Stat) RAISES {VMError.E};
    getXrefTbl(): ExternalRef.T;
    setXrefTbl(t: ExternalRef.T): ExternalRef.T;
    
    clear() RAISES {VMError.E};
    print(): TEXT;
  END;
  Stat = RECORD
    virtualSize, residentSize: CARDINAL;
  END;
  
CONST
  Brand = "AddressSpace-1.1";

PROCEDURE Equal(as1, as2: T): BOOLEAN;
PROCEDURE Hash(as: T): CARDINAL;
PROCEDURE Compare(as1, as2: T): [-1..1];

END AddressSpace. 

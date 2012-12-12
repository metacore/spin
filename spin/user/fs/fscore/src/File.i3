(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 27-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed writeref/readref signatures.
 * 26-May-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added unauthorized error code.
 *
 * 23-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Cleaned up Error interface.
 * 16-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added truncate.
 *
 * 08-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added getRoot
 * 17-Sep-96  becker at the University of Washington
 *	Added WOULDBLOCK result code
 *	
 * 02-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	read and write now takes var array of char instead of ref array of char
 *	
 * 05-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Made "from" in both the read and write methods a CARDINAL.
 *
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed read/write argument types so that we can avoid data copying.
 *      ExplainError, stat is added.
 * 22-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added copyright.
 *      Changed to use Device exceptions.
 *)

(* File.i3"

   File.T is an opaque type representing a file.
 *) 

INTERFACE File;
IMPORT Error, FileId, FileStat;
IMPORT FSRoot;

TYPE
  OffsetT = CARDINAL;
  Attr = {Ordinary, MountPoint, MountShadow, Stale};

TYPE T <: TPublic;
     TPublic = OBJECT 
       id : FileId.T;
       attr: Attr;
     METHODS
       read(VAR data : ARRAY OF CHAR;
	    offset   : OffsetT): CARDINAL 
           RAISES {Error.E};
       
       write(READONLY data : ARRAY OF CHAR;
	     offset        : OffsetT): CARDINAL 
           RAISES {Error.E};

       readRef((*IN/OUT*)VAR buf: REF ARRAY OF CHAR;
	       bytes: CARDINAL;
	       offset: OffsetT;
	       (*IN/OUT*)VAR from: CARDINAL): CARDINAL RAISES {Error.E};
       (* "readRef" works differently depending on whether "buf" is nil on
	  entry. If "buf" is NIL on entry, "readRef" allocates buffer by its
	  own and returns the contents on it. In that case, "from" holds the
	  starting index within "buf" from which the contents are stored.

	  If "buf" is not NIL, then "readRef" uses "buf" to store the contents.
	  "from" becomes an input parameter that specifies the starting index
	  from which to store the contents. "from" is left intact in this case.

	  The default implementation just calls the read method.
	  *)
       
       writeRef(buf: REF ARRAY OF CHAR;
		bytes: CARDINAL;
		offset: OffsetT;
		from: CARDINAL;
		(*OUT*)VAR retain: BOOLEAN): CARDINAL RAISES {Error.E};
       (*
	  Write the region "from" .. "from+bytes" of "buf".
	  "bufShouldBeRetained" is an output variable indicating whether
	  "buf" can be discarded after this function finishes.

	  If "retain" is TRUE, then the implementor of
	  File.T *MUST* free the "buf" using "CharArray.Free" after
	  I/O completes.

	  *)
       
       stat((*OUT*)VAR s: FileStat.T) RAISES {Error.E};
       (* Implements fstat(2).
	  
	  Note to implementors: You must fill in all the fields of
	  the stat structure, or applications will behave strangely
	  (especially, if you forget to fill in blksize, OSF/1 applications
	  will run out of memory). The safest way is to call FileStat.Init
	  at the head of each implementation of stat. *)
       
       open(mode: INTEGER): T RAISES{Error.E};
       (* Prepare doing operating on the file.
	  The default implementation just returns the self itself.

	  Note: it is not safe to assume that all clients call open and
	  close properly as they operate on files. Consider this as just
	  a hint. *)
	  
       close() RAISES {Error.E};
       (* End operating on the file. The default implementation does
	  nothing. See also the caveats in "open". *)

       truncate(newSize: CARDINAL) RAISES {Error.E};
       (* Change the size of the file. This can be used to either
	  expand or shrink the file. *)
       
       ioctl(cmd: INTEGER; VAR arg: ARRAY OF CHAR; flags: INTEGER)
         RAISES {Error.E};
       (* This is an interface to the legacy ioctl(2).

	  "cmd" is the 2nd arg to ioctl(2).
	  "arg" holds the bits from the user space when the
	  parameter length(IOCPARM_LEN in sys/ioctl.h) is non-zero.
	  When IOCPARM_LEN is 0, then arg holds the value of 3rd arg itself.
          "flags" is the open mode. *)
       
       root(): FSRoot.T RAISES {Error.E};
     END;

CONST
  FS_OK                   = 0;
  FS_FIRST_ERROR          = FS_NOT_DIRECTORY;
  FS_NOT_DIRECTORY        = 5000; (* not a directory *)
  FS_NO_ENTRY             = 5001; (* name not found *)
  FS_NAME_TOO_LONG        = 5002; (* name too long *)
  FS_SYMLINK_LOOP         = 5003; (* symbolic link loop *)
  FS_INVALID_FS           = 5004; (* bad file system *)
  FS_NOT_IN_FILE          = 5005; (* offset not in file *)
  FS_INVALID_PARAMETER    = 5006; (* bad parameter to a routine *)
  FS_CANT_MOUNT           = 5007; (* can't mount device *)
  FS_CROSS_DEVICE         = 5008; (* no cross device support *)
  FS_WOULD_BLOCK          = 5009; (* operation would block/EAGAIN *)
  FS_NOT_SUPPORTED        = 5010; (* operation not supported *)
  FS_UNAUTHORIZED         = 5011; (* not authorized for operations *)
  FS_LAST_ERROR           = FS_UNAUTHORIZED;

(* Use File.ErrorT to raise FS_FOO_BAR errors *)
TYPE ErrorT <: Error.T;
  
END File.

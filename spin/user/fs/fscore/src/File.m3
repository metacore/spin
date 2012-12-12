(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Glue layer between device calls and Modula-3 types
 *
 * HISTORY 
 * 27-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Implemented readref/writeref defaults.
 * 26-May-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added unauthorized error code.
 *
 * 23-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Cleaned up Error interface.
 * 08-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added getRoot
 * 17-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed up error handling support.
 *
 * 17-Sep-96  becker at the University of Washington
 *	Added WOULDBLOCK result code
 *	
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed read/write argument types so that we can avoid data copying.
 *      ExplainError, stat is added.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *)

MODULE File;
IMPORT Error, IO, FileStat;
IMPORT FSRoot;

REVEAL T = TPublic BRANDED OBJECT
OVERRIDES
  ioctl    := Ioctl;
  read     := Read;
  readRef  := ReadRef;
  write    := Write;
  writeRef := WriteRef;
  stat     := Stat;
  close    := Close;
  open     := Open;
  root     := Root;
  truncate := Truncate;
END;

PROCEDURE Close(<*UNUSED*>self : T) =
  BEGIN
  END Close;

PROCEDURE Open(self : T; <*UNUSED*>mode : INTEGER): T =
  BEGIN
    RETURN self;
  END Open;

  
PROCEDURE Stat(
    <*UNUSED*>self     : T;
    <*UNUSED*>VAR stat : FileStat.T) RAISES{Error.E} =
  BEGIN
    IO.Put("File.Stat default not implemented.\n");
    RAISE Error.E(NEW(ErrorT).init(FS_NOT_SUPPORTED));
  END Stat;
  
PROCEDURE Root (<*UNUSED*>self: T): FSRoot.T
  RAISES{Error.E} =
  BEGIN
    IO.Put("File.Root default not implemented.\n");
    RAISE Error.E(NEW(ErrorT).init(FS_NOT_SUPPORTED));
  END Root;

PROCEDURE Truncate (<*UNUSED*>self: T;
		    <*UNUSED*>size: CARDINAL) RAISES {Error.E} =
  BEGIN
    IO.Put("File.Truncate not implemented.\n");
    RAISE Error.E(NEW(ErrorT).init(FS_NOT_SUPPORTED));
  END Truncate;
  
  
PROCEDURE Ioctl(
    <*UNUSED*>self    : T;
    <*UNUSED*>cmd     : INTEGER;
    <*UNUSED*>VAR arg : ARRAY OF CHAR;
    <*UNUSED*>mode    : INTEGER) RAISES {Error.E} =
  BEGIN
    IO.Put("File.Ioctl default not implemented.\n");
    RAISE Error.E(NEW(ErrorT).init(FS_NOT_SUPPORTED));
  END Ioctl;

PROCEDURE Write(<*UNUSED*>self          : T;
		<*UNUSED*>READONLY data : ARRAY OF CHAR;
		<*UNUSED*>off: CARDINAL): CARDINAL =
  BEGIN
    IO.Put("File.Write default not implemented.\n");
    RETURN 0;
  END Write;


PROCEDURE Read (<*UNUSED*>self     : T;
		<*UNUSED*>VAR data : ARRAY OF CHAR;
		<*UNUSED*>offset   : OffsetT): CARDINAL =
  BEGIN
    IO.Put("File.Read default not implemented.\n");
    RETURN 0;
  END Read;
  
PROCEDURE ReadRef(self: T; 
		  VAR buf: REF ARRAY OF CHAR;
		  bytes: CARDINAL;
		  offset: OffsetT; 
		  VAR from: CARDINAL): CARDINAL RAISES {Error.E} =
  BEGIN
    IF buf = NIL THEN
      buf := NEW(REF ARRAY OF CHAR, bytes);
      from := 0;
    END;
    RETURN self.read(SUBARRAY(buf^, from, bytes), offset);
  END ReadRef;


PROCEDURE WriteRef (self: T; 
		    buf: REF ARRAY OF CHAR;
		    bytes: CARDINAL;
		    offset: OffsetT; 
		    from: CARDINAL;
		    VAR blah: BOOLEAN): CARDINAL RAISES {Error.E} =
  BEGIN
    blah := FALSE;
    RETURN self.write(SUBARRAY(buf^, from, bytes), offset);
  END WriteRef;

CONST
  ErrorMessages = ARRAY [ FS_FIRST_ERROR .. FS_LAST_ERROR] OF TEXT 
  {
  "Not a directory",
  "No such file or directory",
  "File name too long",
  "Too many levels of symbolic link",
  "Bad file system",
  "Offset not in file",
  "Bad parameter to a routine",
  "Can't mount a device",
  "No cross device support",
  "Operation would block",
  "Operation not supported",
  "Not authorized"
  };

CONST
  Errnos = ARRAY [FS_FIRST_ERROR..FS_LAST_ERROR] OF INTEGER {
  20, (*Errno.ENOTDIR,*)
  2,  (*Errno.ENOENT,*)
  63, (*Errno.ENAMETOOLONG,*)
  62, (*Errno.ELOOP,*)
  30, (*Errno.EROFS,   *)
  34, (*Errno.ERANGE, *)
  22, (*Errno.EINVAL,*)
  15, (*Errno.ENOTBLK,*)
  18, (*Errno.EXDEV,*)
  35, (*Errno.EWOULDBLOCK*)
  45, (*Errno.EOPNOTSUPP*)
  13  (*Errno.EACCESS*)
  };

REVEAL ErrorT = Error.T BRANDED OBJECT
  OVERRIDES
    errno   := Errno;
    message := Message;
  END;

PROCEDURE Errno (self: ErrorT): INTEGER =
  VAR
    rc := self.resultCode();
  BEGIN
    IF rc >= FS_FIRST_ERROR AND rc <= FS_LAST_ERROR THEN
      RETURN Errnos[rc];
    END;
    RETURN rc;
  END Errno;
  
PROCEDURE Message (self: ErrorT): TEXT =
  VAR
    rc := self.resultCode();
  BEGIN
    IF rc >= FS_FIRST_ERROR AND rc <= FS_LAST_ERROR THEN
      RETURN ErrorMessages[rc];
    END;
    RETURN Error.T.message(self);
  END Message;

BEGIN
END File.

(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 21 11:15:30 PST 1994 by kalsow     *)
(*      modified on Thu Jul  1 08:46:40 PDT 1993 by mcjones    *)
(*      modified on Thu May  6 13:32:07 PDT 1993 by mjordan    *)

UNSAFE MODULE FileWin32;

IMPORT File, RegularFile, Terminal, Pipe, OSError;
IMPORT WinDef, WinError, WinNT, WinBase;
IMPORT OSErrorWin32, TimeWin32;

REVEAL
  File.T = T BRANDED OBJECT
    ds: DirectionSet; 
  OVERRIDES
    write := FileWrite;
    close := FileClose;
    status := FileStatus
  END;

  Pipe.T = File.T BRANDED OBJECT
  OVERRIDES
    read := PipeRead
  END;

  Terminal.T = File.T BRANDED OBJECT 
  OVERRIDES
    read := RegularFileRead
  END;

  RegularFile.T = RegularFile.Public BRANDED OBJECT 
  OVERRIDES
    read := RegularFileRead;
    seek := RegularFileSeek;
    flush := RegularFileFlush;
    lock := RegularFileLock;
    unlock := RegularFileUnlock
  END;

PROCEDURE New(handle: WinNT.HANDLE; ds: DirectionSet)
  : File.T RAISES {OSError.E}=
  VAR
    ft := WinBase.GetFileType(handle);
  BEGIN 
    CASE ft OF
    | WinBase.FILE_TYPE_DISK =>
        RETURN NEW(RegularFile.T, handle := handle, ds := ds)
    | WinBase.FILE_TYPE_CHAR =>
        RETURN NEW(Terminal.T, handle := handle, ds := ds)
    | WinBase.FILE_TYPE_PIPE => RETURN NewPipe(handle, ds)
    ELSE (* includes FILE_TYPE_UNKNOWN, FILE_TYPE_REMOTE *)
      OSErrorWin32.Raise0(WinError.ERROR_INVALID_HANDLE);
      <*ASSERT FALSE*>
    END;
  END New;

PROCEDURE NewPipe(handle: WinNT.HANDLE; ds: DirectionSet): Pipe.T =
  BEGIN
    RETURN NEW(Pipe.T, handle := handle, ds := ds)
  END NewPipe;

(*---------------------------File methods------------------------------------*)

PROCEDURE FileWrite(h: File.T; READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR nWritten: INTEGER;
  BEGIN
    IF NOT(Direction.Write IN h.ds) THEN BadDirection(); END;
    IF WinBase.WriteFile(h.handle, ADR(b[0]), NUMBER(b),
                         ADR(nWritten), NIL) = 0 THEN
      OSErrorWin32.Raise()
    END;
    <*ASSERT nWritten = NUMBER(b) *>
  END FileWrite;

PROCEDURE FileClose(h: File.T) RAISES {OSError.E} =
  BEGIN
    IF WinBase.CloseHandle(h.handle) = 0 THEN OSErrorWin32.Raise() END
  END FileClose;

PROCEDURE FileStatus(h: File.T): File.Status  RAISES {OSError.E}=
  VAR
    ffd: WinBase.BY_HANDLE_FILE_INFORMATION;
    status: File.Status;
    ft := WinBase.GetFileType(h.handle);
  BEGIN
    CASE ft OF
    | WinBase.FILE_TYPE_DISK =>
        IF WinBase.GetFileInformationByHandle(h.handle, ADR(ffd)) = 0 THEN
          OSErrorWin32.Raise();
        END;
        status.type := RegularFile.FileType;
        status.modificationTime := TimeWin32.FromFileTime(ffd.ftLastWriteTime);
        status.size := ffd.nFileSizeLow
    | WinBase.FILE_TYPE_CHAR => status.type := Terminal.FileType
    | WinBase.FILE_TYPE_PIPE => status.type := Pipe.FileType
    ELSE (* includes FILE_TYPE_UNKNOWN, FILE_TYPE_REMOTE *)
      <* ASSERT FALSE *>
    END;
    RETURN status
  END FileStatus;  

(*------------------------------Pipe methods---------------------------------*)

PROCEDURE PipeRead(
    h: Pipe.T;
    VAR (*out*) b: ARRAY OF File.Byte;
    mayBlock: BOOLEAN := TRUE)
  : INTEGER RAISES {OSError.E} =
  VAR numToRead: WinDef.DWORD := NUMBER(b); numAvail, numRead: WinDef.DWORD;
  BEGIN
    IF NOT(Direction.Read IN h.ds) THEN BadDirection(); END;
    IF NOT mayBlock THEN
      IF WinBase.PeekNamedPipe(
         hNamedPipe := h.handle,
         lpBuffer := NIL,
         nBufferSize := 0,
         lpBytesRead := NIL,
         lpTotalBytesAvail := ADR(numAvail),
         lpBytesLeftThisMessage := NIL) = 0 THEN
        OSErrorWin32.Raise()
      END;
      IF numAvail = 0 THEN RETURN -1 END;
      numToRead := MIN(numAvail, numToRead)
    END;
    IF WinBase.ReadFile(
       h.handle, ADR(b[0]), numToRead, ADR(numRead), NIL) = 0 THEN
      IF WinBase.GetLastError() = WinError.ERROR_BROKEN_PIPE THEN RETURN 0 END;
        (* *** What about ERROR_NO_DATA -- "The pipe is being closed."
           or ERROR_PIPE_NOT_CONNECTED -- "No process is on the other
           end of the pipe."? *)
      OSErrorWin32.Raise()
    END;
    RETURN numRead
  END PipeRead;

(*---------------------------RegularFile methods-----------------------------*)

PROCEDURE RegularFileRead(h: (*Regular*)File.T;
    VAR (*out*) b: ARRAY OF File.Byte;
    <* UNUSED *>mayBlock: BOOLEAN := TRUE): INTEGER RAISES {OSError.E} =
  VAR numRead: INTEGER;
  BEGIN
    IF NOT(Direction.Read IN h.ds) THEN BadDirection(); END;
    IF WinBase.ReadFile(h.handle, ADR(b[0]), NUMBER(b),
                        ADR(numRead), NIL) = 0 THEN
      OSErrorWin32.Raise()
    END;
    RETURN numRead
  END RegularFileRead;

PROCEDURE RegularFileSeek(
    h: RegularFile.T; origin: RegularFile.Origin; offset: INTEGER)
  : INTEGER RAISES {OSError.E} =
  BEGIN
    WITH res = WinBase.SetFilePointer(h.handle, offset, NIL, ORD(origin)) DO
      IF res < 0 THEN OSErrorWin32.Raise() END;
      RETURN res
    END
  END RegularFileSeek;

PROCEDURE RegularFileFlush(h: RegularFile.T) RAISES {OSError.E}=
  BEGIN
    IF WinBase.FlushFileBuffers(h.handle) = 0 THEN OSErrorWin32.Raise() END
  END RegularFileFlush;

PROCEDURE RegularFileLock(h: RegularFile.T): BOOLEAN RAISES {OSError.E}=
  BEGIN
    IF WinBase.LockFile(
         hFile := h.handle,
         dwFileOffsetLow := 0,
         dwFileOffsetHigh := 0,
         nNumberOfBytesToLockLow := LAST(WinDef.DWORD),
         nNumberOfBytesToLockHigh := LAST(WinDef.DWORD)) = 0 THEN
      IF WinBase.GetLastError() = WinError.ERROR_LOCK_VIOLATION THEN
        RETURN FALSE
      END;
      OSErrorWin32.Raise()
    END;
    RETURN TRUE
  END RegularFileLock;

PROCEDURE RegularFileUnlock(h: RegularFile.T) RAISES {OSError.E}=
  BEGIN
    IF WinBase.UnlockFile(
         hFile := h.handle,
         dwFileOffsetLow := 0,
         dwFileOffsetHigh := 0,
         nNumberOfBytesToUnlockLow := LAST(WinDef.DWORD),
         nNumberOfBytesToUnlockHigh := LAST(WinDef.DWORD)) = 0 THEN
      OSErrorWin32.Raise()
    END;
  END RegularFileUnlock;

(*------------------------------------------------ checked runtime errors ---*)

EXCEPTION IllegalDirection;

PROCEDURE BadDirection () =
  <*FATAL IllegalDirection*>
  BEGIN
    RAISE IllegalDirection;
  END BadDirection;

BEGIN
END FileWin32.

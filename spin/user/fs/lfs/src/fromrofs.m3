MODULE RofsUfs;
IMPORT FileDefs, UfsFs, Device, Disk, File, FileStat, FileSystem, Ctypes;
IMPORT DirEnt;
IMPORT IO, Text, Fmt, Word, Error, NameServer;

CONST
  BLOCK_SIZE = 512;

CONST debug = FALSE;
  

(*
 * Search a directory FP for a NAME and return its
 * i_number.
 *
 *  Return 0 if found, and FS_NO_ENTRY if not found. Raise exception
 *  if a device error happens.
 *)
PROCEDURE SearchDirectory (name: TEXT; fp: T; VAR inumber_p: ShortCard)
: INTEGER RAISES {Error.E} =
VAR
  buf : ARRAY OF DirData; (* temp used to hold directory contents. *)
  offset : INTEGER; (* offset within the directory FP *)
  bufSize, bufStart : INTEGER;
  length : INTEGER; (* length of NAME *)
  dirSize := UfsFs.size(fp.inode);
  tmpBuf : ARRAY [0..BYTESIZE(DirHeader)-1] OF CHAR;
  
  TYPE DirHeader = RECORD
    d_ino     : Ctypes.unsigned_int;   (* inode number of entry *)
    d_reclen  : Ctypes.unsigned_short; (* length of this record *)
    d_namelen : Ctypes.unsigned_short; (* length of d_name  *)
  END;
BEGIN
   IO.Put("SearchDirectory ("&name&"), dirsize="
	 &Fmt.Int(UfsFs.size(fp.inode))&"\n");
  
  length := Text.Length(name);
  offset := 0;
  
  WHILE offset < dirSize DO
    BufReadFile(fp, offset, buf, bufStart, bufSize);
    IF bufSize = 0 THEN EXIT; END;

    tmpBuf := SUBARRAY(buf^, bufStart, BYTESIZE(DirHeader));
    WITH dp = VIEW(tmpBuf, DirHeader) DO
      IF dp.d_ino # 0 THEN
	IF dp.d_namelen > UfsFs.MAXNAMLEN THEN
	  IO.Put("text length is too big " & Fmt.Int(dp.d_namelen) & "\n");
	  dp.d_namelen := UfsFs.MAXNAMLEN;
	END;
	IF dp.d_namelen = Text.Length(name) AND
	  Text.Equal(name,
		Text.FromChars(SUBARRAY(buf^, bufStart+BYTESIZE(DirHeader),
					dp.d_namelen))) THEN
	  (* found entry *)
	  inumber_p := dp.d_ino;
          IF debug THEN
            dprint(".\n");
          END;

	  RETURN 0;
	END;
      END;
      
      INC(offset, dp.d_reclen);
    END;
  END;
  IF debug THEN
    dprint("..ERROR.\n");
  END;

  RETURN File.FS_NO_ENTRY;
END SearchDirectory;


PROCEDURE GetDirEntries (fp : T; pos : INTEGER; VAR ent : ARRAY OF DirEnt.T) 
    	: INTEGER RAISES {Error.E} =
VAR
  idx := FIRST(ent);
  maxIdx := LAST(ent);
  tmpBuf : ARRAY [0..BYTESIZE(UfsFs.DirEntry)-1] OF CHAR;
  start, size : INTEGER;
  buf : REF ARRAY OF CHAR;
  dp : UfsFs.DirEntry;
BEGIN
  WHILE idx <= maxIdx DO 
    (* Skip over the deleted directory entries, and find an valid
     entry. *)
    LOOP

      (* Reached end of file *)
      IF pos >= UfsFs.size(fp.inode) THEN
	RETURN idx - FIRST(ent);
      END;
      
      BufReadFile(fp, pos, buf, start, size);
      <*ASSERT buf # NIL AND size > 0*>
      
      tmpBuf := SUBARRAY(buf^, start, BYTESIZE(UfsFs.DirEntry));
      dp := VIEW(tmpBuf, UfsFs.DirEntry);
      INC(pos, dp.d_reclen);
      IF dp.d_ino # 0 THEN
	ent[idx].nextPos := pos;
	ent[idx].ino := dp.d_ino;
	ent[idx].name
	 := Text.FromChars(SUBARRAY(buf^, start+BYTESIZE(UfsFs.DirEntry),
				    dp.d_namelen));
	INC(idx);
	EXIT;
      END;
    END;
  END;
  RETURN idx - FIRST(ent);
END GetDirEntries;

END;

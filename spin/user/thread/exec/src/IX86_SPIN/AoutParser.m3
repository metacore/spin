(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Coff aout parser for bootstrapping user level tasks.
 *
 *)
MODULE AoutParser;
IMPORT Aout, IO, Fmt, Word;
IMPORT File, Space, Translation, CPU;
IMPORT Error, VMError, VirtAddr;

PROCEDURE DirectedRead(fp : File.T; s : Space.T;
		       virt : VirtAddr.Address;
		       fileOff : INTEGER;
		       bytes : CARDINAL) =
  
  VAR buf: REF ARRAY OF CHAR;
  BEGIN
    buf := NEW(REF ARRAY OF CHAR, bytes);

    TRY 
      bytes := fp.read(buf^, fileOff);
      IF bytes # NUMBER(buf^) THEN
	IO.Put("couldn't read all the file contents.\n");
      ELSE
	Translation.Write(s, buf^, virt);
      END;
    EXCEPT
    | Error.E(e) =>
      IO.Put("File.DirectedRead : read (" & e.message() & ".\n");
      
    | VMError.E(ec) =>
      IO.Put("File.DirectedRead : translation failure(" & Fmt.Int(ec) & ".\n");
    END;
    buf := NIL;
  END DirectedRead;
  
PROCEDURE IsExecutable (fp: File.T) : BOOLEAN RAISES {Error.E} =
  VAR
    bytes: CARDINAL;
    data : ARRAY [0 .. BYTESIZE(Aout.AoutHeader)-1] OF CHAR;
  BEGIN
    bytes := fp.read(data, 0);
    IF bytes # BYTESIZE(Aout.AoutHeader) THEN
      RETURN FALSE;
    END;
    WITH hdr = VIEW(data, Aout.AoutHeader) DO 
      (* the following can be made into a safe VIEW *)
      IF Aout.N_BADMAG(hdr) THEN 
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsExecutable;
  
PROCEDURE GetExecutable (s: Space.T; fp: File.T;
			 VAR info : Info) RAISES {Error.E} =
VAR
  bytes: CARDINAL;
  data : ARRAY [0 .. BYTESIZE(Aout.AoutHeader)-1] OF CHAR;
  textStart: Word.T;
  textOffset: Word.T;
  dataStart: Word.T;
  dataOffset: Word.T;
  bssStart: Word.T;
  bssOffset: Word.T;
BEGIN
  TRY
    bytes := fp.read(data, 0);
    IF bytes # BYTESIZE(Aout.AoutHeader) THEN
      RAISE Error.E(NEW(Error.T).init(8));
    END;
    
    WITH hdr = VIEW(data, Aout.AoutHeader) DO 
      (* the following can be made into a safe VIEW *)
      IF Aout.N_BADMAG(hdr) THEN 
	RAISE Error.E(NEW(Error.T).init(8));
      END;
      
      (* Text *)
      bytes := hdr.a_text;
      textStart := Aout.N_TXTADDR(hdr);
      textOffset := Aout.N_TXTOFF(hdr);
      Space.Allocate(s, textStart, bytes); (* can this fail? *)
      
      (* Virtual addr *)(* offset *)(* nbytes *)
      DirectedRead(fp, s, textStart, textOffset, bytes);
      Space.Protect(s, textStart, hdr.a_text, Space.ExecuteProtection);
      (* Data *)
      bytes := hdr.a_data;
      IF bytes > 0 THEN
	dataStart := textStart + hdr.a_text;
	dataOffset := textOffset + hdr.a_text;
	Space.Allocate(s, dataStart, hdr.a_data);
	DirectedRead(fp, s, dataStart, dataOffset, bytes);
	Space.Protect(s, dataStart, hdr.a_data, Space.ReadWriteProtection);
      END;

      (* BSS *)
      IF hdr.a_bss > 0 THEN
	bssStart := dataStart + hdr.a_data;
	bssOffset := dataOffset + hdr.a_data;
	Space.Allocate(s, bssStart, hdr.a_bss);
	Space.Zero(s, bssStart, hdr.a_bss);
	Space.Protect(s, bssStart, hdr.a_bss, Space.ReadWriteProtection);
      END;
      
      info.break := bssStart + hdr.a_bss;
      CPU.FlushInstructionCache();
      info.pc := hdr.a_entry;
    END;
      
  EXCEPT
  | VMError.E(ec) =>
    IO.Put("exec : vm access fault.\n");
    RAISE Error.E(NEW(Error.T).init(ec));
  ELSE
    IO.Put("Get Executable: unknown exception\n");
    RAISE Error.E(NEW(Error.T).init(999));
  END;
END GetExecutable;

BEGIN
END AoutParser.





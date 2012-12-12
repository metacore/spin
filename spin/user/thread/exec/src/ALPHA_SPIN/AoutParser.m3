(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed DiretedRead so that it won't allocate huge array.
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added dynlink support.
 * 10-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Coff aout parser for bootstrapping user level tasks.
 *
 *)
MODULE AoutParser;
IMPORT IO, Fmt;
IMPORT Coff, File, Space, Translation, CPU;
IMPORT Error, VMError, VirtAddr;

(* heck tftp can't handle this code :( *)
<*UNUSED*>
PROCEDURE UnusedDirectedRead(fp : File.T; s : Space.T;
		       virt : VirtAddr.Address;
		       fileOff : INTEGER;
		       bytes : CARDINAL) =
  
  VAR
    buf: REF ARRAY OF CHAR;
    n, bytesRead: CARDINAL;
    remaining := bytes;
  BEGIN
    buf := NEW(REF ARRAY OF CHAR, 8192*4);

    TRY
      WHILE remaining > 0 DO
	bytesRead := MIN(NUMBER(buf^), remaining);
	n := fp.read(SUBARRAY(buf^, 0, bytesRead), fileOff);
	IF n # bytesRead THEN
	  IO.Put("aoutparser:directed read error.\n");
	ELSE
	  Translation.Write(s, SUBARRAY(buf^, 0, bytesRead), virt);
	END;
	INC(virt, bytesRead);
	DEC(remaining, bytesRead);
      END;
    EXCEPT
    | Error.E(e) =>
      IO.Put("File.DirectedRead : read (" & e.message() & ").\n");
      
    | VMError.E(ec) =>
      IO.Put("File.DirectedRead : translation failure("& Fmt.Int(ec) &").\n");
    END;
  END UnusedDirectedRead;


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

PROCEDURE GetInfo (fp: File.T; VAR info: Info) RAISES {Error.E} =
  VAR
    bytes: CARDINAL;
    data : ARRAY [0 .. BYTESIZE(Coff.Exechdr)-1] OF CHAR;
  BEGIN
    bytes := fp.read(data, 0);
    IF bytes # BYTESIZE(Coff.Exechdr) THEN
      IO.Put("Exec.LoadFile: the file is short.\n");
      RAISE Error.E(NEW(Error.T).init(8)); (* XXX *)
    END;
    WITH hdr = VIEW(data, Coff.Exechdr) DO 
      (* the following can be made into a safe VIEW *)
      IF Coff.N_BADMAG(hdr.a) THEN 
	IO.Put("Exec.LoadFile: the file has bad magic.\n");
	RAISE Error.E(NEW(Error.T).init(8)); (* XXX *)
      END;
      info.dynamic := Coff.Dynamic(hdr.f);
      info.break := hdr.a.bss_start + hdr.a.bsize;
      info.pc := hdr.a.entry;
      info.gp := hdr.a.gp_value;
    END;
  END GetInfo;
  
PROCEDURE GetExecutable (s: Space.T; fp: File.T) RAISES {Error.E} =
VAR
  bytes: CARDINAL;
  data : ARRAY [0 .. BYTESIZE(Coff.Exechdr)-1] OF CHAR;
BEGIN
  TRY
    bytes := fp.read(data, 0);
    IF bytes # BYTESIZE(Coff.Exechdr) THEN
      RAISE Error.E(NEW(Error.T).init(8));
    END;
    
    WITH hdr = VIEW(data, Coff.Exechdr) DO 
      (* the following can be made into a safe VIEW *)
      IF Coff.N_BADMAG(hdr.a) THEN 
	RAISE Error.E(NEW(Error.T).init(8));
      END;

      (* Text *)
      bytes := hdr.a.tsize;
      Space.Allocate(s, hdr.a.text_start, bytes); (* can this fail? *)
      
      (* Virtual addr *)(* offset *)(* nbytes *)
      DirectedRead(fp, s, hdr.a.text_start, Coff.N_TXTOFF(hdr.f,hdr.a), bytes);
      
      (* Data *)
      bytes := hdr.a.dsize;
      IF bytes > 0 THEN
	Space.Allocate(s, hdr.a.data_start, bytes);
	DirectedRead(fp, s, hdr.a.data_start,
		     Coff.N_TXTOFF(hdr.f, hdr.a) + hdr.a.tsize, bytes);
      END;

      (* BSS *)
      IF hdr.a.bsize > 0 THEN
	Space.Allocate(s, hdr.a.bss_start, hdr.a.bsize);
	Space.Zero(s, hdr.a.bss_start, hdr.a.bsize);
      END;

      
      CPU.FlushInstructionCache();
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





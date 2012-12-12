(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE PagingBasic;
IMPORT IO;
IMPORT Fmt;
IMPORT Error;
IMPORT VMError;
IMPORT Space;
IMPORT VirtAddr;
IMPORT PhysAddr;
IMPORT PhysFreeMem;
IMPORT MachineMem;
IMPORT DefaultPager;
IMPORT FileSystem;
IMPORT EFS;

<*NOWARN*>
IMPORT RealDefaultPager; (* not used, but it has to be loaded. *)

CONST
  NumPages = 100;
  Base = 16_100000000;

VAR buf: ARRAY [0 .. MachineMem.PAGESIZE-1] OF CHAR;
  
PROCEDURE PagingBasic(): BOOLEAN =
  VAR
    orgParams, newParams : PhysFreeMem.Params;
    stat: PhysFreeMem.Stat;
    efs: EFS.T;
    space: Space.T;
    addr: VirtAddr.Address;
    baseChar : [0 .. 255];
  BEGIN
    (* Shrink the vm parameters so that paging happens early. *)
    PhysFreeMem.GetParams(orgParams);
    PhysFreeMem.GetStat(stat);
    newParams.requestThreshold := stat.sizes[PhysAddr.State.Free] - 10;
    newParams.reclaimThreshold := stat.sizes[PhysAddr.State.Free] - 12;
    newParams.repossesThreshold := stat.sizes[PhysAddr.State.Free] - 14;
    PhysFreeMem.SetParams(orgParams);

    (* Mount the swap device *)
    TRY
      FileSystem.Mount("rz3b", "efs", "/efs");
    EXCEPT
    ELSE
      IO.Put("mount rz3b efs /efs failed, but ignoring.\n");
    END;

    (* Create /efs/swap *)
    TRY
      efs := FileSystem.Open(0, "/efs");
      EFS.Nuke(efs);
      EVAL EFS.Create(efs, "swap", 8000); (* 4M bytes of swap space *)
    EXCEPT
    | Error.E(e) =>
      IO.Put("/efs/swap: " & e.message() & ".\n");
    END;

    DefaultPager.SwapOn("/efs/swap");

    space := Space.Create();

    (* Dump shits on pages. *)
    addr := Base;
    baseChar := 0;

    TRY
      EVAL Space.Allocate(space, addr, MachineMem.PAGESIZE * NumPages);
      FOR i := 1 TO NumPages DO
	FOR j := 0 TO LAST(buf) DO
	  buf[j] := VAL((baseChar+j) MOD 16_100, CHAR);
	END;
	Space.Write(space, buf, addr, MachineMem.PAGESIZE);
	INC(addr, MachineMem.PAGESIZE);
	baseChar := (baseChar+1) MOD 16_100;
      END;

      (* Check the shits are same as before *)
      addr := Base;
      baseChar := 0;
      
      FOR i := 1 TO NumPages DO
	Space.Read(space, addr, buf, MachineMem.PAGESIZE);
	FOR j := 0 TO LAST(buf) DO
	  IF buf[j] # VAL((baseChar+j) MOD 16_100, CHAR) THEN
	    IO.Put("content mismatch@" & Fmt.Int(addr + j, 16) & ".\n");
	    Space.Destroy(space);
	    RETURN FALSE;
	  END;
	END;
	
	INC(addr, MachineMem.PAGESIZE);
	baseChar := (baseChar+1) MOD 16_100;
      END;
    EXCEPT
    | VMError.E(ec) =>
      IO.Put("vm error " & Fmt.Int(ec) & "@" & Fmt.Int(addr, 16) & ".\n");
      Space.Destroy(space);
      RETURN FALSE;
    END;
    
    Space.Destroy(space);
    PhysFreeMem.SetParams(orgParams);
    RETURN TRUE;
  END PagingBasic;
  
PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END PagingBasic.

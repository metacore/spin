(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE DefaultPager;
IMPORT PagerObject;
IMPORT VMStat;
IMPORT PhysAddr;
IMPORT Protection;
IMPORT Wr;

PROCEDURE Create(<*UNUSED*>size : CARDINAL) : PagerObject.T =
  BEGIN
    RETURN NEW(PagerObject.T,
	       pageIn := PageIn);
  END Create;
  
VAR zeroBuf: PhysAddr.Content;
    
PROCEDURE PageIn (<*UNUSED*>self : PagerObject.T;
		  <*UNUSED*>off : PagerObject.PageNumber;
		  <*UNUSED*>type: INTEGER;
		  frame: PhysAddr.T;
		  VAR prot: Protection.T) : PagerObject.ResultCode =
  PROCEDURE Callback(VAR buf: PhysAddr.Content) =
    BEGIN
      buf := zeroBuf;
    END Callback;
  BEGIN
    prot := Protection.All;
    PhysAddr.Access(frame, Callback);
    IF VMStat.Enabled THEN
      INC(VMStat.zero);
    END;
    RETURN PagerObject.ResultCode.Success;
  END PageIn;

PROCEDURE SwapOn(<*UNUSED*>dev : REFANY) =
  BEGIN
  END SwapOn;

PROCEDURE Stat(<*UNUSED*>wr: Wr.T) =
  BEGIN
  END Stat;
  
BEGIN
END DefaultPager.

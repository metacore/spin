(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE Types;
IMPORT Ctypes;
TYPE
  Pid = Ctypes.unsigned_int;
  UWord8 = BITS 8 FOR [0 .. 16_FF];
  SWord8 = BITS 8 FOR [-16_80 .. 16_7F];
    
  UWord16 = BITS 16 FOR [0 .. 16_FFFF];
  SWord16 = BITS 16 FOR [-16_8000 .. 16_7FFF];
    
  UWord32 = BITS 32 FOR [0 .. 16_FFFFFFFF];
  SWord32 = BITS 32 FOR [-16_80000000 .. 16_7FFFFFFF];
    
  SWord64 = INTEGER;
  UWord64 = INTEGER;
  
END Types.

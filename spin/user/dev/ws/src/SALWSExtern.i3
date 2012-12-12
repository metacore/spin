(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *  2-Apr-96 oystr at the University of Washington
 *	Created
 *
 *)

UNSAFE INTERFACE SALWSExtern;

IMPORT Ctypes, PhysAddr, VirtAddr;

<* EXTERNAL *> PROCEDURE wsopen(dev, flag: INTEGER ) : INTEGER ;
<* EXTERNAL *> PROCEDURE wsclose(dev, flag: INTEGER ): INTEGER ;
<* EXTERNAL *> PROCEDURE wsioctl( dev: INTEGER;
				 iocmd: Ctypes.unsigned_int;
				 VAR iocdata: ARRAY OF CHAR;
				 flag : INTEGER;
		map_func : PROCEDURE(phys: PhysAddr.Address;
				     virt: VirtAddr.Address;
				     size: VirtAddr.Size) : INTEGER) : INTEGER ;
<* EXTERNAL *> PROCEDURE wsselect( dev: INTEGER;
				   VAR events:  Ctypes.short_int;
				   VAR revents: Ctypes.short_int;
				   scanning : INTEGER) : INTEGER;
END SALWSExtern.



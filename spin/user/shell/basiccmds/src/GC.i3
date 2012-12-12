(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Removed "blink" and "warnLow".
 *
 * 12-Dec-96  Richard Robinson (robinson) at the University of Washington
 *	"gc stat" can be -full, meaning traverse the heap counting objects.
 *	Added "unpin".
 *
 * 16-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the help line.
 *
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added enable and disable
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added ShellText for httpd.  
 *
 *)

INTERFACE GC;

IMPORT ParseParams;

CONST CommandName = "gc";
CONST CommandHelp = " [-html] collect|check|collection|motion|background|incremental|generational|vm|ratio|untraced|sanity|verbose|stat|pages|strongrefs|heap|reach|where|pc|test|inc|kill|unpin|frag|histo|dist";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

PROCEDURE SanityCheck();

PROCEDURE Stat(traverseHeap := FALSE);

PROCEDURE Enable();

PROCEDURE Disable();

END GC.

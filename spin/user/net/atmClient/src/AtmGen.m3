(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Replaced obsolete interface Clib with IO.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 * 30-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Took out the atm_output_upcall variable.  
 *	Needs to go back after SOSP.
 *
 *)

MODULE AtmGen EXPORTS AtmGen, AtmGenPrivate;

IMPORT Net;
IMPORT IO;
IMPORT Mbuf;
IMPORT If;
IMPORT SocketRep;
IMPORT Ctypes;
IMPORT Fore;

(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT SpinShell;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "AtmGen");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

   IF Text.Equal(argv[1], "debug") THEN
     debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
   ELSIF Text.Equal(argv[1], "sendtoggle") THEN
     sendtoggle := NOT sendtoggle;
   ELSE
     IO.Put("AtmGen.RunCommand: no such command ");
     FOR i := 0 TO argc DO
       IO.Put(argv[i]);
     END;
     IO.Put("\n");
   END;
   RETURN TRUE;
  END RunCommand;

PROCEDURE Help () =
  BEGIN
  END Help;


VAR
  debug_level: Net.Level := Net.oLevel.NODEBUG;
  sendtoggle : BOOLEAN := TRUE;

PROCEDURE PacketSend(
    VAR ifp: If.ifnet;
    mbuf: Mbuf.T;
    VAR s: SocketRep.M3sockaddr;
    <* UNUSED *> rt: ADDRESS := NIL): Ctypes.int = 
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"AtmGen.Output ");
    END;
    RETURN Fore.Output(ifp,mbuf,s);
  END PacketSend; 

BEGIN
  (* atm_output_upcall := PacketSend; *)
END AtmGen. 



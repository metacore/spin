(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 *)

UNSAFE (* to import unsafe interface *)
MODULE T3Gen EXPORTS T3Gen, T3GenPrivate;

IMPORT Net;
IMPORT IO;
IMPORT Mbuf;
IMPORT If;
IMPORT SocketRep;
IMPORT TT;
IMPORT Spy;
IMPORT Ctypes;

(* DynShell support *)
IMPORT Text,SafeConvert;
IMPORT SpinShell;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand,Help);

PROCEDURE EC_Guard(<* UNUSED *> argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN Text.Equal(argv[0], "T3Gen");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

   IF Text.Equal(argv[1], "debug") THEN
     debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
   ELSIF Text.Equal(argv[1], "sendtoggle") THEN
     sendtoggle := NOT sendtoggle;
   ELSE
     IO.Put("T3Gen.RunCommand: no such command ");
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
  send_timer : Spy.T;
  debug_level: Net.Level := Net.oLevel.NODEBUG;
  sendtoggle : BOOLEAN := TRUE;

PROCEDURE PacketSend(
    VAR ifp : If.ifnet;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.M3sockaddr;
    <* UNUSED *> rte     : ADDRESS := NIL): Ctypes.int = 
  VAR error: Ctypes.int;
  BEGIN
    (* #ifdef debug_level != NODEBUG *)
    IF debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,"T3Gen.Output ");
    END;

    Spy.Enter(send_timer);
    error := TT.Output(ifp,mbuf,s);
    Spy.Exit(send_timer);
    RETURN error;
  END PacketSend; 

BEGIN
  send_timer := Spy.Create("t3_output");
  (* reroute output packets to this function *)
  t3_output_upcall := PacketSend;
END T3Gen. 



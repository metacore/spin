(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * BindingInfo
 *
 *
 * HISTORY
 * 27-Oct-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed to hold all possible access modes.
 *
 * 20-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created
 *
 *)

INTERFACE BindingInfo;

IMPORT AccessMode, SecurityManager, SecurityManagerPrivate, Dispatcher;

TYPE
  T = REF RECORD
    sid         : SecurityManager.SID;
    kind        : SecurityManagerPrivate.AccessControlT;
    executeMode : AccessMode.T;
    inModes     : AccessMode.ListT;
    resultMode  : AccessMode.T;
    exceptMode  : AccessMode.T;
    derefArgs   : REF ARRAY OF BOOLEAN;
    guard       : Dispatcher.ImposedGuard;
  END;
  (*
     This record is used by the security manager to store per-binding
     information on access control checks.

     "sid"         : The security identifier of the binding.
     "kind"        : What access control to perform.
     "executeMode" : Required access modes for code when entering.
     "inModes"     : Required access modes for arguments when entering.
     "resultMode"  : Required access mode for result.
     "exceptMode"  : Required access mode for exception argument.
     "derefArgs"   : TRUE iff argument is a VAR or READONLY argument.
     "guard"       : Dispatcher guard for access control.
   *)

CONST Brand = "BindingInfo";

END BindingInfo.

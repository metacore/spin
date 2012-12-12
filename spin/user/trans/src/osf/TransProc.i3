(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE TransProc;
IMPORT TransGroup;
IMPORT Storage;
TYPE T = TransGroup.T;
PROCEDURE CleanupTrans (t: T; st: Storage.T);
END TransProc.

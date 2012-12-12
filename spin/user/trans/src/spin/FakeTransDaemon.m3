(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE FakeTransDaemon EXPORTS TransServer;
PROCEDURE Loop () =
  BEGIN
    <*ASSERT FALSE*>
  END Loop;
BEGIN
END FakeTransDaemon.

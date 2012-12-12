(*
 * HISTORY
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *
 *)


MODULE ExprCmds;

IMPORT SpinException, IO;


PROCEDURE Error (ei: SpinException.ExceptionInfo) =
  BEGIN
    IF ei.code = SpinException.ExceptionCode.NoHandlerInvoked THEN
      IO.Put("command not found\n");
    ELSE
      IO.Put("Thread fault -- " & ei.msg & "\n");
    END;
  END Error;

BEGIN
END ExprCmds.

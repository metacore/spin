(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

UNSAFE MODULE MpegShell;

IMPORT ParseParams;
IMPORT MpegShellCmd;
IMPORT MpegPlay, BitIO, MpegData, IO;

(* Extra import for accessing the framebuffer. *)
IMPORT Draw;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
VAR
  file:      BitIO.T;                        (* The MPEG movie file          *)
  state:     MpegData.MpegState;             (* Holds all of the MPEG state  *)
  pathname : TEXT;
  success:   INTEGER;
BEGIN
  pp.reset();
  TRY
    pp.skipNext(); (* mpeg *)
    IF pp.testNext("-zap") THEN
      MpegShellCmd.Uninstall();
    ELSIF pp.testNext("-play") THEN
      pathname := pp.getNext();
      TRY
        state.mode := 0;
        file := BitIO.OpenRead(pathname);

        (* Decode a frame and use the Draw framebuffer interface to 
           access the framebuffer. *)
        IF state.mode # 3 THEN
          MpegPlay.MpegPlay(file, state);
        END;

      EXCEPT
      |  BitIO.EOF => IO.Put("Problem with the given file.\n");
      END;
    ELSE
      Usage();
    END;
  EXCEPT
  |  ParseParams.Error => Usage();
  END;
    
  RETURN TRUE;
END Run;

PROCEDURE Usage() = 
BEGIN
  (* describes how to use the shell command(s) that you implement. *)
  IO.Put("Help:\n");
  IO.Put("     mpeg -play <filename>\n");
END Usage;


BEGIN
END MpegShell.





(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
	InfoFile.i3

	An Info File makes procfs style files so extensions can supply
	data to user spaces via the filesystem interface.

	Create places the new file in the filesystem and associates it
	with a FillFileT routine.  Whenever the new file is opened, the
	FillFileT routine is called so the creator can write the contents.

	A sample client
		MODULE InfoTest;
		IMPORT InfoFile, Wr;

		PROCEDURE WriteFile (wr: Wr.T) =
		  BEGIN
		    Wr.PutText(wr,"writer line of text\n");
		  END WriteFile;

		BEGIN
		  InfoFile.Create("/proc/writer",WriteFile);
		END InfoTest.
 *)

INTERFACE InfoFile;
IMPORT Wr, Error;

TYPE FillFileT = PROCEDURE(wr:Wr.T);

PROCEDURE Create(path:TEXT; fillFile: FillFileT) RAISES {Error.E};

END InfoFile.

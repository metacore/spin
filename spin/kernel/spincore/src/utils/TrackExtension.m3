(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * ??-Nov-96  David Becker (becker) at the University of Washington
 *      Keep track of resource usage per extension.
 *)

MODULE TrackExtension;
IMPORT Wr, Domain, DomainRep;
IMPORT Track;
IMPORT NameServer;

REVEAL T = Track.T BRANDED OBJECT
		domain: Domain.T;
		table:Track.ResourceTable;
		OVERRIDES
		print:=Print;
		END;

TYPE BootT = T OBJECT OVERRIDES
		print:=PrintBoot;
		END;

VAR
  extensionTable: Track.ResourceTable;

PROCEDURE Print(self:T; wr:Wr.T) =
  BEGIN
    Wr.PutText(wr,"Track extension "&self.domain.name&"\n");
  END Print;

PROCEDURE PrintBoot(self:BootT; wr:Wr.T) =
  BEGIN
    Wr.PutText(wr,"Track boot time\n");
  END PrintBoot;


PROCEDURE Table(extension: T) : Track.ResourceTable =
  BEGIN
    IF extension # NIL THEN 
      RETURN extension.table;
    ELSE
      RETURN NIL;
    END;
  END Table;

PROCEDURE Create(domain: Domain.T) : T =
  VAR
    ext := NEW(T);
  BEGIN
    ext.table := extensionTable.mkdir(domain.name);
    NameServer.Attach(ext.table, "extension_info", ext);
    ext.domain := domain;
    RETURN ext;
  END Create;

PROCEDURE Init() =
  VAR
    boot := NEW(BootT);
  BEGIN
    extensionTable := Track.GlobalTrack().mkdir("extension");
    boot.table := extensionTable.mkdir("BootTime");
    NameServer.Attach(boot.table, "extension_info", boot);
  END Init;

BEGIN
END TrackExtension.

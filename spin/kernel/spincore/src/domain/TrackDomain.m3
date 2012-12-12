(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed representation of Nameserver.Name
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * ??-Nov-96  David Becker (becker) at the University of Washington
 *      Keep track of domain usage per extension.
 *)

MODULE TrackDomain;
IMPORT Wr;
IMPORT NameServer, Domain, DomainRep, DomainPrivate, IO, Fmt;
IMPORT Track, TrackExtension, TrackStrand;

REVEAL T = Track.T BRANDED OBJECT
	  domain : Domain.T;
	  malloc: INTEGER;
	  free: INTEGER;
	  extension: Track.T;
	OVERRIDES
		print:=Print;
	END;

CONST
	OrphanDir = "unassigned_domains";

PROCEDURE Print(self:T; wr:Wr.T) =
  BEGIN
    TRY
      Wr.PutText(wr,"Track domain "&self.domain.name&"\n");
      Wr.PutText(wr,"domain malloc "&Fmt.Int(self.malloc)&"\n");
      Wr.PutText(wr,"domain free   "&Fmt.Int(self.free)&"\n");
    EXCEPT
    ELSE
    END;
  END Print;

PROCEDURE Create(d: Domain.T) =
  VAR
    track :=  NEW(T);
    entry: Track.ResourceTable := NIL;
    name := "domain_" & Fmt.Int(d.id);
    root: Track.ResourceTable;
  BEGIN
    TRY
      track.domain := d;
      track.extension := TrackStrand.Extension();
      root := TrackExtension.Table(track.extension);
      entry := NameServer.LookupSoft(root, OrphanDir);
      IF entry = NIL THEN
	entry := root.mkdir(OrphanDir);
      END;
      d.track := track;
      NameServer.Attach(entry, name, track);
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackDomain.Create: nameserver fault\n");
    END;
  END Create;


TYPE DomainClosure = Domain.Closure OBJECT
        oldTable : Track.ResourceTable;
        newTable : Track.ResourceTable;
	ext      : TrackExtension.T;
	OVERRIDES
	  apply := Apply
	END;

PROCEDURE Apply(self: DomainClosure; domain: Domain.T):BOOLEAN =
  VAR
    track := domain.track;
    name  := "domain_"&Fmt.Int(domain.id);
  BEGIN
    TRY
      NameServer.Detach(self.oldTable, name);
      track.extension := self.ext;
      NameServer.Attach(self.newTable, name, track);
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackDomain.SetExtension: nameserver fault\n");
    END;
    RETURN TRUE; (* continue applying *)
  END Apply;

PROCEDURE SetExtension(ext:TrackExtension.T; domain: Domain.T) =
  VAR
    domainTable := TrackExtension.Table(ext).mkdir("domain");
    oldTable : Track.ResourceTable;
  BEGIN
    TRY
      oldTable:= NameServer.Lookup(TrackExtension.Table(domain.track.extension),
				   OrphanDir);
      EVAL DomainPrivate.Apply(domain,NEW(DomainClosure,
                                          oldTable:=oldTable,
                                          newTable:=domainTable,
                                          ext:=ext));
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackDomain.SetExtension: nameserver fault\n");
    END;
  END SetExtension;

PROCEDURE SetNewborn(d: Domain.T) =
  BEGIN
    IF CurrentNewborn() # NIL THEN 
      IO.PutError("TrackDomain: already have domain_in_progress\n");
      RETURN;
    END;
    TRY
      NameServer.Attach(TrackStrand.Table(), "domain_in_progress", d.track);
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackDomain.SetNewborn: nameserver fault\n");
    END;
  END SetNewborn;

PROCEDURE UnsetNewborn(d: Domain.T) =
  VAR
    obj := CurrentNewborn();
  BEGIN
    IF obj # d.track THEN 
      IO.PutError("TrackDomain: wrong domain_in_progress\n");
      RETURN;
    END;
    TRY
      NameServer.Detach(TrackStrand.Table(), "domain_in_progress");
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackDomain.UnsetNewborn: nameserver fault\n");
    END;
  END UnsetNewborn;

PROCEDURE CurrentNewborn(): T =
  BEGIN
    TRY
      RETURN NameServer.Lookup(TrackStrand.Table(), "domain_in_progress");
    EXCEPT
    | NameServer.Error(ec) =>
      IF ec # NameServer.EC.NameNotFound THEN
	IO.PutError("TrackDomain.CurrentNewBorn: something wrong.\n");
      END;
      RETURN NIL;
    END;
  END CurrentNewborn;

PROCEDURE Malloc(bytes:INTEGER) =
  VAR
    track := CurrentNewborn();
  BEGIN
    IF track = NIL THEN 
      (* ARGH !!! *)
      (*
      IO.PutError("TrackDomain: malloc in untracked domain\n");
      *)
      RETURN;
    END;
    INC(track.malloc, bytes);
  END Malloc;

PROCEDURE Free(bytes:INTEGER) =
  VAR
    track := CurrentNewborn();
  BEGIN
    IF track = NIL THEN 
      IO.PutError("TrackDomain: free in untracked domain\n");
      RETURN;
    END;
    INC(track.free, bytes);
  END Free;

BEGIN
END TrackDomain.

(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel and CPU interfaces
 *
 * 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Clean up warnings.
 *
 * 24-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional with David.
 *
 * ??-Nov-96  David Becker (becker) at the University of Washington
 *      Keep track of resource usage per extension.
 *)

MODULE TrackStrand;
IMPORT Wr, IO, Fmt, NameServer, Strand, StrandRep;
IMPORT Track, TrackExtension, TrackMalloc;
IMPORT CPU, Word;
IMPORT DebugOption;

REVEAL T = Track.T BRANDED OBJECT
	  name: TEXT;
	  table: Track.ResourceTable;
	  malloc: TrackMalloc.T;
	  extension : TrackExtension.T;
	  strand: Strand.T;
	  cpuTime: Word.T; (* cumulative cpu time in cycles *)
	  switches: Word.T;(* Strand.Run event count *)
	OVERRIDES
	  print:=Print;
	END;

VAR
  strandTable: Track.ResourceTable;
  bootStrand: T;

PROCEDURE Print(self:T; wr:Wr.T) =
  BEGIN
    TRY
      Wr.PutText(wr,"Track Strand "&self.name&"\n");
      Wr.PutText(wr,"CPU time: "&Fmt.Int(CPU.CycleToMicrosec(self.cpuTime))&" microsecs\n");
      Wr.PutText(wr,"Run events: "&Fmt.Int(self.switches)&" \n");
    EXCEPT
    ELSE
    END;
  END Print;


PROCEDURE Create(s: Strand.T) =
  VAR
    track    : T;
    table    : Track.ResourceTable;
    root    :  Track.ResourceTable;
  BEGIN
    IF DebugOption.DoTrackStrand = FALSE THEN
      s.track := untracked;
      RETURN;
    END;
    track := NEW(T);
    TRY
      s.track := track;
      track.strand:=s;
      track.name := "kernel_"&Fmt.Int(s.tid);
      track.extension := Extension();

      root := TrackExtension.Table(track.extension);
      table := NameServer.Lookup(root, "strand");
      IF table = NIL THEN
        table := root.mkdir("strand");
      END;
  
      (* This should never happen *)
      IF NameServer.Lookup(table, track.name) # NIL THEN
        IO.Put("TrackStrand.Create:  "&track.name&" already exists\n");
      END;
  
      track.table := table.mkdir(track.name);
      (* IO.Put("attach "&track.name&"\n"); *)
      NameServer.Attach(track.table, "strand_info",track);
  
      (* could use lookup to get malloc obj, this is a shortcut *)
      track.malloc := NEW(TrackMalloc.T);
      NameServer.Attach(track.table, "malloc",track.malloc);
      NameServer.Attach(strandTable, track.name, track.table);
    EXCEPT
    | NameServer.Error =>
      IO.Put("delete nameserver fault\n");
    END;
  END Create;

PROCEDURE Delete(track:T) =
  VAR
    table    : Track.ResourceTable;
  BEGIN
    TRY
      table := NameServer.Lookup(TrackExtension.Table(track.extension), 
				 "strand");

      NameServer.Detach(table, track.name);
      NameServer.Detach(strandTable, track.name);
      (* make obj collectable *)
      track.table := NIL;
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackServer.Delete: nameserver fault for "&track.name &"\n");
    END;
  END Delete;

PROCEDURE SetName(track:T; label: TEXT) =
  VAR
    table    : Track.ResourceTable;
  BEGIN
    IF DebugOption.DoTrackStrand = FALSE THEN RETURN END;

    TRY
      table := NameServer.Lookup(TrackExtension.Table(track.extension),
				 "strand");
      NameServer.Detach(table, track.name);
      NameServer.Detach(strandTable, track.name);
      (* IO.Put("rename "&label&" from "&track.name&"\n"); *)
      track.name := label;
      NameServer.Attach(table, track.name, track.table);
      NameServer.Attach(strandTable, track.name, track.table);
    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackStrand.SetName: nameserver fault\n");
    END;
  END SetName;

PROCEDURE SetExtension(extension: TrackExtension.T) =
  BEGIN
    Strand.GetCurrent().track.extension := extension;
  END SetExtension;

PROCEDURE Extension() : TrackExtension.T =
  BEGIN
    IF Strand.GetCurrent()=NIL THEN
      IO.Put("TrackStrand.Extension: cur nil\n");
      RETURN NIL
    END;
    IF Strand.GetCurrent().track=NIL THEN
      IO.Put("TrackStrand.Extension: track nil\n");
      RETURN NIL
    END;
    IF Strand.GetCurrent().track.extension=NIL THEN
      IO.Put("TrackStrand.Extension: ext nil\n");
      RETURN NIL
    END;
    RETURN Strand.GetCurrent().track.extension;
  END Extension;

PROCEDURE Malloc() : TrackMalloc.T =
  BEGIN
    RETURN Strand.GetCurrent().track.malloc;
  END Malloc;

PROCEDURE Table() : Track.ResourceTable =
  BEGIN
    RETURN Strand.GetCurrent().track.table;
  END Table;

(* ContextSwitch is installed as a Strand.Run handler so it is
	non-preemtible and these globals are synchronized, right? *)
VAR
  curTrack:T; (* strand of most recent Strand.Run event *)
  startStamp:Word.T; (* timestamp for when curTrack started *)

(* ContextSwitch is installed in Thread.m3 for correct ordering *)
PROCEDURE ContextSwitch(strand:Strand.T) =
  VAR
    now := CPU.CycleCounter();
  BEGIN
    INC(curTrack.cpuTime, CPU.CycleMinus(startStamp, now));
    INC(curTrack.switches);

    curTrack := strand.track;
    startStamp:=now;
  END ContextSwitch;

PROCEDURE Init() =
  VAR strandDir: Track.ResourceTable;
  BEGIN
    TRY
      strandTable := Track.GlobalTrack().mkdir("strand");
      (* setup bootstrand tracker *)
      bootStrand := NEW(T);
      bootStrand.name := "BootStrand";
      bootStrand.extension :=
        NameServer.Lookup(Track.GlobalTrack(),
			  "extension/BootTime/extension_info");
      
      strandDir := NameServer.Lookup(Track.GlobalTrack(), "extension/BootTime");
      bootStrand.malloc := NEW(TrackMalloc.T);
      bootStrand.table := strandDir.mkdir(bootStrand.name);
      NameServer.Attach(bootStrand.table, "strand_info", bootStrand);
      NameServer.Attach(bootStrand.table, "malloc", bootStrand.malloc);
      NameServer.Attach(strandTable, bootStrand.name, bootStrand.table);

      (* setup untracked tracker *)
      untracked := NEW(T);
      untracked.name := "Untracked";
      untracked.extension := NameServer.Lookup(Track.GlobalTrack(), 
			      "extension/BootTime/extension_info");
      untracked.malloc := NEW(TrackMalloc.T);
      untracked.table := strandDir.mkdir(untracked.name);
      NameServer.Attach(untracked.table, "strand_info", untracked);
      NameServer.Attach(untracked.table, "malloc", untracked.malloc);
      NameServer.Attach(strandTable, untracked.name, untracked.table);

      (* get boot strand started *)
      Strand.GetCurrent().track := bootStrand;
      curTrack := bootStrand;
      startStamp := CPU.CycleCounter();

    EXCEPT
    | NameServer.Error =>
      IO.Put("TrackStrand.Init: nameserver fault\n");
    END;
  END Init;

BEGIN
END TrackStrand.

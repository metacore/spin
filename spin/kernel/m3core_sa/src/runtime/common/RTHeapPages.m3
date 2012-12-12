(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

UNSAFE MODULE RTHeapPages;

IMPORT RTIO, RTHeapRep, RTOS;

VAR
  simplePutter : RTIO.SimplePutter := NIL;

(*
 * status of all pages
 * 	scan the the descriptors of all pages (RTHeapRep.desc)
 *	and count the number of pages falling into all categories,
 *	the counts should be consistent with the running counts
 *	kept in RTHeapRep
 *)

PROCEDURE DumpPageStatus (putter: RTIO.SimplePutter; full: BOOLEAN) =
  VAR lastSpace: RTHeapRep.Space;
      lastNote: RTHeapRep.Note;
      lastGeneration: RTHeapRep.Generation;
      lastProtected: BOOLEAN;
      lastPage: RTHeapRep.Page;
      last: RTHeapRep.Page;
      previousPages, freePages, activePages: INTEGER;
      allocatedPages, unallocatedPages : INTEGER;
      ambRootPages, strongRefPages, protectedPages: INTEGER;
      summary: ARRAY RTHeapRep.Space OF 
                 ARRAY RTHeapRep.Note OF 
                   ARRAY RTHeapRep.Generation OF 
                     ARRAY BOOLEAN OF 
                       INTEGER;
  BEGIN
    IF simplePutter = NIL THEN
      RTIO.PutText("GC ERROR >> RTHeapPages uninitialized\n");
      RETURN;
    END;
    
    IF putter = NIL THEN
      putter := simplePutter;
    END;

    RTOS.LockHeap ();

    FOR i := FIRST(RTHeapRep.Space) TO LAST(RTHeapRep.Space) DO
      FOR j := FIRST(RTHeapRep.Note) TO LAST(RTHeapRep.Note) DO
        FOR k := FIRST(RTHeapRep.Generation) TO LAST(RTHeapRep.Generation) DO
          FOR l := FIRST(BOOLEAN) TO LAST(BOOLEAN) DO
            summary[i, j, k, l] := 0;
          END;
        END;
      END;
    END;

    freePages        := 0;
    activePages      := 0;
    allocatedPages   := 0;
    previousPages    := 0;
    unallocatedPages := 0;
    ambRootPages     := 0;
    strongRefPages   := 0;
    protectedPages   := 0;

    lastPage       := FIRST(RTHeapRep.desc^);
    lastSpace      := RTHeapRep.desc[lastPage].space;
    lastNote       := RTHeapRep.desc[lastPage].note;
    lastGeneration := RTHeapRep.desc[lastPage].generation;
    lastProtected  := RTHeapRep.desc[lastPage].protected;

    FOR i := FIRST(RTHeapRep.desc^) TO LAST(RTHeapRep.desc^) DO
      CASE RTHeapRep.desc[i].space OF
      | RTHeapRep.Space.Free        => INC(freePages);
      | RTHeapRep.Space.Current     => INC(activePages);
      | RTHeapRep.Space.Previous    => INC(previousPages);
      | RTHeapRep.Space.Unallocated => INC(unallocatedPages);
      END;
      IF RTHeapRep.desc[i].space = RTHeapRep.Space.Current OR
        RTHeapRep.desc[i].space = RTHeapRep.Space.Previous
       THEN
        CASE RTHeapRep.desc[i].note OF
        | RTHeapRep.Note.AmbiguousRoot => INC(ambRootPages);
        | RTHeapRep.Note.StrongRef     => INC(strongRefPages);
        ELSE
        END;
        IF RTHeapRep.desc[i].protected THEN INC(protectedPages); END;
      END;

      lastPage  := i;
      lastSpace := RTHeapRep.desc[i].space;
      lastNote  := RTHeapRep.desc[i].note;
      lastGeneration := RTHeapRep.desc[i].generation;
      lastProtected := RTHeapRep.desc[i].protected;
      
      INC(summary[lastSpace, lastNote, lastGeneration, lastProtected]);
    END;

    INC(allocatedPages, freePages);
    INC(allocatedPages, previousPages);
    INC(allocatedPages, activePages);
    INC(allocatedPages, unallocatedPages);

    IF allocatedPages # RTHeapRep.allocatedPages THEN
      putter.putText("ERROR >> allocatedPages # RTHeapRep.allocatedPages: ");
      putter.putInt(allocatedPages); putter.putText(" ");
      putter.putInt(RTHeapRep.allocatedPages); putter.putText("\n");
    END;
    IF freePages # RTHeapRep.freePages THEN
      putter.putText("ERROR >> freePages # RTHeapRep.freePages: ");
      putter.putInt(freePages); putter.putText(" ");
      putter.putInt(RTHeapRep.freePages); putter.putText("\n");
    END;
    IF activePages # RTHeapRep.activePages THEN
      putter.putText("ERROR >> activePages # RTHeapRep.activePages: ");
      putter.putInt(activePages); putter.putText(" ");
      putter.putInt(RTHeapRep.activePages); putter.putText("\n");
    END;

    WITH sum = RTHeapRep.smallCopyPages + RTHeapRep.largeCopyPages + 
         RTHeapRep.smallPromotionPages + RTHeapRep.largePromotionPages +
         RTHeapRep.smallNewPages + RTHeapRep.largeNewPages 
     DO
      IF sum # RTHeapRep.activePages THEN
        putter.putText("ERROR >> sum of active # RTHeapRep.activePages: ");
        putter.putInt(sum); putter.putText(" ");
        putter.putInt(RTHeapRep.activePages); putter.putText("\n");
      END
    END;

    putter.putText(">>> Page counts:\n"); 
    putter.putText("  allocated   : "); 
    putter.putInt(allocatedPages);
    putter.putText("\n  unallocated : "); 
    putter.putInt(unallocatedPages);
    putter.putText("\n  active      : "); 
    putter.putInt(activePages);
    putter.putText("\n  previous    : "); 
    putter.putInt(previousPages);
    putter.putText("\n  free        : "); 
    putter.putInt(freePages);
    putter.putText("\n  pinned      : "); 
    putter.putInt(strongRefPages + ambRootPages);
    putter.putText("\n  protected   : "); 
    putter.putInt(protectedPages);
    putter.putText("\n\n");

    putter.putText(">>> Page counts of active pages:\n"); 
    putter.putText("\n  smallNewPages       : "); 
    putter.putInt(RTHeapRep.smallNewPages);
    putter.putText("\n  largeNewPages       : ");
    putter.putInt(RTHeapRep.largeNewPages);
    putter.putText("\n  smallCopyPages      : ");
    putter.putInt(RTHeapRep.smallCopyPages);
    putter.putText("\n  largeCopyPages      : "); 
    putter.putInt(RTHeapRep.largeCopyPages);
    putter.putText("\n  smallPromotionPages : "); 
    putter.putInt(RTHeapRep.smallPromotionPages);
    putter.putText("\n  largePromotionPages : "); 
    putter.putInt(RTHeapRep.largePromotionPages);
    putter.putText("\n\n");

    putter.putText(">>> Page status summary:\n"); 
    FOR i := FIRST(RTHeapRep.Space) TO LAST(RTHeapRep.Space) DO
      FOR j := FIRST(RTHeapRep.Note) TO LAST(RTHeapRep.Note) DO
        FOR k := FIRST(RTHeapRep.Generation) TO LAST(RTHeapRep.Generation) DO
          FOR l := FIRST(BOOLEAN) TO LAST(BOOLEAN) DO
            IF summary[i, j, k, l] # 0 THEN
              PutSpace(putter, i); putter.putText(" ");
              PutNote(putter, j);  putter.putText(" ");
              PutGeneration(putter, k);  putter.putText(" ");
              PutProtected(putter, l);  putter.putText(" - ");
              putter.putInt(summary[i, j, k, l]); putter.putText("\n");
            END;
          END;
        END;
      END;
    END;

    IF full THEN
      lastPage       := FIRST(RTHeapRep.desc^);
      lastSpace      := RTHeapRep.desc[lastPage].space;
      lastNote       := RTHeapRep.desc[lastPage].note;
      lastGeneration := RTHeapRep.desc[lastPage].generation;
      lastProtected  := RTHeapRep.desc[lastPage].protected;

      putter.putText("\n>>> Page status list:\n"); 
      putter.putText(
          "\t(it may change underneath if printing does allocation)\n");

      FOR i := FIRST(RTHeapRep.desc^) TO LAST(RTHeapRep.desc^) DO
        IF i # FIRST(RTHeapRep.desc^) AND
          (RTHeapRep.desc[i].space # lastSpace OR 
          RTHeapRep.desc[i].note # lastNote OR
          RTHeapRep.desc[i].generation # lastGeneration OR
          RTHeapRep.desc[i].protected # lastProtected OR
          i = LAST(RTHeapRep.desc^))
         THEN
          IF i = LAST(RTHeapRep.desc^) THEN last := i; ELSE last := i-1 END;
          IF full THEN
            putter.putInt(lastPage, 4); putter.putText(" - "); 
            putter.putInt(last, 4); putter.putText(" : "); 
            PutSpace(putter, lastSpace); putter.putText(" ");
            PutNote(putter, lastNote);  putter.putText(" ");
            PutGeneration(putter, lastGeneration); putter.putText(" ");
            PutProtected(putter, lastProtected);  putter.putText("\n");
          END;
          lastPage  := i;
          lastSpace := RTHeapRep.desc[i].space;
          lastNote  := RTHeapRep.desc[i].note;
          lastGeneration := RTHeapRep.desc[i].generation;
          lastProtected := RTHeapRep.desc[i].protected;
        END;
      END;
    END;
    RTOS.UnlockHeap ();
  END DumpPageStatus;

(*
 * pretty printing
 *)

PROCEDURE PutSpace (putter: RTIO.SimplePutter; s: RTHeapRep.Space) = 
  BEGIN
    CASE s OF 
    | RTHeapRep.Space.Unallocated => putter.putText("Unallocated");
    | RTHeapRep.Space.Free => putter.putText("Free");
    | RTHeapRep.Space.Previous => putter.putText("Previous");
    | RTHeapRep.Space.Current => putter.putText("Current");
    END;
  END PutSpace;

PROCEDURE PutGeneration (putter: RTIO.SimplePutter; s: RTHeapRep.Generation) = 
  BEGIN
    CASE s OF 
    | RTHeapRep.Generation.Older   => putter.putText("Older");
    | RTHeapRep.Generation.Younger => putter.putText("Younger");
    END;
  END PutGeneration; 

PROCEDURE PutProtected (putter: RTIO.SimplePutter; s: BOOLEAN) = 
  BEGIN
    IF s THEN
      putter.putText("Protected");
    ELSE
      putter.putText("Unprotected");
    END;
  END PutProtected; 

PROCEDURE PutPure (putter: RTIO.SimplePutter; s: BOOLEAN) = 
  BEGIN
    IF s THEN
      putter.putText("Pure");
    ELSE
      putter.putText("Impure");
    END;
  END PutPure; 

PROCEDURE PutNote(putter: RTIO.SimplePutter; n: RTHeapRep.Note) =
  BEGIN
    CASE n OF 
    | RTHeapRep.Note.OlderGeneration => putter.putText("OlderGeneration");
    | RTHeapRep.Note.AmbiguousRoot => putter.putText("AmbiguousRoot");
    | RTHeapRep.Note.StrongRef => putter.putText("StrongRef");
    | RTHeapRep.Note.PartialCleanup => putter.putText("PartialCleanup");
    | RTHeapRep.Note.Large => putter.putText("Large");
    | RTHeapRep.Note.Frozen => putter.putText("Frozen");
    | RTHeapRep.Note.Allocated => putter.putText("Allocated");
    | RTHeapRep.Note.Copied => putter.putText("Copied");
    END;
  END PutNote;

PROCEDURE PutPageStatus (putter: RTIO.SimplePutter; page: RTHeapRep.Page) =
  BEGIN
    IF simplePutter = NIL THEN
      RTIO.PutText("GC ERROR >> RTHeapPages uninitialized\n");
      RETURN;
    END;
    IF putter = NIL THEN
      putter := simplePutter;
    END;
    PutSpace(putter, RTHeapRep.desc[page - RTHeapRep.p0].space);  
    putter.putText(" ");
    PutNote(putter, RTHeapRep.desc[page - RTHeapRep.p0].note);
    putter.putText(" ");
    PutGeneration(putter, RTHeapRep.desc[page - RTHeapRep.p0].generation);
    putter.putText(" ");
    PutProtected(putter, RTHeapRep.desc[page - RTHeapRep.p0].protected);
    putter.putText(" ");
    PutPure(putter, RTHeapRep.desc[page - RTHeapRep.p0].pure);
    putter.putText(" G:");
    putter.putBoolean(RTHeapRep.desc[page - RTHeapRep.p0].gray);
    putter.putText(" R:");
    putter.putBoolean(RTHeapRep.desc[page - RTHeapRep.p0].reused);
    putter.putText(" C:");
    putter.putBoolean(RTHeapRep.desc[page - RTHeapRep.p0].cleanedup);
  END PutPageStatus;

(*
 * initialization
 *)

PROCEDURE Init() =
  BEGIN
    IF simplePutter = NIL THEN
      simplePutter := NEW(RTIO.SimplePutter);
    END;
  END Init;

BEGIN
END RTHeapPages.


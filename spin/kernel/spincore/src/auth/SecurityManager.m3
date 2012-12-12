(* 
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * SecurityManager
 *   The module is unsafe
 *   b/c SIDStack is an unsafe interface
 *   which simply provides the rightly typed view
 *   onto Enqueue and Dequeue
 *   in machine/src/[Platform]/AtomicOps.s
 *
 * HISTORY
 *
 * 04-Feb-98  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for domains.
 *
 * 07-Jan-98  Robert Grimm (rgrimm) at the University of Washington
 *      Added auditing.
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      More implementation work and clean up.
 *
 * 03-Nov-97  Robert Grimm (rgrimm) at the University of Washington
 *      More implementation work and various bug fixes.
 *
 * 30-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      Created.
 *
 *)

UNSAFE MODULE SecurityManager EXPORTS SecurityManager,
                                      SecurityManagerProtected,
                                      SecurityManagerPrivate,
                                      SecurityContext;

IMPORT RT0, RTProcDesc, RTExRep, RTThreadExtern, RTType;
IMPORT RTTypeSRC, RTTypeSecurity, RTHeapRep, WeakRef;
IMPORT Word, SafeConvert, TextRefTbl, Wr, IO, Fmt;
IMPORT FastList, Strand, StrandRep, Clock;
IMPORT Dispatcher;
IMPORT Domain, DomainPrivate;
IMPORT SecurityError, AccessMode, AccessModeRep;
IMPORT SIDStack, BindingInfo, RefBindingInfoTbl, SecurityPolicy;
IMPORT DebugOption, Spy;


(*
   Public context and user/group id management
  ---------------------------------------------
 *)

TYPE
  PropertyT = MUTEX OBJECT
    properties : TextRefTbl.Default;
  METHODS
    init() : PropertyT := PropertyInit;
  END;

PROCEDURE PropertyInit( self: PropertyT ) : PropertyT =
  BEGIN
    LOCK self DO
      self.properties := NEW(TextRefTbl.Default).init();
    END;
    RETURN self;
  END PropertyInit;

REVEAL
  ContextT = BRANDED "SecurityContext.ContextT" REF RECORD
    user       : UID;           (* User id             *)
    properties : PropertyT;     (* Table of properties *)
  END;

PROCEDURE GetCurrentProperty( name: TEXT; remove: BOOLEAN := FALSE) : REFANY =
  (*
     Get the property value associated with name,
     return NIL if no such property exists,
     destroy property if remove is TRUE.
   *)
  BEGIN
    RETURN GetProperty( Strand.GetCurrent(), name, remove );
  END GetCurrentProperty;

PROCEDURE GetProperty( s : Strand.T; name: TEXT; remove: BOOLEAN := FALSE )
  : REFANY =
  (*
     Get the property value associated with name,
     return NIL if no such property exists,
     destroy property if remove is TRUE.
   *)
  VAR
    result  : REFANY;
    ok      : BOOLEAN;
  BEGIN
    WITH properties = s.context.properties DO
      IF properties = NIL THEN RETURN NIL; END;
      LOCK properties DO
        IF remove THEN
          ok := properties.properties.delete( name, result );
        ELSE
          ok := properties.properties.get   ( name, result );
        END;
      END;
      IF ok THEN RETURN result; ELSE RETURN NIL; END;
    END;
  END GetProperty;

PROCEDURE SetCurrentProperty( name: TEXT; property: REFANY ) : REFANY =
  (*
     Associate the specified property value with name.
     Return old value if already exists.
   *)
  BEGIN
    RETURN SetProperty( Strand.GetCurrent(), name, property );
  END SetCurrentProperty;

PROCEDURE SetProperty( s : Strand.T; name: TEXT; property: REFANY ) : REFANY =
  (*
     Associate the specified property value with name.
     Return old value if already exists.
   *)
  VAR
    result  : REFANY;
  BEGIN
    WITH properties = s.context.properties DO
      IF properties = NIL THEN
        properties := NEW(PropertyT).init();
      END;
      LOCK properties DO
        result := property;
        IF properties.properties.put( name, result ) THEN
          RETURN result;
        END;
      END;
      RETURN NIL;
    END;
  END SetProperty;

PROCEDURE GetCurrentUid() : UID =
  (* Get current user id *)
  BEGIN
    RETURN Strand.GetCurrent().context.user;
  END GetCurrentUid;

PROCEDURE GetUid( s : Strand.T ) : UID = 
  (* Get user id. *)
  BEGIN
    RETURN s.context.user;
  END GetUid;

PROCEDURE GetGroups( uid : UID ) : REF ARRAY OF GID
  RAISES { SecurityError.T } =
  (*
     Get groups a user is a member in;
     raise "SecurityError.EC.InvalidId" if invalid id.
   *)
  VAR
    groups  : REF ARRAY OF GID;
    timeout : INTEGER;
  BEGIN
    EVAL SecurityPolicy.GetGroups( uid, groups, timeout );
    RETURN groups;
  END GetGroups;

PROCEDURE GetUserName ( uid : UID ) : TEXT RAISES { SecurityError.T } =
  (*
     Turn "uid" into human-readable form;
     raise "SecurityError.EC.InvalidId" if invalid id.
   *)
  BEGIN
    RETURN SecurityPolicy.GetUserName( uid );
  END GetUserName;
  
PROCEDURE GetGroupName( gid : GID ) : TEXT RAISES { SecurityError.T } =
  (*
     Turn "gid" into human-readable form;
     raise "SecurityError.EC.InvalidId" if invalid id.
   *)
  BEGIN
    RETURN SecurityPolicy.GetGroupName( gid );
  END GetGroupName;


(*
   The main mediation cache (mc)
  -------------------------------
 *)

TYPE
  MediationCacheT = REF RECORD
    next : MediationCacheT; (* Pointer to next cache entry *)
    k1   : SID;             (* First key                   *)
    k2   : SID;             (* Second key                  *)
    mode : AccessMode.T;    (* Access mode                 *)
    s    : SID;             (* Subject security id         *)
    o    : SID;             (* Object default security id  *)
    used : BOOLEAN;         (* Coarse-grained LRU flag     *)
  END;

  MCTimeOutT = REF RECORD
    k1 : SID; (* First key  *)
    k2 : SID; (* Second key *)
  END;

VAR
  mcTotalSize : INTEGER := 64;   (* Total size of mediation cache     *)
  mcSize1     : INTEGER :=  8;   (* Size factor for first key         *)
  mcMask1     : INTEGER :=  7;   (* Bit-mask for hashing first key    *)
  mcShift1    : INTEGER :=  3;   (* Number of positions to shift left *)
  mcSize2     : INTEGER :=  8;   (* Size factor for second key        *)
  mcMask2     : INTEGER :=  7;   (* Bit-mask for hashing second key   *)
  mcLock      : MUTEX;           (* Lock for mediation cache          *)
  mcFreeList  : MediationCacheT; (* Free-list of cache entries        *)
  mcHashTable : REF ARRAY OF MediationCacheT; (* Hash into cache      *)
  mcLruIndex  : INTEGER :=  0;   (* Index of last LRU element         *)
  (*
     Given two security identifiers "sid1" and "sid2",
     the right hash-table index is computed as follows:
         Word.LeftShift( Word.And( sid1, mcMask1 ), mcShift1 )
       + Word.And( sid2, mcMask2 )
   *)

PROCEDURE ClearMediationCache() =
  (* Clear the entire mediation cache. *)
  BEGIN
    LOCK mcLock DO
      ReallyClearMediationCache();
    END;
  END ClearMediationCache;

PROCEDURE ReallyClearMediationCache() = 
  (* Really clear the entire mediation cache. *)
  VAR
    entry : MediationCacheT;
  BEGIN
    FOR i := FIRST(mcHashTable^) TO LAST(mcHashTable^) DO
      WHILE mcHashTable[i] # NIL DO
        entry          := mcHashTable[i];
        mcHashTable[i] := entry.next;
        entry.next     := mcFreeList;
        mcFreeList     := entry;
      END; (* while *)
    END;
    mcLruIndex := 0;
  END ReallyClearMediationCache;

PROCEDURE ClearMediationCacheEntry( k1, k2 : SID ) : BOOLEAN =
  (*
     Delete entry for the pair of SIDs "k1", "k2" from mediation cache.
     Returns true iff such entry was in the cache.
   *)
  VAR
    index : INTEGER;
    entry, entry2 : MediationCacheT;
  BEGIN
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      IF entry = NIL THEN RETURN FALSE; END;
      IF entry.k1 = k1 AND entry.k2 = k2 THEN
        mcHashTable[index] := entry.next;
        entry.next         := mcFreeList;
        mcFreeList         := entry;
        RETURN TRUE;
      END;
      WHILE entry.next # NIL DO
        IF entry.next.k1 = k1 AND entry.next.k2 = k2 THEN
          entry2      := entry.next;
          entry.next  := entry2.next;
          entry2.next := mcFreeList;
          mcFreeList  := entry2;
          RETURN TRUE;
        END;
        entry := entry.next;
      END;
    END;
    RETURN FALSE;
  END ClearMediationCacheEntry;

PROCEDURE SetMediationCacheSize( size1, size2 : INTEGER )
  RAISES { SecurityError.T } =
  (*
     Set size of mediation cache to be the product of "size1" and "size2".
     Both numbers must be powers of two
     and represent the hash-size per security id
     for the two security ids used on each lookup.
     Raises "SecurityError.EC.BadNumber" if numbers are not powers of two.
   *)
  VAR
    oldSize : INTEGER;
    entry   : MediationCacheT;
  BEGIN
    IF NOT PowerOfTwo(size1) OR NOT PowerOfTwo(size2) THEN
      RAISE SecurityError.T(SecurityError.EC.BadNumber);
    END;
    IF size1 = mcSize1 AND size2 = mcSize2 THEN RETURN; END;
    LOCK mcLock DO
      (* Clear cache first and move all elements to free list *)
      ReallyClearMediationCache();
      (* Calculate new cache information *)
      oldSize     := mcTotalSize;
      mcSize1     := size1;
      mcMask1     := size1 - 1;
      mcShift1    := LogTwo( size1 );
      mcSize2     := size2;
      mcMask2     := size2 - 1;
      mcTotalSize := size1 * size2;
      mcLruIndex  := 0;
      (* Fill up free list or remove element as necessary *)
      IF mcTotalSize > oldSize THEN
        FOR i := 1 TO mcTotalSize - oldSize DO
          entry      := NEW( MediationCacheT, next := mcFreeList );
          mcFreeList := entry;
        END;
      ELSIF mcTotalSize < oldSize THEN
        FOR i := 1 TO oldSize - mcTotalSize DO
          mcFreeList := mcFreeList.next;
        END;
      END;
    END;
  END SetMediationCacheSize;

PROCEDURE AddToMediationCache( k1, k2  : SID;
                               mode    : AccessMode.T;
                               s, o    : SID;
                               timeout : INTEGER;      ) = 
  (* Add entry to mediation cache, but check if such entry exists first *)
  VAR
    index  : INTEGER;
    entry  : MediationCacheT;
    entry2 : MediationCacheT;
    found  : BOOLEAN := FALSE;
  BEGIN
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      (* Make sure entry is not already in cache *)
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          (* Update entry *)
          entry.mode := mode;
          entry.s    := s;
          entry.o    := o;
          entry.used := TRUE;
          found      := TRUE; (* Do not add a new cache entry *)
          IO.Put("==> SecurityManager: race on mediation cache entry!\n");
          EXIT;
        END;
        entry := entry.next;
      END;

      IF NOT found THEN
        (* Get slot for new cache entry *)
        entry := mcFreeList;
        IF entry # NIL THEN
          mcFreeList := entry.next;
        ELSE
          (*
             Cache is full, we need to make space:

             Start scan of cache at "mcLruIndex" and find the
             first entry whose "used" flag is not set.
             If no entry can be found during this first scan,
             the scan will continue and use the first entry
             found at/after "mcLruIndex" whose "used" flag has
             been clear during the first scan.
             While scanning, clear all "used" flags
             (which includes all entries in the hash table slot
             of the entry to be re-used, since the next scan
             will start at the next hash-table slot).
           *)
          LOOP
            entry := mcHashTable[mcLruIndex];
            IF entry # NIL THEN
              IF entry.used THEN
                (* Clear usage flag *)
                entry.used := FALSE;
              ELSE
                (* Use this entry. *)
                mcHashTable[mcLruIndex] := entry.next;
                EXIT;
              END;

              (* Scan rest of hash-table slot *)
              WHILE entry.next # NIL DO
                IF entry.next.used THEN
                  (* Clear usage flag *)
                  entry.next.used := FALSE;
                ELSE
                  (* Use this entry. *)
                  entry2     := entry.next;
                  entry.next := entry2.next;
                  entry      := entry2;
                  found      := TRUE;
                  EXIT;
                END;
                entry := entry.next;
              END;
              IF found THEN EXIT; END;
            END;

            (* Merry go around *)
            INC( mcLruIndex );
            IF mcLruIndex = mcTotalSize THEN mcLruIndex := 0; END;
          END; (* LOOP *)

          (* Clean up usage flags *)
          entry2 := entry.next;
          WHILE entry2 # NIL DO
            entry2.used := FALSE;
            entry2      := entry2.next;
          END;

          (* Fix search index *)
          INC( mcLruIndex );
          IF mcLruIndex = mcTotalSize THEN mcLruIndex := 0; END;
        END;

        (* Fill in new cache entry *)
        entry.next         := mcHashTable[index];
        entry.k1           := k1;
        entry.k2           := k2;
        entry.mode         := mode;
        entry.s            := s;
        entry.o            := o;
        entry.used         := TRUE;
        mcHashTable[index] := entry;
      END;
    END;

    (*
       Time out cache entry if requested.
       Do this outside of mcLock as to avoid deadlock.
     *)
    IF timeout > 0 THEN
      Clock.SetAlarm( timeout, TimeOutMediationCacheEntry,
                      NEW( MCTimeOutT, k1 := k1, k2 := k2 ) );
    END;
  END AddToMediationCache;

PROCEDURE TimeOutMediationCacheEntry( closure : REFANY ) =
  (* Remove mediation cache entry on timeout. *)
  BEGIN
    WITH entry = NARROW( closure, MCTimeOutT ) DO
      EVAL ClearMediationCacheEntry( entry.k1, entry.k2 );
    END;
  END TimeOutMediationCacheEntry;


(*
   Internal security id management
  ---------------------------------
 *)

REVEAL
  SIDStackT =  FastList.UT BRANDED "SecurityContext.SIDStackT" OBJECT
    s        : SID;
    o        : SID;
    eframe   : RTExRep.EF2
      := RTExRep.EF2{ next    := NIL,
                      class   := ORD(RTExRep.ScopeKind.FinallyProc),
                      handler := PopSIDStack,
                      frame   := NIL };
  END;
  (*
     An element of the security id stack consists of two security
     identifiers and a "FinallyProc" exception frame, in particular:
     . the "s" slot for the subjects security id,
     . the "o" slot for the object default security id,
     . the "eframe" slot for the exception frame which in turn consists of
       . the "next" slot points to the next exception frame,
       . the "class" slot is always "ORD(RTExRep.ScopeKind.FinallyProc)",
       . the "handler" slot is always "PopSIDStack".
       . the "frame" slot is always 0.
     The exception frame is used to ensure that on security domain changes,
     an exception will not cause an inconsistent sid stack.

     This data structure must be consistent with "FastList.T"!
   *)

CONST
  SidPoolInit = 1500; (* Preallocate pool of size        *)
  SidPoolIncr =  100; (* Grow by this if we ever run out *)

VAR
  sidPool : SIDStackT; (* Global pool of pre-allocated elements *)

PROCEDURE PopSIDStack() =
  (* Exception handler: pop one element from security id stack. *)
  VAR
    element : SIDStackT;
  BEGIN
    element := SIDStack.Dequeue( currentSid );
    <* ASSERT element # NIL *>
    <* ASSERT currentSid # NIL *>
    SIDStack.Enqueue( element, sidPool );
  END PopSIDStack;

PROCEDURE CleanUpSIDStack( <*UNUSED*> READONLY self : WeakRef.T; x : REFANY ) =
  (*
     WeakRef clean-up procedure for user-level strands:
     Dispose of the one and only security id stack element.
   *)
  BEGIN
    WITH s = NARROW( x, Strand.T ) DO
      <* ASSERT s.sid.nextelem = NIL *>
      DISPOSE( s.sid );
    END;
  END CleanUpSIDStack;

PROCEDURE InitSIDStack( VAR new : SIDStackT; old : SIDStackT ) =
  (*
     Initialize the SID stack of a new kernel thread
     to a copy of the top of the old thread.
   *)
  VAR
    element : SIDStackT;
  BEGIN
    <* ASSERT new = NIL *>
    element := SIDStack.Dequeue( sidPool );
    IF element = NIL THEN
      IO.Put("Burp: we need more security id stack elements\n");
      FOR i := 1 TO SidPoolIncr - 1 DO
        SIDStack.Enqueue( NEW( SIDStackT ), sidPool );
      END;
      element := NEW( SIDStackT );
    END;
    element.s := old.s;
    element.o := old.o;
    SIDStack.Enqueue( element, new );
  END InitSIDStack;

PROCEDURE CollectSIDStack( VAR stack : SIDStackT ) =
  (* Collect all elements from the SID stack of a kernel thread. *)
  VAR
    element : SIDStackT;
  BEGIN
    element := SIDStack.Dequeue( stack );
    WHILE element # NIL DO
      SIDStack.Enqueue( element, sidPool );
      element := SIDStack.Dequeue( stack );
    END;
  END CollectSIDStack;

PROCEDURE Tazify( s : Strand.T ) =
  (*
     Tazify the original strand.
     Only to be called once in Sched.m3 !
   *)
  BEGIN
    (*
       User contexts must always be available,
       since they support properties used by the shell.
       SIDs are only available if security is switched on.
     *)
    <* ASSERT s.context = NIL *>
    s.context := NEW( ContextT, user := TazUid, properties := NIL );
    IF DebugOption.Security THEN
      <* ASSERT s.sid = NIL *>
      s.sid      := NEW( SIDStackT, s := TazSid, o := TazDataSid  );
      currentSid := s.sid;
    END;
  END Tazify;

PROCEDURE InheritContext( s : Strand.T ) =
  (*
     To be used by extension thread packages:
     If the strand s has no security context or SID stack,
     inherit from current one.
   *)
  VAR
    parent : Strand.T;
  BEGIN
    parent := Strand.GetCurrent();
    (* Fix s if necessary *)
    IF s.context = NIL THEN
      s.context := parent.context;
    ELSE
      IO.Put("==> SecurityManagerProtected.InheritContext: " &
             "strand already has context!\n");
    END;

    IF NOT DebugOption.Security THEN RETURN; END;
    IF s.sid = NIL THEN
      (*
         User-level threads dont get their SIDStackTs from the
         pre-allocated pool, but rather freshly allocated ones
         which are reclaimed by weakref-ing the strand record.
       *)
      s.sid := NEW( SIDStackT, s := currentSid.s, o := currentSid.o );
      EVAL WeakRef.FromRef( s, CleanUpSIDStack );
    ELSE
      IO.Put("==> SecurityManagerProtected.InheritContext: " &
             "strand already has security id!\n");
    END;
  END InheritContext;

PROCEDURE HandOffContext( user, kernel : Strand.T ) =
  BEGIN
    (* Fix user context if necessary. *)
    IF user.context = NIL THEN
      user.context := NEW( ContextT, user := AnonUid );
    END;
    (* Pass context on. *)
    kernel.context := user.context;
    (* Fix user sid stack if necessary. *)
    IF user.sid = NIL THEN
      user.sid := NEW( SIDStackT, s := AnonSid, o := AnonDataSid );
      EVAL WeakRef.FromRef( user, CleanUpSIDStack );
    END;
    (* Pass sid info on. *)
    kernel.sid.s := user.sid.s;
    kernel.sid.o := user.sid.o;
    (* Fix currentSid. *)
    currentSid := kernel.sid;
    (* XXX this is not needed so far as trap handler is bound statically
       -- yaz *)
  END HandOffContext;


(*
    Access Control and Binding Information (bi)
   ---------------------------------------------
 *)

CONST
  CHECK_NONE = AccessControlT{};
  CHECK_D    = AccessControlT{ AccessCheckT.DomainTransfer  };
  CHECK_X    = AccessControlT{ AccessCheckT.CodeCheck       };
  CHECK_O    = AccessControlT{ AccessCheckT.ObjectCheck     };
  CHECK_XD   = AccessControlT{ AccessCheckT.CodeCheck,
                               AccessCheckT.DomainTransfer  };
  CHECK_OD   = AccessControlT{ AccessCheckT.ObjectCheck,
                               AccessCheckT.DomainTransfer  };

TYPE
  ClosureT = REF RECORD
    sid         : SID;
    kind        : AccessControlT;
    executeMode : AccessMode.T;
    inModes     : AccessMode.ListT;
    resultMode  : AccessMode.T;
    exceptMode  : AccessMode.T;
    derefArgs   : REF ARRAY OF BOOLEAN;
    binding     : Dispatcher.Binding;
  END;
  (*
     This record is the closure for guards and postguards
     that execute the access control operations. It is the
     same as BindingInfo.T, only that it does not store a
     reference to the guard for a binding, but rather the
     binding itself.
   *)

VAR
  biLock  : MUTEX;
  biTable : RefBindingInfoTbl.Default;

  spyDomain   : Spy.T;   (* Spy to count protection domain changes. *)
  spyCode     : Spy.T;   (* Spy to count access checks on code.     *)
  spyObject   : Spy.T;   (* Spy to count access checks on objects.  *)
  spyExplicit : Spy.T;   (* Spy to count explicit access checks.    *)


PROCEDURE PermissionContain( mode1, mode2 : AccessMode.T ) : BOOLEAN =
  (*
     Does "mode1" entail all "Permission.T" objects in "mode2"?
     Assumes that "mode2" is non-NIL.
   *)
  VAR
    found : BOOLEAN;
  BEGIN
    WITH p1 = mode1.permissions, p2 = mode2.permissions DO
      IF p1 = NIL THEN RETURN FALSE; END;
      FOR i := FIRST(p2^) TO LAST(p2^) DO
        found := FALSE;
        FOR j := FIRST(p1^) TO LAST(p1^) DO
          IF     TYPECODE(p1[j]) = TYPECODE(p2[i])
            AND p1[j].contains(p2[i]) THEN
            found := TRUE;
            EXIT;
          END;
        END;
        IF NOT found THEN RETURN FALSE; END;
      END;
    END;
    
    RETURN TRUE;
  END PermissionContain;

PROCEDURE GuardDomain( closure : ClosureT ) : BOOLEAN =
  VAR
    index   : INTEGER;
    entry   : MediationCacheT;
    k1, k2  : SID;
    mode    : AccessMode.T;
    s, o    : SID;
    timeout : INTEGER;
    found   : BOOLEAN := FALSE;
    cache   : BOOLEAN;
    element : SIDStackT;
  BEGIN
    (* Collect statistics *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyDomain, 0, 1 );
    END;

    k1 := currentSid.s;
    k2 := closure.sid;
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE;    (* Set usage flag *)
          s          := entry.s; (* Remember sids  *)
          o          := entry.o;
          found      := TRUE;    (* Found in cache *)
          EXIT;
        END;
        entry := entry.next;
      END;
    END;

    IF NOT found THEN
      (* Entry is not in cache, go ask policy. *)
      cache := SecurityPolicy.Mediate( k1, k2, mode, s, o, timeout );
      IF cache THEN AddToMediationCache( k1, k2, mode, s, o, timeout ); END;
    END;

    (* Get sid stack element. *)
    element := SIDStack.Dequeue( sidPool );
    IF element = NIL THEN
      IO.Put("Burp: we need more security id stack elements\n");
      FOR i := 1 TO SidPoolIncr - 1 DO
        SIDStack.Enqueue( NEW( SIDStackT ), sidPool );
      END;
      element := NEW( SIDStackT );
    END;
    (* Push new exception frame within old top of sid stack *)
    currentSid.eframe.next := RTThreadExtern.RTThread__handlerStack;
    RTThreadExtern.RTThread__handlerStack := ADR(currentSid.eframe);
    (* Push new security sid element *)
    element.s := s;
    element.o := o;
    SIDStack.Enqueue( element, currentSid );

    RETURN TRUE;
  END GuardDomain;

PROCEDURE PostGuardDomain() =
  VAR
    element : SIDStackT;
  BEGIN
    (* Collect statistics *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyDomain, 0, 1 );
    END;

    (* Pop top from SID stack. *)
    element := SIDStack.Dequeue( currentSid );
    <*ASSERT element # NIL*>
    <*ASSERT currentSid # NIL*>
    SIDStack.Enqueue( element, sidPool );
    (* Restore exception frame *)
    <*ASSERT ADR(currentSid.eframe)=RTThreadExtern.RTThread__handlerStack*>
    RTThreadExtern.RTThread__handlerStack := currentSid.eframe.next;
  END PostGuardDomain;

PROCEDURE GuardCode( closure : ClosureT ) : BOOLEAN =
  VAR
    mode  : AccessMode.T;
    s, o  : SID;
  BEGIN
    (* Collect statistics *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyCode, 0, 1 );
    END;

    mode := Mediate( currentSid.s, closure.sid, s, o );
    WITH mode2 = closure.executeMode DO
      IF    ( NOT mode.simple >= mode2.simple )
         OR (     mode2.permissions # NIL
              AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END GuardCode;

PROCEDURE GuardCodeDomain( closure : ClosureT ) : BOOLEAN =
  VAR
    sid     : SID := currentSid.s;
    mode    : AccessMode.T;
    s, o    : SID;
    element : SIDStackT;
  BEGIN
    (* Code Check. *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyCode, 0, 1 );
    END;

    mode := Mediate( sid, closure.sid, s, o );
    WITH mode2 = closure.executeMode DO
      IF    ( NOT mode.simple >= mode2.simple )
         OR (     mode2.permissions # NIL
              AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
        RETURN FALSE;
      END;
    END;

    (* Protection Domain Transfer. *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyDomain, 0, 1 );
    END;

    (* Get sid stack element. *)
    element := SIDStack.Dequeue( sidPool );
    IF element = NIL THEN
      IO.Put("Burp: we need more security id stack elements\n");
      FOR i := 1 TO SidPoolIncr - 1 DO
        SIDStack.Enqueue( NEW( SIDStackT ), sidPool );
      END;
      element := NEW( SIDStackT );
    END;
    (* Push new exception frame within old top of sid stack *)
    currentSid.eframe.next := RTThreadExtern.RTThread__handlerStack;
    RTThreadExtern.RTThread__handlerStack := ADR(currentSid.eframe);
    (* Push new security sid element *)
    element.s := s;
    element.o := o;
    SIDStack.Enqueue( element, currentSid );

    RETURN TRUE;
  END GuardCodeDomain;

PROCEDURE GuardObject( closure : ClosureT; VAR a : ARRAY OF Word.T )
  : BOOLEAN =
  VAR
    sid    : SID := currentSid.s;
    mode   : AccessMode.T;
    s, o   : SID;
    adr    : Word.T;
    sidp   : RTTypeSecurity.SR;
  BEGIN
    (* Argument Check. *)
    WITH m = closure.inModes^ DO
      FOR i := FIRST(m) TO LAST(m) DO
        IF m[i] # NIL THEN
          IF DebugOption.SecurityStatistics THEN
            Spy.Hit( spyObject, 0, 1 );
          END;

          adr := a[i];
          IF closure.derefArgs[i] THEN
            (* VAR and READONLY always pass in an address *)
            adr := LOOPHOLE(adr, UNTRACED REF Word.T)^;
          END;
          IF LOOPHOLE(adr,REFANY) # NIL THEN
            sidp := RTTypeSecurity.Get( LOOPHOLE(adr, REFANY) );
            IF sidp = NIL THEN
              RETURN FALSE;
            END;
            mode := Mediate( sid, sidp.sid, s, o );
            WITH mode2 = m[i] DO
              IF    ( NOT mode.simple >= mode2.simple )
                 OR (     mode2.permissions # NIL
                      AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
                RETURN FALSE;
              END;
            END;
          END;
        END;
      END;
    END;

    RETURN TRUE;
  END GuardObject;

PROCEDURE GuardObjectDomain( closure : ClosureT; VAR a : ARRAY OF Word.T )
  : BOOLEAN =
  VAR
    sid     : SID := currentSid.s;  (* Subject sid           *)
    mode    : AccessMode.T;         (* Access mode           *)
    s, o    : SID;                  (* Default sids          *)
    element : SIDStackT;            (* Sid stack element     *)
    adr     : Word.T;               (* Object address        *)
    sidp    : RTTypeSecurity.SR;    (* Pointer to object sid *)
  BEGIN
    (* Argument Check. *)
    WITH m = closure.inModes^ DO
      FOR i := FIRST(m) TO LAST(m) DO
        IF m[i] # NIL THEN
          IF DebugOption.SecurityStatistics THEN
            Spy.Hit( spyObject, 0, 1 );
          END;

          adr := a[i];
          IF closure.derefArgs[i] THEN
            (* VAR and READONLY always pass in an address *)
            adr := LOOPHOLE(adr, UNTRACED REF Word.T)^;
          END;
          IF LOOPHOLE(adr,REFANY) # NIL THEN
            sidp := RTTypeSecurity.Get( LOOPHOLE(adr, REFANY) );
            IF sidp = NIL THEN
              RETURN FALSE;
            END;
            mode := Mediate( sid, sidp.sid, s, o );
            WITH mode2 = m[i] DO
              IF    ( NOT mode.simple >= mode2.simple )
                 OR (     mode2.permissions # NIL
                      AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
                RETURN FALSE;
              END;
            END;
          END;
        END;
      END;
    END;

    (* Protection Domain Transfer. *)
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyDomain, 0, 1 );
    END;

    EVAL Mediate( sid, closure.sid, s, o );

    (* Get sid stack element. *)
    element := SIDStack.Dequeue( sidPool );
    IF element = NIL THEN
      IO.Put("Burp: we need more security id stack elements\n");
      FOR i := 1 TO SidPoolIncr - 1 DO
        SIDStack.Enqueue( NEW( SIDStackT ), sidPool );
      END;
      element := NEW( SIDStackT );
    END;
    (* Push new exception frame within old top of sid stack *)
    currentSid.eframe.next := RTThreadExtern.RTThread__handlerStack;
    RTThreadExtern.RTThread__handlerStack := ADR(currentSid.eframe);
    (* Push new security sid element *)
    element.s := s;
    element.o := o;
    SIDStack.Enqueue( element, currentSid );

    RETURN TRUE;
  END GuardObjectDomain;

PROCEDURE GuardAll( closure : ClosureT; VAR a : ARRAY OF Word.T )
  : BOOLEAN =
  VAR
    sid     : SID := currentSid.s;  (* Subject sid           *)
    mode    : AccessMode.T;         (* Access mode           *)
    s, o    : SID;                  (* Default sids          *)
    element : SIDStackT;            (* Sid stack element     *)
    adr     : Word.T;               (* Object address        *)
    sidp    : RTTypeSecurity.SR;    (* Pointer to object sid *)
  BEGIN
    (* Code Check. *)
    IF AccessCheckT.CodeCheck IN closure.kind THEN
      IF DebugOption.SecurityStatistics THEN
        Spy.Hit( spyCode, 0, 1 );
      END;

      mode := Mediate( sid, closure.sid, s, o );
      WITH mode2 = closure.executeMode DO
        IF    ( NOT mode.simple >= mode2.simple )
           OR (     mode2.permissions # NIL
                AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
          IF AccessCheckT.Audit IN closure.kind THEN
            Add2AuditLog( closure.binding,
                          SecurityPolicy.AuditStatusT.FailedEnterCode );
          END;
          RETURN FALSE;
        END;
      END;
    END;

    (* Argument Check. *)
    IF AccessCheckT.ObjectCheck IN closure.kind THEN
      WITH m = closure.inModes^ DO
        FOR i := FIRST(m) TO LAST(m) DO
          IF m[i] # NIL THEN
            IF DebugOption.SecurityStatistics THEN
              Spy.Hit( spyObject, 0, 1 );
            END;

            adr := a[i];
            IF closure.derefArgs[i] THEN
              (* VAR and READONLY always pass in an address *)
              adr := LOOPHOLE(adr, UNTRACED REF Word.T)^;
            END;
            IF LOOPHOLE(adr,REFANY) # NIL THEN
              sidp := RTTypeSecurity.Get( LOOPHOLE(adr, REFANY) );
              IF sidp = NIL THEN
                IF AccessCheckT.Audit IN closure.kind THEN
                  Add2AuditLog( closure.binding,
                                SecurityPolicy.AuditStatusT.FailedEnterNIL );
                END;
                RETURN FALSE;
              END;
              mode := Mediate( sid, sidp.sid, s, o );
              WITH mode2 = m[i] DO
                IF    ( NOT mode.simple >= mode2.simple )
                   OR (     mode2.permissions # NIL
                        AND ( NOT PermissionContain( mode, mode2 ) ) ) THEN
                  IF AccessCheckT.Audit IN closure.kind THEN
                    Add2AuditLog(closure.binding,
                                 SecurityPolicy.AuditStatusT.FailedEnterObject);
                  END;
                  RETURN FALSE;
                END;
              END;
            END;
          END;
        END;
      END;
    END;

    (* Protection Domain Transfer. *)
    IF AccessCheckT.DomainTransfer IN closure.kind THEN
      IF DebugOption.SecurityStatistics THEN
        Spy.Hit( spyDomain, 0, 1 );
      END;

      EVAL Mediate( sid, closure.sid, s, o );

      (* Get sid stack element. *)
      element := SIDStack.Dequeue( sidPool );
      IF element = NIL THEN
        IO.Put("Burp: we need more security id stack elements\n");
        FOR i := 1 TO SidPoolIncr - 1 DO
          SIDStack.Enqueue( NEW( SIDStackT ), sidPool );
        END;
        element := NEW( SIDStackT );
      END;
      (* Push new exception frame within old top of sid stack *)
      currentSid.eframe.next := RTThreadExtern.RTThread__handlerStack;
      RTThreadExtern.RTThread__handlerStack := ADR(currentSid.eframe);
      (* Push new security sid element *)
      element.s := s;
      element.o := o;
      SIDStack.Enqueue( element, currentSid );
    END;

    IF AccessCheckT.Audit IN closure.kind THEN
      Add2AuditLog( closure.binding, SecurityPolicy.AuditStatusT.NormalEnter );
    END;

    (* Done *)
    RETURN TRUE;
  END GuardAll;

PROCEDURE PostGuardAll( closure : ClosureT ) =
  VAR
    element : SIDStackT;
  BEGIN
    IF AccessCheckT.DomainTransfer IN closure.kind THEN
      IF DebugOption.SecurityStatistics THEN
        Spy.Hit( spyDomain, 0, 1 );
      END;

      (* Pop top from SID stack. *)
      element := SIDStack.Dequeue( currentSid );
      <*ASSERT element # NIL*>
      <*ASSERT currentSid # NIL*>
      SIDStack.Enqueue( element, sidPool );
      (* Restore exception frame *)
      <*ASSERT ADR(currentSid.eframe)=RTThreadExtern.RTThread__handlerStack*>
      RTThreadExtern.RTThread__handlerStack := currentSid.eframe.next;
    END;
    IF AccessCheckT.ResultCheck IN closure.kind THEN
      (* XXX fix me and do access check *)
    END;
    IF AccessCheckT.Audit IN closure.kind THEN
      Add2AuditLog( closure.binding, SecurityPolicy.AuditStatusT.NormalExit );
    END;
  END PostGuardAll;

PROCEDURE TypeCheck( binding    : Dispatcher.Binding;
                     inModes    : AccessMode.ListT;
                     resultMode : AccessMode.T;       ) : BOOLEAN =
  (*
     Type-check access modes versus event signature of "binding".
     Ignore "inModes" or "resultMode" if NIL. Type-checking means
     that any argument with a non-NIL access mode must be a traced
     reference. Returns TRUE iff access modes type-check.
   *)
  VAR
    proc        : PROCANY             := binding.getEvent();
    procType    : RTProcDesc.ProcType := RTProcDesc.GetType(proc);
    numArgs     : INTEGER := RTProcDesc.NumberOfArgs(procType);
    argType     : INTEGER;
    argTypeDefn : RT0.TypeDefn;
    count       : INTEGER;
  BEGIN
    (* Type-check "inModes" *)
    IF inModes # NIL THEN
      IF NUMBER(inModes^) # numArgs THEN RETURN FALSE; END;
      count := 1;
      FOR i := FIRST(inModes^) TO LAST(inModes^) DO
        IF inModes[i] # NIL THEN
          argType     := RTProcDesc.ArgType(procType, count);
          argTypeDefn := RTTypeSRC.FindType(argType);
          IF argTypeDefn = NIL OR argTypeDefn.traced # 1 THEN
            RETURN FALSE;
          END;
        END;
        INC(count);
      END;
    END;

    (* Type-check "resultMode" *)
    IF resultMode # NIL THEN
      argType     := RTProcDesc.ResultType(procType);
      argTypeDefn := RTTypeSRC.FindType(argType);
      IF argTypeDefn = NIL OR argTypeDefn.traced # 1 THEN
        RETURN FALSE;
      END;
    END;

    (* Done *)
    RETURN TRUE;
  END TypeCheck;

PROCEDURE MakeDereferenceList( binding : Dispatcher.Binding )
  : REF ARRAY OF BOOLEAN =
  (*
     Construct a list of BOOLEANs which has the same number of entries
     as there are arguments to the event signature. An entry in the
     list is TRUE iff the argument is a VAR or a READONLY.
   *)
  VAR
    proc        : PROCANY             := binding.getEvent();
    procType    : RTProcDesc.ProcType := RTProcDesc.GetType(proc);
    numArgs     : INTEGER := RTProcDesc.NumberOfArgs(procType);
    dereference : REF ARRAY OF BOOLEAN;
  BEGIN
    dereference := NEW( REF ARRAY OF BOOLEAN, numArgs );
    FOR i := 1 TO numArgs DO
      CASE RTProcDesc.ArgMode(procType, i) OF
      | RT0.FormalModeVALUE => dereference[i-1] := FALSE;
      | RT0.FormalModeVAR   => dereference[i-1] := TRUE;
      | RT0.FormalModeCONST => dereference[i-1] := TRUE;
      | RT0.FormalModeCVAR  => dereference[i-1] := TRUE;
      ELSE <* ASSERT FALSE *> (* Should never happen *)
      END;
    END;

    RETURN dereference;
  END MakeDereferenceList;

PROCEDURE FindProcedureSid( proc : PROCANY ) : SID
  RAISES { SecurityError.T} =
  (*
     For the given procedure "proc", find the domain it is in,
     and returns its security identifier.
   *)
  VAR
    domain : Domain.T;
    sidp   : RTTypeSecurity.SR;
  BEGIN
    (* Find interface domain for procedure *)
    TRY
      domain := DomainPrivate.AddressToDomain( proc, TRUE );
    EXCEPT
    | DomainPrivate.NonDomainAddress =>
      RAISE SecurityError.T( SecurityError.EC.Panic );
    END;

    (* Get SID from that domain *)
    sidp := RTTypeSecurity.Get( domain );
    IF sidp = NIL THEN
      RAISE SecurityError.T( SecurityError.EC.Panic );
    END;

    RETURN sidp.sid;
  END FindProcedureSid;

PROCEDURE CopyModes( oldList, newList : AccessMode.ListT ) 
  : AccessMode.ListT =
  (*
     Returns a copy of "newList", but overwrites the elements
     of "oldList" if it exists.
   *)
  VAR
    list : AccessMode.ListT;
  BEGIN
    IF newList = NIL THEN RETURN oldList; END;
    IF oldList = NIL THEN
      list := NEW(AccessMode.ListT, NUMBER(newList^));
    ELSE
      list := oldList;
    END;
    list^ := newList^;
    RETURN list;
  END CopyModes;

CONST
  USE_POST_GUARDS = TRUE;

PROCEDURE ImposeGuard( binding     : Dispatcher.Binding;
                       info        : BindingInfo.T;
                       kind        : AccessControlT;
                       linearize   : BOOLEAN := FALSE;
                       postclosure : BOOLEAN := FALSE;
                       guard       : PROCANY := NIL;
                       postguard   : PROCANY := NIL;     )
  RAISES { SecurityError.T } =
  (* 
     Impose the actual "guard" and "postguard".
     Must be called while "biLock" is held.
     Assumes that if "kind" is not "AccessControlT{}",
     "guard" is non-NIL and takes a closure.
   *)
  VAR
    closure  : ClosureT; (* The real closure      *)
    closure1 : ClosureT; (* Closure for guard     *)
    closure2 : ClosureT; (* Closure for postguard *)
  BEGIN
    (* Remove old security access checks *)
    (* XXX should be done atomically with installation of new guards *)
    IF info.kind # CHECK_NONE THEN
      TRY
        Dispatcher.UninstallGuard( info.guard );
      EXCEPT
      ELSE
        RAISE SecurityError.T(SecurityError.EC.Panic);
      END;
      IF ( NOT USE_POST_GUARDS )
        AND AccessCheckT.DomainTransfer IN info.kind THEN
        TRY
          Dispatcher.InstallResultHandler(binding.getEvent(),NIL,NIL);
        EXCEPT
        ELSE
          RAISE SecurityError.T(SecurityError.EC.Panic);
        END;
      END;
    END;

    IF kind = CHECK_NONE THEN
      info.kind := kind;
      RETURN;
    END;

    (* Construct closures for guard and postguard and linearize if necessary *)
    closure             := NEW( ClosureT );
    closure.sid         := FindProcedureSid(binding.getHandler());
    closure.kind        := kind;
    closure.executeMode := info.executeMode;
    IF AccessCheckT.ObjectCheck IN kind THEN
      closure.inModes     := CopyModes( NIL, info.inModes );
    END;
    closure.resultMode  := info.resultMode;
    closure.exceptMode  := info.exceptMode;
    closure.derefArgs   := info.derefArgs;
    closure.binding     := binding;

    IF linearize THEN
      VAR
        proc     : PROCANY             := binding.getEvent();
        procType : RTProcDesc.ProcType := RTProcDesc.GetType(proc);
        numArgs  : INTEGER := RTProcDesc.NumberOfArgs(procType);
      BEGIN
        TRY
          guard := Dispatcher.CreateLinear(guard,numArgs,closure);
        EXCEPT
          Dispatcher.Error =>
          BEGIN
            info.kind  := CHECK_NONE;
            info.guard := NIL;
            RAISE SecurityError.T(SecurityError.EC.InvalidBinding);
          END;
        END;
        closure1 := NIL;
      END;
    ELSE
      closure1 := closure;
    END;

    IF postclosure THEN
      closure2 := closure;
    ELSE
      closure2 := NIL;
    END;

    TRY
      IF USE_POST_GUARDS THEN
        info.guard := Dispatcher.ImposeGuard( binding,
                                              guard,
                                              closure1,
                                              postguard,
                                              closure2   );
      ELSE
        info.guard := Dispatcher.ImposeGuard( binding, guard, closure1 );
      END;
    EXCEPT
      Dispatcher.Error =>
      BEGIN
        info.kind  := CHECK_NONE;
        info.guard := NIL;
        RAISE SecurityError.T(SecurityError.EC.InvalidBinding);
      END;
    END;

    IF NOT USE_POST_GUARDS THEN
      TRY
        Dispatcher.InstallResultHandler( binding.getEvent(),
                                         postguard,
                                         NIL,
                                         closure2 );
      EXCEPT
        Dispatcher.Error =>
        BEGIN
          TRY
            info.kind  := CHECK_NONE;
            Dispatcher.UninstallGuard(info.guard);
            info.guard := NIL;
          EXCEPT
          ELSE (* Ignore *)
          END;
          RAISE SecurityError.T(SecurityError.EC.InvalidBinding);
        END;
      END;
    END;
    
    (* Update house-keeping information *)
    info.kind := kind;
  END ImposeGuard;

PROCEDURE SetAccessControl( binding     : Dispatcher.Binding;
                            kind        : AccessControlT;
                            executeMode : AccessMode.T     := NIL;
                            inModes     : AccessMode.ListT := NIL;
                            resultMode  : AccessMode.T     := NIL;
                            exceptMode  : AccessMode.T     := NIL; )
  RAISES { SecurityError.T } =
  (*
     Impose access control of sort "kind" onto "binding".

     Raises "SecurityError.EC.InvalidBinding" if "binding" is illegal.
     Raises "SecurityError.EC.InvalidCheck" if "kind" is illegal for
     "binding". E.g., an access mode is provided for a non-reference
     argument (see below).

     If "kind" includes "AccessCheckT.CodeCheck", "executeMode"
     determines the required access mode for the code itself.

     If "kind" includes "AccessCheckT.ObjectCheck",
     . "inModes" denotes the required access modes for each argument
       in order of the procedure arguments, to be checked before the
       procedure is executed.
     If "kind" includes "AccessCheckT.ResultCheck",
     . "resultMode" denotes the required access mode for the result,
       to be checked after the procedure is executed.
     If "kind" includes "AccessCheckT.ExceptCheck",
     . "exceptMode" denotes the required access mode for exception
       arguments, to be checked when an exception is raised.
     Note that
     . there is no list of access modes for outgoing arguments (i.e.,
       for VARs), since VARs rely on shared memory with no implicit
       concurrency control (a return value is only visible to the
       caller, after the call completes).
     If the access mode for a given object is NIL, no access check is
     performed for this argument. Access modes for non-referenced
     arguments must be NIL (since they can not be associated with
     security identifiers).
     If "executeMode", "inModes", "resultMode", or "exceptMode is NIL,
     but has been non-NIL on a prior call to "SetAccessControl", the
     old access modes are used. As a result, the policy server can
     disable and enable access control checks without providing access
     modes, once the access modes have been established with the
     security manager.
   *)
  VAR
    info    : BindingInfo.T;
  BEGIN
    IF NOT DebugOption.Security THEN
      RAISE SecurityError.T(SecurityError.EC.Disabled);
    END;

    (* REF ARRAY contents are not thread-safe, so we must copy inModes *)
    IF inModes # NIL THEN
      inModes := CopyModes(NIL, inModes);
    END;

    (* Type-check access modes *)
    IF inModes # NIL OR resultMode # NIL THEN
      IF NOT TypeCheck( binding, inModes, resultMode ) THEN
        RAISE SecurityError.T(SecurityError.EC.InvalidCheck);
      END;
    END;

    LOCK biLock DO

      (* What's there already? *)
      IF biTable.get( binding, info ) THEN

        (* Update entry *)
        IF executeMode # NIL THEN info.executeMode := executeMode; END;
        info.inModes  := CopyModes( info.inModes,  inModes  );
        IF resultMode  # NIL THEN info.resultMode  := resultMode;  END;
        IF exceptMode  # NIL THEN info.exceptMode  := exceptMode;  END;

      ELSE

        (* Create new entry *)
        info := NEW( BindingInfo.T,
                     kind        := CHECK_NONE,
                     executeMode := executeMode,
                     inModes     := CopyModes(NIL, inModes),
                     resultMode  := resultMode,
                     exceptMode  := exceptMode,
                     derefArgs   := MakeDereferenceList(binding),
                     guard       := NIL );
        EVAL biTable.put( binding, info );

      END;

      (* Minimize kind *)
      IF AccessCheckT.CodeCheck IN kind AND info.executeMode = NIL THEN
        kind := kind - CHECK_X;
      END;
      IF AccessCheckT.ObjectCheck IN kind THEN
        VAR
          needsCheck : BOOLEAN := FALSE;
        BEGIN
          IF info.inModes # NIL THEN
            FOR i := FIRST(info.inModes^) TO LAST(info.inModes^) DO
              IF info.inModes[i] # NIL THEN
                needsCheck := TRUE;
                EXIT;
              END;
            END;
          END;
          IF NOT needsCheck THEN
            kind := kind - CHECK_O;
          END;
        END;
      END;
      IF AccessCheckT.ResultCheck IN kind AND info.resultMode = NIL THEN
        kind := kind - AccessControlT{AccessCheckT.ResultCheck};
      END;
      IF AccessCheckT.ExceptCheck IN kind AND info.exceptMode = NIL THEN
        kind := kind - AccessControlT{AccessCheckT.ExceptCheck};
      END;

      (* Add the real work *)
      
      IF kind = CHECK_NONE THEN
        (* No access checks *)
        ImposeGuard( binding, info, kind, FALSE, FALSE,
                     NIL, NIL                           );
      ELSIF kind = CHECK_D THEN
        IF info.kind = kind THEN RETURN; END;
        ImposeGuard( binding, info, kind, FALSE, FALSE,
                     GuardDomain, PostGuardDomain       );
      ELSIF kind = CHECK_X THEN
        ImposeGuard( binding, info, kind, FALSE, FALSE,
                     GuardCode, NIL                     );
      ELSIF kind = CHECK_XD THEN
        ImposeGuard( binding, info, kind, FALSE, FALSE,
                     GuardCodeDomain, PostGuardDomain   );
      ELSIF kind = CHECK_O THEN
        ImposeGuard( binding, info, kind, TRUE, FALSE,
                     GuardObject, NIL                   );
      ELSIF kind = CHECK_OD THEN
        ImposeGuard( binding, info, kind, TRUE, FALSE,
                     GuardObjectDomain, PostGuardDomain );
      ELSE
        ImposeGuard( binding, info, kind, TRUE, TRUE,
                     GuardAll, PostGuardAll             );
      END;
    END; (* LOCK *)
  END SetAccessControl;

PROCEDURE GetAccessControl(     binding     : Dispatcher.Binding;
                            VAR executeMode : AccessMode.T;
                            VAR inModes     : AccessMode.ListT;
                            VAR resultMode  : AccessMode.T;
                            VAR exceptMode  : AccessMode.T;       )
  : AccessControlT =
  (* Returns the current sort of access control checks on "binding". *)
  VAR
    info : BindingInfo.T;
  BEGIN
    LOCK biLock DO
      IF biTable.get( binding, info ) THEN
        executeMode := info.executeMode;
        IF info.inModes # NIL THEN
          inModes    := NEW(AccessMode.ListT, NUMBER(info.inModes^));
          inModes^   := info.inModes^;
        ELSE
          inModes    := NIL;
        END;
        resultMode  := info.resultMode;
        exceptMode  := info.exceptMode;
        RETURN info.kind;
      ELSE
        executeMode := NIL;
        inModes     := NIL;
        resultMode  := NIL;
        exceptMode  := NIL;
        RETURN AccessControlT{};
      END;
    END;
  END GetAccessControl;

PROCEDURE SetTypeSecurity( tc     : RTType.Typecode;
                           secure : BOOLEAN          ) =
  (*
     Set security attribute of type with typecode "tc" to "secure".
     If a type is secure, all instances are associated
     with a security identifier on instance creation.
     Raises "SecurityError.EC.InvalidType" if "tc" does not denote
     a reference type.
   *)
  BEGIN
    (* XXX add type check and DebugOption.Security *)
    RTTypeSecurity.SetTypeSecurity( tc, secure );
  END SetTypeSecurity;

PROCEDURE GetTypeSecurity( tc : RTType.Typecode ) : BOOLEAN =
  (* Get security attribute of type with typecode "tc". *)
  BEGIN
    RETURN RTTypeSecurity.IsSecure( tc );
  END GetTypeSecurity;


(*
   Security id management
  ------------------------
 *)

PROCEDURE GetCurrentSubjectSid() : SID =
  (* Get current subject security id. *)
  BEGIN
    IF NOT DebugOption.Security THEN RETURN AnonSid; END;
    RETURN currentSid.s;
  END GetCurrentSubjectSid;

PROCEDURE GetSubjectSid( s : Strand.T ) : SID =
  (* Get subject security id. *)
  BEGIN
    IF NOT DebugOption.Security THEN RETURN AnonSid; END;
    IF s = Strand.GetCurrent() THEN
      RETURN currentSid.s;
    ELSE
      RETURN s.sid.s;
    END;
  END GetSubjectSid;

PROCEDURE GetCurrentObjectSid() : SID =
  (* Get current default object security id. *)
  BEGIN
    IF NOT DebugOption.Security THEN RETURN AnonDataSid; END;
    RETURN currentSid.o;
  END GetCurrentObjectSid;

PROCEDURE GetObjectSid( s : Strand.T ) : SID =
  (* Get default object security id. *)
  BEGIN
    IF NOT DebugOption.Security THEN RETURN AnonDataSid; END;
    IF s = Strand.GetCurrent() THEN
      RETURN currentSid.o;
    ELSE
      RETURN s.sid.o;
    END;
  END GetObjectSid;

PROCEDURE SetCurrentObjectSid( sid : SID ) RAISES { SecurityError.T } =
  (*
     Set default object security security id to "sid".
     Raises "SecurityError.EC.Unauthorized"
     if subject is unauthorized for this "sid".
     Note that the subject security id is implicitly managed
     by the security manager and can not be changed.
   *)
  BEGIN
    SetObjectSid( Strand.GetCurrent(), sid );
  END SetCurrentObjectSid;

PROCEDURE SetObjectSid( s : Strand.T; sid : SID ) RAISES { SecurityError.T } =
  (*
     Set default object security security id to "sid".
     Raises "SecurityError.EC.Unauthorized"
     if subject is unauthorized for this "sid".
     Note that the subject security id is implicitly managed
     by the security manager and can not be changed.
   *)
  VAR
    legal   : BOOLEAN := FALSE;
    timeout : INTEGER;
    realSid : SIDStackT;
  BEGIN
    IF NOT DebugOption.Security THEN
      RAISE SecurityError.T(SecurityError.EC.Disabled);
    END;

    IF s = Strand.GetCurrent() THEN
      realSid := currentSid;
    ELSE
      realSid := s.sid;
    END;
    EVAL SecurityPolicy.MediateObjectDefault( realSid.s,
                                              sid,
                                              legal,
                                              timeout );
    IF legal THEN
      realSid.o := sid;
    ELSE
      RAISE SecurityError.T(SecurityError.EC.Unauthorized);
    END;
  END SetObjectSid;

PROCEDURE GetSid( o : REFANY ) : SID RAISES { SecurityError.T } =
  (*
     Get the security identifier associated with object "o".
     Raise "SecurityError.EC.InvalidType" if the objects
     type is insecure.
   *)
  VAR
    sidp : RTTypeSecurity.SR;
  BEGIN
    sidp := RTTypeSecurity.Get( o );
    IF sidp # NIL THEN
      RETURN sidp.sid;
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidType);
    END;
  END GetSid;

PROCEDURE SetSid( o : REFANY; sid : SID ) RAISES { SecurityError.T } =
  (*
     Set the security identifier associated with object "o".
     Raise "SecurityError.EC.InvalidType" if the objects type
     is insecure. This procedure is part of the
     "SecurityManagerPrivate" interface and should only be used
     by a trusted service that maps objects to security identifiers,
     possibly at a fine granularity than the default object security
     identifier provides.
   *)
  VAR
    sidp : RTTypeSecurity.SR;
  BEGIN
    sidp := RTTypeSecurity.Get( o );
    IF sidp # NIL THEN
      sidp.sid := sid;
    ELSE
      RAISE SecurityError.T(SecurityError.EC.InvalidType);
    END;
  END SetSid;

PROCEDURE GetSidName( sid : SID ) : TEXT RAISES { SecurityError.T } =
  (*
     Turn "sid" into human-readable form;
     raise "SecurityError.EC.InvalidId" if invalid id.
   *)
  BEGIN
    RETURN SecurityPolicy.GetSidName( sid );
  END GetSidName;


(*
    Mediation
   -----------
 *)

PROCEDURE Mediate( k1, k2 : SID; VAR s, o : SID ) : AccessMode.T =
  (*
     Given the two security identifiers "k1" and "k2",
     return the currently legal access mode,
     as well as the subject and object sids.
   *)
  VAR
    index   : INTEGER;
    entry   : MediationCacheT;
    mode    : AccessMode.T;
    s2, o2  : SID;
    timeout : INTEGER;
    cache   : BOOLEAN;
  BEGIN
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE; (* Set usage flag *)
          s := entry.s;
          o := entry.o;
          RETURN entry.mode;
        END;
        entry := entry.next;
      END;
    END;

    (*
       Access mode is not in cache, go ask security policy.
       We release the lock onto the mediation cache,
       since the policy server might be rather slow.
     *)
    cache := SecurityPolicy.Mediate( k1, k2, mode, s2, o2, timeout );

    (* Add to mediation cache if allowed *)
    IF cache THEN AddToMediationCache( k1, k2, mode, s2, o2, timeout ); END;

    (* Done *)
    s := s2;
    o := o2;
    RETURN mode; 
  END Mediate;

PROCEDURE CheckPermissions( k2 : SID; m : AccessMode.T ) : BOOLEAN =
  (*
     Given the object SID "k2" and the access mode "m" required for an
     operation on that object, determine if the current subject is
     allowed to perform that operation. This interface is provided
     to perform an explicit check efficiently.
   *)
  VAR
    k1      : SID;
    index   : INTEGER;
    entry   : MediationCacheT;
    mode    : AccessMode.T;
    s, o    : SID;
    timeout : INTEGER;
    cache   : BOOLEAN;
    found   : BOOLEAN := FALSE;
  BEGIN
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyExplicit, 0, 1 );
    END;

    (* Get SIDs *)
    k1    := currentSid.s;

    (* Get mediation cache entry *)
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE; (* Set usage flag *)
          mode       := entry.mode;
          found      := TRUE;
          EXIT;
        END;
        entry := entry.next;
      END;
    END;

    IF NOT found THEN
      (* Entry is not in cache, go ask policy. *)
      cache := SecurityPolicy.Mediate( k1, k2, mode, s, o, timeout );
      IF cache THEN AddToMediationCache( k1, k2, mode, s, o, timeout ); END;
    END;

    (* Check access modes *)
    IF    ( NOT mode.simple >= m.simple )
       OR (     m.permissions # NIL
            AND ( NOT PermissionContain( mode, m ) ) ) THEN
      RETURN FALSE;
    END;

    RETURN TRUE;
  END CheckPermissions;

PROCEDURE CheckSimplePermissions( k2 : SID; simple : AccessMode.SimpleT )
  : BOOLEAN =
  (*
     Given the object SID "k2" and the simple permissions "simple" required
     for an operation on that object, determine if the current subject
     is allowed to perform that operation. This interface is provided
     to perform an explicit check efficiently.
   *)
  VAR
    k1      : SID;
    index   : INTEGER;
    entry   : MediationCacheT;
    mode    : AccessMode.T := NIL;
    s, o    : SID;
    timeout : INTEGER;
    cache   : BOOLEAN;
    found   : BOOLEAN;
  BEGIN
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyExplicit, 0, 1 );
    END;

    (* Get SIDs *)
    k1    := currentSid.s;

    (* Get mediation cache entry *)
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE; (* Set usage flag *)
          mode       := entry.mode;
          found      := TRUE;
          EXIT;
        END;
        entry := entry.next;
      END;
    END;

    IF NOT found THEN
      (* Entry is not in cache, go ask policy. *)
      cache := SecurityPolicy.Mediate( k1, k2, mode, s, o, timeout );
      IF cache THEN AddToMediationCache( k1, k2, mode, s, o, timeout ); END;
    END;

    RETURN mode.simple >= simple;
  END CheckSimplePermissions;

PROCEDURE CheckObjectPermissions( obj : REFANY; m : AccessMode.T ) : BOOLEAN =
  (*
     Given the object "obj" and the access mode "m" required for an
     operation on that object, determine if the current subject is
     allowed to perform that operation. This interface is provided
     to perform an explicit check efficiently.
   *)
  VAR
    k1, k2  : SID;
    index   : INTEGER;
    entry   : MediationCacheT;
    mode    : AccessMode.T;
    s, o    : SID;
    timeout : INTEGER;
    cache   : BOOLEAN;
    found   : BOOLEAN := FALSE;
    sidp    : RTTypeSecurity.SR;
  BEGIN
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyExplicit, 0, 1 );
    END;

    (* Get SIDs *)
    k1    := currentSid.s;
    sidp  := RTTypeSecurity.Get( obj );
    IF sidp = NIL THEN RETURN FALSE; END;
    k2    := sidp.sid;

    (* Get mediation cache entry *)
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE; (* Set usage flag *)
          mode       := entry.mode;
          found      := TRUE;
          EXIT;
        END;
        entry := entry.next;
      END;
    END;

    IF NOT found THEN
      (* Entry is not in cache, go ask policy. *)
      cache := SecurityPolicy.Mediate( k1, k2, mode, s, o, timeout );
      IF cache THEN AddToMediationCache( k1, k2, mode, s, o, timeout ); END;
    END;

    (* Check access modes *)
    IF    ( NOT mode.simple >= m.simple )
       OR (     m.permissions # NIL
            AND ( NOT PermissionContain( mode, m ) ) ) THEN
      RETURN FALSE;
    END;

    RETURN TRUE;
  END CheckObjectPermissions;

PROCEDURE CheckObjectSimplePermissions( obj    : REFANY;
                                        simple : AccessMode.SimpleT )
  : BOOLEAN =
  (*
     Given the object "obj" and the simple permissions "simple" required
     for an operation on that object, determine if the current subject
     is allowed to perform that operation. This interface is provided
     to perform an explicit check efficiently.
   *)
  VAR
    k1, k2  : SID;
    index   : INTEGER;
    entry   : MediationCacheT;
    mode    : AccessMode.T := NIL;
    s, o    : SID;
    timeout : INTEGER;
    cache   : BOOLEAN;
    found   : BOOLEAN;
    sidp    : RTTypeSecurity.SR;
  BEGIN
    IF DebugOption.SecurityStatistics THEN
      Spy.Hit( spyExplicit, 0, 1 );
    END;

    (* Get SIDs *)
    k1    := currentSid.s;
    sidp  := RTTypeSecurity.Get( obj );
    IF sidp = NIL THEN RETURN FALSE; END;
    k2    := sidp.sid;

    (* Get mediation cache entry *)
    index :=   Word.LeftShift( Word.And( k1, mcMask1 ), mcShift1 )
             + Word.And( k2, mcMask2 );
    LOCK mcLock DO
      entry := mcHashTable[index];
      WHILE entry # NIL DO
        IF entry.k1 = k1 AND entry.k2 = k2 THEN
          entry.used := TRUE; (* Set usage flag *)
          mode       := entry.mode;
          found      := TRUE;
          EXIT;
        END;
        entry := entry.next;
      END;
    END;

    IF NOT found THEN
      (* Entry is not in cache, go ask policy. *)
      cache := SecurityPolicy.Mediate( k1, k2, mode, s, o, timeout );
      IF cache THEN AddToMediationCache( k1, k2, mode, s, o, timeout ); END;
    END;

    RETURN mode.simple >= simple;
  END CheckObjectSimplePermissions;


(*
    Additional cache support
   --------------------------

   Legal default security ids and user to groups mappings
   are not currently cached in the security manager,
   but may very well be in the future...
 *)

PROCEDURE ClearObjectCache() =
  (* Clear the object default security id cache. *)
  BEGIN
  END ClearObjectCache;

PROCEDURE ClearObjectCacheEntry( <*UNUSED*> k1, k2 : SID ) : BOOLEAN =
  (*
     Delete entry for the pair of SIDs "k1", "k2"
     from object default security id cache.
     Returns true iff such entry was in the cache.
   *)
  BEGIN
    RETURN FALSE;
  END ClearObjectCacheEntry;

PROCEDURE SetObjectCacheSize( size : INTEGER ) RAISES { SecurityError.T } =
  (*
     Set "size" of object default security id cache,
     where "size" is a power of two.
     Raises "SecurityError.EC.BadNumber" if "size" is not a power of two.
   *)
  BEGIN
    IF NOT PowerOfTwo( size ) THEN
      RAISE SecurityError.T(SecurityError.EC.BadNumber);
    END;
  END SetObjectCacheSize;

PROCEDURE ClearGroupCache() =
  (* Clear the user to groups cache. *)
  BEGIN
  END ClearGroupCache;

PROCEDURE ClearGroupCacheEntry( <*UNUSED*> uid : UID ) : BOOLEAN =
  (*
     Delete entry for the user id from groups cache.
     Returns true iff such entry was in the cache.
   *)
  BEGIN
    RETURN FALSE;
  END ClearGroupCacheEntry;

PROCEDURE SetGroupCacheSize( size : INTEGER )
  RAISES { SecurityError.T } =
  (*
     Set "size" of groups cache, where "size" is a power of two.
     Raises "SecurityError.EC.BadNumber" if "size" is not a power of two.
   *)
  BEGIN
    IF NOT PowerOfTwo( size ) THEN
      RAISE SecurityError.T(SecurityError.EC.BadNumber);
    END;
  END SetGroupCacheSize;


(*
    Auditing
   ----------
 *)

VAR
  alLog   : SecurityPolicy.AuditLogT; (* Audit log         *)
  alLock  : MUTEX;                    (* Lock for log      *)
  alSize  : INTEGER := 64;            (* Size of log       *)
  alIndex : INTEGER := 0;             (* Next free element *)

PROCEDURE SetAuditLogSize( size : INTEGER ) RAISES { SecurityError.T } =
  (* Set "size" (which must be greater 0) of audit log. *)
  BEGIN
    IF size = alSize THEN RETURN; END;
    IF size <= 0     THEN
      RAISE SecurityError.T(SecurityError.EC.BadNumber);
    END;
    LOCK alLock DO
      SecurityPolicy.FlushAuditLog( alLog, alIndex );
      alSize  := size;
      alLog   := NEW( SecurityPolicy.AuditLogT, alSize );
      alIndex := 0;
    END;
  END SetAuditLogSize;      

PROCEDURE GetAuditLogSize() : INTEGER =
  (* Get size of audit log. *)
  BEGIN
    RETURN alSize;
  END GetAuditLogSize;

PROCEDURE Add2AuditLog( binding : Dispatcher.Binding;
                        status  : SecurityPolicy.AuditStatusT; ) =
  (* Add entry to audit log. *)
  BEGIN
    LOCK alLock DO
      IF alIndex = alSize THEN
        SecurityPolicy.FlushAuditLog( alLog, alIndex );
        alIndex := 0;
      END;
      WITH entry = alLog[alIndex] DO
        entry.subject := currentSid.s;
        entry.user    := Strand.GetCurrent().context.user;
        entry.binding := binding;
        entry.time    := Clock.ReadTicks();
        entry.status  := status;
      END;
      INC( alIndex );
    END;
  END Add2AuditLog;


(*
    Initialization
   ----------------
 *)

VAR
  once : BOOLEAN := FALSE;

PROCEDURE Init( verbose : BOOLEAN := FALSE ) =
  (*
     Initialize the security manager at boot time.
     Must be called before the SPIN threads and scheduler are activated.
     This call is currently located in "kernel/start/src/Boot.m3".
   *)
  VAR
    mcEntry : MediationCacheT;
  BEGIN
    IF NOT once THEN
      once := TRUE;
      (* Initialize mediation cache *)
      mcLock      := NEW( MUTEX );
      mcHashTable := NEW( REF ARRAY OF MediationCacheT, mcTotalSize );
      FOR i := 1 TO mcTotalSize DO
        mcEntry    := NEW( MediationCacheT, next := mcFreeList );
        mcFreeList := mcEntry;
      END;
      (* Initialize security stack pool *)
      FOR i := 1 TO SidPoolInit DO
        SIDStack.Enqueue( NEW(SIDStackT), sidPool );
      END;
      (* Initialize dispatcher binding to internal info table *)
      biLock  := NEW( MUTEX );
      biTable := NEW( RefBindingInfoTbl.Default ).init();
      (* Initialize spys *)
      IF DebugOption.SecurityStatistics THEN
        spyDomain   := Spy.Create("Security - Domain change");
        spyCode     := Spy.Create("Security - Code check");
        spyObject   := Spy.Create("Security - Object check");
        spyExplicit := Spy.Create("Security - Explicit check");
      END;
      (* Initialize audit log *)
      alLock := NEW( MUTEX );
      alLog  := NEW( SecurityPolicy.AuditLogT, alSize );
      (* Initialize access control on SPIN domains *)
      IF RTHeapRep.TypeSecurityOn THEN
        SetTypeSecurity( TYPECODE(Domain.T), TRUE );
        (* XXX Impose access checks here *)
      END;
      (* Done, print message if desired. *)
      IF verbose THEN
        IO.Put("Initialized security manager.\n");
      END;
    END;
  END Init;

PROCEDURE Reset() =
  (*
     Reset all state within the security manager.
     To be called when a new security policy is installed,
     to clear the state of the previous policy.
   *)
  BEGIN
    (* Clear all caches *)
    ClearMediationCache();
    ClearObjectCache();
    ClearGroupCache();
    (* Clear audit log *)
    LOCK alLock DO
      SecurityPolicy.FlushAuditLog( alLog, alIndex );
    END;
  END Reset;


(*
    State Information 
   -------------------

    Two booleans are used to control whether security
    is activated. One controls whether type security is
    enabled and is returned by "CheckTypeSecurity". The
    other is "DebugOption.Security" and controls whether
    threads are associated with security information.
    The public interface "CheckSecurity" returns the
    logical AND of both booleans.
 *)

PROCEDURE CheckTypeSecurity() : BOOLEAN =
  (* Returns "TRUE" iff type security is activated. *)
  BEGIN
    RETURN RTHeapRep.TypeSecurityOn;
  END CheckTypeSecurity;

PROCEDURE CheckSecurity() : BOOLEAN =
  (* Returns "TRUE" iff security is activated. *)
  BEGIN
    RETURN RTHeapRep.TypeSecurityOn AND DebugOption.Security;
  END CheckSecurity;


(*
    Debugging
   -----------
 *)

PROCEDURE DumpCurrentSIDStack( wr : Wr.T := NIL) =
  (* Print debugging information about the current security state. *)
  VAR
    element : SIDStackT := currentSid;
  BEGIN
    IO.Put("Current sid stack: \n", wr);
    WHILE element # NIL DO
      IO.Put("   s-sid " & Fmt.Int(element.s)
             & "  o-sid " & Fmt.Int(element.o),
             wr);
      IO.Put("  at 0x" & Fmt.Unsigned(SafeConvert.AdrToWord(element), 16) &
             "\n", wr);
      element := element.nextelem;
    END;
  END DumpCurrentSIDStack;


(*
    My little helpers
   -------------------
 *)

PROCEDURE PowerOfTwo( number : INTEGER ) : BOOLEAN = 
  (* Is "number" a positive power of two ? *)
  VAR
    n : Word.T := number;
  BEGIN
    IF n <= 0 THEN RETURN FALSE; END;
    (*
       Shift out all zeros.

       The loop terminates because the number
       is guaranteed to be a positive integer.
     *)
    WHILE Word.And( n, 1 ) = 0 DO
      n := Word.RightShift( n, 1 );
    END;
    (* Must be one now *)
    RETURN n = 1;
  END PowerOfTwo;

PROCEDURE LogTwo( number : INTEGER ) : INTEGER =
  (*
     Return the log base two of "number",
     where "number" is a positive power of two.
   *)
  VAR
    n   : Word.T  := number;
    log : INTEGER := 0;
  BEGIN
    (* Loop wont terminate for zero!!*)
    WHILE Word.And( n, 1 ) = 0 DO
      n := Word.RightShift( n, 1 );
      INC(log);
    END;
    RETURN log;
  END LogTwo;

BEGIN
END SecurityManager.

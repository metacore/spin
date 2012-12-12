(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Last modified on Fri Jan  6 11:35:48 PST 1995 by kalsow         *)
(*      modified on Tue Oct  4 10:34:00 PDT 1994 by isard          *)
(*      modified on Tue May  4 10:20:03 PDT 1993 by mjordan        *)
(*      modified on Wed Apr 21 16:31:21 PDT 1993 by mcjones        *)
(*      modified on Fri Mar 26 15:04:39 PST 1993 by birrell        *)

UNSAFE MODULE ThreadWin32
  EXPORTS Scheduler, Thread, ThreadF, RTThreadInit, RTHooks;

IMPORT RTHeapRep, RTLinker, RTMisc, WinBase, WinDef, WinNT, ThreadContext;
IMPORT Word;

(*----------------------------------------- Exceptions, types and globals ---*)

VAR
  cm: WinBase.LPCRITICAL_SECTION;
    (* Global lock for internals of Mutex and Condition *)

  default_stack: WinDef.DWORD := 16384;

REVEAL
  Mutex = BRANDED "MUTEX Win32-1.0" OBJECT
      cs: WinBase.LPCRITICAL_SECTION := NIL;
      held: BOOLEAN := FALSE;
        (* LL = self.cs *)
        (* Because critical sections are thread re-entrant *)
    END;

  Condition = BRANDED "Thread.Condition Win32-1.0" OBJECT
      waiters: T := NIL;
        (* LL = cm *)
        (* List of threads waiting on this CV. *)
    END;

  T = BRANDED "Thread.T Win32-1.0" OBJECT
      next, prev: T := NIL;
        (* LL = threadMu; global doubly-linked, circular list of all threads *)
      nextIdle: T := NIL;
        (* LL = threadMu; global list of idle threads *)
      handle: WinNT.HANDLE := NIL;
        (* LL = threadMu; thread handle in Windows *)
      stackbase: ADDRESS := NIL;
        (* LL = threadMu; base of thread stack for use by GC *)
      closure: Closure := NIL;
        (* LL = threadMu *)
      result: REFANY := NIL;
        (* LL = threadMu;  if not self.completed, used only by self;
           if self.completed, read-only. *)
      cond: Condition;
        (* LL = threadMu; wait here to join, or for rebirth *)
      waitingOn: Condition := NIL;
        (* LL = cm; CV that we're blocked on *)
      nextWaiter: T := NIL;
        (* LL = cm; queue of threads waiting on the same CV *)
      waitSema: WinNT.HANDLE := NIL;
        (* binary semaphore for blocking during "Wait" *)
      alertable: BOOLEAN := FALSE;
        (* LL = cm; distinguishes between "Wait" and "AlertWait" *)
      alerted: BOOLEAN := FALSE;
        (* LL = cm; the alert flag, of course *)
      completed: BOOLEAN := FALSE;
        (* LL = threadMu; indicates that "result" is set *)
      joined: BOOLEAN := FALSE;
        (* LL = threadMu; "Join" or "AlertJoin" has already returned *)
    END;

(*------------------------------------------- Caches of critical sections ---*)

CONST
  CSectCacheSize = 20;
    (* Everything should work OK if these are 0 *)

VAR
  cSectCache: ARRAY [0..CSectCacheSize-1] OF WinBase.LPCRITICAL_SECTION;
  cSectCacheContents := 0;

PROCEDURE AllocCSect(m: Mutex) =
    (* LL = 0 *)
    (* If we can take a critical section from the cache, 
       do so; otherwise create it. In any case, register the containing
       Mutex with the GC so that we can clean-up on de-allocation. *)
  VAR mcs: WinBase.LPCRITICAL_SECTION := NIL;  lost_race := FALSE;
  BEGIN
    WinBase.EnterCriticalSection(cm);
      IF cSectCacheContents > 0 THEN
        DEC(cSectCacheContents);
        m.cs := cSectCache[cSectCacheContents];
      ELSE
        WinBase.LeaveCriticalSection(cm);
          mcs := NEW(WinBase.LPCRITICAL_SECTION);
        WinBase.EnterCriticalSection(cm);
        IF (m.cs = NIL) THEN
          m.cs := mcs;
          WinBase.InitializeCriticalSection(m.cs);
        ELSE
          (* somebody else beat us thru the preceding NEW *)
          lost_race := TRUE;
        END;
      END;
    WinBase.LeaveCriticalSection(cm);

    IF lost_race
      THEN DISPOSE (mcs);
      ELSE RTHeapRep.RegisterFinalCleanup(m, FreeCSect);
    END;
  END AllocCSect;

PROCEDURE FreeCSect(r: REFANY (*Mutex*) ) =
    (* LL < cm *)
    (* Must not dereference any traced REF when called from GC *)
    VAR m: Mutex := r;
  BEGIN
    WinBase.EnterCriticalSection(cm);
    IF m.cs # NIL THEN
      IF cSectCacheContents < CSectCacheSize THEN
        cSectCache[cSectCacheContents] := m.cs;
        INC(cSectCacheContents);
      ELSE
        DISPOSE(m.cs);
      END;
      m.cs := NIL;
    END;
    WinBase.LeaveCriticalSection(cm)
  END FreeCSect;

(*----------------------------------------------------------------- Mutex ---*)
(* Note: RTHooks.{Unlock,Lock}Mutex are the routines called directly by
   the compiler.  Acquire and Release are the routines exported through
   the Thread interface *)
         
PROCEDURE Acquire (m: Mutex) =
  BEGIN
    LockMutex (m);
  END Acquire;

PROCEDURE Release (m: Mutex) =
  BEGIN
    UnlockMutex (m);
  END Release;

PROCEDURE (*RTHooks.*)LockMutex (m: Mutex) =
  BEGIN
    IF (m.cs = NIL) THEN AllocCSect(m); END;
    WinBase.EnterCriticalSection(m.cs);
    IF m.held THEN Die("attempt to lock mutex already locked by self") END;
    m.held := TRUE;
  END LockMutex;

PROCEDURE (*RTHooks.*)UnlockMutex(m: Mutex) =
  BEGIN
    IF NOT m.held THEN Die("attempt to release an unlocked mutex") END;
    m.held := FALSE;
    WinBase.LeaveCriticalSection(m.cs);
  END UnlockMutex;

(*---------------------------------------- Condition variables and Alerts ---*)

PROCEDURE InnerWait(m: Mutex; c: Condition; self: T) =
    (* LL = cm+m on entry; LL = m on exit *)
  BEGIN
    <* ASSERT( (self.waitingOn=NIL) AND (self.nextWaiter=NIL) ) *>
    self.waitingOn := c;
    self.nextWaiter := c.waiters;
    c.waiters := self;
    WinBase.LeaveCriticalSection(cm);
    UnlockMutex(m);
    IF WinBase.WaitForSingleObject(self.waitSema, WinBase.INFINITE) # 0 THEN
      Choke();
    END;
    LockMutex(m);
  END InnerWait;

PROCEDURE InnerTestAlert(self: T) RAISES {Alerted} =
  (* LL = cm on entry; LL = cm on normal exit, 0 on exception exit *)
  (* If self.alerted, clear "alerted", leave cm and raise
     "Alerted". *)
  BEGIN
    IF self.alerted THEN
      self.alerted := FALSE;
      WinBase.LeaveCriticalSection(cm);
      RAISE Alerted
    END;
  END InnerTestAlert;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die("AlertWait called from non-Modula-3 thread") END;
    WinBase.EnterCriticalSection(cm);
    InnerTestAlert(self);
    self.alertable := TRUE;
    InnerWait(m, c, self);
    WinBase.EnterCriticalSection(cm);
    InnerTestAlert(self);
    WinBase.LeaveCriticalSection(cm);
  END AlertWait;

PROCEDURE Wait (m: Mutex; c: Condition) =
  (* LL = m *)
  VAR self := Self();
  BEGIN
    IF self = NIL THEN Die("Wait called from non-Modula-3 thread") END;
    WinBase.EnterCriticalSection(cm);
    InnerWait(m, c, self);
  END Wait;

PROCEDURE DequeueHead(c: Condition) =
  (* LL = cm *)
  VAR t: T; prevCount: WinDef.LONG;
  BEGIN
    t := c.waiters; c.waiters := t.nextWaiter;
    t.nextWaiter := NIL;
    t.waitingOn := NIL;
    t.alertable := FALSE;
    IF WinBase.ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
      Choke();
    END;
  END DequeueHead;

PROCEDURE Signal (c: Condition) =
  BEGIN
    WinBase.EnterCriticalSection(cm);
    IF c.waiters # NIL THEN DequeueHead(c) END;
    WinBase.LeaveCriticalSection(cm);
  END Signal;

PROCEDURE Broadcast (c: Condition) =
  BEGIN
    WinBase.EnterCriticalSection(cm);
    WHILE c.waiters # NIL DO DequeueHead(c) END;
    WinBase.LeaveCriticalSection(cm);
  END Broadcast;

PROCEDURE Alert(t: T) =
    VAR prevCount: WinDef.LONG; prev, next: T;
  BEGIN
    IF t = NIL THEN Die("Alert called from non-Modula-3 thread") END;
    WinBase.EnterCriticalSection(cm);
    t.alerted := TRUE;
    IF t.alertable THEN
      (* Dequeue from any CV and unblock from the semaphore *)
      IF t.waitingOn # NIL THEN
        next := t.waitingOn.waiters; prev := NIL;
        WHILE next # t DO
          <* ASSERT(next#NIL) *>
          prev := next; next := next.nextWaiter;
        END;
        IF prev = NIL THEN
          t.waitingOn.waiters := t.nextWaiter
        ELSE
          prev.nextWaiter := t.nextWaiter;
        END;
        t.nextWaiter := NIL;
        t.waitingOn := NIL;
      END;
      t.alertable := FALSE;
      IF WinBase.ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
        Choke();
      END;
    END;
    WinBase.LeaveCriticalSection(cm);
  END Alert;

PROCEDURE TestAlert(): BOOLEAN =
    VAR self := Self(); result: BOOLEAN;
  BEGIN
    IF self = NIL THEN
      (* Not created by Fork; not alertable *)
      RETURN FALSE
    ELSE
      WinBase.EnterCriticalSection(cm);
      result := self.alerted; IF result THEN self.alerted := FALSE END;
      WinBase.LeaveCriticalSection(cm);
      RETURN result
    END;
  END TestAlert;

(*------------------------------------------------------------------ Self ---*)

VAR
  threadIndex: WinDef.DWORD;
    (* read-only;  TLS (Thread Local Storage) index *)

PROCEDURE Self(): T =
  BEGIN
    (* If not the initial thread and not created by Fork, returns NIL *)
    RETURN LOOPHOLE(WinBase.TlsGetValue(threadIndex), T)
  END Self;

PROCEDURE SetSelf (t: T) =
  BEGIN
    IF WinBase.TlsSetValue(threadIndex, LOOPHOLE(t, WinDef.LPVOID)) = 0 THEN
      Choke();
    END;
  END SetSelf;

(*------------------------------------------------------------ Fork, Join ---*)

CONST
  MaxIdle = 20;

VAR (* LL=threadMu *)
  threadMu: Mutex;
  allThreads: T := NIL;   (* global list of registered threads *)
  idleThreads: T := NIL;  (* global list of idle threads *)
  nIdle := 0;

PROCEDURE CreateT(): T =
  (* LL < threadMu, because allocated a traced reference may cause
     the allocator to start a collection which will call SuspendOthers
     which will try to acquire threadMu. *)
  BEGIN
    RETURN NEW(T, waitSema := WinBase.CreateSemaphore(NIL, 0, 1, NIL),
               cond := NEW(Condition));
  END CreateT;

(* ThreadBase calls ThreadMain after finding (approximately) where
   its stack begins.  This dance ensures that all of ThreadMain's
   traced references are within the stack scanned by the collector. *)

PROCEDURE ThreadBase(param: WinDef.DWORD): WinDef.DWORD =
  VAR self := LOOPHOLE(param, T);
  BEGIN
    self.stackbase := ADR(self);
    ThreadMain(self);
    RETURN 0;
  END ThreadBase;

PROCEDURE ThreadMain(self: T) =
  VAR next_self: T; cl: Closure; res: REFANY;
  BEGIN
    LOOP (* The incarnation loop. *)
      SetSelf (self);

      LockMutex(threadMu);
        cl := self.closure;
      UnlockMutex(threadMu);

      res := cl.apply();

      next_self := NIL;
      IF nIdle < MaxIdle THEN
        (* apparently the cache isn't full, although we don't hold threadMu
           so we can't be certain... *)
        next_self := NEW(T);
      END;

      LockMutex(threadMu);
        self.result := res;
        self.completed := TRUE;

        IF next_self # NIL THEN
          (* transplant the guts of "self" into next_self *)
          next_self.handle    := self.handle;
          next_self.stackbase := self.stackbase;
          next_self.waitSema  := self.waitSema;
          next_self.cond      := self.cond;

          (* put "next_self" on the list of all threads *)
          next_self.next := allThreads;
          next_self.prev := allThreads.prev;
          allThreads.prev.next := next_self;
          allThreads.prev := next_self;

          (* put "next_self" on the list of idle threads *)
          next_self.nextIdle := idleThreads;
          idleThreads := next_self;
          INC(nIdle);

          (* finish making "self" an orphan *)
          IF allThreads = self THEN allThreads := self.next; END;
          self.next.prev := self.prev;
          self.prev.next := self.next;
          self.next := NIL;
          self.prev := NIL;
          self.handle := NIL;
          self.stackbase := NIL;
        END;
      UnlockMutex(threadMu);

      Broadcast(self.cond); (* let everybody know that "self" is done *)

      IF next_self = NIL THEN EXIT; END;
      self := next_self;
      IF WinBase.WaitForSingleObject(self.waitSema, WinBase.INFINITE) # 0 THEN
        Choke();
      END;
    END;

    (* remove ourself from the list of all threads *)
    LockMutex(threadMu);
      IF allThreads = self THEN allThreads := self.next; END;
      self.next.prev := self.prev;
      self.prev.next := self.next;
      self.next := NIL;
      self.prev := NIL;
      IF WinBase.CloseHandle(self.waitSema) = 0 THEN Choke() END;
      IF WinBase.CloseHandle(self.handle) = 0 THEN Choke() END;
      self.handle := NIL;
      self.waitSema := NIL;
    UnlockMutex(threadMu);
  END ThreadMain;

PROCEDURE Fork(closure: Closure): T =
  VAR
    t: T := NIL;
    id, stack_size: WinDef.DWORD;
    prevCount: WinDef.LONG;
    new_born: BOOLEAN;
  BEGIN
    (* determine the initial size of the stack for this thread *)
    stack_size := default_stack;
    TYPECASE closure OF
    | SizedClosure (scl) => IF scl.stackSize # 0 THEN 
                              stack_size := scl.stackSize * BYTESIZE(INTEGER);
                            END;
    ELSE (*skip*)
    END;

    (* try the cache for a thread *)
    LockMutex(threadMu);
      IF nIdle > 0 THEN
        new_born := FALSE;
        <* ASSERT(idleThreads # NIL) *>
        DEC(nIdle);
        t := idleThreads;
        idleThreads := t.nextIdle;
        t.nextIdle := NIL;
      ELSE (* empty cache => we need a fresh thread *)
        new_born := TRUE;
        UnlockMutex(threadMu);
          t := CreateT();
        LockMutex(threadMu);
        t.handle := WinBase.CreateThread(NIL, stack_size,
                      LOOPHOLE(ThreadBase, WinBase.LPTHREAD_START_ROUTINE),
                      LOOPHOLE(t,WinDef.LPVOID), WinBase.CREATE_SUSPENDED,
                      ADR(id));
        t.next := allThreads;
        t.prev := allThreads.prev;
        allThreads.prev.next := t;
        allThreads.prev := t;
      END;
      IF (t.handle = NIL) THEN Choke() END;
      t.closure := closure;
    UnlockMutex(threadMu);

    IF new_born THEN
      IF WinBase.ResumeThread(t.handle) = -1 THEN Choke() END;
    ELSE
      IF WinBase.ReleaseSemaphore(t.waitSema, 1, ADR(prevCount)) = 0 THEN
        Choke();
      END;
    END;

    RETURN t
  END Fork;

PROCEDURE Join(t: T): REFANY =
  VAR res: REFANY;
  BEGIN
    LockMutex(threadMu);
      IF t.joined THEN Die("attempt to join with thread twice"); END;
      WHILE NOT t.completed DO Wait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
    UnlockMutex(threadMu);
    RETURN res;
  END Join;

PROCEDURE AlertJoin(t: T): REFANY RAISES {Alerted} =
  VAR res: REFANY;
  BEGIN
    LockMutex(threadMu);
    TRY
      IF t.joined THEN Die("attempt to join with thread twice"); END;
      WHILE NOT t.completed DO AlertWait(threadMu, t.cond) END;
      res := t.result;
      t.result := NIL;
      t.joined := TRUE;
    FINALLY
      UnlockMutex(threadMu);
    END;
    RETURN res;
  END AlertJoin;

(*---------------------------------------------------- Scheduling support ---*)

PROCEDURE Pause(n: LONGREAL) =
  VAR amount, thisTime: LONGREAL;
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  BEGIN
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      WinBase.Sleep(ROUND(thisTime*1000.0D0));
    END;
  END Pause;

PROCEDURE AlertPause(n: LONGREAL) RAISES {Alerted} =
  VAR amount, thisTime: LONGREAL;
  CONST Limit = FLOAT(LAST(CARDINAL), LONGREAL) / 1000.0D0 - 1.0D0;
  VAR self: T;
  BEGIN
    self := Self();
    amount := n;
    WHILE amount > 0.0D0 DO
      thisTime := MIN (Limit, amount);
      amount := amount - thisTime;
      WinBase.EnterCriticalSection(cm);
      InnerTestAlert(self);
      self.alertable := TRUE;
      <* ASSERT(self.waitingOn = NIL) *>
      WinBase.LeaveCriticalSection(cm);
      EVAL WinBase.WaitForSingleObject(self.waitSema, ROUND(thisTime*1000.0D0));
      WinBase.EnterCriticalSection(cm);
      self.alertable := FALSE;
      IF self.alerted THEN
        (* Sadly, the alert might have happened after we timed out on the
           semaphore and before we entered "cm". In that case, we need to
           decrement the semaphore's count *)
        EVAL WinBase.WaitForSingleObject(self.waitSema, 0);
        InnerTestAlert(self);
      END;
      WinBase.LeaveCriticalSection(cm);
    END;
  END AlertPause;

PROCEDURE Yield() =
  BEGIN
    WinBase.Sleep(0);
  END Yield;

(*--------------------------------------------------- Stack size controls ---*)

PROCEDURE GetDefaultStackSize(): CARDINAL=
  BEGIN
    RETURN default_stack DIV BYTESIZE (INTEGER);
  END GetDefaultStackSize;

PROCEDURE MinDefaultStackSize(new_min: CARDINAL)=
  BEGIN
    default_stack := MAX (default_stack, new_min * BYTESIZE (INTEGER));
  END MinDefaultStackSize;

PROCEDURE IncDefaultStackSize(inc: CARDINAL)=
  BEGIN
    INC (default_stack, inc * BYTESIZE (INTEGER));
  END IncDefaultStackSize;

(*-------------------------------------------- Exception handling support ---*)

VAR handlersIndex: INTEGER;

PROCEDURE GetCurrentHandlers(): ADDRESS=
  BEGIN
    RETURN WinBase.TlsGetValue(handlersIndex);
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers(h: ADDRESS)=
  BEGIN
    EVAL WinBase.TlsSetValue(handlersIndex, h);
  END SetCurrentHandlers;

PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    f.next := WinBase.TlsGetValue(handlersIndex);
    EVAL WinBase.TlsSetValue(handlersIndex, f);
  END PushEFrame;

PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    EVAL WinBase.TlsSetValue(handlersIndex, frame);
  END PopEFrame;

(*--------------------------------------------- Garbage collector support ---*)

PROCEDURE SuspendOthers () =
  (* LL=0. Always bracketed with ResumeOthers, which will unlock threadMu *)
  VAR t: T;  self := Self ();
  BEGIN
    LockMutex(threadMu);

    WinBase.EnterCriticalSection(cm);
    (* We must hold 'cm' to guarantee that no suspended thread holds it.
       Otherwise, when the collector tries to acquire a mutex or signal a
       condition, it will deadlock with the suspended thread that holds cm. *)

    t := self.next;
    WHILE (t # self) DO
      IF WinBase.SuspendThread(t.handle) = -1 THEN Choke() END;
      t := t.next;
    END;

    WinBase.LeaveCriticalSection(cm);
  END SuspendOthers;

PROCEDURE ResumeOthers () =
  (* LL=threadMu.  Always preceded by SuspendOthers, which locks threadMu *)
  VAR t: T;  self := Self ();
  BEGIN
    t := self.next;
    WHILE (t # self) DO
      IF WinBase.ResumeThread(t.handle) = -1 THEN Choke() END;
      t := t.next;
    END;
    UnlockMutex(threadMu);
  END ResumeOthers;

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS)) =
  (* LL=threadMu.  Only called within {SuspendOthers, ResumeOthers} *)
  VAR t := allThreads;  context: ThreadContext.CONTEXT;
  BEGIN
    context.ContextFlags := Word.Or(ThreadContext.CONTEXT_CONTROL,
                                    ThreadContext.CONTEXT_INTEGER);
    REPEAT
      IF (t.stackbase # NIL) THEN
        IF WinBase.GetThreadContext(t.handle, ADR(context))=0 THEN Choke() END;
        p(LOOPHOLE(context.Esp, ADDRESS), t.stackbase); (* Process the stack *)
        p(ADR(context.Edi), ADR(context.Eip));  (* Process the registers *)
      END;
      t := t.next;
    UNTIL (t = allThreads);
  END ProcessStacks;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE Die(msg: TEXT) =
  BEGIN
    RTMisc.FatalError ("ThreadWin32.m3", 0, "Thread client error: ", msg);
  END Die;

PROCEDURE Choke() =
  BEGIN
    RTMisc.FatalError ("ThreadWin32.m3: Windows OS failure, GetLastError = ",
                       WinBase.GetLastError ());
  END Choke;

(*-------------------------------------------------------- Initialization ---*)


PROCEDURE Init() =
  VAR
    self: T;
    threadhandle, processhandle: WinNT.HANDLE;
  BEGIN
    handlersIndex := WinBase.TlsAlloc();
    IF handlersIndex < 0 THEN Choke() END;

    threadIndex := WinBase.TlsAlloc();
    IF threadIndex < 0 THEN Choke() END;

    cm := NEW(WinBase.LPCRITICAL_SECTION);
    WinBase.InitializeCriticalSection(cm);

    threadMu := NEW(Mutex);
    self := CreateT();

    LockMutex(threadMu);
      threadhandle := WinBase.GetCurrentThread();
      processhandle := WinBase.GetCurrentProcess();
      IF WinBase.DuplicateHandle(processhandle, threadhandle, processhandle,
                                 LOOPHOLE(ADR(self.handle), WinNT.PHANDLE), 0,
                                 0, WinNT.DUPLICATE_SAME_ACCESS) = 0 THEN
        Choke();
      END;
      self.next  := self;
      self.prev  := self;
      allThreads := self;
      self.stackbase := RTLinker.info.bottom_of_stack;
      IF self.stackbase = NIL THEN Choke(); END;
    UnlockMutex(threadMu);
    SetSelf (self);
  END Init;

BEGIN
END ThreadWin32.

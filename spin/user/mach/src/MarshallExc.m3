(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 10-Mar-96  David Dion (ddion) at the University of Washington
 *	Changed to mirror OSF/1 server's expectations of syscall handling.
 *	A user-level task has a syscall thread bound to it for life.
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 14-Dec-95  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)


MODULE MarshallExc;

IMPORT UserSpaceThread, Word, Thread, Sema, Fmt, Space, Textify;
IMPORT Strand; (* for yield *)
IMPORT HandlerUtils;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;


TYPE
  ExcDataNode = RECORD
    uthread: UserSpaceThread.T;
    type: Word.T;
    code0: Word.T;
    code1: Word.T;
    statePtr: REF AlphaThreadState;
    stateCount: CARDINAL;
  END;

(* XXX - this is only a temporary fix until Tasks are a reality.  
   XXX - synchronization is all off. *)
TYPE
  ServerKickNode = RECORD
    sema: Sema.T;
    space: Space.T;
    (* task: Word.T; *)
  END;

CONST
  PoolSize = 5;
  NumTasks = 20;  (* XXX - temporary - get rid of this *)

VAR
  (* Global structure to pass marshalled data *)
  excData: ARRAY [0..PoolSize-1] OF ExcDataNode;

  (* Sychronization *)
  serverKick: ARRAY [0..NumTasks-1] OF ServerKickNode;
  nextServerNode: CARDINAL;
  resultReady: ARRAY [0..PoolSize-1] OF Sema.T;

  (* Variables for the pool of handles *)
  poolMutex: MUTEX;
  poolCond: Thread.Condition;
  poolEntryInUse: ARRAY [0..PoolSize-1] OF BOOLEAN;
  poolUnusedCount: CARDINAL;
  nextRequest: ARRAY [0..PoolSize-1] OF CARDINAL; (* FIFO of handles *)
  fifoInsert: CARDINAL;
  fifoRemove: CARDINAL;

  (* Mapping from server thread to handle *)
  uthHandleMap: ARRAY [0..PoolSize-1] OF UserSpaceThread.T;

    

(*
 * Client get a new marshalling handle.
 *)
PROCEDURE ClientGetHandle(VAR statePtr: REF AlphaThreadState) : Handle =
  BEGIN
    (* Acquire the mutex on the pool. *)
    LOCK poolMutex DO
      (* Get past the condition variable. *)
      WHILE poolUnusedCount <= 0 DO
        (* Condition.Wait(poolCond, poolMutex); *)
        Thread.Wait(poolMutex, poolCond);
      END;
      (* Decrement the pool count. *)
      DEC(poolUnusedCount);
      (* Find an open pool entry.  Because of asynchrony, we just have
         to find one.  Could maintain a FIFO queue of open pool entries,
         but that would be a major pain and the benefits are not clear. 
         On the other hand, a FIFO would scale better. *)
      FOR i := FIRST(poolEntryInUse) TO LAST(poolEntryInUse) DO
        IF NOT poolEntryInUse[i] THEN
          (* Set in-use flag. *)
          poolEntryInUse[i] := TRUE;
          (* Put request in FIFO and increment FIFO index. *)
          nextRequest[fifoInsert] := i;
          INC(fifoInsert);
          (* Loop index back to beginning of array if necessary. *)
          fifoInsert := (fifoInsert - FIRST(nextRequest)) MOD
            NUMBER(nextRequest) + FIRST(nextRequest);
          (* Set the statePtr to the state for this handle. *)
          statePtr := excData[i].statePtr;
          (* Return index.  Note this RETURN statement releases mutex. *)
          RETURN i;
        END;
      END;
      HandlerUtils.PrintError("Server pool count skewed!\n");
      (* this error reporting mechanism stinks.  eventually raise exception. *)
      RETURN LAST(poolEntryInUse) + 1;
    END;
  END ClientGetHandle;

(*
 * Client return marshalling handle to the pool.
 *)
PROCEDURE ClientReturnHandle(h: Handle) =
  BEGIN
    (* Acquire the mutex on the pool. *)
    LOCK poolMutex DO
      IF h < FIRST(poolEntryInUse) OR 
        h > LAST(poolEntryInUse) THEN
        HandlerUtils.PrintError("PoolClientReturnEntry: bad handle.\n");
        RETURN;
      END;
      (* Mark entry as unused. *)
      poolEntryInUse[h] := FALSE;
      (* Increment the pool count. *)
      INC(poolUnusedCount);
      (* Signal the condition variable. *)
      Thread.Signal(poolCond);
    END;
  END ClientReturnHandle;

(*
 * Client input marshalled data.
 *)
PROCEDURE ClientSetSyscallData(         h:          Handle;
                                        uth:        UserSpaceThread.T;
                                        type:       Word.T;
                                        code0:      Word.T;
                                        code1:      Word.T;
                                        stateCount: CARDINAL) =
  BEGIN
    excData[h].uthread := uth;
    excData[h].type := type;
    excData[h].code0 := code0;
    excData[h].code1 := code1;
    excData[h].stateCount := stateCount;
  END ClientSetSyscallData;

(*
 * Server get a handle from the pool.  
 * 
 * Wait on the serverKick semaphore until a client enters a request.  
 * Then get the next available handle.  Finally, register this server
 * thread as holding the handle.
 *)
PROCEDURE ServerGetNextHandle(serverUthread: UserSpaceThread.T;
                              appSpace: Space.T) : Handle =
  VAR
    handle: Handle;
    i: CARDINAL;
  BEGIN
    (* 
     * Find proper task and wait on that semaphore.  The proper task is
     * determined by a space match.  Each server thread is bound to handle
     * syscalls from one task.
     *)
    i := 0;
    WHILE i < nextServerNode DO
      IF serverKick[i].space = appSpace THEN
        Sema.P(serverKick[i].sema);
        EXIT;
      ELSE
        INC(i);
      END;
    END;
    IF i = nextServerNode THEN (* task not found, so create its entry *)
      IF nextServerNode > LAST(serverKick) THEN
        HandlerUtils.PrintError("Ran out of serverKickNodes.\n");
        RETURN 0; (* should raise some sort of error exception *)
      END;
      IF Debug THEN Print("Installing new system call handler for space " &
        Textify.Ref(appSpace) & "\n");
      END;
      serverKick[nextServerNode].space := appSpace;
      INC(nextServerNode);
      Sema.P(serverKick[i].sema);
    END;      

    (* 
     * Get a handle. 
     *)
    (* Acquire the mutex on the pool. *)
    LOCK poolMutex DO
      (* Check if there's any work to do. *)
      IF poolUnusedCount >= NUMBER(nextRequest) THEN
        HandlerUtils.PrintError("Server kicked but no work to do!\n");
        (* Perhaps raise an exception here?  Later ... *)
        RETURN LAST(nextRequest) + 1;
      END;
      (* Get the handle from the FIFO. *)
      handle := nextRequest[fifoRemove];
      (* Update fifoRemove pointer. *)
      INC(fifoRemove);
      (* Loop to beginning of array if necessary. *)
      fifoRemove := (fifoRemove - FIRST(nextRequest)) MOD
        NUMBER(nextRequest) + FIRST(nextRequest);
    END;

    (*
     * Register thread.
     *)
    uthHandleMap[handle] := serverUthread;

    (* 
     * Return the handle.
     *)
    RETURN handle;
  END ServerGetNextHandle;

(*
 * Server get the handle held by its thread.  This handle was acquired
 * with ServerGetNextHandle() and then the thread returned to user-level.
 * Now it's back and it wants its handle.  Return TRUE if handle located.
 * If the handle is not found, then this server thread is looking to handle
 * its first system call.  Return FALSE.
 * Also return a ref to the state for this handle.  This avoids some extra
 * copying by allowing the server thread to write directly onto the handle
 * state.
 *)
PROCEDURE ServerGetHandleFromThread(VAR h:             Handle;
                                    VAR statePtr:      REF AlphaThreadState;
                                        serverUthread: UserSpaceThread.T) :
  BOOLEAN =
  BEGIN
    FOR handle := FIRST(uthHandleMap) TO LAST(uthHandleMap) DO
      IF uthHandleMap[handle] = serverUthread THEN
        h := handle;
        statePtr := excData[handle].statePtr;
        RETURN TRUE;
      END;
    END;

    statePtr := NIL;
    RETURN FALSE;
  END ServerGetHandleFromThread;

(*
 * Server get marshalled data entered by client.
 *)
PROCEDURE ServerGetSyscallData(    h:          Handle;
                               VAR uth:        UserSpaceThread.T;
                               VAR type:       Word.T;
                               VAR code0:      Word.T;
                               VAR code1:      Word.T;
                               VAR statePtr:   REF AlphaThreadState;
                               VAR stateCount: CARDINAL) =
  BEGIN
    uth := excData[h].uthread;
    type := excData[h].type;
    code0 := excData[h].code0;
    code1 := excData[h].code1;
    statePtr := excData[h].statePtr;
    stateCount := excData[h].stateCount;
  END ServerGetSyscallData;

(*
 * Client kick a server thread.  Any server thread can handle any client 
 * request.  Then wait for the result state for this request (determined by
 * handle).
 *)
PROCEDURE ClientKickServerAndWaitForState(h: Handle; space: Space.T) =
  CONST
    YieldThreshold = 5;
  VAR 
    i: CARDINAL;
    yieldCount : CARDINAL;
  BEGIN
    (* We need to find the right server thread to kick.  The server thread
       is forked when the process is created.  However, it may not have 
       had a chance to drop into the server yet (especially if we're 
       running without preemption).  Give it a few chances by yielding if
       it's not there.*)
    yieldCount := 0;
    LOOP
      (* Find the right server thread (in this task) and kick it *)
      i := 0;
      WHILE i < nextServerNode DO
        IF Debug THEN Print("\tserverKick[" & Fmt.Int(i) & "].space = " &
          Textify.Ref(serverKick[i].space) & " space = " &
          Textify.Ref(space) & "\n"); END;
        IF serverKick[i].space = space THEN
          Sema.V(serverKick[i].sema);
          EXIT;
        ELSE
          INC(i);
        END;
      END;

      (* Check if we actually found the thread. *)
      IF i = nextServerNode THEN
        (* thread not found *)
        IF yieldCount < YieldThreshold THEN
          INC(yieldCount);
          Strand.Yield();
        ELSE
          HandlerUtils.PrintError("Client can't find server task to kick!\n");
          RETURN;
        END;
      ELSE
        (* thread found.  exit the loop *)
        EXIT;
      END;
    END;

    (* Wait on result *)
    Sema.P(resultReady[h]);

    (* Return to client.  No need to copy back state, since the client
       still holds a ref to it. *)
  END ClientKickServerAndWaitForState;

(*
 * Server indicate that client's request has been serviced.
 *)
PROCEDURE ServerRequestCompleted(h: Handle) =
  BEGIN
    (* Clear entry in thread-handle mapping *)
    uthHandleMap[h] := NIL;

    (* Tell client that request has been satisified *)
    Sema.V(resultReady[h]);
  END ServerRequestCompleted;

(*
 * Initialize the marshalling mechanism.
 *)
PROCEDURE Init() =
  BEGIN
    (* Pool state *)
    poolMutex := NEW(MUTEX);
    poolCond := NEW(Thread.Condition);
    poolUnusedCount := NUMBER(poolEntryInUse);
    FOR i := FIRST(poolEntryInUse) TO LAST(poolEntryInUse) DO
      poolEntryInUse[i] := FALSE;
    END;
    fifoInsert := FIRST(nextRequest);
    fifoRemove := FIRST(nextRequest);

    (* Mapping from server thread to handle *)
    FOR i := FIRST(uthHandleMap) TO LAST(uthHandleMap) DO
      uthHandleMap[i] := NIL;
    END;

    (* State data structures *)
    FOR i := FIRST(excData) TO LAST(excData) DO
      excData[i].statePtr := NEW(REF AlphaThreadState);
    END;

    (* Synchronization semaphores *)
    FOR i := FIRST(serverKick) TO LAST(serverKick) DO
      serverKick[i].sema := Sema.Alloc(0);
    END;
    nextServerNode := FIRST(serverKick);
    FOR i := FIRST(resultReady) TO LAST(resultReady) DO
      resultReady[i] := Sema.Alloc(0);
    END;
  END Init;

BEGIN
END MarshallExc.

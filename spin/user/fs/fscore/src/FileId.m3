(*
 * HISTORY
 * 27-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made Hash faster by just looking at head and tail of fileids.
 *	
 * 09-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

MODULE FileId EXPORTS FileId, FileIdPrivate;
IMPORT FileIdTbl, Word, IO, Fmt;

TYPE Tbl = MUTEX OBJECT 
    tbl: FileIdTbl.Default;
  METHODS
    init() : Tbl := Initialize;
  END;
VAR fileIds : Tbl;

PROCEDURE Initialize (self: Tbl): Tbl = 
  BEGIN
    LOCK self DO
      self.tbl := NEW(FileIdTbl.Default).init();
    END;
    RETURN self;
  END Initialize;

PROCEDURE Get(READONLY id: T): REFANY =
  VAR opaque: REFANY;
  BEGIN
    LOCK fileIds DO
      IF fileIds.tbl.get(id,opaque) = TRUE THEN
	RETURN opaque;
      END;
    END;
    RETURN NIL;
  END Get;

PROCEDURE Put(READONLY id: T; obj: REFANY) = 
  BEGIN
    (* XXX check permissions *)
    LOCK fileIds DO
      EVAL fileIds.tbl.put(id,obj);
    END;
  END Put;

PROCEDURE Delete (READONLY id: T): REFANY = 
  VAR opaque: REFANY;
  BEGIN
    (* XXX check permissions *)
    LOCK fileIds DO
      IF fileIds.tbl.delete(id,opaque) = TRUE THEN
        RETURN opaque;
      ELSE
        RETURN NIL;
      END;
    END;
  END Delete;

PROCEDURE Equal (READONLY id1,id2 : T): BOOLEAN = 
  BEGIN
    (* pray that the compiler does something efficient here. *)
    RETURN id1 = id2;
  END Equal;

PROCEDURE Hash (READONLY id:T): Word.T =
  VAR hash: Word.T;
  BEGIN
    WITH a = VIEW(id, ARRAY OF INTEGER) DO
      hash := a[0];
      hash := Word.LeftShift(hash, 1);
      hash := Word.Plus(hash, a[LAST(a)]);
    END;
    RETURN hash;
  END Hash;

PROCEDURE Inc(VAR id:T):T =
  BEGIN
    FOR i := LAST(id) TO FIRST(id) BY -1 DO 
      IF id[i] # LAST(CHAR) THEN 
        INC(id[i]); 
        EXIT; 
      ELSE
        id[i] := VAL(0,CHAR);
      END;
    END;
    RETURN id;
  END Inc;

PROCEDURE Print(READONLY id:T) =
  BEGIN
    FOR i := FIRST(id) TO LAST(id) DO
      IO.Put(Fmt.Unsigned(ORD(id[i])));
      IO.Put(" ");
    END;
    IO.Put("\n");
  END Print;

PROCEDURE Init(<*UNUSED*> verbose: BOOLEAN) =
  BEGIN
    fileIds := NEW(Tbl).init();
  END Init;

BEGIN
END FileId.

INTERFACE DirObject;
IMPORT DirEnt;
IMPORT DirObjTbl;

CONST Brand = "DirObject";

EXCEPTION IllegalFileName;
EXCEPTION FileNotFound;

TYPE

  Public = OBJECT
  METHODS
    mvElement(oldBlock:CARDINAL; newBlock:CARDINAL) RAISES{IllegalFileName};
    addElement(iNum:CARDINAL;name:TEXT):CARDINAL RAISES{IllegalFileName};
    rmElement(iNum:CARDINAL;name:TEXT):CARDINAL RAISES{IllegalFileName};
    getSize():CARDINAL;
    getINumWithName(name:TEXT):CARDINAL RAISES{IllegalFileName, FileNotFound};
    getFileArray(pos: INTEGER;VAR buf: ARRAY OF DirEnt.T):INTEGER;
    initialize(initialSize:CARDINAL):T;
  END;
  T<: Public;
  Iterator = DirObjTbl.Iterator;

  PROCEDURE NewDirObject(initialSize:CARDINAL):T;

END DirObject.








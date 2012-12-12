INTERFACE CleanerIMap;
IMPORT INodeTbl;

CONST Brand = "CleanerIMap";

TYPE

  Public = OBJECT
  METHODS
    addElement(newINumber:CARDINAL;newINode:TEXT):CARDINAL;
    getINodeWithINum(iNumber:CARDINAL):TEXT;
    getSize():CARDINAL;
    initialize(initialSize:CARDINAL):T;
    iterate():Iterator;
  END;
  T<: Public;
  Iterator = INodeTbl.Iterator;

  PROCEDURE NewIMap(initialSize:CARDINAL):T;

END CleanerIMap.








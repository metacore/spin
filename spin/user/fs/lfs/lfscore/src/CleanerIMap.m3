MODULE CleanerIMap EXPORTS CleanerIMap;

IMPORT IO, Wr, BootIO;
IMPORT INodeTbl;

REVEAL T = Public BRANDED
OBJECT
  index: CARDINAL;
  iNodeHash: INodeTbl.Default;
OVERRIDES
  addElement       := AddElement;
  getINodeWithINum := GetINodeWithINum;
  getSize          := GetSize;
  initialize       := Initialize;
  iterate          := Iterate;
END;

VAR
  mywr: Wr.T;

PROCEDURE AddElement(self:T; newINum:CARDINAL; newINode:TEXT):CARDINAL =
  BEGIN
    self.index := self.index + 1;
    EVAL self.iNodeHash.put(newINum, newINode);
    RETURN self.index;
  END AddElement;



PROCEDURE GetINodeWithINum(self:T; iNumber:CARDINAL):TEXT =
  VAR retVal:TEXT;

  BEGIN
    EVAL self.iNodeHash.get(iNumber,retVal);
    RETURN retVal;
  END GetINodeWithINum;
  


PROCEDURE GetSize(self:T):CARDINAL =
  BEGIN
    RETURN self.iNodeHash.size();
  END GetSize;



PROCEDURE Initialize(self:T; initialSize:CARDINAL):T=
  BEGIN

    self:=NEW(T);
    self.index := 0;

    self.iNodeHash := NEW(INodeTbl.Default).init(initialSize);

    RETURN self;
  END Initialize;


PROCEDURE Iterate(self:T):Iterator = 
  BEGIN
    RETURN self.iNodeHash.iterate();
  END Iterate;




PROCEDURE NewIMap(initialSize:CARDINAL):T =
  VAR
    temp:T;

  BEGIN
    IO.Put("NewIMap Called with arg: ",mywr);
    IO.PutInt(initialSize,0,mywr);
    IO.Put("\n",mywr);

    temp:= NEW(T);
    temp:= temp.initialize(initialSize);

    RETURN temp;

  END NewIMap;


BEGIN
(*  EVAL CleanerIMapInterface.Export(NEW (Auth.AuthAlways));*)
  mywr := BootIO.Writer();
END CleanerIMap.












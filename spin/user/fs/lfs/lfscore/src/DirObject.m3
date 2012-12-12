MODULE DirObject;

IMPORT Text, Fmt;
IMPORT IO;
IMPORT DirObjTbl;
IMPORT DirBlockTbl;
IMPORT DirEnt;


REVEAL T = Public BRANDED
OBJECT
  index: CARDINAL;
  (* both of these are tables mapping text to integer*)
  nameINumHash:    DirObjTbl.Default;     (* maps filename to iNum *)
  nameBlockHash:   DirObjTbl.Default;     (* maps filename to block offset in directory object *)
  blockNameHash:   DirBlockTbl.Default;   (* maps block offset to name *)
OVERRIDES
  mvElement        := MvElement;
  addElement       := AddElement;
  rmElement        := RmElement;
  getINumWithName  := GetINumWithName;
  getSize          := GetSize;
  getFileArray     := GetFileArray;
  initialize       := Initialize;
END;


CONST DEBUG = FALSE;



(* does following:
   1 finds name of file to be moved, from blockNameHash.
   2 delete name from blockNameHash,
   3 add newblock with name to blockName hash,
   4 delete name from nameINumHash
   5 add name with new block to nameINumHash.
*)
PROCEDURE MvElement(self:T; oldBlock:CARDINAL; newBlock:CARDINAL) RAISES{IllegalFileName}=
  VAR
    oldBlockInt,newBlockInt, retVal:INTEGER;
    name:TEXT;
  BEGIN


    IF DEBUG = TRUE THEN
      IO.Put("DirObject.MvElement called with newBlock:"&Fmt.Int(newBlock)&" and oldBlock:"&Fmt.Int(oldBlock)&".\n");
    END;

    oldBlockInt:=VAL(oldBlock,INTEGER);
    newBlockInt:=VAL(newBlock,INTEGER);


    (* 1 get name *)
    IF self.blockNameHash.get(oldBlockInt,name) = FALSE THEN
      IF DEBUG = TRUE THEN
        IO.Put("DirObject.mvElement passed non-existant blockNum:"&Fmt.Int(oldBlock)&".\n");
      END;
      RAISE IllegalFileName;
    END;


    IF DEBUG = TRUE THEN
      IO.Put("Remapping file with name:"&name&".\n");
    END;


    (* 2 delete name from blockNameHash *)
    retVal := oldBlockInt;
    EVAL self.blockNameHash.delete(retVal, name);


    (* 3 add newblock with name to blockName hash *)
    EVAL self.blockNameHash.put(newBlockInt, name);


    (* 4 delete name from nameBlockHash *)
    (* and delete the old blockNum/name mapping *)
    (* and put the new value in *)
    EVAL self.nameBlockHash.delete(name, oldBlockInt);

    (* 5 add name with new block to nameBlockHash. *)
    EVAL self.nameBlockHash.put(name, newBlockInt);

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.MvElement exits having moved file:"&name&" to block:"&Fmt.Int(retVal)&".\n");
    END;

  END MvElement;









(* returns the number of names in the table *)
PROCEDURE AddElement(self:T; iNum:CARDINAL; name:TEXT):CARDINAL RAISES{IllegalFileName}=
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.AddElement called with name:"&name&" and iNum:"&Fmt.Int(iNum)&".\n");
    END;


    WITH i = Text.Length(name) DO
      IF i<1 OR i>255 THEN
        IO.Put("DirObject, addElement says filename is illegal, length:"&Fmt.Int(i)&"\n");
        RAISE IllegalFileName END;
    END;


    EVAL self.nameINumHash.put(name,iNum);
    EVAL self.nameBlockHash.put(name,self.index);
    EVAL self.blockNameHash.put(self.index,name);

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.AddElement exits with index="&Fmt.Int(self.index)&".\n");
    END;

    INC(self.index);

    RETURN self.index-1;

  END AddElement;





(* returns the number of names in the table *)
PROCEDURE RmElement(self:T; iNum:CARDINAL; name:TEXT):CARDINAL RAISES{IllegalFileName}=
  VAR
    retVal : INTEGER;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.RmElement called with name:"&name&" and iNum:"&Fmt.Int(iNum)&".\n");
    END;


    WITH i = Text.Length(name) DO
      IF i<1 OR i>255 THEN
        IO.Put("DirObject, rmElement says filename is illegal, length:"&Fmt.Int(i)&"\n");
        RAISE IllegalFileName END;
    END;


    retVal := VAL(iNum,INTEGER);
    EVAL self.nameINumHash.delete(name,retVal);
      

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.RmElement exits having deleted objHash="&Fmt.Int(retVal)&".\n");
    END;

    (* this removes the entry from the block hash table and lets retVal = the location of the file in the dirFile*)
    EVAL self.nameBlockHash.delete(name, retVal);

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.RmElement exits, returning deleted blockHash="&Fmt.Int(retVal)&".\n");
    END;

    EVAL self.blockNameHash.delete(retVal,name);

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.RmElement exits, deleted blockNameHash="&Fmt.Int(retVal)&".\n");
    END;

    self.index := self.index - 1;

    RETURN VAL(retVal,CARDINAL);  (* this is the block number in the dir-file of the entry we just deleted *)

  END RmElement;




PROCEDURE GetINumWithName(self:T; name:TEXT):CARDINAL RAISES{IllegalFileName, FileNotFound} =
  VAR retVal:INTEGER;
  BEGIN


    IF DEBUG = TRUE THEN
      IO.Put("DirObject.GetINumWithName called with name:"&name&".\n");
    END;


    (* Maybe this isnt necessary*)
    WITH i = Text.Length(name) DO
      IF i<1 OR i>255 THEN 
        IO.Put("DirObject.getINumWithName says filename is wrong length:"&Fmt.Int(i)&".\n");
        RAISE IllegalFileName 
      END;
    END;


    (* if we find the file return its iNode *)
    IF DEBUG = TRUE THEN
      IO.Put("Fetching iNum from dirObject hash.\n");
    END;


    IF self.nameINumHash.get(name,retVal) = TRUE THEN
      RETURN retVal;
    END;


    (* else we raise an exception *)
    (*IF DEBUG = TRUE THEN

      IO.Put("GetINumWithName couldn't find requested file.\n");
      IO.Put("Perhaps you would like to one of these other fine files?\n");
      
      fileList:= self.nameINumHash.iterate();
      WHILE fileList.next(name,retVal) DO
        IO.Put(name&" at iNum:"&Fmt.Int(retVal)&".\n");
      END;

    END;*)

    RAISE FileNotFound;

  END GetINumWithName;



PROCEDURE GetSize(self:T):CARDINAL =
  BEGIN
    (*    RETURN self.nameINumHash.size();*)
    IF DEBUG = TRUE THEN
      IO.Put("DirObject.getSize returning size:"&Fmt.Int(self.index)&".\n");
    END;
    RETURN self.index;
  END GetSize;



PROCEDURE GetFileArray(self:T;pos:INTEGER;VAR buf: ARRAY OF DirEnt.T):INTEGER =
  VAR it       : Iterator;
      counter   : CARDINAL := 0;
      iNum      : INTEGER;
      fileName  : TEXT;
      fileArray : REF ARRAY OF DirEnt.T;
      size      : CARDINAL;
  BEGIN

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.GetFileArray called.\n");
    END;

    size := self.getSize();

    IF DEBUG = TRUE THEN
      IO.Put("DirObject.getFileArray, size="&Fmt.Int(size)&
        " buf pos="&Fmt.Int(pos)&
        " buf size="&Fmt.Int(NUMBER(buf))&".\n");
    END;

    IF pos>(size-1) THEN
      RETURN 0;
    END;

    fileArray := NEW(REF ARRAY OF DirEnt.T,NUMBER(buf));

    it := self.nameINumHash.iterate();

    WHILE it.next(fileName,iNum) = TRUE DO
      fileArray[counter].ino := iNum;
      fileArray[counter].name := fileName;
      fileArray[counter].nextPos := counter + 1; (* I've no clue what this is for *)
     
      IF DEBUG = TRUE THEN
        IO.Put("Filename: "&fileArray[counter].name&" added to fileArray.\n");
      END;

      counter := counter + 1;
    END;

    fileArray[counter-1].nextPos := -1;

    size := MIN(size - pos,LAST(buf)-FIRST(buf)+1);


    IF DEBUG = TRUE THEN
      IO.Put("DirObject.getFileArray, finished packing filenames. Size="&Fmt.Int(size)&".\n");
    END;

    SUBARRAY(buf,0,NUMBER(buf)-pos) := SUBARRAY(fileArray^,pos,NUMBER(buf));
    
    IF DEBUG = TRUE THEN
      IO.Put("DirObject.getFileArray returning with size="&Fmt.Int(size)&".\n");
    END;

    RETURN size;

  END GetFileArray;




PROCEDURE Initialize(self:T; initialSize:CARDINAL):T=
  BEGIN

    self:=NEW(T);
    self.index := 0;

    self.nameINumHash   := NEW(DirObjTbl.Default).init(initialSize);
    self.nameBlockHash := NEW(DirObjTbl.Default).init(initialSize);
    self.blockNameHash:= NEW(DirBlockTbl.Default).init(initialSize);

    RETURN self;
  END Initialize;


PROCEDURE NewDirObject(initialSize:CARDINAL):T =
  VAR
    temp:T;

  BEGIN
    IF DEBUG = TRUE THEN
      IO.Put("NewDirObj Called with arg: ");
      IO.PutInt(initialSize,0);
      IO.Put("\n");
    END;

    temp:= NEW(T);
    temp:= temp.initialize(initialSize);

    RETURN temp;

  END NewDirObject;


BEGIN
(*  EVAL CleanerIMapInterface.Export(NEW (Auth.AuthAlways));*)
(*  mywr := BootIO.Writer();*)
END DirObject.
















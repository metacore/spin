(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Array used to communicate update info to inodes.
 *
 *)
MODULE INodeInfoArray;

IMPORT IO, Wr, BootIO;
IMPORT INodeInfo;
IMPORT INodeInfoArraySort;

REVEAL T = Public BRANDED
OBJECT
  index: INTEGER;  (* Holds number of elements currently stored in array *)
  array: REF ARRAY OF INodeInfo.T;
OVERRIDES
  addElement       := AddElement;
  getElement       := GetElement;
  size             := Size;
  sort             := Sort;
  getArray         := GetArray;
  initialize       := Initialize;
END;

VAR
  mywr: Wr.T;


CONST DEBUG = FALSE;

PROCEDURE AddElement(self:T; READONLY newINodeInfo:INodeInfo.T):BOOLEAN =
  VAR newArray : REF ARRAY OF INodeInfo.T;
      size     : CARDINAL;
      tempInfo:INodeInfo.T;
  BEGIN

    (* Get the current array size *)
    IF self.array # NIL THEN
      size := NUMBER(self.array^);
    ELSE
      size := 0;
    END;


    (* If the array needs expanding then expand it *)
    IF self.index >= size THEN

      newArray := NEW(REF ARRAY OF INodeInfo.T, size*2+10);
      
      IF self.array # NIL THEN
        SUBARRAY(newArray^, 0, size) := SUBARRAY(self.array^, 0, size);
      END;

      self.array := newArray;

    END;

    tempInfo := NEW(INodeInfo.T);

    tempInfo.iNode := newINodeInfo.iNode;
    tempInfo.block := newINodeInfo.block;
    tempInfo.diskAddress:= newINodeInfo.diskAddress;

    self.array[self.index] := tempInfo;

    self.index := self.index+1;

    RETURN TRUE;
  END AddElement;



(* 1 is the first element, 0 is illegal *)
PROCEDURE GetElement(self:T; index:CARDINAL):INodeInfo.T RAISES {INodeInfoArrayError} =

    BEGIN

      IF self.array # NIL THEN
        IF index >= 1 OR index <= NUMBER(self.array^) THEN
          RETURN self.array[index-1];
        END;
      END;

      (* If we're here then the index was out of bounds *)
      RAISE INodeInfoArrayError;
      
    END GetElement;





(* Returns number of elements currently in array *)
PROCEDURE Size(self:T):CARDINAL =
  BEGIN
    RETURN  self.index;
  END Size;



PROCEDURE Sort(self:T):BOOLEAN =
  BEGIN

    IF self.array # NIL THEN
      INodeInfoArraySort.Sort(self.array^);
    END;

    RETURN TRUE;
  END Sort;




PROCEDURE GetArray(self:T):REF ARRAY OF INodeInfo.T =
BEGIN
  RETURN self.array;
END GetArray;




PROCEDURE Initialize(self:T; initialSize:CARDINAL):T =
VAR size:CARDINAL;

  BEGIN

    self:=NEW(T);
    self.index := 0;

    (* Those damn users dont have any reason to make this array less than 10 in size *)
    IF initialSize < DEFAULTSIZE THEN
      size := DEFAULTSIZE;
    ELSE
      size := initialSize;
    END;

    self.array := NEW(REF ARRAY OF INodeInfo.T, size);

    RETURN self;

  END Initialize;




PROCEDURE NewINodeInfoArray(initialSize:CARDINAL):T =
  VAR
    temp:T;

  BEGIN


    IF DEBUG = TRUE THEN
      IO.Put("NewINodeInfoArray Called with arg: ",mywr);
      IO.PutInt(initialSize,0,mywr);
      IO.Put("\n",mywr);
    END;

    temp:= NEW(T);
    temp:= temp.initialize(initialSize);

    RETURN temp;

  END NewINodeInfoArray;


BEGIN
  mywr := BootIO.Writer();
END INodeInfoArray.

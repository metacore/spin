(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Object for storing directory info.  Essentially an expandable array that 
 *      supports alphabetically sorting.
 *)
MODULE DirInfoArray;

IMPORT IO, Wr, BootIO;
IMPORT DirInfo;
IMPORT DirInfoArraySort;
IMPORT Text;

REVEAL T = Public BRANDED
OBJECT
  index: INTEGER;  (* Holds number of elements currently stored in array *)
  array: REF ARRAY OF DirInfo.T;
OVERRIDES
  addElement       := AddElement;
  getElement       := GetElement;
  size             := Size;
  sort             := Sort;
  getArray         := GetArray;
  initialize       := Initialize;
  isMember         := IsMember;
END;

VAR
  mywr: Wr.T;


CONST DEBUG = FALSE;

PROCEDURE AddElement(self:T; READONLY newDirInfo:DirInfo.T):BOOLEAN RAISES {DirInfoArrayError} =
  VAR newArray : REF ARRAY OF DirInfo.T;
      size     : CARDINAL;
      tempInfo:DirInfo.T;
  BEGIN

    (* It is an error to try and add an unallocated object to the array *)
    (*    IF newDirInfo = NIL THEN RAISE DirInfoArrayError END;*)


    (* Get the current array size *)
    IF self.array # NIL THEN
      size := NUMBER(self.array^);
    ELSE
      size := 0;
    END;


    (* If the array needs expanding then expand it *)
    IF self.index >= size THEN

      newArray := NEW(REF ARRAY OF DirInfo.T, size*2+10);
      
      IF self.array # NIL THEN
        SUBARRAY(newArray^, 0, size) := SUBARRAY(self.array^, 0, size);
      END;

      self.array := newArray;

    END;

    tempInfo := NEW(DirInfo.T);

    tempInfo.name := newDirInfo.name;
    tempInfo.iNum := newDirInfo.iNum;

    self.array[self.index] := tempInfo;

    self.index := self.index+1;

    RETURN TRUE;
  END AddElement;



(* 1 is the first element, 0 is illegal *)
PROCEDURE GetElement(self:T; index:CARDINAL):DirInfo.T RAISES {DirInfoArrayError} =

    BEGIN

      IF self.array # NIL THEN
        IF index >= 1 OR index <= NUMBER(self.array^) THEN
          RETURN self.array[index-1];
        END;
      END;

      (* If we're here then the index was out of bounds *)
      RAISE DirInfoArrayError;
      
    END GetElement;





(* Returns number of elements currently in array *)
PROCEDURE Size(self:T):CARDINAL =
  BEGIN
    RETURN  self.index;
  END Size;



PROCEDURE Sort(self:T):BOOLEAN =
  BEGIN

    IF self.array # NIL THEN
      DirInfoArraySort.Sort(self.array^);
    END;

    RETURN TRUE;
  END Sort;




PROCEDURE GetArray(self:T):REF ARRAY OF DirInfo.T =
BEGIN
  RETURN self.array;
END GetArray;


(* function finds if text filename is in the directory *)
(* this is currently O(n), can be O(ln n) if search were
   smarter

   Hash table would be O(1) *)
PROCEDURE IsMember(self:T;fileName:TEXT; VAR iNum:BITS 16 FOR [0..65535]):BOOLEAN =
  VAR
    result : INTEGER;

  BEGIN
    IF self.index = 0 THEN 
      RETURN FALSE;
    END;

    FOR i:=0 TO self.index-1 DO

      (*      result := DirInfo.Compare(Text.FromChar(self.array[i].name),fileName); *)
      result := Text.Compare(Text.FromChars(self.array[i].name),fileName);
      IF result = 1 THEN 
        RETURN FALSE;
      ELSIF result = 0 THEN
        iNum := self.array[i].iNum;
        
        (*      IF Text.Compare(Text.FromChars(self.array[i].name),fileName) = TRUE THEN*)
        RETURN TRUE;
      END;

    END;
    RETURN FALSE;

  END IsMember;        



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

    self.array := NEW(REF ARRAY OF DirInfo.T, size);

    RETURN self;

  END Initialize;




PROCEDURE NewDirInfoArray(initialSize:CARDINAL):T =
  VAR
    temp:T;

  BEGIN


    IF DEBUG = TRUE THEN
      IO.Put("NewDirInfoArray Called with arg: ",mywr);
      IO.PutInt(initialSize,0,mywr);
      IO.Put("\n",mywr);
    END;

    temp:= NEW(T);
    temp:= temp.initialize(initialSize);

    RETURN temp;

  END NewDirInfoArray;





BEGIN
  mywr := BootIO.Writer();
END DirInfoArray.

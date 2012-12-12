(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Cleaned up.
 *
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Allocates iNums, distributes iNodes new and old.
 *
 *)


MODULE IMap;

IMPORT Fmt,IO;

(* Types *)
IMPORT Segment;
IMPORT SegBuffer;
FROM Segment IMPORT DiskAddress;
FROM LFSTypes IMPORT RCharArray;
FROM LFSTypes IMPORT ShortCard;

(* Constants *)
FROM LFSTypes IMPORT MAXINODES;
FROM LFSTypes IMPORT ILLEGALDISKVALUE;

FROM LFSTypes IMPORT INumInfo;

(* Stuff given by the private INode interface *)
FROM INodePrivate IMPORT ConstructINode;
FROM INodePrivate IMPORT NewINode;

REVEAL T = Public BRANDED
 OBJECT
  segInfo      : Segment.T;
  buffer       : SegBuffer.Buffer;
  iMap         : REF ARRAY OF IMapElement;
  size         : CARDINAL;    (* holds the number of INodes that *)
                              (* the IMap can hold *)
  index        : ShortCard;   (* holds current inum index *)

  mu           : MUTEX;

OVERRIDES

  (* Methods of T *)
  flush            := Flush;
  initialize       := Initialize;
  getLocation      := GetLocation;
  setLocation      := SetLocation;

  getINode         := GetINode;
  getNewINode      := GetNewINode;

  rmINode          := RmINode;
END;

CONST DEBUG = FALSE;

  (********************************************************)
  (* Procedure is used to get an inum for a new file,
     this assures that an inum is not used for two files
     at once *)
PROCEDURE GetNewINum(self :T):INumInfo RAISES{OutOfINums} =
  VAR counter: ShortCard;
      retVal  :INumInfo;
  BEGIN

    IF DEBUG THEN IO.Put("GetNewINum Called, iNum:"); END;

    LOCK self.mu DO
      counter := 0;
      WHILE counter < self.size DO
      (* XXX.  FIXME. It's a linear search.  to slow! *)

        (* this is totally redundant and shouldn't be needed *)
        IF self.index > MAXINODES THEN
          self.index := 0;
        END;
  
        (* Hand out that iNum only if the disk address is illegal,
          AND iNode value is NIL, if not NIL then a user may
          have AND open file that hasn't been written to disk yet.*)
        IF (self.iMap[self.index].entry.diskAddress.segment = ILLEGALDISKVALUE
  	 AND self.iMap[self.index].iNode = NIL) THEN
  
          (* if version gets too big then go back to zero *)
          IF self.iMap[self.index].entry.version < MAXINODES THEN
            self.iMap[self.index].entry.version :=
  		self.iMap[self.index].entry.version + 1;
          ELSE
            self.iMap[self.index].entry.version := 0;
          END;
  
          retVal:= INumInfo{self.index, self.iMap[self.index].entry.version};
          IF DEBUG THEN IO.Put(Fmt.Int(retVal.iNum)&" returned.\n"); END;
          RETURN retVal;
        END;
  
        (* increment the inum index *)
        IF self.index > MAXINODES THEN
          self.index := 0;
        ELSE
          self.index := self.index + 1;
        END;

	(* increment the loop counter *)
	counter := counter+1;
      END;

      IO.PutError("in iMap.getNewINum, No iNum found to return, ");
      IO.PutError("Raising OutOfINums exception.\n");

      RAISE OutOfINums;
    END;  (* end of mu lock *)
  END GetNewINum;


(********************************************************)
(* Function Returns user a new inode instance with an associated inode
   attached to it
   The imap recognizes inodes only as ROOTs, the caller NARROWS it down
   later *)
PROCEDURE GetNewINode(self:T):ROOT RAISES {OutOfINums}=
  VAR newINode:ROOT;
      iNumInfo:INumInfo;
  BEGIN

    IF DEBUG = TRUE THEN IO.Put("GetNewINode called..."); END;

    iNumInfo := GetNewINum(self);  (* get a free inum for it *)

    (* ok, now we need to lock *)
    LOCK self.mu DO
      
      (* get an inode instance for it *)
      newINode := NewINode(self.buffer, self, iNumInfo);

      (* Just a little friendly check for debugging *)
      IF newINode = NIL THEN
        IO.PutError("Error!! Call to NewINode returned NIL.\n");
      END;

      self.iMap[iNumInfo.iNum].iNode := newINode;
        
      IF DEBUG THEN IO.Put("IMap.GetNewINode returning newINode.\n"); END;

      RETURN newINode;

    END; (* end of mu lock *)

  END GetNewINode;

(********************************************************)
(* User knows the inum of the file he wants the inode of,
   user passes iNum to this function, function returns the
   files inode, if it exists, otherwise exception city. *)
PROCEDURE GetINode(self :T; iNum:ShortCard): ROOT RAISES {InvalidINum}=
  BEGIN

    LOCK self.mu DO

      IF DEBUG THEN
        IO.Put("IMap.GetINode Called, iNum:"&Fmt.Int(iNum)&".\n");
      END;

      IF DEBUG THEN
        IF iNum = 0 THEN
          IO.Put("Imap: root at s:"&
		 Fmt.Int(self.iMap[0].entry.diskAddress.segment)&
                 " b:"&
		 Fmt.Int(self.iMap[0].entry.diskAddress.block)&".\n");
        END;
      END;
      

      IF iNum>MAXINODES THEN
        (* too large, raising InvalidINum exception *)
	IO.PutError("Map.GetINode, iNum("&Fmt.Int(iNum)&">MAXINODES\n");
        RAISE InvalidINum;
      END;

      (* if inode is in memory then return it *)
      IF self.iMap[iNum].iNode # NIL THEN
        IF DEBUG THEN
          IO.Put(", IMap.GetINode returning a memory-resident iNode.\n");
        END;
        RETURN self.iMap[iNum].iNode;
      END;

      (****************************************************)
      (* else INode isn't in memory, we must fetch it from
         disk *)
      IF DEBUG THEN IO.Put("iMap.getINode fetching iNode from disk.\n"); END;

      (* case where inode has no recorded disk address *)
      (* I'm not sure if this can occur, just being safe *)
      IF DEBUG THEN
        IO.Put("Location of INode is segment:"&
		Fmt.Int(self.iMap[iNum].entry.diskAddress.segment)&
          	" block:"&
		Fmt.Int(self.iMap[iNum].entry.diskAddress.block)&".\n");
      END;

      IF self.iMap[iNum].entry.diskAddress.segment = ILLEGALDISKVALUE THEN
	IO.PutError("iMap.getINode, requested iNode segment is illegal.\n");
        (* requested iNode doesn't exist, raising InvalidINum exception *)
        RAISE InvalidINum;
      END;
      
      (* otherwise, the inode ain't in memory and needs to be
         retrieved from disk *)
      
      (*******************************)
      (* I don't currently know the name of the exception it will raise *)
      (*******************************)

      IF DEBUG THEN
        IO.Put("\nIMap.GetINode Calling Construct INode\n");
        IO.Put("INode is allegedly located at segment:"&
          Fmt.Int(self.iMap[iNum].entry.diskAddress.segment)&
          " and block "&Fmt.Int(self.iMap[iNum].entry.diskAddress.block)&".\n");
      END;

      TRY self.iMap[iNum].iNode :=
          ConstructINode(self.buffer, self, self.iMap[iNum].entry.diskAddress);
      EXCEPT
      END;

      IF DEBUG THEN IO.Put(" IMap.GetINode returning an iNode.\n"); END;

      RETURN self.iMap[iNum].iNode;

    END; (* end of mu lock *)

  END GetINode;

  (********************************************************)  
  (* GetLocation, returns the location on disk of the iNode cooresponding
     to parameter iNum *)
PROCEDURE GetLocation(self:T;iNum:ShortCard):DiskAddress =
  BEGIN

    LOCK self.mu DO

      IF DEBUG THEN
        IO.Put("GetLocation called with iNum:"&Fmt.Int(iNum)&".\n");
      END;

      IF (iNum>=0 AND iNum<MAXINODES) THEN
        RETURN self.iMap[iNum].entry.diskAddress;
      END;

      RETURN DiskAddress{ILLEGALDISKVALUE,ILLEGALDISKVALUE};

    END; (* end of mu lock *)

  END GetLocation;

  (********************************************************)
  (* SetLocation, used to set the location 
     of an iNode on the disk *)
PROCEDURE SetLocation(self:T;iNum:ShortCard; newLocation:DiskAddress):BOOLEAN =
  BEGIN
    
    LOCK self.mu DO
      
      IF DEBUG THEN
        IO.Put("SetLocation called with iNum:"&Fmt.Int(iNum)&
          " s:"&Fmt.Int(newLocation.segment)&" b:"&
          Fmt.Int(newLocation.block)&".\n");
      END;

      IF (iNum>=0 AND iNum<MAXINODES) THEN
        self.iMap[iNum].entry.diskAddress := newLocation;
        RETURN TRUE;
      END;

      RETURN FALSE;
      
    END; (* end of mu lock *)
    
  END SetLocation;
  
  (* destroys all trace of an iNode from the iMap *)
PROCEDURE RmINode(self:T;iNum:ShortCard) =
  BEGIN
    WITH iMapEntry = self.iMap[iNum] DO
      iMapEntry.entry.diskAddress :=
		DiskAddress{ILLEGALDISKVALUE,ILLEGALDISKVALUE};
      iMapEntry.iNode := NIL;
      INC(iMapEntry.entry.version);
    END;

  END RmINode;


  (********************************************************)
  (* Expects an UNINITIALIZED array, this function allocates space for it
     and fills it up.
     Function returns size of alloced array, in chars *)
  (* This function doesn't store the inode portion of the
     INodeBlockInfo *)
PROCEDURE Flush(self:T):RCharArray =
  VAR buff: RCharArray;
  BEGIN

    buff := NEW(RCharArray, self.size*IMAPENTRYSIZE);

    LOCK self.mu DO
      FOR i := 0 TO (self.size-1) DO
	VIEW(SUBARRAY(buff^, i*IMAPENTRYSIZE, IMAPENTRYSIZE), IMapEntry) :=
		self.iMap[i].entry;
      END;
    END; (* end of mu lock *)

    RETURN buff;

  END Flush;
  
PROCEDURE Initialize(self:T;
                     segInfo:Segment.T;
                     buffer: SegBuffer.Buffer;
                     numberOfINums:ShortCard;
                     READONLY charArray: REF ARRAY OF CHAR):T=
  BEGIN

    IF DEBUG THEN IO.Put("Entering iMap.initialize.\n"); END;

    self.size := numberOfINums;
    self.segInfo := segInfo;
    self.buffer := buffer;
    self.iMap := NEW(REF ARRAY OF IMapElement, numberOfINums);

    IF self.mu=NIL THEN self.mu := NEW(MUTEX); END;

    (* this says that the iMap will always try to allocate
       0 as the first iNum *)
    self.index := 0;

    (* set the disk addresses that hang off the array  to illegal values *)
    FOR i:=0 TO numberOfINums-1 DO
      self.iMap[i].entry.diskAddress:=
	DiskAddress{ILLEGALDISKVALUE,ILLEGALDISKVALUE};
    END;

    IF DEBUG = TRUE THEN IO.Put("Initializing IMap from char array.\n"); END;

    (* Make sure the array we were passed has enough chars to
       fill the size requested *)
    IF (NUMBER(charArray^)= (numberOfINums*IMAPENTRYSIZE)) THEN
      FOR i := 0 TO (self.size-1) DO
        self.iMap[i].entry :=
          VIEW(SUBARRAY(charArray^, i*IMAPENTRYSIZE, IMAPENTRYSIZE), IMapEntry);
        self.iMap[i].iNode := NIL;
      END;
      
      IF DEBUG = TRUE THEN IO.Put("IMap Initialized"); END;

    ELSE
      IO.Put("Attempt in IMap Initialization to Alloc with bad sizes\n");
      IO.Put("Char Array Size = ");
      IO.PutInt(NUMBER(charArray^),0);
      IO.Put(" and numberOfINums*IMAPENTRYSIZE = ");
      IO.PutInt(numberOfINums*IMAPENTRYSIZE,0);
      IO.Put("\n");
    END;
    
    RETURN self;
    
  END Initialize;

PROCEDURE NewIMap(segInfo: Segment.T;
                  buffer:SegBuffer.Buffer; 
                  READONLY charArray: REF ARRAY OF CHAR):T =
  VAR
    temp:T;
  BEGIN

    IF DEBUG THEN IO.Put("NewIMap Called.\n"); END;

    temp:= NEW(T);
    temp:= temp.initialize(segInfo,buffer,MAXINODES,charArray);

    IF DEBUG THEN IO.Put("NewIMap Returning.\n"); END;

    RETURN temp;
    
  END NewIMap;
  
BEGIN
END IMap.

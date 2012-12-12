(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-96  Eric Christoffersen (ericc) at the University of Washington
 *	Type for populating DirObject, maps Text to INum, sortable by Text.  Created.
 *)
MODULE DirInfo;
IMPORT Text;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1] =
VAR 
  BEGIN

  
    (*****************************************************)
    (* If the objects are NIL then we want them to migrate
       to the end of the array, ie: they are bigger*)
    
    IF a = NIL THEN 
      IF b = NIL THEN 
        RETURN 0;
      ELSE
        RETURN 1;END;
    END;
    
    IF b = NIL THEN
      RETURN -1;
    END;
    
    (* end of nil comparison stuff *)
    (*****************************************************)
    
    
    
    (*****************************************************)
    (* comparison code roughly taken from Harbison M-3 book;1992(C)- page 91 *)
    
    WITH 
      aName = Text.FromChars(a.name),
      bName = Text.FromChars(b.name),
      aLen = Text.Length(aName),
      bLen = Text.Length(bName)
     DO
      FOR i:= 0 TO MIN(aLen,bLen)-1 DO
        
        WITH
          aChar = Text.GetChar(aName,i),
          bChar = Text.GetChar(bName,i)
         DO
          IF aChar#bChar THEN
            
            IF aChar<bChar THEN
              RETURN 1;
            ELSE
              RETURN -1;
            END;
            
          END;
        END;
        
      END; (* end of for loop *)
      
      (* case where all prefix chars are the same, shorter one wins *)
      IF aLen = bLen THEN
        RETURN 0;
      ELSE
        IF aLen < bLen THEN
          RETURN 1;
        ELSE
          RETURN -1;
        END;
      END;
    END;        
  
  END Compare;

BEGIN
END DirInfo.






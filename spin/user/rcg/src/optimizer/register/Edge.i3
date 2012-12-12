INTERFACE Edge;
IMPORT Word;

CONST Brand = "Edge";

TYPE 
  T = RECORD
    u, v: INTEGER;
  END;

PROCEDURE Equal(k1, k2: T): BOOLEAN;
PROCEDURE Hash(k: T): Word.T;

END Edge.

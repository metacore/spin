INTERFACE Move;

IMPORT Word;

CONST Brand = "Move";

TYPE
  State = {Coalesced, Constrained, Frozen, Worklist, Active};

  T = RECORD
    u, v : INTEGER;
    state: State := State.Active;
  END;

PROCEDURE Equal(k1, k2: T): BOOLEAN;
PROCEDURE Hash(k: T): Word.T;

END Move.


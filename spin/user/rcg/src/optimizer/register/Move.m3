MODULE Move;

IMPORT Word;

PROCEDURE Equal(k1, k2: T): BOOLEAN =
  BEGIN
    RETURN k1.u = k2.u AND k1.v = k2.v;
  END Equal;

PROCEDURE Hash(k: T): Word.T =
  BEGIN
    (* The low order bytes should be the most important. *)
    RETURN Word.Insert(k.u, k.v, 8, 8);
  END Hash;

BEGIN
END Move.

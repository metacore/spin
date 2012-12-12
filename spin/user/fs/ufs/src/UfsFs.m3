(*
 *HISTORY
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 * 30-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	fsT changed to T.
 *	
 * 21-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Converted from fs.h
 *)
MODULE UfsFs;
IMPORT Ctypes, FileDefs, Word;

(*
 * Cylinder group macros to locate things in cylinder groups.
 * They calc file system addresses of cylinder group data structures.
 *)
<*INLINE*>
PROCEDURE cgbase(READONLY fs: T; c:INTEGER) : INTEGER =
  BEGIN
    RETURN fs.fs_fpg*c;
  END cgbase;
  
<*INLINE*>
PROCEDURE cgstart(READONLY fs: T; c:INTEGER) : INTEGER =
  BEGIN
    RETURN cgbase(fs, c) +
           fs.fs_cgoffset*(Word.And(c, Word.Not(fs.fs_cgmask)));
  END cgstart;

<*INLINE*>
PROCEDURE cgimin(READONLY fs: T; c:INTEGER) :INTEGER =
  BEGIN
    RETURN cgstart(fs,c)+fs.fs_iblkno;
  END cgimin;

(*
  calculates (blks * fs->fs_frag)
*)
<*INLINE*>
PROCEDURE blkstofrags(READONLY fs:T; blks:INTEGER) : INTEGER =
  BEGIN
    <*ASSERT fs.fs_frag = Word.LeftShift(1,fs.fs_fragshift) *>
    RETURN Word.LeftShift(blks,fs.fs_fragshift);
  END blkstofrags;

<*INLINE*>
PROCEDURE INOPB(READONLY fs:T) : Ctypes.int = 
  BEGIN
    RETURN fs.fs_inopb;
  END INOPB;

<*INLINE*>  
PROCEDURE INOPF(READONLY fs: T) : INTEGER =
  BEGIN
    <*ASSERT fs.fs_frag = Word.LeftShift(1,fs.fs_fragshift) *>
    RETURN Word.RightShift(fs.fs_inopb, fs.fs_fragshift);
  END INOPF;    

PROCEDURE itoo(READONLY fs: T; x: INTEGER): INTEGER =
  BEGIN
    RETURN x MOD INOPB(fs);
  END itoo;

PROCEDURE itog(READONLY fs: T; x: INTEGER): INTEGER =
  BEGIN
    RETURN x DIV fs.fs_ipg;
  END itog;

PROCEDURE itod(READONLY fs: T; x: INTEGER): FileDefs.daddr_t =
  BEGIN
    RETURN cgimin(fs, itog(fs, x)) + 
           blkstofrags(fs, (x MOD fs.fs_ipg) DIV INOPB(fs));
  END itod;

PROCEDURE fsbtodb(READONLY fs:T; b:INTEGER) : INTEGER =
  BEGIN
    RETURN Word.LeftShift(b, fs.fs_fsbtodb);
  END fsbtodb;

PROCEDURE dbtofsb(READONLY fs:T; b:INTEGER) : INTEGER =
  BEGIN
    RETURN Word.RightShift(b, fs.fs_fsbtodb);
  END dbtofsb;

PROCEDURE blkoff(READONLY fs : T; loc : INTEGER) : INTEGER =
  BEGIN
    RETURN loc MOD fs.fs_bsize;
  END blkoff;
PROCEDURE lblkno(READONLY fs : T; loc : INTEGER) : INTEGER =
  BEGIN
    RETURN loc DIV fs.fs_bsize;
  END lblkno;

PROCEDURE fragoff(READONLY fs : T; loc : INTEGER) : INTEGER =
  BEGIN
    RETURN loc MOD fs.fs_fsize;
  END fragoff;
PROCEDURE numfrags(READONLY fs : T; loc : INTEGER) : INTEGER =
  BEGIN
    RETURN loc DIV fs.fs_fsize;
  END numfrags;

PROCEDURE fragroundup(READONLY fs : T; size : INTEGER) : INTEGER =
  BEGIN
    RETURN Word.And((size + fs.fs_fsize - 1),fs.fs_fmask);
  END fragroundup;

PROCEDURE GetLinkNameFromInode(READONLY d: icommon; VAR buf : ARRAY OF CHAR) =
  BEGIN
    (* Since ic_indirect has c-style text, look for '\0'. *)
    FOR i := 0 TO LAST(d.ic_indirect) DO
      buf[i] := d.ic_indirect[i];
      IF d.ic_indirect[i] = '\000' THEN
	RETURN;
      END;
    END;
  END GetLinkNameFromInode;

BEGIN
END UfsFs.

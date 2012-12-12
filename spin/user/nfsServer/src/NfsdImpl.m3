MODULE NfsdImpl EXPORTS NfsdImpl, Nfsd;
IMPORT TextF, Text, IO, Mbuf, Ctypes, Nfsd_x, XDR, RPC, Misc;

PROCEDURE GETATTR(READONLY inParm: nfs_fh; VAR outParm: attrstat) = 
  VAR fp: REF FP;
  BEGIN
    fp := FindFileByFh(inParm);
    IF fp # NIL THEN 
      outParm.status := nfsstat_NFS_OK;
      outParm.attributes := fp.attr;
    ELSE
      outParm.status := nfsstat_NFSERR_NOENT;
    END;
  END GETATTR;

PROCEDURE SETATTR  (<*UNUSED*>READONLY inParm: sattrargs;   VAR outParm: attrstat)=
    BEGIN
      outParm.status := nfsstat_NFSERR_ROFS;
    END SETATTR;

PROCEDURE LOOKUP   (READONLY inParm: diropargs;   VAR outParm: diropres)=
  VAR fp: REF FP;
  BEGIN
    fp := FindFileByName(inParm.name);
    IF fp # NIL THEN
      outParm.status := nfsstat_NFS_OK;
      outParm.ok.file := fp.fh;
      outParm.ok.attributes := fp.attr;
    ELSE
      outParm.status := nfsstat_NFSERR_PERM;
    END
  END LOOKUP;

PROCEDURE READLINK (<*UNUSED*>READONLY inParm: nfs_fh;          VAR outParm: readlinkres)=
    BEGIN
      outParm.status := nfsstat_NFSERR_PERM;
    END READLINK;

PROCEDURE PROC_READ2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : readargs;
    (* outParm : readres; *)
    size    : CARDINAL;
    fp: REF FP;
  BEGIN
    Nfsd_x.Get_readargs(input,inputPos,inParm);
    fp := FindFileByFh(inParm.file);

    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    (* inlined & modified Nfsd_x.Put_readres(); *)
    IF fp # NIL THEN
      size := size + Nfsd_x.Put_nfsstat(output,outputPos,nfsstat_NFS_OK);
      size := size + Nfsd_x.Put_fattr(output,outputPos,fp.attr);
      size := size + XDR.PutWord32(output,outputPos,fp.data.len);

      IF size + fp.data.len < output.mh_hdr.mh_len THEN
        WITH buf = SUBARRAY(fp.data.data^,0,fp.data.len) DO
          size := size + XDR.PutBytes(output,outputPos,buf);
        END;
      ELSE
        TRY
          output.mh_hdr.mh_next := Mbuf.MclGetOa(fp.data.data, fp.data.len);
        EXCEPT
        | Mbuf.LengthMismatch =>
          IO.Put("Internal mbuf length error\n");
        END;
        output.mh_hdr.mh_len := size;
        RETURN size + fp.data.len;
      END;
    ELSE
      size := size + Nfsd_x.Put_nfsstat(output,outputPos,nfsstat_NFSERR_NOENT);
    END;
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_READ2; 

(* XXX not supported 
  PROCEDURE WRITECACHE(READONLY inParm: xxx; VAR outParm: xxx) = 
    BEGIN
    END WRITECACHE;
*)

PROCEDURE WRITE    (<*UNUSED*> READONLY inParm: writeargs;   VAR outParm: attrstat)=
    BEGIN
      outParm.status := nfsstat_NFSERR_ROFS;
    END WRITE;

PROCEDURE CREATE   (<*UNUSED*>READONLY inParm: createargs;  VAR outParm: diropres)=
    BEGIN
      outParm.status := nfsstat_NFSERR_ROFS;
    END CREATE;

PROCEDURE REMOVE   (<*UNUSED*>READONLY inParm: diropargs;   VAR outParm: nfsstat)=
    BEGIN
      outParm := nfsstat_NFSERR_ROFS;
    END REMOVE;

PROCEDURE RENAME   (<*UNUSED*>READONLY inParm: renameargs;  VAR outParm: nfsstat)=
    BEGIN
      outParm := nfsstat_NFSERR_ROFS;
    END RENAME;

PROCEDURE LINK     (<*UNUSED*>READONLY inParm: linkargs;    VAR outParm: nfsstat)=
    BEGIN
      outParm := nfsstat_NFSERR_ROFS;
    END LINK;

PROCEDURE SYMLINK  (<*UNUSED*>READONLY inParm: symlinkargs; VAR outParm: nfsstat)=
    BEGIN
      outParm := nfsstat_NFSERR_ROFS;
    END SYMLINK;

PROCEDURE MKDIR    (<*UNUSED*>READONLY inParm: createargs;  VAR outParm: diropres)=
    BEGIN
      outParm.status := nfsstat_NFSERR_ROFS;
    END MKDIR;

PROCEDURE RMDIR    (<*UNUSED*>READONLY inParm: diropargs;   VAR outParm: nfsstat)=
    BEGIN
      outParm := nfsstat_NFSERR_ROFS;
    END RMDIR;

PROCEDURE PROC_READDIR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : readdirargs;
    (* outParm : readdirres; *)
    fp   : REF FP;
    eSize, size : CARDINAL;
    curr, data : Mbuf.T;

  BEGIN
    Nfsd_x.Get_readdirargs(input,inputPos,inParm);
    IO.Put("\nReaddir with count = ");
    IO.PutInt(inParm.count);
    IO.Put(" and cookie ");
    IO.PutInt(VIEW(inParm.cookie,Ctypes.unsigned_int));
    IO.Put("\n");

    fp := FindFileByFh(inParm.dir);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);

    IF fp # NIL THEN
      size := size + Nfsd_x.Put_nfsstat(output,outputPos,nfsstat_NFS_OK);
      output.mh_hdr.mh_len := size;
      curr := output;
      FOR i := FIRST(files)+1 TO LAST(files) DO
        (* DEBUG 
        IO.Put("entry ");
        IO.PutInt(i);
        *)

        outputPos := 0;
        data := Misc.GetMbuf(Mbuf.MLEN);
        curr.mh_hdr.mh_next := data;
        curr := data;

        (* DEBUG
        IO.Put("|got mbuf|putbool");
        *)
        eSize := XDR.PutBoolean(curr,outputPos,TRUE);

        (* put entry *)
        (* DEBUG
        IO.Put("|putword32");
        *)
        eSize := eSize + XDR.PutWord32(curr,outputPos,files[i].attr.fileid);
        (* DEBUG
        IO.Put("|puttext");
        *)
        eSize := eSize + XDR.PutText(curr,outputPos,files[i].name); (* put filename *)
        (* DEBUG
        IO.Put("|putbytes");
        *)
        eSize := eSize + XDR.PutBytes(curr,outputPos,nfscookie{VAL(i,CHAR),'\000',..}); (* put nfs cookie *)
        curr.mh_hdr.mh_len := eSize;
        INC(size,eSize);
        (* DEBUG
        IO.Put("\n");
        *)
      END;
      curr.mh_hdr.mh_len := curr.mh_hdr.mh_len + 8; (* XXX HACK! *)
      (* no more entries *)
      (* DEBUG
      IO.Put("no entries|putbool");
      *)
      size := size + XDR.PutBoolean(curr,outputPos, FALSE);
      (* eof of dirlist *)
      (* DEBUG
      IO.Put("eof|putbool\n");
      *)
      size := size + XDR.PutBoolean(curr,outputPos,TRUE);
    ELSE
      size := size + Nfsd_x.Put_nfsstat(output,outputPos,nfsstat_NFSERR_NOENT);
      output.mh_hdr.mh_len := size;
    END;
    RETURN size;
  END PROC_READDIR2; 

PROCEDURE STATFS   (<*UNUSED*>READONLY inParm: nfs_fh;          VAR outParm: statfsres)=
    BEGIN
      outParm.status := nfsstat_NFS_OK;
      outParm.ok.tsize  := 1024;
      outParm.ok.bsize  := 512;
      outParm.ok.blocks := 10000;
      outParm.ok.bfree  :=  9000;
      outParm.ok.bavail :=  1000;
    END STATFS;


TYPE MP = RECORD
  name : TEXT;
  fh  : nfs_fh;
END;

CONST
  MountPoints = ARRAY OF MP{MP{"/",nfs_fh{'m','e','f','\000',..}}};

PROCEDURE LookupMountPoint(READONLY name: ARRAY OF CHAR; VAR fh: nfs_fh):BOOLEAN = 
  VAR t: TEXT;

  BEGIN
    FOR i := FIRST(MountPoints) TO LAST(MountPoints) DO
      t := Text.FromChars(name);
      IO.Put("LookupMountPoint comparing ");
      IO.Put(t);
      IO.Put(" with ");
      IO.Put(MountPoints[i].name);
      IO.Put("\n");
      IF name = SUBARRAY(MountPoints[i].name^,0,Text.Length(MountPoints[i].name)) THEN
        fh := MountPoints[i].fh;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END LookupMountPoint;

TYPE FP = RECORD
  name : TEXT;
  fh   : nfs_fh;
  attr : fattr;
  data : nfs_opaquedata;
END;

VAR
  files : ARRAY [1..12] OF REF FP;

CONST
  myUID = 10683;
  myGID = 1069;

PROCEDURE FindFileByFh(READONLY fh: nfs_fh): REF FP = 
  BEGIN
    FOR i := FIRST(files) TO LAST(files) DO
      IF files[i].fh = fh THEN RETURN files[i]; END;
    END;
    RETURN NIL;
  END FindFileByFh;

PROCEDURE FindFileByName(READONLY name: nfs_text): REF FP = 
  BEGIN
    FOR i := FIRST(files) TO LAST(files) DO
      IF Text.Length(files[i].name) = name.len AND
        SUBARRAY(name.name,0,name.len) = SUBARRAY(files[i].name^,0,name.len) THEN
        RETURN files[i]; 
      END;
    END;
    RETURN NIL;
  END FindFileByName;

PROCEDURE Init2(<*UNUSED*>verbose:BOOLEAN) = 
  VAR 
    datasize : CARDINAL := 8;
    blocks : CARDINAL;
  BEGIN
    files[FIRST(files)] := NEW(REF FP);
    files[FIRST(files)].name := MountPoints[FIRST(MountPoints)].name;
    files[FIRST(files)].fh := MountPoints[FIRST(MountPoints)].fh;


    files[FIRST(files)].attr := fattr{ftype_NFDIR, 8_777, 1, myUID,
                                      myGID, 512, 512, 0,1, 777, 0,
                                      nfstime{0,0},nfstime{0,0},nfstime{0,0}};


    FOR i := FIRST(files)+1 TO LAST(files) DO
      files[i] := NEW(REF FP);
      files[i].name := "file" & Text.FromChar(VAL(ORD('0')+i,CHAR));
      files[i].fh   := nfs_fh{VAL(i,CHAR),'\000',..};
      files[i].data.len := datasize;
      files[i].data.data := NEW(REF ARRAY OF CHAR,datasize);
      (* fill buffer with A..Z *)
      WITH buf = files[i].data.data^ DO
        FOR j := FIRST(buf) TO LAST(buf) DO
          buf[j] := VAL(ORD('A') + (j MOD (ORD('Z')-ORD('A'))),CHAR);
        END;
      END;
      blocks := MAX(1,datasize DIV 512);
      files[i].attr := fattr{ftype_NFREG,8_666,1,myUID,myGID,
                             files[i].data.len,512,0,blocks,777,i,
                             nfstime{0,0},nfstime{0,0},nfstime{0,0}};

      (* double data size for next file *)
      datasize := datasize+datasize;
    END;
  END Init2;

PROCEDURE Uninit2(<*UNUSED*>verbose:BOOLEAN) = 
  BEGIN
    FOR i := FIRST(files)+1 TO LAST(files) DO
      files[i] := NIL;
    END;
  END Uninit2; 


BEGIN
END NfsdImpl.

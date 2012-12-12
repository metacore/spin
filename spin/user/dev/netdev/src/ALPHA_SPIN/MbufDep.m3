UNSAFE (* to import Externs *)
MODULE MbufDep EXPORTS Mbuf;

IMPORT MbufDep, MbufExtern, Ctypes, Word;

PROCEDURE MbufMalloc(nowait: Ctypes.int): ADDRESS =
  BEGIN
    RETURN MbufExtern.malloc(MSIZE,MbufDep.MALLOCBUCKETINDEX,MbufDep.M_MBUF,nowait);
  END MbufMalloc;

(* EPHEMERAL *)
<*UNUSED*>
PROCEDURE CsumProc(
    buf  : REF ARRAY OF CHAR; 
    size : CARDINAL; 
    <* UNUSED *> arg  : REFANY;
    csum : Ctypes.unsigned_short): Ctypes.unsigned_short = 
  BEGIN
    RETURN in_checksum(buf^,size,csum);    
  END CsumProc;


<* INLINE *>
PROCEDURE MclGetx(
    methods :Word.T;
    arg1    :Word.T;
    arg2    :Word.T;
    addr    :Word.T;
    len     :CARDINAL;
    wait    :HowT): T =
  VAR mbuf: T;
  BEGIN
    (* allocate an mbuf header *)
    mbuf := m_gethdr(wait,MT_DATA);
    IF mbuf = NIL THEN RETURN NIL; END;
    
    (* set the EXT flag bit *)
    mbuf.mh_hdr.mh_flags := Word.Or(mbuf.mh_hdr.mh_flags, M_EXT);

    WITH ext =
    LOOPHOLE(ADR(mbuf.M_dat[FIRST(mbuf.M_dat)])+extOffset,UNTRACED REF MbufDep.m_extT) DO

      mbuf.mh_hdr.mh_data := LOOPHOLE(addr, Word.T);
      mbuf.mh_hdr.mh_len  := len;

      ext.ext_buf      := addr;
      ext.ext_size     := len;
      ext.ext_methods  := methods;
      ext.ext_arg      := arg1;
      ext.ext_m3oa     := arg2;
      ext.ext_ref.forw := ADR(ext.ext_ref);
      ext.ext_ref.forw := ADR(ext.ext_ref);
    END;
    RETURN mbuf;
  END MclGetx;

(* EPHEMERAL *)
PROCEDURE in_checksum(
    READONLY 
    packet   : ARRAY OF CHAR;
    size     : CARDINAL := 0; 
    prevcsum : Ctypes.unsigned_short := 0):Ctypes.unsigned_short = 
  VAR
    ck:Ctypes.unsigned_short;
  BEGIN
    (* allow client to checksum less than what's in the array *)
    WITH len = NUMBER(packet) DO
      IF size =  0 THEN
        size := len;
      ELSIF size > len THEN
        size := len;
      END;      
    END;

    WITH addr = LOOPHOLE(ADR(packet[FIRST(packet)]),ADDRESS) DO
      IF prevcsum # 0 THEN
        prevcsum := Word.And(Word.Not(prevcsum),16_ffff);
      END;
      ck := MbufExtern.in_checksum(addr,size,prevcsum);
    END;
    ck := Word.And(Word.Not(ck),16_ffff);
    RETURN ck;
  END in_checksum; 

CONST extOffset = BYTESIZE(pkthdrT);
      (* offset of ext in the mbuf M_Dat area *)

PROCEDURE Checksum(
    m:T; 
    csum:Ctypes.unsigned_short; 
    len:CARDINAL): Ctypes.unsigned_short = 
  CONST checkflags = Word.Or(M_EXT,M_M3METHODS);
  VAR ext: UNTRACED REF MbufDep.m_extT;
      rlen, size, offset: CARDINAL;
      m0:T := m;
  BEGIN
    rlen := 0;
    WHILE m0 # NIL AND len > 0 DO
      WITH data_buf = Array(m0) DO
        IF BYTESIZE(data_buf^) > 0 THEN
          size := MIN(len,BYTESIZE(data_buf^));
          IF Word.And(m0.mh_hdr.mh_flags,checkflags) = checkflags THEN 
            ext  :=
	    LOOPHOLE(ADR(m0.M_dat[FIRST(m0.M_dat)])+extOffset,UNTRACED REF MbufDep.m_extT);
            WITH methods = LOOPHOLE(ext.ext_methods, Methods) DO
              IF methods # NIL AND methods.csum # NIL THEN
                (* compute offset into the external buffer *)
                offset := m0.mh_hdr.mh_data - ext.ext_buf;
                (* This is a EPHEMERAL upcall and we need to put
                   something here to terminate the user's procedure *)

                csum := methods.csum(
                            LOOPHOLE(ext.ext_m3oa,REF ARRAY OF CHAR),
                            size,
                            LOOPHOLE(ext.ext_arg, REFANY),
                            csum, offset);
              ELSE
                (* standard checksum on mbuf
                   - offset into databuf^ is computed above
                   - size should also be <= BYTESIZE(data_but^)
                *)

                (* XXX weird hack to compute correct check sum if
                   the address and the length are unaligned and odd *)
                IF Word.And(Word.Xor(rlen,LOOPHOLE(ADR(data_buf^[0]),Word.T)),1) = 0 THEN
                  csum := in_checksum(data_buf^,size,csum);
                ELSE
                  csum := MbufExtern.nstoh(in_checksum(data_buf^,size,MbufExtern.nstoh(csum)));
                END;
              END;
            END;
          ELSE
            (* standard checksum on mbuf
               - offset into databuf^ is always zero
               - size should also be <= BYTESIZE(data_but^)
            *)
            (* XXX weird hack to compute correct check sum if
               the address and the length are unaligned and odd *)
            IF Word.And(Word.Xor(rlen,LOOPHOLE(ADR(data_buf^[0]),Word.T)),1) = 0 THEN
              csum := in_checksum(data_buf^,size,csum);
            ELSE
              csum := MbufExtern.nstoh(in_checksum(data_buf^,size,MbufExtern.nstoh(csum)));
            END;
          END;

          DEC(len,size);
          INC(rlen,size);
        END;
      END;
      m0 := m0.mh_hdr.mh_next;
    END;
    RETURN csum;
  END Checksum;

BEGIN
END MbufDep.

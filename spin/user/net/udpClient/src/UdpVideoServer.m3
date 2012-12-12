(*
  * Copyright 1994, University of Washington
  * All rights reserved.
  * See COPYRIGHT file for a full description
  *
  *
  * HISTORY
  * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
  *      Implements the server side of the video application.  Sends a
  *      video file to N video clients.
  *      
  *
*)

(* Untrusted *) 
MODULE UdpVideoServer;

IMPORT IO;
IMPORT Clock;
IMPORT Ctypes;
IMPORT Device;
IMPORT Fli;
IMPORT Fmt;
IMPORT IpPktFormat;
IMPORT Mbuf;
IMPORT Net;
IMPORT NetDb;
IMPORT Rofs;
IMPORT RofsTftp;
IMPORT Sema;
IMPORT Spy,SAL;
IMPORT ThreadExtra;
IMPORT UdpGen;
IMPORT UdpPktFormat;
IMPORT UdpVideo;
IMPORT Video;


(* DynShell support *)
IMPORT Dispatcher;
IMPORT SpinShell;
IMPORT Text;
IMPORT SafeConvert;
<* UNUSED *> VAR shell := SpinShell.InstallCommand(EC_Guard,RunCommand);

PROCEDURE EC_Guard(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    RETURN argc >= 0 AND Text.Equal(argv[0], "UdpVideoServer");
  END EC_Guard;

PROCEDURE RunCommand(argc: CARDINAL; READONLY argv: ARRAY OF TEXT): BOOLEAN =
  VAR
  BEGIN

    argc := argc;
    IF Text.Equal(argv[1], "debug") THEN
      debug_level := Net.MapDebug(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 2 AND Text.Equal(argv[1],"cport") THEN
      client_port := Net.htons(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 2 AND Text.Equal(argv[1],"sport") THEN
      server_port := Net.htons(SafeConvert.Atoi(argv[2]));
    ELSIF argc > 5 AND Text.Equal(argv[1],"send") THEN
      VideoData(argv[2],argv[3],argv[4],SafeConvert.Atoi(argv[5]));
    ELSIF argc > 5 AND Text.Equal(argv[1],"blast") THEN
      VideoBlast(argv[2],argv[3],argv[4],SafeConvert.Atoi(argv[5]));
    ELSIF argc > 5 AND Text.Equal(argv[1],"blasttftp") THEN
      VideoBlastTftp(argv[2],argv[3],argv[4],SafeConvert.Atoi(argv[5]));
    ELSIF argc > 2 AND Text.Equal(argv[1],"suck") THEN
      suckoverfirst(argv[2]);
    ELSIF argc > 1 AND Text.Equal(argv[1],"csumtoggle") THEN
      dochecksum := NOT dochecksum;
    ELSE
      IO.Put("UdpRpc.RunCommand: no such command ");
      FOR i := 0 TO argc DO
        IO.Put(argv[i]);
      END;
      IO.Put("\n");
    END;
    RETURN TRUE;
  END RunCommand;


VAR
  fbuffer: REF ARRAY OF CHAR;
  dbuf: ARRAY [0..63] OF REF ARRAY OF CHAR;

  debug_level : Net.Level := Net.oLevel.NODEBUG;

  readfromdisk: Spy.T;
  sendtonet: Spy.T;

  client_port: Ctypes.unsigned_short;
  server_port: Ctypes.unsigned_short;

  dochecksum :BOOLEAN := TRUE;


PROCEDURE VideoFree(<* UNUSED *> buffer: UNTRACED REF ARRAY OF CHAR;
  <* UNUSED *> len: CARDINAL; 
  <* UNUSED *> arg: ADDRESS) = 
  BEGIN
  END VideoFree;

PROCEDURE VideoData(READONLY src,dst:TEXT; READONLY fname:TEXT; nstreams:CARDINAL) = 
  CONST
    udp_hdr_len = BYTESIZE(UdpPktFormat.Header);
    ip_hdr_len  = BYTESIZE(IpPktFormat.Header);

  VAR
    vid: UNTRACED REF UdpVideo.DataPacket;
    dpy: Video.T;
    f:Rofs.T;
    height, width, frames: CARDINAL;
    lsb: BOOLEAN;
    offset: CARDINAL;

    data    : Mbuf.T;
    (* for rofs *)
    mode    : Device.ModeT := 0;
    index   : Device.OffsetT;
    sync    : BOOLEAN := TRUE;
    size    : CARDINAL;
    vidsize : CARDINAL;

    bytestoread : CARDINAL;

    ipaddr  : Ctypes.unsigned_int;
    ip      : IpPktFormat.Header;
    udp     : UdpPktFormat.Header;
  BEGIN

    ipaddr := NetDb.GetHostByName(src);
    IF ipaddr # 0 THEN 
      ip.saddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & src & "\n");
      RETURN;
    END;
    ipaddr := NetDb.GetHostByName(dst);
    IF ipaddr # 0 THEN 
      ip.daddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & dst & "\n");
      RETURN;
    END;
    IO.Put("Sending " & fname & " video to " & dst & ".\n");

    (* open fli file *)
    
    TRY
      TRY
        f := Rofs.GetRootDevice().open(0, fname);
      EXCEPT
      | Device.Error (ec) =>
        IO.Put("RofsLoadFile couldn't open " & fname & " and got error " &
          Rofs.GetRootDevice().errorName(ec) & "\n");
        RETURN;
      END;

      dpy := Video.New(0);
      Video.ClearScreen(dpy);

      (* read fli header *)
      index := 0;
      size := Fli.FILE_HEADER_SIZE;
      IO.Put("Reading fli header\n");
      Spy.On();

      WITH fh = fbuffer^ DO

        TRY
          bytestoread := size;
          f.read2(mode, index, sync, size, fh);
          IO.Put("Read " & Fmt.Int(size) & " from file " & fname & "\n");
        EXCEPT
        | Device.Error => IO.Put("Could not read " & fname & "\n");
          RETURN;
        END;

        IF size # bytestoread THEN
          IO.Put("filesystem read didn't return requested size bytes.\n");
          RETURN;
        END;

        IF Fli.fli_magic(fh) = FALSE THEN
          IO.Put("Fli magic doesn't match.\n");
          RETURN;
        END;

        frames := Fli.fli_frames(fh);
        height := Fli.fli_height(fh);
        width  := Fli.fli_width(fh);

        (* read the rest of fli file *)
        FOR i := 1 TO frames DO

          (* read frame at a time from disk *)

          (* increment index to current point in file *)
          INC(index,size);

          (* next size to read from disk *)
          size := Fli.FRAME_HEADER_SIZE+8;

          WITH netbuf = dbuf[i MOD LAST(dbuf)],
               fhbuffer = SUBARRAY(netbuf^, BYTESIZE(UdpVideo.DataPacket),size),
               fh = fhbuffer
           DO 

            data := Mbuf.MclGetOa(netbuf,VideoFree);
            IF data = NIL THEN
              IO.Put("UdpVideoServer PANIC Mbuf.mclgetoa returned NIL.\n");
              EXIT;
            END;

            WITH vid = VIEW(netbuf^,UdpVideo.DataPacket) DO
              vid.type := UdpVideo.Data; (* set packet to contain video data *)
            END;

            Spy.Enter(readfromdisk);

            (* read frame header bytes *)
            TRY
              bytestoread := size;
              f.read2(mode, index, sync, size, fh);
              IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n");
            EXCEPT
            | Device.Error => 
              IO.Put("Could not read fli frame header.\n");
              EXIT;
            END;

            (* verify that we got the right number of bytes *)
            IF size < Fli.FRAME_HEADER_SIZE THEN
              IO.Put("size of data read too small.\n");
              EXIT;
            END;

            (* find the frame header in the bytes just read in *)
            IF Fli.fli_read_frame_header(fh,fh,offset,lsb) = FALSE THEN
              IO.Put("fli frame header incorrect.\n");
              Fli.fli_dump(fh);
              EXIT;
            END;

            (* get size of this frame and  frameheader size *)
            size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);

            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.INFO, "fli_size = "&Fmt.Int(size)&"\n");
            END;

            DEC(size,Fli.FRAME_HEADER_SIZE);

            WITH frame = 
                 SUBARRAY (netbuf^,
                           BYTESIZE(UdpVideo.DataPacket)+Fli.FRAME_HEADER_SIZE,
                           size) DO
              (* set index to start of frame data and read frame from disk *)
              INC(index,Fli.FRAME_HEADER_SIZE+offset);
              TRY
                bytestoread := size;
                f.read2(mode, index, sync, size, frame);
                IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n");
              EXCEPT
              | Device.Error => 
                IO.Put("Could not read fli frame data.\n");
                EXIT;
              END;
              Spy.Exit(readfromdisk);

              IF size # bytestoread THEN
                EXIT;
              END;
            END; (* frame *)

            vidsize := size + Fli.FRAME_HEADER_SIZE + BYTESIZE(UdpVideo.DataPacket);
            (* #ifdef debug_level != NODEBUG *)
            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.INFO,
                        "["&Fmt.Int(i)&":"&Fmt.Int(vidsize)&"]");
            END;
            Spy.Enter(sendtonet);

            (* adjust mbuf length so that devices don't go nuts *)
            data.mh_hdr.mh_len := vidsize;

            (* XXX need an ip header template generator *)
            ip.hlen := 5;
            ip.tos := 0;
            ip.tot_len := ip_hdr_len + udp_hdr_len + vidsize;
            ip.id  := 0;
            ip.frag_off := 0;
            ip.ttl := 16_ff;
            ip.protocol := IpPktFormat.IPPROTO_UDP;

            (* set udp information *)
            udp.sport := server_port;
            udp.dport := client_port;
            udp.len   := udp_hdr_len + vidsize;

            FOR i := 1 TO nstreams DO 
              UdpGen.PacketSend(ip, udp, data);
            END;
            Spy.Exit(sendtonet);
          END; (* with netbuf, data fhbuffer, fh *)
        END; (* for iterate over number of frames *)
      END;
      Spy.Off();
    FINALLY
      TRY
        IF f # NIL THEN f.close(); END;
      EXCEPT
      | Device.Error => IO.Put("Could not close " & fname & "\n");
      END;
    END;
  END VideoData; 

PROCEDURE VideoBlast(READONLY src,dst:TEXT; READONLY fname:TEXT; nstreams:CARDINAL) = 
  CONST
    udp_hdr_len = BYTESIZE(UdpPktFormat.Header);
    ip_hdr_len  = BYTESIZE(IpPktFormat.Header);

  VAR
    vid: UNTRACED REF UdpVideo.DataPacket;
    dpy: Video.T;
    f:Rofs.T;
    height, width, frames: CARDINAL;
    lsb: BOOLEAN;
    offset: CARDINAL;

    data    : Mbuf.T;
    (* for rofs *)
    mode    : Device.ModeT := 0;
    index   : Device.OffsetT;
    sync    : BOOLEAN := TRUE;
    size    : CARDINAL;
    vidsize : CARDINAL;

    bytestoread : CARDINAL;

    ipaddr  : Ctypes.unsigned_int;
    ip      : IpPktFormat.Header;
    udp     : UdpPktFormat.Header;
  BEGIN

    ipaddr := NetDb.GetHostByName(src);
    IF ipaddr # 0 THEN 
      ip.saddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & src & "\n");
      RETURN;
    END;
    ipaddr := NetDb.GetHostByName(dst);
    IF ipaddr # 0 THEN 
      ip.daddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & dst & "\n");
      RETURN;
    END;
    IO.Put("Sending " & fname & " video to " & dst & ".\n");

    (* open fli file *)
    
    TRY
      TRY
        f := Rofs.GetRootDevice().open(0, fname);
      EXCEPT
      | Device.Error (ec) =>
        IO.Put("RofsLoadFile couldn't open " & fname & " and got error " &
          Rofs.GetRootDevice().errorName(ec) & "\n");
        RETURN;
      END;

      dpy := Video.New(0);
      Video.ClearScreen(dpy);

      (* read fli header *)
      index := 0;
      size := Fli.FILE_HEADER_SIZE;
      IO.Put("Reading fli header\n");
      Spy.On();

      WITH fh = fbuffer^ DO

        TRY
          bytestoread := size;
          f.read2(mode, index, sync, size, fh);
          (* IO.Put("Read " & Fmt.Int(size) & " from file " & fname & "\n"); *)
        EXCEPT
        | Device.Error => IO.Put("Could not read " & fname & "\n");
          RETURN;
        END;

        IF size # bytestoread THEN
          IO.Put("filesystem read didn't return requested size bytes.\n");
          RETURN;
        END;

        IF Fli.fli_magic(fh) = FALSE THEN
          IO.Put("Fli magic doesn't match.\n");
          RETURN;
        END;

        frames := Fli.fli_frames(fh);
        height := Fli.fli_height(fh);
        width  := Fli.fli_width(fh);

        (* PHASE 1: read in all of the fli buffer data *)


        (* read the rest of fli file *)
        FOR i := 1 TO frames DO

          (* read frame at a time from disk *)

          (* increment index to current point in file *)
          INC(index,size);

          (* next size to read from disk *)
          size := Fli.FRAME_HEADER_SIZE+8;

          WITH netbuf = dbuf[i MOD LAST(dbuf)],
               fhbuffer = SUBARRAY(netbuf^, BYTESIZE(UdpVideo.DataPacket),size),
               fh = fhbuffer
           DO 

            WITH vid = VIEW(netbuf^,UdpVideo.DataPacket) DO
              vid.type := UdpVideo.Data; (* set packet to contain video data *)
            END;

            Spy.Enter(readfromdisk);

            (* read frame header bytes *)
            TRY
              bytestoread := size;
              f.read2(mode, index, sync, size, fh);
              IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n");
            EXCEPT
            | Device.Error => 
              IO.Put("Could not read fli frame header.\n");
              EXIT;
            END;

            (* verify that we got the right number of bytes *)
            IF size < Fli.FRAME_HEADER_SIZE THEN
              IO.Put("size of data read too small.\n");
              EXIT;
            END;

            (* find the frame header in the bytes just read in *)
            IF Fli.fli_read_frame_header(fh,fh,offset,lsb) = FALSE THEN
              IO.Put("fli frame header incorrect.\n");
              Fli.fli_dump(fh);
              EXIT;
            END;

            (* get size of this frame and  frameheader size *)
            size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);

            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.INFO, "fli_size = "&Fmt.Int(size)&"\n");
            END;

            DEC(size,Fli.FRAME_HEADER_SIZE);

            WITH frame = 
                 SUBARRAY (netbuf^,
                           BYTESIZE(UdpVideo.DataPacket)+Fli.FRAME_HEADER_SIZE,
                           size) DO
              (* set index to start of frame data and read frame from disk *)
              INC(index,Fli.FRAME_HEADER_SIZE+offset);
              TRY
                bytestoread := size;
                f.read2(mode, index, sync, size, frame);
                IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n");
              EXCEPT
              | Device.Error => 
                IO.Put("Could not read fli frame data.\n");
                EXIT;
              END;
              Spy.Exit(readfromdisk);

              IF size # bytestoread THEN
                EXIT;
              END;
            END; (* frame *)
          END; (* with netbuf, data fhbuffer, fh *)
        END; (* for iterate over number of frames *)
      END;
      Spy.Off();
    FINALLY
      TRY
        IF f # NIL THEN f.close(); END;
      EXCEPT
      | Device.Error => IO.Put("Could not close " & fname & "\n");
      END;
    END;

    ip.hlen := 5;
    ip.tos := 0;
    ip.id  := 0;
    ip.frag_off := 0;
    ip.ttl := 16_ff;
    ip.protocol := IpPktFormat.IPPROTO_UDP;
    udp.sport := server_port;
    udp.dport := client_port;

    (* PHASE 1: write all of the fli buffer data to network *)

    (* send the fli file over network *)
    FOR i := 1 TO frames DO
      Spy.Enter(sendtonet);

      WITH netbuf = dbuf[i MOD LAST(dbuf)],
           fhbuffer = SUBARRAY(netbuf^, BYTESIZE(UdpVideo.DataPacket),size),
           fh = fhbuffer
       DO
        (* get size of this frame and  frameheader size *)
        size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);
        
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO, "fli_size = "&Fmt.Int(size)&"\n");
        END;

        vidsize := size + BYTESIZE(UdpVideo.DataPacket);
        (* #ifdef debug_level != NODEBUG *)
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,
                    "["&Fmt.Int(i)&":"&Fmt.Int(vidsize)&"]");
        END;

        (* XXX need an ip header template generator *)
        ip.tot_len := ip_hdr_len + udp_hdr_len + vidsize;
        (* set udp information *)
        udp.len   := udp_hdr_len + vidsize;

        data := Mbuf.MclGetOa(netbuf,VideoFree);
        IF data = NIL THEN
          IO.Put("UdpVideoServer PANIC Mbuf.mclgetoa returned NIL.\n");
          RETURN;
        END;
        (* adjust mbuf length to actual size of frame and length of buffer *)
        data.mh_hdr.mh_len := vidsize;
        FOR i := 1 TO nstreams DO 
          UdpGen.PacketSend(ip, udp, data);
        END;
      END;
      Spy.Exit(sendtonet);
    END;
  END VideoBlast; 


VAR f:Rofs.T;
PROCEDURE suckoverfirst(fname:TEXT) = 
  BEGIN

      TRY
        f := RofsTftp.GetRootDevice().open(0, fname);
      EXCEPT
      | Device.Error (ec) =>
        IO.Put("RofsLoadFile couldn't open " & fname & " and got error " &
          Rofs.GetRootDevice().errorName(ec) & "\n");
        RETURN;
      END;

  END suckoverfirst;

PROCEDURE VideoBlastTftp(READONLY src,dst:TEXT; READONLY fname:TEXT; nstreams:CARDINAL) = 
  CONST
    udp_hdr_len = BYTESIZE(UdpPktFormat.Header);
    ip_hdr_len  = BYTESIZE(IpPktFormat.Header);

  VAR
    idlestart : INTEGER;
    idlestop  : INTEGER;

    vid: UNTRACED REF UdpVideo.DataPacket;
    dpy: Video.T;
    height, width, frames: CARDINAL;
    lsb: BOOLEAN;
    offset: CARDINAL;

    data    : Mbuf.T;
    (* for rofs *)
    mode    : Device.ModeT := 0;
    index   : Device.OffsetT;
    sync    : BOOLEAN := TRUE;
    size    : CARDINAL;
    vidsize : CARDINAL;

    bytestoread : CARDINAL;

    ipaddr  : Ctypes.unsigned_int;
    ip      : IpPktFormat.Header;
    udp     : UdpPktFormat.Header;

    pace : Sema.T := Sema.Alloc(0);
  BEGIN

    ipaddr := NetDb.GetHostByName(src);
    IF ipaddr # 0 THEN 
      ip.saddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & src & "\n");
      RETURN;
    END;
    ipaddr := NetDb.GetHostByName(dst);
    IF ipaddr # 0 THEN 
      ip.daddr := ipaddr;
    ELSE
      IO.Put("Couldn't find IP address for " & dst & "\n");
      RETURN;
    END;
    IO.Put("Sending " & fname & " video to " & dst & ".\n");

    IF nstreams = 0 THEN nstreams := 1 END;
    (* open fli file *)
    
    TRY
      dpy := Video.New(0);
      Video.ClearScreen(dpy);

      (* read fli header *)
      index := 0;
      size := Fli.FILE_HEADER_SIZE;
      IO.Put("Reading fli header\n");
      Spy.On();

      WITH fh = fbuffer^ DO

        TRY
          bytestoread := size;
          f.read2(mode, index, sync, size, fh);
          (* IO.Put("Read " & Fmt.Int(size) & " from file " & fname & "\n"); *)
        EXCEPT
        | Device.Error => IO.Put("Could not read " & fname & "\n");
          RETURN;
        END;

        
          IO.Put("Reading fli header\n");


        IF size # bytestoread THEN
          IO.Put("filesystem read didn't return requested size bytes.\n");
          RETURN;
        END;

        IF Fli.fli_magic(fh) = FALSE THEN
          IO.Put("Fli magic doesn't match.\n");
          RETURN;
        END;

        frames := Fli.fli_frames(fh);
        height := Fli.fli_height(fh);
        width  := Fli.fli_width(fh);

        (* PHASE 1: read in all of the fli buffer data *)


        (* read the rest of fli file *)
        FOR i := 1 TO frames DO

          (* read frame at a time from disk *)

          (* increment index to current point in file *)
          INC(index,size);

          (* next size to read from disk *)
          size := Fli.FRAME_HEADER_SIZE+8;

          WITH netbuf = dbuf[i MOD LAST(dbuf)],
               fhbuffer = SUBARRAY(netbuf^, BYTESIZE(UdpVideo.DataPacket),size),
               fh = fhbuffer
           DO 

            WITH vid = VIEW(netbuf^, UdpVideo.DataPacket) DO
              vid.type := UdpVideo.Data; (* set packet to contain video data *)
            END;

            Spy.Enter(readfromdisk);

            (* read frame header bytes *)
            TRY
              bytestoread := size;
              f.read2(mode, index, sync, size, fh);
              (* IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n"); *)
            EXCEPT
            | Device.Error => 
              IO.Put("Could not read fli frame header.\n");
              EXIT;
            END;

            (* verify that we got the right number of bytes *)
            IF size < Fli.FRAME_HEADER_SIZE THEN
              IO.Put("size of data read too small.\n");
              EXIT;
            END;

            (* find the frame header in the bytes just read in *)
            IF Fli.fli_read_frame_header(fh,fh,offset,lsb) = FALSE THEN
              IO.Put("fli frame header incorrect.\n");
              Fli.fli_dump(fh);
              EXIT;
            END;

            (* get size of this frame and  frameheader size *)
            size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);

            IF debug_level # Net.oLevel.NODEBUG THEN
              Net.Debug(debug_level, Net.oLevel.INFO, "fli_size = "&Fmt.Int(size)&"\n");
            END;

            DEC(size,Fli.FRAME_HEADER_SIZE);

            WITH frame = 
                 SUBARRAY (netbuf^,
                           BYTESIZE(UdpVideo.DataPacket)+Fli.FRAME_HEADER_SIZE,
                           size) DO
              (* set index to start of frame data and read frame from disk *)
              INC(index,Fli.FRAME_HEADER_SIZE+offset);
              TRY
                bytestoread := size;
                f.read2(mode, index, sync, size, frame);
                (* IO.Put("Read " & Fmt.Int(size) & " from file " & fname &"\n"); *)
              EXCEPT
              | Device.Error => 
                IO.Put("Could not read fli frame data.\n");
                EXIT;
              END;
              Spy.Exit(readfromdisk);

              IF size # bytestoread THEN
                EXIT;
              END;
            END; (* frame *)
          END; (* with netbuf, data fhbuffer, fh *)
        END; (* for iterate over number of frames *)
      END;
      Spy.Off();
    FINALLY
      (* 
      TRY
        IF f # NIL THEN f.close(); END;
      EXCEPT
      | Device.Error => IO.Put("Could not close " & fname & "\n");
      END;
      *)
      IO.Put("Done reading fli file.\n");
    END;

    ip.hlen := 5;
    ip.tos := 0;
    ip.id  := 0;
    ip.frag_off := 0;
    ip.ttl := 16_ff;
    ip.protocol := IpPktFormat.IPPROTO_UDP;
    udp.sport := server_port;
    udp.dport := client_port;

    (* PHASE 1: write all of the fli buffer data to network *)

    (* send the fli file over network *)
    Spy.On();
    Spy.Enter(sendtonet);
    idlestart := ThreadExtra.idletimer;
    FOR i := 1 TO frames DO


      (* set rate that we are going to send video out over network *)
      Clock.SetAlarm(1024 DIV (30), VideoPaceMaker, pace);

      WITH netbuf = dbuf[i MOD LAST(dbuf)],
           fhbuffer = SUBARRAY(netbuf^, BYTESIZE(UdpVideo.DataPacket),size),
           fh = fhbuffer
       DO
        (* get size of this frame and  frameheader size *)
        size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);
        
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO, "fli_size = "&Fmt.Int(size)&"\n");
        END;

        vidsize := size + BYTESIZE(UdpVideo.DataPacket);
        (* #ifdef debug_level != NODEBUG *)
        IF debug_level # Net.oLevel.NODEBUG THEN
          Net.Debug(debug_level, Net.oLevel.INFO,
                    "["&Fmt.Int(i)&":"&Fmt.Int(vidsize)&"]");
        END;

        (* XXX need an ip header template generator *)
        ip.tot_len := ip_hdr_len + udp_hdr_len + vidsize;
        (* set udp information *)
        udp.len   := udp_hdr_len + vidsize;

        FOR i := 1 TO nstreams DO 
          data := Mbuf.MclGetOa(netbuf,VideoFree);
          IF data = NIL THEN
            IO.Put("UdpVideoServer PANIC Mbuf.mclgetoa returned NIL.\n");
            RETURN;
          END;
          (* adjust mbuf length to actual size of frame and length of buffer *)
          data.mh_hdr.mh_len := vidsize;
          UdpGen.PacketSend(ip, udp, data, dochecksum);
        END;
      END;

      Sema.P(pace);

    END;
    idlestop := ThreadExtra.idletimer;
    Spy.Exit(sendtonet);
    Spy.Off();
    Spy.Dump();
    IO.Put("UdpVideoServer idletime = " & Fmt.Int(idlestop-idlestart) & "\n");
  END VideoBlastTftp; 

PROCEDURE VideoPaceMaker(arg:REFANY) = 
  BEGIN
    WITH dudad = NARROW(arg,Sema.T) DO
      Sema.V(dudad);
    END;
  END VideoPaceMaker;


PROCEDURE Init() =
  BEGIN

  readfromdisk := Spy.Create("video_server cam_read");
  sendtonet    := Spy.Create("video_server udp_output");
  client_port  := Net.htons(12121);
  server_port  := Net.htons(21212);

  fbuffer      := NEW(REF ARRAY OF CHAR, Fli.FILE_HEADER_SIZE);
  FOR i := FIRST(dbuf) TO LAST(dbuf) DO
    dbuf[i]    := NEW(REF ARRAY OF CHAR, (32*1000));
  END;

  IO.Put("UdpVideoServer initialized.\n");
  END Init;

BEGIN
END UdpVideoServer.

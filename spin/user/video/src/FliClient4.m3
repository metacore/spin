UNSAFE MODULE FliClient4 EXPORTS FliClient;
IMPORT Video;
IMPORT Fli;
IMPORT Rofs;
IMPORT Clib;
IMPORT Fmt;
IMPORT ThreadExtra;


VAR
  fbuffer: ARRAY [0..Fli.FILE_HEADER_SIZE] OF CHAR;
  fhbuffer: ARRAY [0..Fli.FRAME_HEADER_SIZE+8] OF CHAR;
  databuffer: ARRAY [0..(16*8192)] OF CHAR;

TYPE Info = REF RECORD
  x,y, repeat: CARDINAL;
  fd:Rofs.T;
END;


PROCEDURE load4_thread(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    dpy:Video.T;
    fd:Rofs.T;
    fh:Fli.T;
    rc:Rofs.Errors;
    start, size, index, height, width, frames, bytesread: CARDINAL;
    f, flibuffer: ADDRESS;
    lsb: BOOLEAN;
    offset: CARDINAL;
    i:Info;
    x,y,repeat: CARDINAL;
  BEGIN
    i := LOOPHOLE(arg,Info);
    fd := i.fd;
    repeat := i.repeat;

    x := i.x;
    y := i.y;

    dpy := Video.video_new(0);
    Video.video_clear_screen(dpy);


    (* read fli header *)
    index := 0;
    size := Fli.FILE_HEADER_SIZE;
    rc := Rofs.Read_file(fd,index,fbuffer,size,bytesread);
    f := (* CAST *) LOOPHOLE(ADR(fbuffer[FIRST(fbuffer)]),ADDRESS);
    
    start:=bytesread;
    IF rc = Rofs.Errors.NONE AND bytesread = Fli.FILE_HEADER_SIZE AND Fli.fli_magic(f) THEN
      frames := Fli.fli_frames(f);
      height := Fli.fli_height(f);
      width  := Fli.fli_width(f);


      FOR count := 1 TO repeat DO 
        index := 0;
        bytesread := start;
        
        (* read the rest of fli file *)
        FOR i := 1 TO frames DO
          INC(index,bytesread);

          (* read frame header bytes *)
          rc := Rofs.Read_file(fd,index,fhbuffer,BYTESIZE(fhbuffer),bytesread);

          (* verify that we got the right number of bytes *)
          IF rc = Rofs.Errors.NONE AND bytesread = BYTESIZE(fhbuffer) THEN

            (* find the frame header in the bytes just read in *)
            fh := (* CAST *) LOOPHOLE (ADR(fhbuffer[FIRST(fhbuffer)]),Fli.T);
            IF Fli.fli_read_frame_header(ADR(fhbuffer[FIRST(fhbuffer)]),fh,offset,lsb) THEN

              (* compute size of this frame *)
              size := Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb);

              (* compensate size with frameheader size *)
              DEC(size,Fli.FRAME_HEADER_SIZE);

              (* set index to start of frame data *)
              INC(index,Fli.FRAME_HEADER_SIZE+offset);

              (* read in the frame data *)
              rc := Rofs.Read_file(fd,index,databuffer,size,bytesread);

              IF rc = Rofs.Errors.NONE AND bytesread = size THEN
                
                flibuffer:= ADR(databuffer[FIRST(databuffer)]);


                (*
                  IF count = 1 AND i = 1 THEN
                  Fli.fli_display_frame(fh, dpy, height, width, flibuffer,lsb);
                  ELSE
                  FOR x := 0 TO Video.video_frame_width(dpy) BY width DO
                  FOR y := 0 TO Video.video_frame_height(dpy) BY height DO
                  Video.video_set_screen_pos(dpy,x,y);
                  Fli.fli_display_frame(fh, dpy, height, width, flibuffer,lsb);
                  END;
                  END;
                  END;
                *)

                Video.video_set_screen_pos(dpy,x,y);
                Fli.fli_display_frame(fh, dpy, height, width, flibuffer,lsb);

              ELSE

                Clib.Print("Could not read fli frame data.\n");
                EXIT;

              END;
            ELSE
              Clib.Print("fli frame header incorrect.\n");
              Fli.fli_dump(fh);
              EXIT;
            END;
          ELSE
            Clib.Print("Could not read fli frame header.\n");
            EXIT;
          END;
        END; (* for *)
      END;
      Rofs.Close_file(fd);
      Video.video_dealloc(dpy);
    ELSE
      Clib.Print("Could not read fli file header.\n");
    END;
    RETURN 0;
  END load4_thread;


PROCEDURE load4(fname:TEXT; x:CARDINAL; y:CARDINAL; repeat:CARDINAL) =
  VAR
    rc:Rofs.Errors;
    fd:Rofs.T;
    i: Info;
  BEGIN
    Clib.Print("FliClient.load4 loading... " & 
      fname & " repeating " & Fmt.Int(repeat)& "\n");
    (* open fli file *)
    rc := Rofs.Open_file(fd,fname);
    IF rc = Rofs.Errors.NONE THEN    
      (* create thread *)
      i:= NEW(Info);
      i.fd := fd;
      i.repeat := repeat;
      i.x := x;
      i.y := y;
      EVAL ThreadExtra.Fork(load4_thread,LOOPHOLE(i,ThreadExtra.ArgT));
    ELSE
      Clib.Print("Could not open fli file.\n");
    END;
  END load4; 

BEGIN

END FliClient4. 

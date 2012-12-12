UNSAFE MODULE FliClient3 EXPORTS FliClient;
IMPORT Video;
IMPORT Fli;
IMPORT Rofs;
IMPORT Clib;
IMPORT Fmt;

VAR
  fbuffer: ARRAY [0..Fli.FILE_HEADER_SIZE] OF CHAR;
  databuffer: ARRAY [0..(1024*1024)-1] OF CHAR;

PROCEDURE load3(fname: TEXT) = 
  VAR
    dpy: Video.T;
    fh:Fli.T;
    rc:Rofs.Errors;
    fd:Rofs.T;
    size, index, data, height, width, frames, bytesread: CARDINAL;
    f,flibuffer:ADDRESS;
    saveoffset, offset: CARDINAL;
    play: BOOLEAN;
    lsb: BOOLEAN;
  BEGIN
    Clib.Print("FliClient.load3 loading... " & fname & "\n");

    (* open fli file *)

    rc := Rofs.Open_file(fd,fname);
    IF rc = Rofs.Errors.NONE THEN    
      dpy := Video.video_new(0);

      (* read fli header *)
      index := 0;
      size := Fli.FILE_HEADER_SIZE;
      rc := Rofs.Read_file(fd,index,fbuffer,size,bytesread);
      f := (* CAST *) LOOPHOLE(ADR(fbuffer[FIRST(fbuffer)]),ADDRESS);

      IF rc = Rofs.Errors.NONE AND bytesread = Fli.FILE_HEADER_SIZE AND Fli.fli_magic(f) THEN

        frames := Fli.fli_frames(f);
        height := Fli.fli_height(f);
        width  := Fli.fli_width(f);

        (* read the rest of fli file *)
        data := 0;
        play := FALSE;

        size := Fli.fli_total_size(f);
        INC(index,Fli.FILE_HEADER_SIZE);
        DEC(size,Fli.FILE_HEADER_SIZE ); (* guessing *)

        rc := Rofs.Read_file(fd,index,databuffer,size,bytesread);
        (* rc := Rofs.Async_read_file(fd,index,databuffer,size,bytesread); *)

        IF rc = Rofs.Errors.NONE AND bytesread = size THEN


          fh := (* CAST *) LOOPHOLE (ADR(databuffer[FIRST(databuffer)]),Fli.T);
          IF Fli.fli_read_frame_header(ADR(databuffer[FIRST(databuffer)]),fh,offset,lsb) THEN
            INC(offset,Fli.FRAME_HEADER_SIZE+offset);
            flibuffer := ADR(databuffer[offset]);
            Fli.fli_display_frame(fh, dpy, height, width, flibuffer,lsb);
            
            INC(offset,Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb));
            
            saveoffset := offset;
            FOR framecount := 1 TO frames DO
              fh := (* CAST *) LOOPHOLE (ADR(databuffer[offset]),Fli.T);
              IF Fli.fli_read_frame_header(ADR(databuffer[offset]),fh,offset,lsb) THEN
                INC(offset,Fli.FRAME_HEADER_SIZE+offset);
                flibuffer := ADR(databuffer[offset]);
                FOR x := 0 TO Video.video_frame_width(dpy) BY width DO
                  FOR y := 0 TO Video.video_frame_height(dpy) BY height DO
                    Video.video_set_screen_pos(dpy,x,y);
                    Fli.fli_display_frame(fh, dpy, height, width, flibuffer,lsb);
                  END;
                END;
                INC(offset,Fli.fli_size(fh,Fli.FRAME_HEADER_SIZE_OFFSET,lsb));
                DEC(offset,Fli.FRAME_HEADER_SIZE);
              ELSE
                Clib.Print("Couldn't read frame " & Fmt.Int(framecount) & "\n");
                Fli.fli_dump(fh);
                Fli.fli_dump(
                EXIT;
              END;
            END;
          END;
          

        ELSE
            Clib.Print("Rofs.Async returned .\n");
        END;

      ELSE
        Clib.Print("Could not read fli file header.\n");
      END;
      Rofs.Close_file(fd);
    ELSE
      Clib.Print("Could not open fli file.\n");
    END;
    Video.video_dealloc(dpy);
  END load3; 

BEGIN

END FliClient3. 

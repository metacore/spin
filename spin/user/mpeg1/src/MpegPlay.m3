(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

UNSAFE MODULE MpegPlay;

IMPORT BitIO, SequenceLayer, GroupOfPicturesLayer, PictureLayer, 
       GeneralTools, PostProcessing, MpegData, IO, Draw; 

PROCEDURE CopyPicture (VAR state: MpegData.MpegState; 
                       a: UNTRACED REF ARRAY OF Draw.Byte;
                       b: UNTRACED REF ARRAY OF Draw.Byte) =
BEGIN
  FOR i := 0 TO (state.sh.horizontalSize*state.sh.verticalSize*3-1) DO
    a[i] := b[i];
  END;
END CopyPicture;

(* The state.mode bit tells us what MPEG layer we're in when called.  Mode 0 means sequence layer, 
   mode 1 means group of pictures layer, mode 2 means picture layer, and mode 3 means we're done 
   (either we've had an error or the video is finished). *)
PROCEDURE MpegPlay (file: BitIO.T; VAR state: MpegData.MpegState) =
VAR
  fbh: Draw.FbHandle;

BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);
         
  TRY
    WHILE state.mode # 3 DO

    IF state.mode = 0 THEN
      (* If this is the first sequence header, initialize the three MpegState picture variables and
         the zig-zag scanning sequence matrix.  *)
      IF state.current = NIL THEN
        SequenceLayer.InitScan(state);
      END;

      (* Decode the header information for the MPEG sequence layer, that is everything up to our 
         first MPEG group of pictures layer. *)
      SequenceLayer.ReadSequenceHeader(file, state);

      IF state.current = NIL THEN
        state.current := NEW (UNTRACED REF ARRAY OF Draw.Byte, state.sh.horizontalSize*
                                                               state.sh.verticalSize*3);
        state.past    := NEW (UNTRACED REF ARRAY OF Draw.Byte, state.sh.horizontalSize*
                                                               state.sh.verticalSize*3);
        state.future  := NEW (UNTRACED REF ARRAY OF Draw.Byte, state.sh.horizontalSize*
                                                               state.sh.verticalSize*3);
      END;

      (* Calculate mbWidth, the number of macroblocks in one row of each picture *)
      state.mbWidth := (state.sh.horizontalSize + 15) DIV 16;

      (* Set mode to 1 because we are now expecting a group of pictures layer. *)
      state.mode := 1;
    END;
  
    IF state.mode = 1 THEN
      (* Decode the header information for the MPEG group of pictures layer, or everything up to 
         our first MPEG picture layer. *)
      GroupOfPicturesLayer.ReadGroupOfPicturesHeader(file, state);

      (* Set the mode to 2 because we now expect an MPEG picture layer. At the start of each group
         of pictures layer, we re-set the past and future pictures to be NIL. We don't actually de-allocate
         the memory for these structures, but set the associated lastTemporalReference and 
         futureTemporalReference variables somewhat arbitrarily to be -1, which I have taken to mean NIL. *) 
      state.mode := 2;
      state.lastTemporalReference := -1;
      state.futureTemporalReference := -1;
    END;

    IF state.mode = 2 THEN
      LOOP (* until we display one frame *)
        (* Is our future picture the next picture? If so, display it, set it 
           to be the past picture, and set the future to nil. *)
        IF (state.futureTemporalReference = state.lastTemporalReference + 1) THEN
          (* Apply the YUV-to-RGB color space conversion to the future picture and write output to
             the current picture. *)
          PostProcessing.YUVtoRGB(state, 1);

          (* Output current picture to the screen using the SpinDraw interface. *)
          EVAL Draw.Bitblt24to8(fbh, state.current, 0, 0, state.sh.horizontalSize, state.sh.verticalSize, 0); 

          (* Housekeeping: Increment the lastTemporalReference var. Since we just displayed the future picture and
             we know it is an I- or P-picture because it was a future picture, set it to be the past picture. And 
             lastly, set the future picture to NIL. *)
          state.lastTemporalReference := state.lastTemporalReference + 1;
          CopyPicture(state, state.past, state.future);
          state.futureTemporalReference := -1;
          EXIT;
        ELSE
          (* 2nd option: If we have a valid future picture, we already know it wasn't the next one in order,
           so read in the next picture in the bitstream. *)
          PictureLayer.ReadPicture(file, state);    

          IF ((state.futureTemporalReference # -1) AND (state.p.pictureCodingType = MpegData.PictureCodingType.B)) THEN 
            (* Apply the YUV-to-RGB color space conversion to the current picture. *)
            PostProcessing.YUVtoRGB(state, 0);

            (* Bitblt state.current to the screen using the SpinDraw interface. *)
            EVAL Draw.Bitblt24to8(fbh, state.current, 0, 0, state.sh.horizontalSize, state.sh.verticalSize, 0); 

            state.lastTemporalReference := state.lastTemporalReference + 1;
            EXIT;
          ELSE
            IF ((state.futureTemporalReference # -1) AND ((state.p.pictureCodingType = MpegData.PictureCodingType.I) OR 
                                                           (state.p.pictureCodingType = MpegData.PictureCodingType.P)))  THEN
              (* This is a hack to get the Mjackson file to play. This corrects for errors where there is a jump in
                 temporal references, but it is just the file's problem, not that there needs to be picture re-order. *)

              (* Save the picture that we are about to display to the past picture. Save the picture that we just 
                 decoded to the future picture, because it needs to be displayed at a later time. And then copy
                 the picture we are about to display to the current picture. This is some out-of-control juggling
                 from a performance standpoint, but this code should never be executed on a perfectly-compliant
                 MPEG file. *)
              CopyPicture(state, state.past, state.future);
              CopyPicture(state, state.future, state.current); 
              CopyPicture(state, state.current, state.past);

              (* Apply the YUV-to-RGB color space conversion to the current picture. *)
              PostProcessing.YUVtoRGB(state, 0);

              (* Bitblt state.current to the screen using the SpinDraw interface. *)
              EVAL Draw.Bitblt24to8(fbh, state.current, 0, 0, state.sh.horizontalSize, state.sh.verticalSize, 0); 

              (* Jump to the temporal reference that we are now at; again, this was more than a one-frame
                 increment according to the temporal reference variables. *)   
              state.lastTemporalReference := state.futureTemporalReference;
              state.futureTemporalReference := state.p.temporalReference;
              EXIT;
            ELSE   
              (* We know there is no valid future picture, so we know this is an I- or P-picture, or a D-picture. 
                 Do not display it yet, however.  We must first figure out what to do. *)
  
              (* This is a hack to try to get the mjackson file to play correctly. This corrects for errors
                 where the first picture is numbered 1 rather than 0 as it should be. *)
              IF state.lastTemporalReference = -1 THEN
                state.lastTemporalReference := state.p.temporalReference-1;
              END; 

              IF state.p.temporalReference = (state.lastTemporalReference + 1) THEN
                (* Display the current frame. Since we know it is an I-, P-, or D-picture, set
                   it to be the past frame. This is the correct thing to do for I- and P-pictures, and
                   is meaningless for D-picture movies since they never access the past picture. Save to 
                   past frame first since we will corrupt the current picture in the process of displaying it. 
                   By corrupt, I mean we will convert the values from YUV to RGB color values. *)
                CopyPicture(state, state.past, state.current);
      
                (* Apply the YUV-to-RGB color space conversion to the current picture. *)
                PostProcessing.YUVtoRGB(state, 0);

                (* Bitblt state.current to the screen using the SpinDraw interface. *)
                EVAL Draw.Bitblt24to8(fbh, state.current, 0, 0, state.sh.horizontalSize, state.sh.verticalSize, 0); 

                (* Additional housekeeping. *)   
                state.lastTemporalReference := state.lastTemporalReference + 1;
                EXIT;
              ELSE
                (* The picture we just decoded is not the next picture in display order, so set it to be 
                   the future picture.  Loop again to get a picture that we can display. *)
                CopyPicture(state, state.future, state.current);
                state.futureTemporalReference := state.p.temporalReference; 
              END;   
            END;
          END;
        END;
      END; (* of loop until we display one frame *)
    

      (* Set the state.mode bit so we know what layer we're in when MPEGplay is called next. If next bits 
         are not a picture start code, then set the mode bit so that player looks for a group of pictures 
         header next, etc. *)
      IF BitIO.ShowBits(file, 32) # PictureLayer.PictureStartCode THEN
        state.mode := 1;
        IF BitIO.ShowBits(file, 32) # GroupOfPicturesLayer.GroupStartCode THEN
          state.mode := 0;
          IF BitIO.ShowBits(file, 32) # SequenceLayer.SequenceHeaderCode THEN
            IF BitIO.ShowBits(file, 32) = SequenceLayer.SequenceEndCode THEN
              (* We're done, the movie is over. Exit gracefully. *)
              state.mode := 3;
            ELSE
              RAISE GeneralTools.Bad("Bad sequence end code in MPEGplay.MPEGplay");
            END;
          END;
        END;
      END;
    END;

  END;

  EXCEPT
    (* Something bad happened. Exit and try to output as much helpful info as possible. *)
  |  GeneralTools.Bad(problem) => IO.Put(problem & "\n");
                                  state.mode := 3;
  |  BitIO.EOF                 => IO.Put("Unexpectedly hit the end of the file\n");
                                  state.mode := 3;
  END;
END MpegPlay;

BEGIN
END MpegPlay.

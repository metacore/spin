(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

UNSAFE MODULE DrawPerfTests;

IMPORT Draw, Spy; 

TYPE LineArgs = REF RECORD
  x1:    INTEGER;
  y1:    INTEGER;
  color: INTEGER;
  x2:    INTEGER;
  y2:    INTEGER;
END;

PROCEDURE DirectLine() =
VAR
  fbh: Draw.FbHandle := Draw.New(1280, 1024);
  fb: UNTRACED REF ARRAY[0..(1280*1024)-1] OF Draw.Byte := Draw.GetDirect(fbh); 
  DirectLineTimer: Spy.T := Spy.Create("DirectLineTimer");
BEGIN

  Draw.SetRGBColorMap(fbh);

  Spy.Enter(DirectLineTimer);
    (* Draw 269,876 white, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 255, centerX+9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 255, centerX+deltaX, centerY-9);
        END;

        FOR deltaY := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 255, centerX-9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 255, centerX+deltaX, centerY+9);
        END;     

      END;
    END;

    (* Draw 269,876 black, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 0, centerX+9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 0, centerX+deltaX, centerY-9);
        END;

        FOR deltaY := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 0, centerX-9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3Line(fb, centerX, centerY, 0, centerX+deltaX, centerY+9);
        END;     

      END;
    END;

  Spy.Exit(DirectLineTimer);
END DirectLine;

PROCEDURE APILine() =
VAR
  fbh: Draw.FbHandle;
  APILineTimer: Spy.T := Spy.Create("APILineTimer");
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  Spy.Enter(APILineTimer);
    (* Draw 269,876 white, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 255, centerX+9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 255, centerX+deltaX, centerY-9);
        END;

        FOR deltaY := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 255, centerX-9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 255, centerX+deltaX, centerY+9);
        END;     

      END;
    END;

    (* Draw 269,876 black, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 0, centerX+9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 0, centerX+deltaX, centerY-9);
        END;

        FOR deltaY := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 0, centerX-9, centerY+deltaY);
        END;

        FOR deltaX := -9 TO 9 DO
          M3LineAPI(fbh, centerX, centerY, 0, centerX+deltaX, centerY+9);
        END;     

      END;
    END;

  Spy.Exit(APILineTimer);
END APILine;

PROCEDURE IndirectLine() =
VAR
  fbh: Draw.FbHandle;
  IndirectLineTimer: Spy.T := Spy.Create("IndirectLineTimer");
  args := NEW(LineArgs);
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  args.color := 255;
  
  Spy.Enter(IndirectLineTimer);
    (* Draw 269,876 white, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO
        args.x1 := centerX;
        args.y1 := centerY;

        args.x2 := centerX+9;
        FOR deltaY := -9 TO 9 DO
          args.y2 := centerY+deltaY;
          EVAL  Draw.Callback(fbh, M3LineIndirect, args);
        END;

        args.y2 := centerY-9;
        FOR deltaX := -9 TO 9 DO
          args.x2 := centerX+deltaX;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;

        args.x2 := centerX-9;
        FOR deltaY := -9 TO 9 DO
          args.y2 := centerY+deltaY;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;

        args.y2 := centerY+9;
        FOR deltaX := -9 TO 9 DO
          args.x2 := centerX+deltaX;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;     

      END;
    END;

    args.color := 0;
    (* Draw 269,876 black, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1014 BY 19 DO
      FOR centerX := 9 TO 1270 BY 19 DO
        args.x1 := centerX;
        args.y1 := centerY;

        args.x2 := centerX+9;
        FOR deltaY := -9 TO 9 DO 
          args.y2 := centerY+deltaY;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;
 
        args.y2 := centerY-9;
        FOR deltaX := -9 TO 9 DO
          args.x2 := centerX+deltaX;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;

        args.x2 := centerX-9;
        FOR deltaY := -9 TO 9 DO
          args.y2 := centerY+deltaY;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;

        args.y2 := centerY+9;
        FOR deltaX := -9 TO 9 DO
          args.x2 := centerX+deltaX;
          EVAL Draw.Callback(fbh, M3LineIndirect, args);
        END;     

      END;
    END;

  Spy.Exit(IndirectLineTimer);
END IndirectLine;

PROCEDURE M3Line (fb: UNTRACED REF ARRAY[0..(1280*1024)-1] OF Draw.Byte; x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                  x2: INTEGER; y2: INTEGER) =
VAR
  dx, dy, x, y, xEnd, yEnd, p: INTEGER;
  screenWidth: INTEGER := 1280;
  j: INTEGER;
  step, stepy, stepx: INTEGER;
  start, end: INTEGER;
BEGIN
  IF (x1 < x2) THEN
     dx := x2-x1;
  ELSE 
     dx := x1-x2;
  END;

  IF (y1 < y2) THEN
    dy := y2-y1;
  ELSE
    dy := y1-y2;
  END;

  IF dy = 0 THEN
    IF x1 < x2 THEN
      start := x1;
      end   := x2;
    ELSE
      start := x2;
      end   := x1;
    END;

    FOR i := start TO end DO 
      fb[screenWidth*y1 + i] := color1;
    END;
  ELSIF dx = 0 THEN
    IF y1 < y2 THEN
      start := y1;
      end   := y2;
    ELSE
      start := y2;
      end   := y1;
    END;
    
    FOR i := start TO end DO
      fb[screenWidth*i + x1] := color1;
    END;
  ELSIF dx = dy THEN
    IF (x1 < x2) AND (y1 < y2) THEN
      j := y1;
      start := x1;
      end   := x2;
      step  := 1;
    ELSIF (x1 < x2) AND (y1 > y2) THEN
      j := y1;
      start := x1;
      end := x2;
      step := -1;
    ELSIF (x1 > x2) AND (y1 < y2) THEN
      j := y2;
      start := x2;
      end := x1;
      step := -1;
    ELSIF ((x1 > x2) AND (y1 > y2)) THEN
      j := y2;
      start := x2;
      end := x1;
      step := 1;
    END;

    FOR i := start TO end DO
      fb[screenWidth*j + i] := color1;
      j := j + step;
    END;

  ELSIF dx > dy THEN
    IF x1 < x2 THEN
      x := x1;
      y := y1;
      xEnd := x2;
      stepy := y2-y1;
    ELSE
      x := x2;
      y := y2;
      xEnd := x1;
      stepy := y1-y2;
    END;
   
    IF stepy < 0 THEN
      step := -1;
    ELSE 
      step := 1;
    END;
 
    p := (2*dy) - dx;

    fb[screenWidth*y+x] := color1;
    WHILE x < xEnd DO
      x := x +1;
      IF p < 0 THEN
        p := p+(2*dy);
      ELSE
        y := y+step;
        p := p + (2*(dy-dx));
      END;
      fb[screenWidth*y+x] := color1;
    END;

  ELSE
    IF y1 < y2 THEN
      x := x1;
      y := y1;
      yEnd := y2;
      stepx := x2-x1;
    ELSE
      x := x2;
      y := y2;
      yEnd := y1;   
      stepx := x1-x2;
    END;
 
    IF stepx < 0 THEN
      step := -1;
    ELSE
      step := 1;
    END;

    p := (2*dx) - dy;

    fb[screenWidth*y+x] := color1;
    WHILE y < yEnd DO
      y := y + 1;
      IF p < 0 THEN
        p := p + (2*dx);
      ELSE
        x := x + step;
        p := p + (2*(dx-dy));
      END;

      fb[screenWidth*y + x] := color1;
    END;
  END;

END M3Line;

PROCEDURE M3LineIndirect (VAR window: UNTRACED REF ARRAY OF UNTRACED REF ARRAY OF Draw.Byte; 
                          VAR args: REFANY) =
VAR
  dx, dy, x, y, xEnd, yEnd, p: INTEGER;
  j: INTEGER;
  step, stepy, stepx: INTEGER;
  start, end: INTEGER;
  x1: INTEGER;
  y1: INTEGER;
  color: INTEGER;
  x2: INTEGER;
  y2: INTEGER;
  args2: LineArgs;
BEGIN
  (* Narrow our REFANY args parameter to be of type LineArgs
     so that we can dereference it and get at its values. *)
  args2 := NARROW(args, LineArgs);
  x1    := args2.x1;
  y1    := args2.y1;
  color := args2.color;
  x2    := args2.x2;
  y2    := args2.y2;

  IF (x1 < x2) THEN
     dx := x2-x1;
  ELSE 
     dx := x1-x2;
  END;

  IF (y1 < y2) THEN
    dy := y2-y1;
  ELSE
    dy := y1-y2;
  END;

  IF dy = 0 THEN
    IF x1 < x2 THEN
      start := x1;
      end   := x2;
    ELSE
      start := x2;
      end   := x1;
    END;

    FOR i := start TO end DO
      window[y1][i] := color; 
    END;
  ELSIF dx = 0 THEN
    IF y1 < y2 THEN
      start := y1;
      end   := y2;
    ELSE
      start := y2;
      end   := y1;
    END;
    
    FOR i := start TO end DO
      window[i][x1] := color;
    END;
  ELSIF dx = dy THEN
    IF (x1 < x2) AND (y1 < y2) THEN
      j := y1;
      start := x1;
      end   := x2;
      step  := 1;
    ELSIF (x1 < x2) AND (y1 > y2) THEN
      j := y1;
      start := x1;
      end := x2;
      step := -1;
    ELSIF (x1 > x2) AND (y1 < y2) THEN
      j := y2;
      start := x2;
      end := x1;
      step := -1;
    ELSIF ((x1 > x2) AND (y1 > y2)) THEN
      j := y2;
      start := x2;
      end := x1;
      step := 1;
    END;

    FOR i := start TO end DO
      window[j][i] := color;
      j := j + step;
    END;

  ELSIF dx > dy THEN
    IF x1 < x2 THEN
      x := x1;
      y := y1;
      xEnd := x2;
      stepy := y2-y1;
    ELSE
      x := x2;
      y := y2;
      xEnd := x1;
      stepy := y1-y2;
    END;
   
    IF stepy < 0 THEN
      step := -1;
    ELSE 
      step := 1;
    END;
 
    p := (2*dy) - dx;

    window[y][x] := color;
    WHILE x < xEnd DO
      x := x +1;
      IF p < 0 THEN
        p := p+(2*dy);
      ELSE
        y := y+step;
        p := p + (2*(dy-dx));
      END;
      window[y][x] := color;
    END;

  ELSE
    IF y1 < y2 THEN
      x := x1;
      y := y1;
      yEnd := y2;
      stepx := x2-x1;
    ELSE
      x := x2;
      y := y2;
      yEnd := y1;   
      stepx := x1-x2;
    END;
 
    IF stepx < 0 THEN
      step := -1;
    ELSE
      step := 1;
    END;

    p := (2*dx) - dy;

    window[y][x] := color;
    WHILE y < yEnd DO
      y := y + 1;
      IF p < 0 THEN
        p := p + (2*dx);
      ELSE
        x := x + step;
        p := p + (2*(dx-dy));
      END;

      window[y][x] := color;
    END;
  END;
END M3LineIndirect;

PROCEDURE M3LineAPI (fbh: Draw.FbHandle; x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                     x2: INTEGER; y2: INTEGER) =
VAR
  dx, dy, x, y, xEnd, yEnd, p: INTEGER;
  j: INTEGER;
  step, stepy, stepx: INTEGER;
  start, end: INTEGER;
BEGIN
  IF (x1 < x2) THEN
     dx := x2-x1;
  ELSE 
     dx := x1-x2;
  END;

  IF (y1 < y2) THEN
    dy := y2-y1;
  ELSE
    dy := y1-y2;
  END;

  IF dy = 0 THEN
    IF x1 < x2 THEN
      start := x1;
      end   := x2;
    ELSE
      start := x2;
      end   := x1;
    END;

    FOR i := start TO end DO 
      EVAL Draw.Point(fbh, i, y1, color1);
    END;
  ELSIF dx = 0 THEN
    IF y1 < y2 THEN
      start := y1;
      end   := y2;
    ELSE
      start := y2;
      end   := y1;
    END;
    
    FOR i := start TO end DO
      EVAL Draw.Point(fbh, x1, i, color1);
    END;
  ELSIF dx = dy THEN
    IF (x1 < x2) AND (y1 < y2) THEN
      j := y1;
      start := x1;
      end   := x2;
      step  := 1;
    ELSIF (x1 < x2) AND (y1 > y2) THEN
      j := y1;
      start := x1;
      end := x2;
      step := -1;
    ELSIF (x1 > x2) AND (y1 < y2) THEN
      j := y2;
      start := x2;
      end := x1;
      step := -1;
    ELSIF ((x1 > x2) AND (y1 > y2)) THEN
      j := y2;
      start := x2;
      end := x1;
      step := 1;
    END;

    FOR i := start TO end DO
      EVAL Draw.Point(fbh, i, j, color1);
      j := j + step;
    END;

  ELSIF dx > dy THEN
    IF x1 < x2 THEN
      x := x1;
      y := y1;
      xEnd := x2;
      stepy := y2-y1;
    ELSE
      x := x2;
      y := y2;
      xEnd := x1;
      stepy := y1-y2;
    END;
   
    IF stepy < 0 THEN
      step := -1;
    ELSE 
      step := 1;
    END;
 
    p := (2*dy) - dx;

    EVAL    Draw.Point(fbh, x, y, color1);
    WHILE x < xEnd DO
      x := x +1;
      IF p < 0 THEN
        p := p+(2*dy);
      ELSE
        y := y+step;
        p := p + (2*(dy-dx));
      END;
      EVAL Draw.Point(fbh, x, y, color1);
    END;

  ELSE
    IF y1 < y2 THEN
      x := x1;
      y := y1;
      yEnd := y2;
      stepx := x2-x1;
    ELSE
      x := x2;
      y := y2;
      yEnd := y1;   
      stepx := x1-x2;
    END;
 
    IF stepx < 0 THEN
      step := -1;
    ELSE
      step := 1;
    END;

    p := (2*dx) - dy;

    EVAL Draw.Point(fbh, x, y, color1);
    WHILE y < yEnd DO
      y := y + 1;
      IF p < 0 THEN
        p := p + (2*dx);
      ELSE
        x := x + step;
        p := p + (2*(dx-dy));
      END;

      EVAL Draw.Point(fbh, x, y, color1);
    END;
  END;

END M3LineAPI;

PROCEDURE DirectPoint () =
VAR
  DirectTimer: Spy.T := Spy.Create("DirectTimer");
  fbh: Draw.FbHandle := Draw.New(1280, 1024);
  screenWidth: INTEGER := Draw.GetScreenWidth(fbh);
  fb: UNTRACED REF ARRAY [0..(1280*1024)-1] OF Draw.Byte := Draw.GetDirect(fbh);
BEGIN
  Draw.SetRGBColorMap(fbh);

  Spy.Enter(DirectTimer);

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      fb[screenWidth*y+x] := 3;
    END;
  END;

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      fb[screenWidth*y+x] := 224;
    END;
  END;

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      fb[screenWidth*y+x] := 0;
    END;
  END; 
 
  Spy.Exit(DirectTimer);
END DirectPoint;

(* This is a practice procedure for use by the indirect routine, which
   accesses the framebuffer through the callback interface. *)
PROCEDURE Dummy (VAR window: UNTRACED REF ARRAY OF UNTRACED REF ARRAY OF Draw.Byte; 
                 VAR args: REFANY) =
VAR
  IndirectPointTimer: Spy.T := Spy.Create("IndirectPointTimer");
BEGIN

  Spy.Enter(IndirectPointTimer);

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      window[y][x] := 3;
    END;
  END;

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      window[y][x] := 224;
    END;
  END;

  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      window[y][x] := 0;
    END;
  END;

  Spy.Exit(IndirectPointTimer);
END Dummy;
  
PROCEDURE IndirectPoint () =
VAR
  fbh: Draw.FbHandle := Draw.New(1280, 1024);
  args: LineArgs;
BEGIN
  Draw.SetRGBColorMap(fbh);
  EVAL Draw.Callback(fbh, Dummy, args);
END IndirectPoint;

PROCEDURE APIPoint () =
VAR
  APIPointTimer: Spy.T := Spy.Create("APIPointTimer");
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  Spy.Enter(APIPointTimer);

  (* Draw the whole screen blue one point at a time. *)
  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      EVAL Draw.Point(fbh, x, y, 3);
    END;
  END;

  (* Now draw the screen red. *)
  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      EVAL Draw.Point(fbh, x, y, 224);
    END;
  END;

  (* Now draw the screen black. *)
  FOR y := 0 TO 1023 DO
    FOR x := 0 TO 1279 DO
      EVAL Draw.Point(fbh, x, y, 0);
    END;
  END;

  Spy.Exit(APIPointTimer);
END APIPoint;

PROCEDURE GouraudLineTest () =
VAR
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetGrayColorMap(fbh);

  EVAL Draw.Line(fbh, 200, 200, 0, 400, 200, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 400, 100, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 400,   0, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 300,   0, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 200,   0, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 100,   0, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0,   0,   0, 255, Draw.GouraudShaded);  
  EVAL Draw.Line(fbh, 200, 200, 0,   0, 100, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0,   0, 200, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0,   0, 300, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0,   0, 400, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 100, 400, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 200, 400, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 300, 400, 255, Draw.GouraudShaded);
  EVAL Draw.Line(fbh, 200, 200, 0, 400, 400, 255, Draw.GouraudShaded);  
  EVAL Draw.Line(fbh, 200, 200, 0, 400, 300, 255, Draw.GouraudShaded);

END GouraudLineTest;

PROCEDURE GouraudLineDemo () =
VAR
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetGrayColorMap(fbh);

        FOR deltaY := 511 TO -512 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 1279, 512+deltaY, 255, Draw.GouraudShaded);
        END;

        FOR deltaX := 639 TO -640 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 0, 255, Draw.GouraudShaded);
        END;

        FOR deltaY := -512 TO 511 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 0, 512+deltaY, 255, Draw.GouraudShaded);
        END;

        FOR deltaX := -640 TO 639 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 1023, 255, Draw.GouraudShaded);
        END;     


        FOR deltaY := 511 TO -512 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 1279, 512+deltaY, 0, Draw.GouraudShaded);
        END;

        FOR deltaX := 639 TO -640 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 0, 0, Draw.GouraudShaded);
        END;

        FOR deltaY := -512 TO 511 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 0, 512+deltaY, 0, Draw.GouraudShaded);
        END;

        FOR deltaX := -640 TO 639 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 1023, 0, Draw.GouraudShaded);
        END;     
END GouraudLineDemo;

(* Draws 539,752 2D, Gouraud-shaded, 10-pixel lines. *)
PROCEDURE GouraudLine10 () =
VAR
  fbh: Draw.FbHandle;
  gouraudLine10Timer: Spy.T := Spy.Create("gouraudLine10Timer");
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetGrayColorMap(fbh);

  Spy.Enter(gouraudLine10Timer);
    (* Draw 269,876 2D, Gouraud-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1024 BY 19 DO
      FOR centerX := 9 TO 1280 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+9, centerY+deltaY, 255, Draw.GouraudShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-9, 255, Draw.GouraudShaded);
        END;

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-9, centerY+deltaY, 255, Draw.GouraudShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+9, 255, Draw.GouraudShaded);
        END;     

      END;
    END;

    (* Draw 269,876 black, 2D, Gouraud-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1024 BY 19 DO
      FOR centerX := 9 TO 1280 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+9, centerY+deltaY, 0, Draw.GouraudShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-9, 0, Draw.GouraudShaded);
        END;

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-9, centerY+deltaY, 0, Draw.GouraudShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+9, 0, Draw.GouraudShaded);
        END;     

      END;
    END;

  Spy.Exit(gouraudLine10Timer);
END GouraudLine10;

(* Draws 47,760 2D, Gouraud-shaded, 100-pixel lines. *)
PROCEDURE GouraudLine100 () =
VAR
  fbh: Draw.FbHandle;
  gouraudLine100Timer: Spy.T := Spy.Create("gouraudLine100Timer");
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetGrayColorMap(fbh);

  Spy.Enter(gouraudLine100Timer);
    (* Draw 23,880 2D, Gouraud-shaded, 100-pixel lines. *)
    FOR centerY := 99 TO 1024 BY 199 DO
      FOR centerX := 99 TO 1280 BY 199 DO
        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+99, centerY+deltaY, 255, Draw.GouraudShaded);
        END;

        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-99, 255, Draw.GouraudShaded);
        END;

        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-99, centerY+deltaY, 255, Draw.GouraudShaded);
        END;
      
        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+99, 255, Draw.GouraudShaded);
        END;
      END;
    END;

    (* Draw 23,880 black, 2D, Gouraud-shaded, 100-pixel lines. *)
    FOR centerY := 99 TO 1024 BY 199 DO
      FOR centerX := 99 TO 1280 BY 199 DO
        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+99, centerY+deltaY, 0, Draw.GouraudShaded);
        END;

        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-99, 0, Draw.GouraudShaded);
        END;

        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-99, centerY+deltaY, 0, Draw.GouraudShaded);
        END;
      
        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+99, 0, Draw.GouraudShaded);
        END;
      END;
    END;

  Spy.Exit(gouraudLine100Timer);

END GouraudLine100;

PROCEDURE LineTest () =
VAR
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(250, 250);
  Draw.SetRGBColorMap(fbh);

  EVAL Draw.Line(fbh, 200, 200, 255, 300, 200, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 200, 100, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 100, 200, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 100, 100, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 200, 300, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 300, 100, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 100, 300, 255, Draw.FlatShaded);  
  EVAL Draw.Line(fbh, 200, 200, 255, 300, 300, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 300, 150, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 250, 100, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 150, 100, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 100, 150, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 100, 250, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 150, 300, 255, Draw.FlatShaded);
  EVAL Draw.Line(fbh, 200, 200, 255, 250, 300, 255, Draw.FlatShaded);  
  EVAL Draw.Line(fbh, 200, 200, 255, 300, 250, 255, Draw.FlatShaded);

END LineTest;

PROCEDURE LineDemo () =
VAR
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

        FOR deltaY := 511 TO -512 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 255, 1279, 512+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := 639 TO -640 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 255, 640+deltaX, 0, 0, Draw.FlatShaded);
        END;

        FOR deltaY := -512 TO 511 DO
          EVAL Draw.Line(fbh, 640, 512, 255, 0, 512+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := -640 TO 639 DO
          EVAL Draw.Line(fbh, 640, 512, 255, 640+deltaX, 1023, 0, Draw.FlatShaded);
        END;     


        FOR deltaY := 511 TO -512 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 1279, 512+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := 639 TO -640 BY -1 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 0, 0, Draw.FlatShaded);
        END;

        FOR deltaY := -512 TO 511 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 0, 512+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := -640 TO 639 DO
          EVAL Draw.Line(fbh, 640, 512, 0, 640+deltaX, 1023, 0, Draw.FlatShaded);
        END;     
END LineDemo;

(* Draws 539,752 2D, flat-shaded, 10-pixel lines. *)
PROCEDURE Line10 () =
VAR
  fbh: Draw.FbHandle;
  Line10Timer: Spy.T := Spy.Create("Line10Timer");
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  Spy.Enter(Line10Timer);
    (* Draw 269,876 white, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1024 BY 19 DO
      FOR centerX := 9 TO 1280 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+9, centerY+deltaY, 255, Draw.FlatShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+deltaX, centerY-9, 255, Draw.FlatShaded);
        END;

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX-9, centerY+deltaY, 255, Draw.FlatShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+deltaX, centerY+9, 255, Draw.FlatShaded);
        END;     

      END;
    END;

    (* Draw 269,876 black, 2D, flat-shaded, 10-pixel lines. *)
    FOR centerY := 9 TO 1024 BY 19 DO
      FOR centerX := 9 TO 1280 BY 19 DO

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+9, centerY+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-9, 0, Draw.FlatShaded);
        END;

        FOR deltaY := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-9, centerY+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := -9 TO 9 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+9, 0, Draw.FlatShaded);
        END;     

      END;
    END;

  Spy.Exit(Line10Timer);
END Line10;

(* Draws 47,760 2D, flat-shaded, 100-pixel lines. *)
PROCEDURE Line100 () =
VAR
  fbh: Draw.FbHandle;
  Line100Timer: Spy.T := Spy.Create("Line100Timer");
BEGIN
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  Spy.Enter(Line100Timer);

    (* Draw 23,880 white, 2D, flat-shaded, 100-pixel lines. *)
    FOR centerY := 99 TO 1024 BY 199 DO
      FOR centerX := 99 TO 1280 BY 199 DO
        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+99, centerY+deltaY, 255, Draw.FlatShaded);
        END;

        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+deltaX, centerY-99, 255, Draw.FlatShaded);
        END;

        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX-99, centerY+deltaY, 255, Draw.FlatShaded);
        END;
      
        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 255, centerX+deltaX, centerY+99, 255, Draw.FlatShaded);
        END;
      END;
    END;

    (* Draw 23,880 black, 2D, flat-shaded, 100-pixel lines. *)
    FOR centerY := 99 TO 1024 BY 199 DO
      FOR centerX := 99 TO 1280 BY 199 DO
        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+99, centerY+deltaY, 0, Draw.FlatShaded);
        END;

        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY-99, 0, Draw.FlatShaded);
        END;

        FOR deltaY := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX-99, centerY+deltaY, 0, Draw.FlatShaded);
        END;
      
        FOR deltaX := -99 TO 99 DO
          EVAL Draw.Line(fbh, centerX, centerY, 0, centerX+deltaX, centerY+99, 0, Draw.FlatShaded);
        END;
      END;
    END;

  Spy.Exit(Line100Timer);
END Line100;

PROCEDURE Bitblt24to8Test () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240*3);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240*3);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will color it the RGB color (100, 50, 100). *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := (i*320 + j) * 3;
      pixelMap1[index] := 10;
      pixelMap1[index+1] := 255;
      pixelMap1[index+2] := 10;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := (i*320 + j) * 3;
      pixelMap2[index] := 0;
      pixelMap2[index+1] := 0;
      pixelMap2[index+2] := 0;
    END;
  END; 

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       

  FOR top := 0 TO (1024-240) BY 240 DO
    FOR left := 0 TO (1280-320) BY 320 DO 
      FOR reps := 0 TO 50 DO
        EVAL Draw.Bitblt24to8(fbh, pixelMap1, top, left, 320, 240, 0);
        EVAL Draw.Bitblt24to8(fbh, pixelMap2, top, left, 320, 240, 0);
      END;
    END;
  END;

END Bitblt24to8Test;

PROCEDURE BitbltTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
  BitbltTimer: Spy.T := Spy.Create("BitbltTimer");
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  Spy.Enter(BitbltTimer);

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-240) BY 240 DO
    FOR left := 0 TO (1280-320) BY 320 DO 
      FOR reps := 0 TO 50 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
      END;
    END;
  END;
  
  Spy.Exit(BitbltTimer);
 
END BitbltTest;

PROCEDURE ClippedBitbltTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
  BitbltTimer: Spy.T := Spy.Create("BitbltTimer");
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(500, 500);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  Spy.Enter(BitbltTimer);

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := -480 TO (1024-240) BY 240 DO
    FOR left := -640 TO (1280-320) BY 320 DO 
      FOR reps := 0 TO 49 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0); 
      END;
    END;
  END;

  Spy.Exit(BitbltTimer);
 
END ClippedBitbltTest;

PROCEDURE StretchTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 109 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
    FOR j := 110 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 3;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-480) BY 480 DO
    FOR left := 0 TO (1280-640) BY 640 DO 
      FOR reps := 0 TO 5 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 0, 0, 0, 0, 
                         2, 1, 2, 1, Draw.Stretch); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 0, 0, 0, 0, 
                         2, 1, 2, 1, Draw.Stretch); 
      END;
    END;
  END;
END StretchTest;

PROCEDURE FilteredTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 109 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
    FOR j := 110 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 3;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-480) BY 480 DO
    FOR left := 0 TO (1280-640) BY 640 DO 
      FOR reps := 0 TO 5 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 0, 0, 0, 0, 
                         2, 1, 2, 1, Draw.Stretch+Draw.Filtered); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 0, 0, 0, 0, 
                         2, 1, 2, 1, Draw.Stretch+Draw.Filtered); 
      END;
    END;
  END;
END FilteredTest;

PROCEDURE TriangleTest () =
VAR
  fbh: Draw.FbHandle;
BEGIN
  fbh := Draw.New(1000, 1000);
  Draw.SetGrayColorMap(fbh);

FOR x := 0 TO 800 BY 50 DO
  FOR y := 0 TO 800 BY 50 DO
    EVAL Draw.Triangle(fbh, 500, 500, 255, 200, 200, 0, x, y, 100, Draw.GouraudShaded);
    EVAL Draw.Triangle(fbh, 500, 500, 0, 200, 200, 0, x, y, 0, Draw.GouraudShaded);
  END;
END;

END TriangleTest;

PROCEDURE ClearTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  ClearTimer: Spy.T := Spy.Create("ClearTimer");
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(500, 500);
  Draw.SetRGBColorMap(fbh);

  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);

  Spy.Enter(ClearTimer);

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-240) BY 240 DO
    FOR left := 0 TO (1280-320) BY 320 DO
        EVAL Draw.Clear(fbh, top, left, 320, 240, 3); 
    END;
  END;
  
  Spy.Exit(ClearTimer);
 
END ClearTest;

PROCEDURE SourceKeyTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
  SourceKeyTimer: Spy.T := Spy.Create("SourceKeyTimer");
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(90, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 100 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
    FOR j := 101 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 3;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  Spy.Enter(SourceKeyTimer);

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-240) BY 240 DO
    FOR left := 0 TO (1280-320) BY 320 DO 
      FOR reps := 0 TO 5 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 10, 0, 0, 0, 0, 0, 0, 0, Draw.SrcColorKey); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 255, 255, 0, 0, 0, 0, 0, 0, Draw.SrcColorKey); 
      END;
    END;
  END;
  
  Spy.Exit(SourceKeyTimer);
 
END SourceKeyTest;

PROCEDURE DestKeyTest () =
VAR
  fbh: Draw.FbHandle;
  pixelMap1: UNTRACED REF ARRAY OF Draw.Byte;
  pixelMap2: UNTRACED REF ARRAY OF Draw.Byte;
  index: INTEGER;
  DestKeyTimer: Spy.T := Spy.Create("DestKeyTimer");
BEGIN
  (* Initialize the framebuffer. The parameter zero specifies which
     device to initialize; zero is the framebuffer device's number. *)
  fbh := Draw.New(1280, 1024);
  Draw.SetRGBColorMap(fbh);

  (* Initialize the pixel maps. *)
  pixelMap1 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);
  pixelMap2 := NEW (UNTRACED REF ARRAY OF Draw.Byte, 320*240);  

  (* Make a typical-size bitmap (full-screen for MPEG-1) 320x240. Just
     arbitrarily I will assign it the index 28. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 100 DO
      index := i*320 + j;
      pixelMap1[index] := 28;
    END;
    FOR j := 101 TO 319 DO
      index := i*320 + j;
      pixelMap1[index] := 3;
    END;
  END; 

  (* Do a clear by blitting a black bitmap to the screen. *)
  FOR i := 0 TO 239 DO
    FOR j := 0 TO 319 DO
      index := i*320 + j;
      pixelMap2[index] := 0;
    END;
  END; 

  Spy.Enter(DestKeyTimer);

  (* I am assuming resolution of 1280x1024 and 8-bit indexed color. *)       
  FOR top := 0 TO (1024-240) BY 240 DO
    FOR left := 0 TO (1280-320) BY 320 DO 
      FOR reps := 0 TO 5 DO
        EVAL Draw.Bitblt(fbh, pixelMap1, top, left, 320, 240, 0, 0, 0, 0, 0, 0, 0, 0, Draw.DestColorKey); 
        EVAL Draw.Bitblt(fbh, pixelMap2, top, left, 320, 240, 0, 0, 5, 3, 0, 0, 0, 0, Draw.DestColorKey); 
      END;
    END;
  END;
  
  Spy.Exit(DestKeyTimer);
 
END DestKeyTest;

BEGIN
END DrawPerfTests.

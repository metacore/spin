(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *       Created. Implements the functions in the Draw interface that
 *       can easily be implemented in Modula-3. Some functions are 
 *       implemented in C because they either deal with graphics device 
 *       driver C code or we need to be able to copy whole words at a 
 *       time (across a scanline) for better performance. 
 *)


UNSAFE MODULE Draw;

IMPORT DrawExtern, Word, DrawInterface, NameServer, IO;

TYPE OpenArrayHeader = UNTRACED REF RECORD
     start: ADDRESS;
     size:  INTEGER;
END;

(* This implements a few of the functions in the Draw interface. It also
   reveals the FbHandle data structure. Most of the functions are grabbed 
   from DrawExtern.i3 however and implemented in C. *)
REVEAL FbHandle = BRANDED REF RECORD
     id:     INTEGER;
     valid:  CARDINAL;
     info:   ADDRESS;
     top:    CARDINAL;
     left:   CARDINAL;
     windowWidth:  CARDINAL;
     windowHeight: CARDINAL;
     screenWidth: CARDINAL;
     screenHeight: CARDINAL;
     bitsPerPixel: CARDINAL;
     window: UNTRACED REF ARRAY OF UNTRACED REF ARRAY OF Byte; 
     direct: ADDRESS;
END;

PROCEDURE New (width: CARDINAL; height: CARDINAL): FbHandle =
VAR
  fbh:           FbHandle := NEW(FbHandle);
  top:           CARDINAL := 0;
  left:          CARDINAL := 0;
  oah:           OpenArrayHeader;

BEGIN
  fbh.id           := 0;
  fbh.valid        := 1;
  fbh.info         := DrawExtern.New();
  fbh.top          := top;
  fbh.left         := left;
  fbh.screenWidth  := DrawExtern.GetScreenWidth(fbh.info);
  fbh.screenHeight := DrawExtern.GetScreenHeight(fbh.info);  
  fbh.bitsPerPixel := DrawExtern.GetBitsPerPixel(fbh.info);
  fbh.window       := NEW (UNTRACED REF ARRAY OF UNTRACED REF ARRAY OF Byte, height);
  fbh.direct       := DrawExtern.GetDirect(fbh.info);

  IF width <= fbh.screenWidth THEN
    fbh.windowWidth := width;
  ELSE
    fbh.windowWidth := fbh.screenWidth;
  END;
 
  IF height <= fbh.screenHeight THEN 
    fbh.windowHeight := height;
  ELSE
    fbh.windowHeight := fbh.screenHeight;
  END;

  FOR i := top TO (top + (fbh.windowHeight-1)) DO
     (* Initialize our faked open array header. *)
     oah       := NEW (OpenArrayHeader);
     oah.start := fbh.direct + (fbh.screenWidth*i) + left;
     oah.size  := fbh.windowWidth;
     fbh.window[i] := LOOPHOLE(oah, UNTRACED REF ARRAY OF Byte);
  END;

  RETURN fbh;
END New;

PROCEDURE Callback (fbh: FbHandle; proc: CallbackProc; args: REFANY): INTEGER = 
BEGIN
  IF fbh.valid = 1 THEN 
    proc(fbh.window, args);
    RETURN 1;
  END;

  RETURN 0;
END Callback;

PROCEDURE GetDirect (fbh: FbHandle): ADDRESS = 
BEGIN
  RETURN fbh.direct;
END GetDirect;

(* This is needed since the framebuffer data structure is allocated in a 
   C routine. *)
PROCEDURE Dealloc (fbh: FbHandle) = 
BEGIN
  DrawExtern.Dealloc(fbh.info);
END Dealloc;

PROCEDURE GetScreenWidth (fbh: FbHandle): CARDINAL =
BEGIN
  RETURN fbh.screenWidth;
END GetScreenWidth;

PROCEDURE GetScreenHeight (fbh: FbHandle): CARDINAL =
BEGIN 
  RETURN fbh.screenHeight;
END GetScreenHeight;

PROCEDURE GetBitsPerPixel (fbh: FbHandle): CARDINAL =
BEGIN
  RETURN fbh.bitsPerPixel;
END GetBitsPerPixel;

PROCEDURE SetRGBColorMap(fbh: FbHandle) = 
BEGIN
  DrawExtern.SetRGBColorMap(fbh.info);
END SetRGBColorMap;

PROCEDURE SetGrayColorMap(fbh: FbHandle) =
BEGIN
  DrawExtern.SetGrayColorMap(fbh.info);
END SetGrayColorMap;

PROCEDURE SetColorMapEntry (fbh: FbHandle; index: CARDINAL;
                            red:CARDINAL; green:CARDINAL; blue:CARDINAL) =
BEGIN
  DrawExtern.SetColorMapEntry(fbh.info, index, red, green, blue);
END SetColorMapEntry;

PROCEDURE Clear (fbh: FbHandle;
                 top: INTEGER; left: INTEGER; width: INTEGER; height: INTEGER;
                 color: INTEGER): INTEGER =
BEGIN
  IF (fbh.valid = 1) AND
     (left >= 0) AND ((left+width) <= fbh.windowWidth) AND
     (top >= 0)  AND ((top+height) <= fbh.windowWidth) THEN     
    RETURN DrawExtern.Clear(fbh.info, top, left, width, height, color);
  END;

  RETURN 0;
END Clear;

PROCEDURE Bitblt (fbh: FbHandle; pixelMap: UNTRACED REF ARRAY OF Byte; 
                  top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER; 
                  srcKeyHigh: INTEGER; srcKeyLow: INTEGER; 
                  destKeyHigh: INTEGER; destKeyLow: INTEGER; 
                  topXStretch: INTEGER; bottomXStretch: INTEGER; 
                  topYStretch: INTEGER; bottomYStretch: INTEGER; 
                  rasterOp: INTEGER): INTEGER =
BEGIN
  IF (fbh.valid = 1) AND
     (left >= 0) AND ((left+mapWidth) <= fbh.windowWidth) AND
     (top >= 0)  AND ((top+mapHeight) <= fbh.windowWidth) THEN
    RETURN DrawExtern.Bitblt(fbh.info, pixelMap, 
                             top, left, mapWidth, mapHeight, 
                             srcKeyHigh, srcKeyLow, destKeyHigh, destKeyLow, 
                             topXStretch, bottomXStretch, topYStretch, bottomYStretch, 
                             rasterOp);
  ELSIF fbh.valid = 1 THEN
    RETURN DrawExtern.ClippedBitblt(fbh.info, pixelMap, 
                                    top, left, mapWidth, mapHeight, 
                                    srcKeyHigh, srcKeyLow, destKeyHigh, destKeyLow, 
                                    topXStretch, bottomXStretch, topYStretch, bottomYStretch, 
                                    rasterOp, fbh.windowWidth, fbh.windowHeight);
  END;

  RETURN 0;
END Bitblt;

PROCEDURE Bitblt24to8 (fbh: FbHandle; pixelMap: UNTRACED REF ARRAY OF Byte;
                       top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER; 
                       rasterOp: INTEGER): INTEGER =
BEGIN
  IF (fbh.valid = 1) AND
     (left >= 0) AND ((left+mapWidth) <= fbh.windowWidth) AND
     (top >= 0)  AND ((top+mapHeight) <= fbh.windowWidth) THEN
    RETURN DrawExtern.Bitblt24to8(fbh.info, pixelMap, top, left, mapWidth, mapHeight, rasterOp);
  END;

  (* Don't support clipping for this special case bitblt, since it will probably
     only be used for MPEG and MPEG pictures do not need clipping. *)
  RETURN 0;
END Bitblt24to8;

PROCEDURE Point (fbh: FbHandle; x1: INTEGER; y1: INTEGER; color1: INTEGER): INTEGER =
VAR 
  fb: UNTRACED REF ARRAY [0..(1280*1024)-1] OF Byte := fbh.direct; 
BEGIN
  IF (fbh.valid = 1) AND 
     (x1 >= 0) AND (x1 < fbh.windowWidth) AND 
     (y1 >= 0) AND (y1 < fbh.windowHeight) THEN
    fb[fbh.screenWidth*y1+x1] := color1;
    RETURN 1;
  END;

  RETURN 0;
END Point;

(* The case of a clipped line is implemented here. *)
PROCEDURE Line (fbh: FbHandle; 
                x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                x2: INTEGER; y2: INTEGER; color2: INTEGER; 
                rasterOp: INTEGER): INTEGER =
VAR
  dx, dy, x, y, xEnd, yEnd, p: INTEGER;
  screenWidth: INTEGER;
  j: INTEGER;
  step, stepy, stepx: INTEGER;
  startcolor, dcolor, shade: INTEGER;
  start, end: INTEGER;

BEGIN
  IF (* (fbh.valid = 1) *) FALSE AND
     (x1 >= 0) AND (x1 < fbh.windowWidth) AND
     (x2 >= 0) AND (x2 < fbh.windowWidth) AND 
     (y1 >= 0) AND (y1 < fbh.windowHeight) AND
     (y2 >= 0) AND (y2 < fbh.windowHeight) THEN
    RETURN DrawExtern.Line(fbh.info, x1, y1, color1, x2, y2, color2, rasterOp);
  ELSIF fbh.valid = 1 THEN
    (* Draw a clipped line. We just check on a pixel-by-pixel basis if line is in
       the window. We can implement this in Modula-3 because we don't do any whole
       word copies for horizontal lines as we do in the non-clipping case. *)
    screenWidth := fbh.screenWidth;

    (************************ 
    *
    *
    *  Flat-shaded lines 
    *
    ************************)
    IF (Word.And(rasterOp, FlatShaded) > 0) THEN  
      IF x1 < x2 THEN
         dx := x2-x1;
      ELSE 
         dx := x1-x2;
      END;
 
      IF y1 < y2 THEN
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
          IF (i  >= 0) AND (i  < fbh.windowWidth) AND
             (y1 >= 0) AND (y1 < fbh.windowHeight) THEN
            fbh.window[y1][i] := color1;
          END;
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
          IF (x1 >= 0) AND (x1 < fbh.windowWidth) AND
             (i  >= 0) AND (i  < fbh.windowHeight) THEN
            fbh.window[i][x1] := color1;
          END;
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
          IF (i  >= 0) AND (i < fbh.windowWidth) AND
             (j  >= 0) AND (j < fbh.windowHeight) THEN
            fbh.window[j][i] := color1;
          END;
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

        IF (x >= 0) AND (x < fbh.windowWidth) AND
           (y >= 0) AND (y < fbh.windowHeight) THEN
          fbh.window[y][x] := color1;
        END;

        WHILE x < xEnd DO
          x := x +1;
          IF p < 0 THEN
            p := p+(2*dy);
          ELSE
            y := y+step;
            p := p + (2*(dy-dx));
          END;
          IF (x >= 0) AND (x  < fbh.windowWidth) AND
             (y >= 0) AND (y < fbh.windowHeight) THEN
            fbh.window[y][x] := color1;
          END;
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

        IF (x >= 0) AND (x < fbh.windowWidth) AND
           (y >= 0) AND (y < fbh.windowHeight) THEN
          fbh.window[y][x] := color1;
        END;
        WHILE y < yEnd DO
          y := y + 1;
          IF p < 0 THEN
            p := p + (2*dx);
          ELSE
            x := x + step;
            p := p + (2*(dx-dy));
          END;

          IF (x >= 0) AND (x < fbh.windowWidth) AND
             (y >= 0) AND (y < fbh.windowHeight) THEN
            fbh.window[y][x] := color1;
          END;
        END;
      END;

  (************************ 
  *
  *
  *  Gouraud-shaded lines 
  *
  ************************)  
  ELSIF (Word.And(rasterOp, GouraudShaded) > 0) THEN 
      (* Should just use absolute value here if we can. *)  
      IF x1 < x2 THEN
          dx := x2-x1;
      ELSE
          dx := x1-x2;
      END;

      IF y1 < y2 THEN
          dy := y2-y1;
      ELSE
          dy := y1-y2;
      END;

      shade := 0;

      (* First need to determine whether this line is perfectly horizontal,
         vertical, |m| = 1, |m| < 1, or |m| > 1. Each of these is a separate case. *)
      IF dy = 0 THEN
          (* This is a horizontal line. Could also just be a point. Deal with point
             case first. *)
          IF dx = 0 THEN
              IF (x1 >= 0) AND (x1 < fbh.windowWidth)  AND
                 (y1 >= 0) AND (y1 < fbh.windowHeight) THEN
                  fbh.window[y1][x1] := (color1+color2) DIV 2;
              END;
          ELSE         
              IF x1 < x2 THEN
                  start      := x1;
                  end        := x2;
                  startcolor := color1;
                  dcolor     := color2-color1;
              ELSE
                  start      := x2;
                  end        := x1;
                  startcolor := color2;
                  dcolor     := color1-color2; 
	      END;

              FOR i := start TO end DO
                  IF (i  >= 0) AND (i  < fbh.windowWidth) AND
                     (y1 >= 0) AND (y1 < fbh.windowHeight) THEN
                      fbh.window[y1][i] := startcolor + (shade*dcolor) DIV dx;
                  END;
                  shade := shade + 1;
	      END;
	  END;
      ELSIF dx = 0 THEN
          (* This is a vertical line. *)
          IF y1 < y2 THEN
              start      := y1;
              end        := y2;
              startcolor := color1;
              dcolor     := color2-color1;
          ELSE
              start      := y2;
              end        := y1;
              startcolor := color2;
              dcolor     := color1-color2;
	  END;

          FOR i := start TO end DO
              IF (x1 >= 0) AND (x1 < fbh.windowWidth)  AND
                 (i  >= 0) AND (i  < fbh.windowHeight) THEN
                  fbh.window[i][x1] := startcolor + (shade*dcolor) DIV dy;
              END;
              shade := shade + 1;
	  END;
      ELSIF dx = dy THEN
         (* This is a perfectly diagonal line. Cannot be a one-pixel line;
            this case was handled earlier. Always start at leftmost endpoint. *)
          IF (x1 < x2) AND (y1 < y2) THEN
              j          := y1;
              start      := x1; 
              end        := x2;
              step       := 1;
              startcolor := color1;
              dcolor     := color2-color1;
          ELSIF (x1 < x2) AND (y1 > y2) THEN
              j          := y1;
              start      := x1;
              end        := x2;
              step       := -1;
              startcolor := color1;
              dcolor     := color2-color1;               
          ELSIF (x1 > x2) AND (y1 < y2) THEN
              j          := y2;
              start      := x2;
              end        := x1;
              step       := -1;
              startcolor := color2;
              dcolor     := color1-color2;
          ELSIF (x1 > x2) AND (y1 > y2) THEN
              j          := y2;
              start      := x2;
              end        := x1;
              step       := 1;
              startcolor := color2;
              dcolor     := color1-color2;
	  END;

          FOR i := start TO end DO
              IF (i >= 0) AND (i < fbh.windowWidth)  AND
                 (j >= 0) AND (j < fbh.windowHeight) THEN
                  fbh.window[j][i] := startcolor + (shade*dcolor) DIV dx;
              END;
              j := j + step;
              shade := shade + 1;
          END;
      ELSIF dx > dy THEN
          (* This line has slope less than 1. *)

          (* Determine which point to use as start and which to use as end. *)
          IF x1 < x2 THEN
              x          := x1;
              y          := y1;
              xEnd       := x2;
              stepy      := y2-y1;
              startcolor := color1;
              dcolor     := color2-color1;
          ELSE
              x          := x2;
              y          := y2;
              xEnd       := x1;
              stepy      := y1-y2;
              startcolor := color2;
              dcolor     := color1-color2;
          END;
   
          IF stepy < 0 THEN
              step := -1;
          ELSE
              step := 1;
          END;

          p := (2*dy) - dx;

          (* Should do some error-checking and figure out which type
             of line this is: flat or Gouraud-shaded, anti-aliased or aliased *)
  
          IF (x  >= 0) AND (x  < fbh.windowWidth) AND
             (y >= 0) AND (y < fbh.windowHeight) THEN
              fbh.window[y][x] := startcolor;
          END;
          shade := shade + 1;
          WHILE x < xEnd DO
              x := x + 1;
              IF p < 0 THEN 
                  p := p + (2*dy);
              ELSE
                  y := y + step;
                  p := p + (2 * (dy-dx));
              END;
              IF (x >= 0) AND (x < fbh.windowWidth)  AND
                 (y >= 0) AND (y < fbh.windowHeight) THEN
                  fbh.window[y][x] := startcolor + (shade*dcolor) DIV dx;
              END;
              shade := shade + 1;
          END;
      ELSE
          (* This line has slope greater than 1. *)

          (* Determine which point to use as start and which to use as end. *)
          IF y1 < y2 THEN
              x          := x1;
              y          := y1;
              yEnd       := y2;
              stepx      := x2-x1;
              startcolor := color1;
              dcolor     := color2-color1;
          ELSE
              x          := x2;
              y          := y2;
              yEnd       := y1;
              stepx      := x1-x2;
              startcolor := color2;
              dcolor     := color1-color2;
          END;
   
          IF stepx < 0 THEN
              step := -1;
          ELSE
              step := 1;
          END;

          p := (2*dx) - dy;

          IF (x >= 0) AND (x < fbh.windowWidth)  AND
             (y >= 0) AND (y < fbh.windowHeight) THEN  
              fbh.window[y][x] := startcolor;
          END;
          shade := shade + 1;
          WHILE y < yEnd DO
              y := y + 1;
              IF p < 0 THEN 
                  p := p + (2*dx);
              ELSE
                  x := x + step;
                  p := p + (2 * (dx-dy));
              END;

              IF (x >= 0) AND (x < fbh.windowWidth)  AND
                 (y >= 0) AND (y < fbh.windowHeight) THEN
                  fbh.window[y][x] := startcolor + (shade*dcolor) DIV dy;
              END;
              shade := shade + 1;
          END;
      END;
    END;

    RETURN 0;
  
  END;

  RETURN 0;
END Line;

(* Triangles in their entirety are implemented here. *)
PROCEDURE Triangle (fbh: FbHandle; 
                    x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                    x2: INTEGER; y2: INTEGER; color2: INTEGER; 
                    x3: INTEGER; y3: INTEGER; color3: INTEGER; 
                    rasterOp: INTEGER): INTEGER =
VAR
  leftX, leftY, leftColor:       INTEGER;
  topX, topY, topColor:          INTEGER;
  rightX, rightY, rightColor:    INTEGER;
  tempLeftX, tempRightX:         INTEGER;
  tempLeftColor, tempRightColor: INTEGER;
  halfY:                         INTEGER;

BEGIN
  IF (fbh.valid # 1) THEN
    RETURN 0;
  END;

  IF (Word.And(rasterOp, FlatShaded) > 0) THEN 
    IF (x1 = x2) AND (y1 = y2) AND (x2 = x3) AND (y2 = y3) THEN
      (* All three points are the same. *)
      RETURN Point(fbh, x1, y1, color1);
    ELSIF (x1 = x2) AND (y1 = y2) THEN
      (* First and second points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x3, y3, color1, rasterOp);
    ELSIF (x2 = x3) AND (y2 = y3) THEN
      (* Second and third points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x2, y2, color1, rasterOp);
    ELSIF (x1 = x3) AND (y1 = y3) THEN
      (* First and third points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x2, y2, color1, rasterOp);
  
    (* All points should be distinct if we get this far. *)
    ELSIF (y1 = y2) AND (y2 = y3) THEN
      (* Horizontal line. *)
      IF ((x2 > x1) AND (x2 < x3)) OR ((x2 < x1) AND (x2 > x3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color1, rasterOp);
      ELSIF ((x1 > x3) AND (x1 < x2)) OR ((x1 < x3) AND (x1 > x2)) THEN
        RETURN Line(fbh, x2, y2, color1, x3, y3, color1, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color1, rasterOp);
      END;

    ELSIF (x1 = x2) AND (x2 = x3) THEN
      (* Vertical line. *)
      IF ((y2 > y1) AND (y2 < y3)) OR ((y2 < y1) AND (y2 > y3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color1, rasterOp);
      ELSIF ((y1 > y3) AND (y1 < y2)) OR ((y1 < y3) AND (y1 > y2)) THEN
        RETURN Line(fbh, x2, y2, color1, x3, y3, color1, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color1, rasterOp);
      END;

    ELSIF (* (((y2-y1) DIV (x2-x1)) = ((y3-y2) DIV (x3-x2))) *)
          ((y2-y1)*(x3-x2) = (y3-y2)*(x2-x1)) AND
          ((y2-y1)*(x3-x1) = (y3-y1)*(x2-x1)) THEN 
      (* All three points are in a line, but not horizontal or vertical. *)
      IF ((x2 > x1) AND (x2 < x3)) OR ((x2 < x1) AND (x2 > x3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color1, rasterOp);
      ELSIF ((x1 > x3) AND (x1 < x2)) OR ((x1 < x3) AND (x1 > x2)) THEN
        RETURN Line(fbh, x2, y2, color1, x3, y3, color1, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color1, rasterOp);
      END; 

    ELSIF (y1 = y2) OR (y2 = y3) OR (y1 = y3) THEN
      (* We only render half a triangle. *)
      IF y1 = y2 THEN
        topX := x3;
        topY := y3;
        IF x1 < x2 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x2;
          rightY := y2;
        ELSE
          leftX  := x2;
          leftY  := y2;
          rightX := x1;
          rightY := y1;
        END;
      ELSIF y2 = y3 THEN
        topX := x1;
        topY := y1;
        IF x2 < x3 THEN
          leftX  := x2;
          leftY  := y2;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x2;
          rightY := y2;
        END;
      ELSE
        topX := x2;
        topY := y2;
        IF x1 < x3 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x1;
          rightY := y1;
        END;
      END;

      (* First, plot the uppermost (or lowermost) point of the triangle. *)
      EVAL Point(fbh, topX, topY, color1);
            
      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values. Draw successive horizontal lines from the point on the left
         side to the point on the right side. *)
      IF topY < leftY THEN
        FOR y := (topY+1) TO leftY BY 1 DO
           IF (leftX # topX) AND (rightX # topX) THEN
             tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSIF leftX = topX THEN
             tempLeftX  := topX;
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSE (* rightX = topX *)
             tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX;
           END;
 
           EVAL Line(fbh, tempLeftX, y, color1, tempRightX, y, color1, rasterOp);         
        END; 
      ELSE
        FOR y := (topY-1) TO leftY BY -1 DO
           IF (leftX # topX) AND (rightX # topX) THEN
             tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSIF leftX = topX THEN
             tempLeftX  := topX;
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSE (* rightX = topX *)
             tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX;
           END;

           EVAL Line(fbh, tempLeftX, y, color1, tempRightX, y, color1, rasterOp);         
        END; 
      END;
    
    ELSIF (y1 # y2) AND (y2 # y3) AND (y3 # y1) THEN
      (* Common case! We render a full triangle, meaning there are no shortcuts. First,
         render the top half of the triangle. *)  
      IF (y3 < y1) AND (y3 < y2) THEN
        topX := x3;
        topY := y3;
        IF x1 < x2 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x2;
          rightY := y2;
        ELSE
          leftX  := x2;
          leftY  := y2;
          rightX := x1;
          rightY := y1;
        END;
      ELSIF (y1 < y2) AND (y1 < y3) THEN
        topX := x1;
        topY := y1;
        IF x2 < x3 THEN
          leftX  := x2;
          leftY  := y2;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x2;
          rightY := y2;
        END;
      ELSE
        topX := x2;
        topY := y2;
        IF x1 < x3 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x1;
          rightY := y1;
        END;
      END;

      (* First, plot the uppermost point of the triangle. *)
      EVAL Point(fbh, topX, topY, color1);
            
      IF leftY < rightY THEN
        halfY := leftY;
      ELSE
        halfY := rightY;
      END;

      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values for the top half of the triangle. Draw successive horizontal 
         lines from the point on the left side to the point on the right side. *)
      FOR y := (topY+1) TO halfY BY 1 DO
        IF (leftX # topX) AND (rightX # topX) THEN
          tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSIF leftX = topX THEN
          tempLeftX  := topX;
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSE (* rightX = topX *)
          tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX;
        END;

        EVAL Line(fbh, tempLeftX, y, color1, tempRightX, y, color1, rasterOp);         
      END; 

      (* Now, render the lower half of the triangle. *)
      IF (y3 > y1) AND (y3 > y2) THEN
        topX := x3;
        topY := y3;
        IF x1 < x2 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x2;
          rightY := y2;
        ELSE
          leftX  := x2;
          leftY  := y2;
          rightX := x1;
          rightY := y1;
        END;
      ELSIF (y1 > y2) AND (y1 > y3) THEN
        topX := x1;
        topY := y1;
        IF x2 < x3 THEN
          leftX  := x2;
          leftY  := y2;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x2;
          rightY := y2;
        END;
      ELSE
        topX := x2;
        topY := y2;
        IF x1 < x3 THEN
          leftX  := x1;
          leftY  := y1;
          rightX := x3;
          rightY := y3;
        ELSE
          leftX  := x3;
          leftY  := y3;
          rightX := x1;
          rightY := y1;
        END;
      END;

      (* Plot the lowermost point of the triangle. *)
      EVAL Point(fbh, topX, topY, color1);
            
      IF leftY > rightY THEN
        halfY := leftY;
      ELSE
        halfY := rightY;
      END;

      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values of the bottom-half of the triangle. Draw successive horizontal 
         lines from the point on the left side to the point on the right side. *)
      FOR y := (topY-1) TO halfY BY -1 DO
        IF (leftX # topX) AND (rightX # topX) THEN
          tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSIF leftX = topX THEN
          tempLeftX  := topX;
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSE (* rightX = topX *)
          tempLeftX  := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX;
        END;

        EVAL Line(fbh, tempLeftX, y, color1, tempRightX, y, color1, rasterOp);         
      END;

    END;

    RETURN 1;
  END; (* Flat-shaded triangles *)

  IF (Word.And(rasterOp, GouraudShaded) > 0) THEN 
    IF (x1 = x2) AND (y1 = y2) AND (x2 = x3) AND (y2 = y3) THEN
      (* All three points are the same. *)
      RETURN Point(fbh, x1, y1, (color1+color2+color3) DIV 3);
    ELSIF (x1 = x2) AND (y1 = y2) THEN
      (* First and second points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x3, y3, color3, rasterOp);
    ELSIF (x2 = x3) AND (y2 = y3) THEN
      (* Second and third points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x2, y2, color2, rasterOp);
    ELSIF (x1 = x3) AND (y1 = y3) THEN
      (* First and third points are the same. *)
      RETURN Line(fbh, x1, y1, color1, x2, y2, color2, rasterOp);
  
    (* All points should be distinct if we get this far. *)
    ELSIF (y1 = y2) AND (y2 = y3) THEN
      (* Horizontal line. *)
      IF ((x2 > x1) AND (x2 < x3)) OR ((x2 < x1) AND (x2 > x3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color3, rasterOp);
      ELSIF ((x1 > x3) AND (x1 < x2)) OR ((x1 < x3) AND (x1 > x2)) THEN
        RETURN Line(fbh, x2, y2, color2, x3, y3, color3, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color2, rasterOp);
      END;

    ELSIF (x1 = x2) AND (x2 = x3) THEN
      (* Vertical line. *)
      IF ((y2 > y1) AND (y2 < y3)) OR ((y2 < y1) AND (y2 > y3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color3, rasterOp);
      ELSIF ((y1 > y3) AND (y1 < y2)) OR ((y1 < y3) AND (y1 > y2)) THEN
        RETURN Line(fbh, x2, y2, color2, x3, y3, color3, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color2, rasterOp);
      END;

    ELSIF (* (((y2-y1) DIV (x2-x1)) = ((y3-y2) DIV (x3-x2))) *)
          ((y2-y1)*(x3-x2) = (y3-y2)*(x2-x1)) AND
          ((y2-y1)*(x3-x1) = (y3-y1)*(x2-x1)) THEN 
      (* All three points are in a line, but not horizontal or vertical. *)
      IF ((x2 > x1) AND (x2 < x3)) OR ((x2 < x1) AND (x2 > x3))THEN
        RETURN Line(fbh, x1, y1, color1, x3, y3, color3, rasterOp);
      ELSIF ((x1 > x3) AND (x1 < x2)) OR ((x1 < x3) AND (x1 > x2)) THEN
        RETURN Line(fbh, x2, y2, color2, x3, y3, color3, rasterOp);
      ELSE
        RETURN Line(fbh, x1, y1, color1, x2, y2, color2, rasterOp);
      END; 

    ELSIF (y1 = y2) OR (y2 = y3) OR (y1 = y3) THEN
      (* We only render half a triangle. *)
      IF y1 = y2 THEN
        topX     := x3;
        topY     := y3;
        topColor := color3;
        IF x1 < x2 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        ELSE
          leftX      := x2;
          leftY      := y2;
          leftColor  := color2;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      ELSIF y2 = y3 THEN
        topX     := x1;
        topY     := y1;
        topColor := color1;
        IF x2 < x3 THEN
          leftX      := x2;
          leftY      := y2;
          leftColor  := color2;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        END;
      ELSE
        topX     := x2;
        topY     := y2;
        topColor := color2;
        IF x1 < x3 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      END;

      (* First, plot the uppermost (or lowermost) point of the triangle. *)
      EVAL Point(fbh, topX, topY, topColor);
            
      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values. Draw successive horizontal lines from the point on the left
         side to the point on the right side. *)
      IF topY < leftY THEN
        FOR y := (topY+1) TO leftY BY 1 DO
           IF (leftX # topX) AND (rightX # topX) THEN
             tempLeftX      := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX     := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSIF leftX = topX THEN
             tempLeftX  := topX;
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSE (* rightX = topX *)
             tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX;
           END;

           tempLeftColor  := (((y-topY)*leftColor)  + ((leftY-y)*topColor)) DIV (leftY-topY);
           tempRightColor := (((y-topY)*rightColor) + ((rightY-y)*topColor)) DIV (rightY-topY);
           EVAL Line(fbh, tempLeftX, y, tempLeftColor, tempRightX, y, tempRightColor, rasterOp);         
        END; 
      ELSE
        FOR y := (topY-1) TO leftY BY -1 DO
           IF (leftX # topX) AND (rightX # topX) THEN
             tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSIF leftX = topX THEN
             tempLeftX := topX;
             tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
           ELSE (* rightX = topX *)
             tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
             tempRightX := topX;
           END;

           tempLeftColor  := (((topY-y)*leftColor)  + ((y-leftY)*topColor)) DIV (topY-leftY);
           tempRightColor := (((topY-y)*rightColor) + ((y-rightY)*topColor)) DIV (topY-rightY);
           EVAL Line(fbh, tempLeftX, y, tempLeftColor, tempRightX, y, tempRightColor, rasterOp);         
        END; 
      END;
    
    ELSIF (y1 # y2) AND (y2 # y3) AND (y3 # y1) THEN
      (* Common case! We render a full triangle, meaning there are no shortcuts. First,
         render the top half of the triangle. *)  
      IF (y3 < y1) AND (y3 < y2) THEN
        topX     := x3;
        topY     := y3;
        topColor := color3;
        IF x1 < x2 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        ELSE
          leftX      := x2;
          leftY      := y2;
          leftColor  := color2;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      ELSIF (y1 < y2) AND (y1 < y3) THEN
        topX     := x1;
        topY     := y1;
        topColor := color1;
        IF x2 < x3 THEN
          leftX      := x2;
          leftY      := y2;
          leftColor  := color2;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        END;
      ELSE
        topX     := x2;
        topY     := y2;
        topColor := color2;
        IF x1 < x3 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      END;

      (* First, plot the uppermost point of the triangle. *)
      EVAL Point(fbh, topX, topY, topColor);
            
      IF leftY < rightY THEN
        halfY := leftY;
      ELSE
        halfY := rightY;
      END;

      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values for the top half of the triangle. Draw successive horizontal 
         lines from the point on the left side to the point on the right side. *)
      FOR y := (topY+1) TO halfY BY 1 DO
        IF (leftX # topX) AND (rightX # topX) THEN
          tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSIF leftX = topX THEN
          tempLeftX := topX;
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSE (* rightX = topX *)
          tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX;
        END;

        tempLeftColor  := (((y-topY)*leftColor)  + ((leftY-y)*topColor)) DIV (leftY-topY);
        tempRightColor := (((y-topY)*rightColor) + ((rightY-y)*topColor)) DIV (rightY-topY);
        EVAL Line(fbh, tempLeftX, y, tempLeftColor, tempRightX, y, tempRightColor, rasterOp);         
      END; 

      (* Now, render the lower half of the triangle. *)
      IF (y3 > y1) AND (y3 > y2) THEN
        topX     := x3;
        topY     := y3;
        topColor := color3;
        IF x1 < x2 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        ELSE
          leftX      := x2;
          leftY      := y2;
          leftColor  := color2;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      ELSIF (y1 > y2) AND (y1 > y3) THEN
        topX     := x1;
        topY     := y1;
        topColor := color1;
        IF x2 < x3 THEN
          leftX      := x2;
          leftY      := y2; 
          leftColor  := color2;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x2;
          rightY     := y2;
          rightColor := color2;
        END;
      ELSE
        topX     := x2;
        topY     := y2;
        topColor := color2;
        IF x1 < x3 THEN
          leftX      := x1;
          leftY      := y1;
          leftColor  := color1;
          rightX     := x3;
          rightY     := y3;
          rightColor := color3;
        ELSE
          leftX      := x3;
          leftY      := y3;
          leftColor  := color3;
          rightX     := x1;
          rightY     := y1;
          rightColor := color1;
        END;
      END;

      (* Plot the lowermost point of the triangle. *)
      EVAL Point(fbh, topX, topY, topColor);
            
      IF leftY > rightY THEN
        halfY := leftY;
      ELSE
        halfY := rightY;
      END;

      (* Next figure out the x-value of both the right and left sides of the triangle as we
         iterate over the y-values of the bottom-half of the triangle. Draw successive horizontal 
         lines from the point on the left side to the point on the right side. *)
      FOR y := (topY-1) TO halfY BY -1 DO
        IF (leftX # topX) AND (rightX # topX) THEN
          tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSIF leftX = topX THEN
          tempLeftX := topX;
          tempRightX := topX + ((rightX-topX)*(y-topY))DIV(rightY-topY);
        ELSE (* rightX = topX *)
          tempLeftX := topX + ((leftX-topX)*(y-topY))DIV(leftY-topY);
          tempRightX := topX;
        END;

        tempLeftColor  := (((topY-y)*leftColor)  + ((y-leftY)*topColor)) DIV (topY-leftY);
        tempRightColor := (((topY-y)*rightColor) + ((y-rightY)*topColor)) DIV (topY-rightY);
        EVAL Line(fbh, tempLeftX, y, tempLeftColor, tempRightX, y, tempRightColor, rasterOp);         
      END;

    END;

    RETURN 1;
  END; (* Gouraud-shaded triangles *)
  

  RETURN 0;
END Triangle;

BEGIN
  TRY 
    (* This is so the Draw package can be used by client programs. *)
    EVAL DrawInterface.Export();
  EXCEPT
  | NameServer.Error (* (ec) *) =>
    IO.PutError("Draw init FAILED\n");
  END;
END Draw.













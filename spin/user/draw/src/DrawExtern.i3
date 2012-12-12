(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *       Created. Interface to the functions in the Draw interface that
 *       are implemented in C either because they deal with graphics 
 *       device driver C code, or because we need to be able to copy whole
 *       words at a time (across a scanline) for better performance. 
 *)

UNSAFE INTERFACE DrawExtern;

IMPORT Draw;

<* EXTERNAL *> PROCEDURE New       (): ADDRESS;
<* EXTERNAL *> PROCEDURE GetDirect (dpy: ADDRESS): ADDRESS;
<* EXTERNAL *> PROCEDURE Dealloc   (dpy: ADDRESS);

<* EXTERNAL *> PROCEDURE GetScreenWidth  (dpy: ADDRESS): CARDINAL;
<* EXTERNAL *> PROCEDURE GetScreenHeight (dpy: ADDRESS): CARDINAL;
<* EXTERNAL *> PROCEDURE GetBitsPerPixel (dpy: ADDRESS): CARDINAL;

<* EXTERNAL *> PROCEDURE SetRGBColorMap   (dpy: ADDRESS);
<* EXTERNAL *> PROCEDURE SetGrayColorMap  (dpy: ADDRESS);
<* EXTERNAL *> PROCEDURE SetColorMapEntry (dpy: ADDRESS; index: CARDINAL; 
                                           red: CARDINAL; green: CARDINAL; blue: CARDINAL);

<* EXTERNAL *> PROCEDURE Clear (dpy: ADDRESS; 
                                top: INTEGER; left: INTEGER; width: INTEGER; height: INTEGER;
                                color: INTEGER): INTEGER;

<* EXTERNAL *> PROCEDURE Bitblt (dpy: ADDRESS; 
                                 pixelMap: UNTRACED REF ARRAY OF Draw.Byte;  
                                 top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER;
                                 srcKeyHigh: INTEGER; srcKeyLow: INTEGER; 
                                 destKeyHigh: INTEGER; destKeyLow: INTEGER; 
                                 topXStretch: INTEGER; bottomXStretch: INTEGER; 
                                 topYStretch: INTEGER; bottomYStretch: INTEGER; 
                                 rasterOp: INTEGER): INTEGER;

<* EXTERNAL *> PROCEDURE ClippedBitblt (dpy: ADDRESS; 
                                        pixelMap: UNTRACED REF ARRAY OF Draw.Byte;  
                                        top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER;
                                        srcKeyHigh: INTEGER; srcKeyLow: INTEGER; 
                                        destKeyHigh: INTEGER; destKeyLow: INTEGER; 
                                        topXStretch: INTEGER; bottomXStretch: INTEGER; 
                                        topYStretch: INTEGER; bottomYStretch: INTEGER; 
                                        rasterOp: INTEGER; 
                                        windowWidth: INTEGER; windowHeight: INTEGER): INTEGER;

<* EXTERNAL *> PROCEDURE Bitblt24to8 (dpy: ADDRESS; pixelMap: UNTRACED REF ARRAY OF Draw.Byte;
                                      top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER; 
                                      rasterOp: INTEGER): INTEGER;

<* EXTERNAL *> PROCEDURE Point (dpy: ADDRESS; x1: INTEGER; y1: INTEGER; color1: INTEGER): INTEGER;

<* EXTERNAL *> PROCEDURE Line (dpy: ADDRESS; 
                               x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                               x2: INTEGER; y2: INTEGER; color2: INTEGER; 
                               rasterOp: INTEGER): INTEGER;

END DrawExtern.





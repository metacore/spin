(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      API interface that supports the following drawing primitives:
 *           Clears
 *           Straight-copy, Source color-keyed, Destination color-keyed,
 *                Stretch, and Filtered stretch Bitblt's
 *           Points
 *           Flat-shaded and Gouraud-shaded Lines
 *           Flat-shaded and Gouraud-shaded Triangles
 *      
 *      The old, unsafe Direct interface to the framebuffer is still available 
 *      through the GetDirect call.
 *      
 *      Callback Indirect interface. This interface is supposed to provide a
 *      safe and performance-conscious way to extend the API interface. At present,
 *      however, it is neither safe nor performant.                 
 *
 * 17-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *	Inkernel framebuffer interface.
 * 
 *
 *)

INTERFACE Draw;

(*************************
*
* Data structures
*
*************************)
TYPE Byte = BITS 8 FOR [0..255];
     FbHandle <: REFANY;                   

     CallbackProc = PROCEDURE (VAR window: UNTRACED REF ARRAY OF UNTRACED REF ARRAY OF Byte; 
                               VAR args: REFANY);


(*************************
*
* Raster operation flags
*
*************************)
(* These constants represent the different kinds of raster operations
   that can be applied to drawing primitives. *)
CONST SrcColorKey   =      2_1;	 (* source color-keying                   (bitblts) *)
      DestColorKey  =     2_10;	 (* destination color-keying              (bitblts) *)
      Stretch       =    2_100;  (* stretch/shrink the bitmap             (bitblts) *)
      Filtered      =   2_1000;  (* filter the stretched bitmap           (bitblts) *)
      FlatShaded    =  2_10000;  (* apply one color to whole primitive    (lines, triangles) *)
      GouraudShaded = 2_100000;	 (* apply smooth-shading across primitive (lines, triangles) *)


(*************************
*
* Initialization and shut-down 
*
*************************)
(* Main control routines. New grabs a handle to the framebuffer, GetDirect
   returns an actual pointer to the first byte of framebuffer memory (upper-left
   corner), and Dealloc de-allocates the space for the framebuffer handle. Dealloc
   is necessary because the handle is created in a C file. Callback is the sole
   routine that makes up the Callback Indirect interface. Client can write a primitive
   drawing function, pass it as the proc parameter to the Callback function, and 
   Callback will then call proc for the Client. 
   
   New should actually take in a resolution, color depth, and RGB/index flag
   to request a particular framebuffer configuration. If it is supported, it should
   be returned; if not, then the next best thing should be returned. Client can then
   examine reality through the GetScreenWidth, GetScreenHeight, and GetBitsPerPixel
   calls. *)
PROCEDURE New       (width: CARDINAL; height: CARDINAL): FbHandle;
PROCEDURE GetDirect (fbh: FbHandle): ADDRESS;
PROCEDURE Dealloc   (fbh: FbHandle);
PROCEDURE Callback  (fbh: FbHandle; proc: CallbackProc; args: REFANY): INTEGER;


(************************
*
* Query for resolution and color depth
*
************************)
(* Routines to query the the display's settings: resolution and color depth. *)
PROCEDURE GetScreenWidth  (fbh: FbHandle): CARDINAL;
PROCEDURE GetScreenHeight (fbh: FbHandle): CARDINAL;
PROCEDURE GetBitsPerPixel (fbh: FbHandle): CARDINAL;


(*************************
*
* Setting the color map
*
*************************)
(* Sets a color map to emulate RGB colors, i.e. selects evenly 
   distributed colors from the overall color spectrum. This call
   sets up the 3-3-2 RGB format, so there are 8 different
   intensities for red and green, and 4 for blue. To be more specific:

   	Entry	Red	Green	Blue
   	0	0	0	1
   	1	0	0	2
   	2	0	0	3
   	3	0	1	0 
	4	0	1	1
	...
*)
PROCEDURE SetRGBColorMap(fbh: FbHandle);

(* Sets a grayscale color map *)
PROCEDURE SetGrayColorMap(fbh: FbHandle);

(* Set an individual color map entry. *)
PROCEDURE SetColorMapEntry (fbh: FbHandle; index: CARDINAL;
                            red:CARDINAL; green:CARDINAL; blue:CARDINAL);


(************************
*
* API drawing primitives: clear, bitblt, point, line, and triangle
*
************************)
PROCEDURE Bitblt24to8 (fbh: FbHandle; pixelMap: UNTRACED REF ARRAY OF Byte;
                       top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER; 
                       rasterOp: INTEGER): INTEGER;

PROCEDURE Clear (fbh: FbHandle; top: INTEGER; left: INTEGER; width: INTEGER; height: INTEGER;
                 color: INTEGER): INTEGER;

(* Bitblt assumes the passed-in pixel map is word-aligned. *)
PROCEDURE Bitblt (fbh: FbHandle; pixelMap: UNTRACED REF ARRAY OF Byte;
                  top: INTEGER; left: INTEGER; mapWidth: INTEGER; mapHeight: INTEGER; 
                  srcKeyHigh: INTEGER; srcKeyLow: INTEGER; 
                  destKeyHigh: INTEGER; destKeyLow: INTEGER; 
                  topXStretch: INTEGER; bottomXStretch: INTEGER; 
                  topYStretch: INTEGER; bottomYStretch: INTEGER; 
                  rasterOp: INTEGER): INTEGER;

PROCEDURE Point (fbh: FbHandle; x1: INTEGER; y1: INTEGER; color1: INTEGER): INTEGER;

PROCEDURE Line (fbh: FbHandle; 
                x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                x2: INTEGER; y2: INTEGER; color2: INTEGER; 
                rasterOp: INTEGER): INTEGER;

PROCEDURE Triangle (fbh: FbHandle; 
                    x1: INTEGER; y1: INTEGER; color1: INTEGER; 
                    x2: INTEGER; y2: INTEGER; color2: INTEGER; 
                    x3: INTEGER; y3: INTEGER; color3: INTEGER; 
                    rasterOp: INTEGER): INTEGER;

END Draw.





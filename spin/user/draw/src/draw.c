/* 
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Added the functions in the Draw interface that must be implemented in C 
 *      either because they deal with graphics device driver code, or because we 
 *      need to be able to copy whole words at a time (across a scanline) for 
 *      better performance.
 * 
 * 19-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *      Tried to abstract any mach dependencies away and make video_xxx
 *      operations safe.  The video structure is handed out as an opaque
 *      reference, to export directly to M3 world.
 *	
 *      The only interface that exposes the underlying hardware is
 *      video_framebuffer(), which returns a pointer to the framebuffer.
 *
 * 16-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *      Modified to work from within the kernel.
 *
 */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/workstation.h>
#include <sys/inputdevice.h>
#include <sys/wsdevice.h>
#include <sys/fbinfo.h>

#define SrcColorKey    1
#define DestColorKey   2
#define Stretch        4
#define Filtered       8
#define FlatShaded    16
#define GouraudShaded 32

struct video {
        unsigned short frame_width;   /* width of frame buffer */
        unsigned short frame_height;  /* height of frame buffer */
        unsigned short frame_bitpxel; /* bits per pixel */
	unsigned short unit;          /* frame buffer unit */
        char *framebuffer;            /* beginning of frame buffer */
	char *frame;
	ws_info wsinfo;
	ws_depth_descriptor dinfo;	
};

typedef struct video *video_t;

char *
GetDirect(video_t refany)
{
	struct video * dpy = (struct video*) refany;
	return (dpy->frame);
}

unsigned short 
GetScreenWidth(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_width);
}

unsigned short 
GetScreenHeight(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_height);
}

unsigned short 
GetBitsPerPixel(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_bitpxel);
}

void
SetColorMapEntry(video_t refany, ws_color_map_data *cd)
{
	struct video * dpy = (struct video*) refany;	
	unsigned short unit = dpy->unit;
	int rc;
	unsigned long size;

	cd->screen =-1;
	
	rc = real_wsioctl (unit, WRITE_COLOR_MAP, cd, 0);
		
	if(rc){
		printf("WRITE_COLOR_MAP failed with %x.",rc);
	}
	return;
}

char *
New ()
{
        extern struct fb_info fb_softc[];
	struct fb_info *fbp = &fb_softc[0];
	ws_screen_descriptor *sp = &fbp->screen;
 	ws_depth_descriptor *dp = &fbp->depth[sp->root_depth];

	struct video * dpy = (struct video*) spin_malloc(sizeof(struct video));
	unsigned long count;
	int rc;

	ws_video_control vc ;
	
	dpy->framebuffer = dp->physaddr + 0x1000 /* MEF */;
	dpy->frame       = dpy->framebuffer;
	dpy->frame_width = sp->width;
        dpy->frame_height = sp->height;
        dpy->frame_bitpxel = 0x8; /* sc->frame_scanline_width? */
	dpy->unit = 0;
	count = 0;
	
	rc = wsopen(0,0);
	
	if (rc) 
	{
	    printf("wsopen failed to open screen.\n");
	    return NULL;
	}

	vc.screen = 0;
	vc.control =SCREEN_ON;
	rc = real_wsioctl(0, VIDEO_ON_OFF, &vc, 0);
	
	if (rc)
	{
	    printf("VIDEO_ON_OFF failed.\n");
	}
	 
	dpy->dinfo.screen = -1;
	dpy->dinfo.which_depth = 0; 
	
	rc = real_wsioctl (0, GET_DEPTH_INFO, &(dpy->dinfo), 0);
	
	if (rc) {
		printf("GET_DEPTH_INFO failed.\n",rc);
	}

	return((char*) dpy);
}

void
Dealloc(video_t refany)
{
	kfree(refany,sizeof(struct video));
}

/* Assuming incoming pixelMap is 24-bit RGB pixel format and starts at the upper-left 
   corner of the image. */
int Bitblt24to8 (video_t dpy, unsigned char pixelMap[],
                 int top, int left, int mapWidth, int mapHeight, int rasterOp) {
  int x, y;
  unsigned char * fbc = GetDirect(dpy);
  int screen_width = GetScreenWidth(dpy);
  int fbcindex;
  int pdindex;
  unsigned long red1;
  unsigned long red2;
  unsigned long red3;
  unsigned long red4;
  unsigned long red5;
  unsigned long red6;
  unsigned long red7;
  unsigned long red8;
  unsigned long green1;
  unsigned long green2;
  unsigned long green3;
  unsigned long green4;
  unsigned long green5;
  unsigned long green6;
  unsigned long green7;
  unsigned long green8;
  unsigned long blue1;
  unsigned long blue2;
  unsigned long blue3;
  unsigned long blue4;
  unsigned long blue5;
  unsigned long blue6;
  unsigned long blue7;
  unsigned long blue8;
  unsigned long * fbl;
  int fblindex;
  int dummy;
  unsigned long * dummy2;
  unsigned long * dummy3;
  unsigned long * dummy4;
  int unaligned;

  fbcindex = (top*screen_width + left) + (mapHeight*screen_width);

  /* Since the pixel map is assumed to have been created in Modula-3, and
     is an open array, we must skip over the open array header, which must
     be 16 bytes. */
  pdindex = 16;

  for (y = mapHeight; y > 0; y--) {
    dummy = fbc + fbcindex;
    unaligned = dummy % 8; 

    /* Take care of non-word-aligned bytes. */
    if (unaligned == 0) {
      /* Fast copy all of the full words on this scanline. We must
         grab 3 words from the pixelMap in order to service each full
         framebuffer word. (24 bits-per-pixel in pixelMap; 8 bits-per-pixel
         in framebuffer.) */
      fbl = (unsigned long *)(fbc+fbcindex);
      fblindex = 0;
      for (x = 0; x < (mapWidth/8); x++) {
        dummy2 = (unsigned long *)(pixelMap+pdindex);
        dummy3 = (unsigned long *)(pixelMap+pdindex+8);
        dummy4 = (unsigned long *)(pixelMap+pdindex+16);
 
        red1   = ((*dummy2)<<56)>>56;
        green1  = ((*dummy2)<<48)>>56;
        blue1 = ((*dummy2)<<40)>>56;

        red2   = ((*dummy2)<<32)>>56;
        green2  = ((*dummy2)<<24)>>56;
        blue2 = ((*dummy2)<<16)>>56;

        red3   = ((*dummy2)<<8)>>56;
        green3  = (*dummy2)>>56;
        blue3 = ((*dummy3)<<56)>>56;

        red4   = ((*dummy3)<<48)>>56;
        green4  = ((*dummy3)<<40)>>56;
        blue4 = ((*dummy3)<<32)>>56;

        red5   = ((*dummy3)<<24)>>56;
        green5  = ((*dummy3)<<16)>>56;
        blue5 = ((*dummy3)<<8)>>56;

        red6   = (*dummy3)>>56;
        green6  = ((*dummy4)<<56)>>56;
        blue6  = ((*dummy4)<<48)>>56;
     
        red7 = ((*dummy4)<<40)>>56;
        green7   = ((*dummy4)<<32)>>56;
        blue7  = ((*dummy4)<<24)>>56;

        red8 = ((*dummy4)<<16)>>56;
        green8   = ((*dummy4)<<8)>>56;
        blue8  = (*dummy4)>>56;  

        fbl[fblindex] = (((red8&0xffffffe0) + ((green8>>5)<<2) + (blue8>>6)) << 56) +
                        (((red7&0xffffffe0) + ((green7>>5)<<2) + (blue7>>6)) << 48) +
                        (((red6&0xffffffe0) + ((green6>>5)<<2) + (blue6>>6)) << 40) +
                        (((red5&0xffffffe0) + ((green5>>5)<<2) + (blue5>>6)) << 32) +
                        (((red4&0xffffffe0) + ((green4>>5)<<2) + (blue4>>6)) << 24) +
                        (((red3&0xffffffe0) + ((green3>>5)<<2) + (blue3>>6)) << 16) +
                        (((red2&0xffffffe0) + ((green2>>5)<<2) + (blue2>>6)) << 8) +
                        ((red1&0xffffffe0) + ((green1>>5)<<2) + (blue1>>6)); 

        fblindex++; 
        pdindex += 24;
      }

      /* Take care of the non-word-aligned bytes at the end of the line. */
      fbcindex = fbcindex + 8*fblindex;
      for (x = 0; x < (mapWidth-(8*fblindex)); x++) {
        red1 = pixelMap[pdindex];
        green1 = pixelMap[pdindex+1];
        blue1 = pixelMap[pdindex+2];
        fbc[fbcindex] = (red1&0xffffffe0) + ((green1>>5)<<2) +(blue1>>6);
        fbcindex++;
        pdindex += 3;
      }
    }
    else {
      for (x = 0; x < mapWidth; x++) {
        red1 = pixelMap[pdindex];
        green1 = pixelMap[pdindex+1];
        blue1 = pixelMap[pdindex+2];
        fbc[fbcindex] = (red1&0xffffffe0) + ((green1>>5)<<2) +(blue1>>6);
        fbcindex++;
        pdindex += 3;
      }
    }
      
    fbcindex -= (screen_width + mapWidth);
  }
 
  return 1;
}

int Clear (video_t dpy, int top, int left, int width, int height, int color) {
  int x, y;
  unsigned char * fbc = GetDirect(dpy);
  int screen_width = GetScreenWidth(dpy);
  int fbcindex;
  int fblindex;
  int unaligned;
  unsigned long * fbl;
  int dummy;
  unsigned long * dummy2;
  unsigned long longClearColor;

  fbcindex = top*screen_width + left;

  longClearColor = color;
  for (y = 0; y < height; y++) {
    dummy = fbc + fbcindex;
    unaligned = dummy % 8; 

    /* If we have a tiny bitmap, make we don't try to draw too much
       while taking care of the unaligned bytes. */
    if (width < unaligned)
      unaligned = width;

    /* Take care of non-word-aligned bytes. */
    for (x = 0; x < unaligned; x++) {
      fbc[fbcindex] = color;
      fbcindex++;
    } 

    /* Fast copy all of the full words on this scanline. */
    fbl = (unsigned long *)(fbc+fbcindex);
    fblindex = 0;
    for (x = 0; x < ((width-unaligned)/8); x++) {
      fbl[fblindex] = (longClearColor<<56)+(longClearColor<<48)+(longClearColor<<40)+(longClearColor<<32)+
                      (longClearColor<<24)+(longClearColor<<16)+(longClearColor<<8)+longClearColor;
      fblindex++; 
    }

    /* Take care of the non-word-aligned bytes at the end of the line. */
    fbcindex = fbcindex + 8*fblindex;
    for (x = 0; x < (width-fbcindex); x++) {
      fbc[fbcindex] = color;
      fbcindex++;
    }

    fbcindex += screen_width - width;
  }  
    
  return 1;
}

/* This is the 8-bit index interface to our 8-bit index color
   hardware. In other words, this is for clients whose pixel
   maps are filled with 8-bit indexed color values. This is the
   "normal" way of doing things. The other interface (for pixel maps
   filled with 24-bit RGB color values) is also awkwardly available 
   here so that apps running on the Pentiums (which only has the 24-bit 
   interface) can also run on the Alphas. */
int Bitblt (video_t dpy, unsigned char pixelMap[],
             int top, int left, int mapWidth, int mapHeight, 
             int srcKeyHigh, int srcKeyLow, int destKeyHigh, int destKeyLow, 
             int topXStretch, int bottomXStretch, int topYStretch, int bottomYStretch, 
             int rasterOp) {
  int x, y;
  int xReps, yReps;
  unsigned char * fbc = GetDirect(dpy);
  int screen_width = GetScreenWidth(dpy);
  int fbcindex;
  int fblindex;
  int pdindex;
  int index;
  int unaligned;
  unsigned long * fbl;
  int dummy;
  unsigned long * dummy2;
  unsigned char *newPixelMap;
  int old, new;
  int destWidth, destHeight;
  int factor1, factor2;
  int temp1, temp2;
  unsigned long longClearColor;

  if (rasterOp & Stretch) {
    if ((topXStretch != 0) && (bottomXStretch != 0) && 
        (topYStretch != 0) && (bottomYStretch != 0)) {
      newPixelMap = (char*)spin_malloc((mapWidth*topXStretch*mapHeight*topYStretch)*
                                        sizeof(char)/(bottomXStretch*bottomYStretch));
      destWidth  = (mapWidth*topXStretch)/bottomXStretch;
      destHeight = (mapHeight*topYStretch)/bottomYStretch;
    }
    else {
      /* One of our stretch factors is zero, so the bitmap has size zero in one dimension.
         Don't draw anything in this case. */
      return 0;
    }
  
    if (rasterOp & Filtered) {
      /* Filter in the x direction. */
      for (y = 0; y < destHeight; y++) {
        old = 1;
        for (new = 1; new <= (2*destWidth-1); new = new+2) {
          if ((new <= ((old*destWidth)/mapWidth)) || ((old/2)+1 == mapWidth)) {
            newPixelMap[(new/2)+(y*destWidth)] = pixelMap[(old/2)+((y*mapHeight/destHeight)*mapWidth)+16];
          } 
          else {
            factor1 = (new*mapWidth)-(old*destWidth);
            factor2 = 2*destWidth;
            newPixelMap[(new/2)+(y*destWidth)] = ((pixelMap[(old/2)+((y*mapHeight/destHeight)*mapWidth)+16]*(factor2-factor1)) +
                                  (pixelMap[(old/2+1)+((y*mapHeight/destHeight)*mapWidth)+16]*factor1))/factor2;
          }

          if ((old+2) <= ((new*mapWidth)/destWidth)) {
            old = old + 2;
          }
        }
      }

      /* Filter in the y direction. */
/*      for (x = 0; x < destWidth; x++) {
        old = 1;
        temp1 = newPixelMap[x];
        temp2 = newPixelMap[x+destWidth];
        index = x+destWidth;

        for (new = 1; new <= (2*destHeight-1); new = new+2) {
          if ((new > ((old*destHeight)/mapHeight)) && ((old/2) != mapHeight)) { 
            factor1 = (new*mapHeight)-(old*destHeight);
            factor2 = 2*destHeight;
            newPixelMap[((new/2)*destWidth)+x] = ((temp1*(factor2-factor1)) + (temp2*factor1)) / factor2;
          }

          if ((old+2) <= ((new*mapHeight)/destHeight)) {
            old = old + 2;
            index = index + (destWidth*(destHeight/mapHeight));
            temp1 = temp2;
            temp2 = newPixelMap[index];
            if (x == 0)
              printf("index = %d\n", index);
          }
        }
      }
*/
    }

    if (!(rasterOp & Filtered)) {
      for (y = 0; y < destHeight; y++) {
        for (x = 0; x < destWidth; x++) {
            newPixelMap[y*destWidth+x] = pixelMap[((y*mapHeight/destHeight)*mapWidth)+(x*mapWidth/destWidth)+16]; 
        }
      }
    }

    mapWidth = destWidth;
    mapHeight = destHeight;
  }

  fbcindex = top*screen_width +left;

  if (rasterOp & Stretch) 
    pdindex = 0;
  else
     /* Since the pixel map is assumed to have been created in Modula-3, and
     is an open array, we must skip over the open array header, which must
     be 16 bytes. */
    pdindex = 16;

/******************
*
* No stretching
*
******************/
if ((rasterOp & Stretch) == 0) { 
  /******************
  * 
  * Source Color-Keying
  *
  ******************/
  /* Destination and source color keying are slow because we can only copy
     one char at a time since we have to check if each value is not a keyed
     color. Could try to copy words at a time, but would probably be a big mess. */
  if (rasterOp & SrcColorKey) {
    for (y = 0; y < mapHeight; y++) {
      for (x = 0; x < mapWidth; x++) {
        if ((pixelMap[pdindex] > srcKeyHigh) || (pixelMap[pdindex] < srcKeyLow)) { 
          fbc[fbcindex] = pixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      fbcindex += screen_width - mapWidth;
    }  
    
    return 1;
  }

  /******************
  * 
  * Destination Color-Keying
  *
  ******************/
  else if (rasterOp & DestColorKey) {
    for (y = 0; y < mapHeight; y++) {
      for (x = 0; x < mapWidth; x++) {
        if ((fbc[fbcindex] <= destKeyHigh) && (fbc[fbcindex] >= destKeyLow)) { 
          fbc[fbcindex] = pixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      fbcindex += screen_width - mapWidth;
    }  
    
    return 1;
  }

  /******************
  * 
  * Straight Copy 
  *
  ******************/
  else {
    for (y = 0; y < mapHeight; y++) {
      dummy = fbc + fbcindex;
      unaligned = dummy % 8; 

      if (unaligned == 0) {
        /* Fast copy all of the full words on this scanline. */
        fbl = (unsigned long *)(fbc+fbcindex);
        fblindex = 0;
        for (x = 0; x < (mapWidth/8); x++) {
          dummy2 = (unsigned long *)(pixelMap+pdindex);
          fbl[fblindex] = *dummy2; 
          fblindex++; 
          pdindex += 8;
        }

        /* Take care of the non-word-aligned bytes at the end of the line. */
        fbcindex = fbcindex + 8*fblindex;
        for (x = 0; x < (mapWidth-(8*fblindex)); x++) {
          fbc[fbcindex] = pixelMap[pdindex];
          fbcindex++;
          pdindex++;
        }

        fbcindex += screen_width - mapWidth;
      }

      else {
        /* Copy one byte at a time. */
        for (x = 0; x < mapWidth; x++) {
          fbc[fbcindex] = pixelMap[pdindex];
          fbcindex++;
          pdindex++;
        } 

        fbcindex += screen_width - mapWidth;
      }

    }  
    
    return 1;
  }

}

/******************
*
* Stretch Bitblts
*
******************/
else if (rasterOp & Stretch) { 
  /******************
  * 
  * Source Color-Keying
  *
  ******************/
  /* Destination and source color keying are slow because we can only copy
     one char at a time since we have to check if each value is not a keyed
     color. Could try to copy words at a time, but would probably be a big mess. */
  if (rasterOp & SrcColorKey) {
    for (y = 0; y < mapHeight; y++) {
      for (x = 0; x < mapWidth; x++) {
        if ((newPixelMap[pdindex] > srcKeyHigh) || (newPixelMap[pdindex] < srcKeyLow)) { 
          fbc[fbcindex] = newPixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      fbcindex += screen_width - mapWidth;
    }  
    
    spin_free(newPixelMap);
    return 1;
  }

  /******************
  * 
  * Destination Color-Keying
  *
  ******************/
  else if (rasterOp & DestColorKey) {
    for (y = 0; y < mapHeight; y++) {
      for (x = 0; x < mapWidth; x++) {
        if ((fbc[fbcindex] <= destKeyHigh) && (fbc[fbcindex] >= destKeyLow)) { 
          fbc[fbcindex] = newPixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      fbcindex += screen_width - mapWidth;
    }  
    
    spin_free(newPixelMap);
    return 1;
  }

  /******************
  * 
  * Straight Copy 
  *
  ******************/
  else {
    for (y = 0; y < mapHeight; y++) {
      dummy = fbc + fbcindex;
      unaligned = dummy % 8; 

      if (unaligned == 0) {
        /* Fast copy all of the full words on this scanline. */
        fbl = (unsigned long *)(fbc+fbcindex);
        fblindex = 0;
        for (x = 0; x < (mapWidth/8); x++) {
          dummy2 = (unsigned long *)(newPixelMap+pdindex);
          fbl[fblindex] = *dummy2; 
          fblindex++; 
          pdindex += 8;
        }

        /* Take care of the non-word-aligned bytes at the end of the line. */
        fbcindex = fbcindex + 8*fblindex;
        for (x = 0; x < (mapWidth-(8*fblindex)); x++) {
          fbc[fbcindex] = newPixelMap[pdindex];
          fbcindex++;
          pdindex++;
        }

        fbcindex += screen_width - mapWidth;
      }

      else {
        /* Copy one byte at a time. */
        for (x = 0; x < mapWidth; x++) {
          fbc[fbcindex] = newPixelMap[pdindex];
          fbcindex++;
          pdindex++;
        } 

        fbcindex += screen_width - mapWidth;
      }
    }  
    
    spin_free(newPixelMap);
    return 1;
  }
}

  return 0;
}

/* This is for bitblts that need clipping. */
int ClippedBitblt (video_t dpy, unsigned char pixelMap[],
                   int top, int left, int mapWidth, int mapHeight, 
                   int srcKeyHigh, int srcKeyLow, int destKeyHigh, int destKeyLow, 
                   int topXStretch, int bottomXStretch, int topYStretch, int bottomYStretch, 
                   int rasterOp, int windowWidth, int windowHeight) {
  int x, y;
  int xReps, yReps;
  unsigned char * fbc = GetDirect(dpy);
  int screen_width = GetScreenWidth(dpy);
  int fbcindex;
  int fblindex;
  int pdindex;
  int index;
  int offset;
  int unaligned;
  unsigned long * fbl;
  int dummy;
  unsigned long * dummy2;
  unsigned char *newPixelMap;
  int old, new;
  int destWidth, destHeight;
  int factor1, factor2;
  int temp1, temp2;
  int clipX1, clipY1, clipX2, clipY2;
  unsigned long longClearColor;

  if (rasterOp & Stretch) {
    if ((topXStretch != 0) && (bottomXStretch != 0) && 
        (topYStretch != 0) && (bottomYStretch != 0)) {
      newPixelMap = (char*)spin_malloc((mapWidth*topXStretch*mapHeight*topYStretch)*
                                        sizeof(char)/(bottomXStretch*bottomYStretch));
      destWidth  = (mapWidth*topXStretch)/bottomXStretch;
      destHeight = (mapHeight*topYStretch)/bottomYStretch;
    }
    else {
      /* One of our stretch factors is zero, so the bitmap has size zero in one dimension.
         Don't draw anything in this case. */
      return 0;
    }
  
    if (rasterOp & Filtered) {
      /* Filter in the x direction. */
      for (y = 0; y < destHeight; y++) {
        old = 1;
        for (new = 1; new <= (2*destWidth-1); new = new+2) {
          if ((new <= ((old*destWidth)/mapWidth)) || ((old/2)+1 == mapWidth)) {
            newPixelMap[(new/2)+(y*destWidth)] = pixelMap[(old/2)+((y*mapHeight/destHeight)*mapWidth)+16];
          } 
          else {
            factor1 = (new*mapWidth)-(old*destWidth);
            factor2 = 2*destWidth;
            newPixelMap[(new/2)+(y*destWidth)] = ((pixelMap[(old/2)+((y*mapHeight/destHeight)*mapWidth)+16]*(factor2-factor1)) +
                                  (pixelMap[(old/2+1)+((y*mapHeight/destHeight)*mapWidth)+16]*factor1))/factor2;
          }

          if ((old+2) <= ((new*mapWidth)/destWidth)) {
            old = old + 2;
          }
        }
      }

      /* Filter in the y direction. */
/*      for (x = 0; x < destWidth; x++) {
        old = 1;
        temp1 = newPixelMap[x];
        temp2 = newPixelMap[x+destWidth];
        index = x+destWidth;

        for (new = 1; new <= (2*destHeight-1); new = new+2) {
          if ((new > ((old*destHeight)/mapHeight)) && ((old/2) != mapHeight)) { 
            factor1 = (new*mapHeight)-(old*destHeight);
            factor2 = 2*destHeight;
            newPixelMap[((new/2)*destWidth)+x] = ((temp1*(factor2-factor1)) + (temp2*factor1)) / factor2;
          }

          if ((old+2) <= ((new*mapHeight)/destHeight)) {
            old = old + 2;
            index = index + (destWidth*(destHeight/mapHeight));
            temp1 = temp2;
            temp2 = newPixelMap[index];
            if (x == 0)
              printf("index = %d\n", index);
          }
        }
      }
*/
    }

    if (!(rasterOp & Filtered)) {
      for (y = 0; y < destHeight; y++) {
        for (x = 0; x < destWidth; x++) {
            newPixelMap[y*destWidth+x] = pixelMap[((y*mapHeight/destHeight)*mapWidth)+(x*mapWidth/destWidth)+16]; 
        }
      }
    }

    mapWidth = destWidth;
    mapHeight = destHeight;
  }

  /* Need to figure out which parts of the bitmap can actually be copied into the window. 
     We need four values to do this: an x,y pair giving the upper-left and lower-right corners
     of the part of the bitmap that can be copied. */
  if ((top >= windowHeight) || (left >= windowWidth)) 
    return 0;
  else if ((top < 0) && (left < 0)) {
    if (((top + mapHeight) <= 0) || ((left + mapWidth) <= 0)) 
      return 0;
     
    clipX1 = -left;
    clipY1 = -top;

    if ((left + mapWidth) <= windowWidth)
      clipX2 = mapWidth-1;
    else  
      clipX2 = clipX1 + (windowWidth-1);    

    if ((top + mapHeight) <= windowHeight)
      clipY2 = mapHeight-1;
    else  
      clipY2 = clipY1 + (windowHeight-1);
  }
  else if (top < 0) {
    if ((top + mapHeight) <= 0)
      return 0;     

    clipX1 = 0;
    clipY1 = -top;
    
    if ((left + mapWidth) <= windowWidth)
      clipX2 = mapWidth-1;
    else  
      clipX2 = (windowWidth-1) - left;    

    if ((top + mapHeight) <= windowHeight)
      clipY2 = (mapHeight-1) - clipY1;
    else  
      clipY2 = (windowHeight-1) - clipY1;
  }
  else if (left < 0) {
    if ((left + mapWidth) <= 0)
      return 0;

    clipX1 = -left;
    clipY1 = 0;

    if ((left + mapWidth) <= windowWidth)
      clipX2 = clipX1 + (mapWidth-1);
    else  
      clipX2 = clipX1 + (windowWidth-1);    

    if ((top + mapHeight) <= windowHeight)
      clipY2 = mapHeight-1;
    else  
      clipY2 = (windowHeight-1)-top;
    
  }
  else {
    clipX1 = 0;
    clipY1 = 0;

    if ((left + mapWidth) < windowWidth)
      clipX2 = mapWidth-1;
    else
      clipX2 = (windowWidth-1) - left;
    
    if ((top + mapHeight) < windowHeight)
      clipY2 = mapHeight-1;
    else
      clipY2 = (windowHeight-1) - top;
  }

  offset = top*screen_width + left;
  fbcindex = offset + (clipY1*screen_width) + clipX1;

  if (rasterOp & Stretch) 
    pdindex = 0 + (clipY1*mapWidth) + clipX1;
  else
    /* Since the pixel map is assumed to have been created in Modula-3, and
    is an open array, we must skip over the open array header, which must
    be 16 bytes. */
    pdindex = 16 + (clipY1*mapWidth) + clipX1;

/******************
*
* No stretching
*
******************/
if ((rasterOp & Stretch) == 0) { 
  /******************
  * 
  * Source Color-Keying
  *
  ******************/
  /* Destination and source color keying are slow because we can only copy
     one char at a time since we have to check if each value is not a keyed
     color. Could try to copy words at a time, but would probably be a big mess. */
  if (rasterOp & SrcColorKey) {
    for (y = 0; y <= clipY2-clipY1; y++) {
      for (x = 0; x <= clipX2-clipX1; x++) {
        if ((pixelMap[pdindex] > srcKeyHigh) || (pixelMap[pdindex] < srcKeyLow)) { 
          fbc[fbcindex] = pixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      pdindex  += mapWidth - (clipX2-clipX1+1);
      fbcindex += screen_width - (clipX2-clipX1+1);
    }  
    
    return 1;
  }

  /******************
  * 
  * Destination Color-Keying
  *
  ******************/
  else if (rasterOp & DestColorKey) {
    for (y = 0; y <= clipY2-clipY1; y++) {
      for (x = 0; x <= clipX2-clipX1; x++) {
        if ((fbc[fbcindex] <= destKeyHigh) && (fbc[fbcindex] >= destKeyLow)) { 
          fbc[fbcindex] = pixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      pdindex  += mapWidth - (clipX2-clipX1+1);
      fbcindex += screen_width - (clipX2-clipX1+1);
    }  
    
    return 1;
  }

  /******************
  * 
  * Straight Copy 
  *
  ******************/
  else {
    for (y = 0; y <= clipY2-clipY1; y++) {
      dummy = fbc + fbcindex;
      unaligned = dummy % 8; 

      if (unaligned == 0) {
        /* Fast copy all of the full words on this scanline. */
        fbl = (unsigned long *)(fbc+fbcindex);
        fblindex = 0;
        for (x = 0; x < (clipX2-clipX1)/8; x++) {
          dummy2 = (unsigned long *)(pixelMap+pdindex);
          fbl[fblindex] = *dummy2; 
          fblindex++; 
          pdindex += 8;
        }
        
        /* Take care of the non-word-aligned bytes at the end of the line. */
        fbcindex = fbcindex + 8*fblindex;

        for (x = 0; x <= clipX2-clipX1-(8*fblindex); x++) {
          fbc[fbcindex] = pixelMap[pdindex];
          fbcindex++;
          pdindex++;
        }

        pdindex  += mapWidth - (clipX2-clipX1+1);
        fbcindex += screen_width - (clipX2-clipX1+1);

      }
      else {
        /* Copy one byte at a time. */
        for (x = 0; x <= clipX2-clipX1; x++) {
          fbc[fbcindex] = pixelMap[pdindex];
          fbcindex++;
          pdindex++;
        } 

        pdindex  += mapWidth - (clipX2-clipX1+1);
        fbcindex += screen_width - (clipX2-clipX1+1);
      }

    }  
    
    return 1;
  }
}

/******************
*
* Stretch Bitblts
*
******************/
else if (rasterOp & Stretch) { 
  /******************
  * 
  * Source Color-Keying
  *
  ******************/
  /* Destination and source color keying are slow because we can only copy
     one char at a time since we have to check if each value is not a keyed
     color. Could try to copy words at a time, but would probably be a big mess. */
  if (rasterOp & SrcColorKey) {
    for (y = 0; y <= clipY2-clipY1; y++) {
      for (x = 0; x <= clipX2-clipX1; x++) {
        if ((newPixelMap[pdindex] > srcKeyHigh) || (newPixelMap[pdindex] < srcKeyLow)) { 
          fbc[fbcindex] = newPixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      pdindex  += mapWidth - (clipX2-clipX1+1);
      fbcindex += screen_width - (clipX2-clipX1+1);
    }  
    
    spin_free(newPixelMap);
    return 1;
  }

  /******************
  * 
  * Destination Color-Keying
  *
  ******************/
  else if (rasterOp & DestColorKey) {
    for (y = 0; y <= clipY2-clipY1; y++) {
      for (x = 0; x <= clipX2-clipX1; x++) {
        if ((fbc[fbcindex] <= destKeyHigh) && (fbc[fbcindex] >= destKeyLow)) { 
          fbc[fbcindex] = newPixelMap[pdindex];
        }
        fbcindex++;
        pdindex++;
      } 

      pdindex  += mapWidth - (clipX2-clipX1+1);
      fbcindex += screen_width - (clipX2-clipX1+1);
    }  
    
    spin_free(newPixelMap);
    return 1;
  }

  /******************
  * 
  * Straight Copy 
  *
  ******************/
  else {
    for (y = 0; y <= clipY2-clipY1; y++) {
      dummy = fbc + fbcindex;
      unaligned = dummy % 8; 

      if (unaligned == 0) {
        /* Fast copy all of the full words on this scanline. */
        fbl = (unsigned long *)(fbc+fbcindex);
        fblindex = 0;
        for (x = 0; x < (clipX2-clipX1)/8; x++) {
          dummy2 = (unsigned long *)(newPixelMap+pdindex);
          fbl[fblindex] = *dummy2; 
          fblindex++; 
          pdindex += 8;
        }
        
        /* Take care of the non-word-aligned bytes at the end of the line. */
        fbcindex = fbcindex + 8*fblindex;

        for (x = 0; x <= clipX2-clipX1-(8*fblindex); x++) {
          fbc[fbcindex] = newPixelMap[pdindex];
          fbcindex++;
          pdindex++;
        }

        pdindex  += mapWidth - (clipX2-clipX1+1);
        fbcindex += screen_width - (clipX2-clipX1+1);
      }
      else {
        /* Copy one byte at a time. */
        for (x = 0; x <= clipX2-clipX1; x++) {
          fbc[fbcindex] = newPixelMap[pdindex];
          fbcindex++;
          pdindex++;
        } 

        pdindex  += mapWidth - (clipX2-clipX1+1);
        fbcindex += screen_width - (clipX2-clipX1+1);
      }

    }  
    
    spin_free(newPixelMap);
    return 1;
  }
}

  return 0;
}

/* Implemented using Bresenham's line-drawing algorithm. */
int Line (video_t dpy, int x1, int y1, int color1,
          int x2, int y2, int color2, int rasterOp) {
  int dx, dy, x, y, xEnd, yEnd, p;
  unsigned char * fb = GetDirect(dpy); 
  int screen_width = GetScreenWidth(dpy);
  int offset;
  int i, j;
  int step, stepy, stepx;
  int startcolor, dcolor, shade;          /* For Gouraud-shaded lines */
  int start, end;
  unsigned long *fbl;
  int fblindex;
  int fbindex;
  int dummy;
  unsigned long longColor1;
  unsigned long longColor2;
  unsigned long longColor3;
  unsigned long longColor4;
  unsigned long longColor5;
  unsigned long longColor6;
  unsigned long longColor7;
  unsigned long longColor8;
  int already_drawn;
  int unaligned;

  /************************ 
  *
  *
  *  Flat-shaded lines 
  *
  ************************/  
  if (rasterOp == FlatShaded) {
      /* Should just use absolute value here if we can. */  
      if (x1 < x2)
          dx = x2-x1;
      else
          dx = x1-x2;

      if (y1 < y2)
          dy = y2-y1;
      else
          dy = y1-y2;

      /* First need to determine whether this line is perfectly horizontal,
         vertical, |m| = 1, |m| < 1, or |m| > 1. Each of these is a separate case. */
      if (dy == 0) {
          /* This is a horizontal line. Could also just be a point. */
          if (x1 < x2) {
              start = x1;
              end   = x2;
	  }
          else {
              start = x2;
              end   = x1; 
	  }

          fbindex = (screen_width*y1+start);
          dummy = fb + fbindex;
          unaligned = dummy % 8; 
          already_drawn = 0;

          /* Take care of the non-word-aligned section of the line at its start. */
          if (unaligned != 0) {
            for (x = 0; x < (8 - unaligned); x++) {
              fb[fbindex] = color1;
              fbindex++;
            }
            already_drawn = 8-unaligned;
          } 

          /* Fast draw the line a word at a time since its horizontal. */
          fbl = (unsigned long *)(fb+fbindex);
          fblindex = 0;
          longColor1 = color1;
          for (x = 0; x <= (((end-start)-already_drawn)/8); x++) {
            fbl[fblindex] = (longColor1 << 56) + (longColor1 << 48) + (longColor1 << 40) +
                            (longColor1 << 32) + (longColor1 << 24) + (longColor1 << 16) +
                            (longColor1 << 8) + longColor1;
                            
            fblindex++; 
          }

          /* Take care of the non-word-aligned section of the line at its end. */
          fbindex = fbindex + 8*fblindex;
          already_drawn += 8*fblindex;
          for (x = 0; x <= ((end-start)-already_drawn); x++) {
            fb[fbindex] = color1;
            fbindex++;
          }
      }

      else if (dx == 0) {
          /* This is a vertical line. */
          if (y1 < y2) {
              start = y1;
              end   = y2;
	  }
          else {
              start = y2;
              end   = y1;
	  }

          for (i = start; i <= end; i++) {
              fb[screen_width*i + x1] = color1;
	  }
      }

      else if (dx == dy) {
         /* This is a perfectly diagonal line. Cannot be a one-pixel line;
            this case was handled earlier. Always start at leftmost endpoint. */
          if ((x1 < x2) && (y1 < y2)) {
              j     = y1;
              start = x1; 
              end   = x2;
              step  = 1;
	  }
          else if ((x1 < x2) && (y1 > y2)) {
              j     = y1;
              start = x1;
              end   = x2;
              step  = -1;
          }               
          else if ((x1 > x2) && (y1 < y2)) {
              j     = y2;
              start = x2;
              end   = x1;
              step  = -1;
          }               
          else if ((x1 > x2) && (y1 > y2)) {
              j     = y2;
              start = x2;
              end   = x1;
              step  = 1;
	  }

          for (i = start; i <= end; i++) {
              fb[screen_width*j+i] = color1;
              j = j + step;
          } 
      }

      else if (dx > dy) {
          /* This line has slope less than 1. */

          /* Determine which point to use as start and which to use as end. */
          if (x1 < x2) {
              x     = x1;
              y     = y1;
              xEnd  = x2;
              stepy = y2-y1;
          }
          else {
              x     = x2;
              y     = y2;
              xEnd  = x1;
              stepy = y1-y2;
          }   
   
          if (stepy < 0)
              step = -1;
          else
              step = 1;

          p  = (2*dy) - dx;

          fb[screen_width*y + x] = color1;
          while (x < xEnd) {
              x = x + 1;
              if (p<0) 
                  p = p + (2*dy);
              else {
                  y = y + step;
                  p = p + (2 * (dy-dx));
              }
              fb[screen_width*y + x] = color1;
          }
      }

      else {
          /* This line has slope greater than 1. */

          /* Determine which point to use as start and which to use as end. */
          if (y1 < y2) {
              x     = x1;
              y     = y1;
              yEnd  = y2;
              stepx = x2-x1;
          }
          else {
              x     = x2;
              y     = y2;
              yEnd  = y1;
              stepx = x1-x2;
          }   
   
          if (stepx < 0)
              step = -1;
          else
              step = 1;

          p  = (2*dx) - dy;
  
          fb[screen_width*y + x] = color1;
          while (y < yEnd) {
              y = y + 1;
              if (p<0) 
                  p = p + (2*dx);
              else {
                  x = x + step;
                  p = p + (2 * (dx-dy));
              }
              fb[screen_width*y + x] = color1;
          }
      }

      return 1;
  }

  /************************ 
  *
  *
  *  Gouraud-shaded lines 
  *
  ************************/  
  else if (rasterOp == GouraudShaded) {
      /* Should just use absolute value here if we can. */  
      if (x1 < x2)
          dx = x2-x1;
      else
          dx = x1-x2;

      if (y1 < y2)
          dy = y2-y1;
      else
          dy = y1-y2;

      shade = 0;

      /* First need to determine whether this line is perfectly horizontal,
         vertical, |m| = 1, |m| < 1, or |m| > 1. Each of these is a separate case. */
      if (dy == 0) {
          /* This is a horizontal line. Could also just be a point. Deal with point
             case first. */
          if (dx == 0)
              fb[screen_width*y1+x1] = (color1+color2)/2;
          else {         
              if (x1 < x2) {
                  start      = x1;
                  end        = x2;
                  startcolor = color1;
                  dcolor     = color2-color1;
	      }
              else {
                  start      = x2;
                  end        = x1;
                  startcolor = color2;
                  dcolor     = color1-color2; 
	      }

              fbindex = (screen_width*y1+start);
              dummy = fb + fbindex;
              unaligned = dummy % 8; 
              already_drawn = 0;

              /* Take care of the non-word-aligned section of the line at its start. */
              if (unaligned != 0) {
                  for (x = 0; x < (8 - unaligned); x++) {
                      fb[fbindex] = startcolor + (shade*dcolor)/dx;
                      fbindex++;
                      shade++;
                  }
                  already_drawn = 8-unaligned;
              } 

              /* Fast draw the line a word at a time since its horizontal. */
              fbl = (unsigned long *)(fb+fbindex);
              fblindex = 0;
              for (x = 0; x <= (((end-start)-already_drawn)/8); x++) {
                  longColor1 = startcolor + (shade*dcolor)/dx;
                  longColor2 = startcolor + ((shade+1)*dcolor)/dx;
                  longColor3 = startcolor + ((shade+2)*dcolor)/dx;
                  longColor4 = startcolor + ((shade+3)*dcolor)/dx;
                  longColor5 = startcolor + ((shade+4)*dcolor)/dx;
                  longColor6 = startcolor + ((shade+5)*dcolor)/dx;
                  longColor7 = startcolor + ((shade+6)*dcolor)/dx;
                  longColor8 = startcolor + ((shade+7)*dcolor)/dx;

                  fbl[fblindex] = (longColor1 << 56) + 
                                  (longColor2 << 48) + 
                                  (longColor3 << 40) +
                                  (longColor4 << 32) + 
                                  (longColor5 << 24) + 
                                  (longColor6 << 16) +
                                  (longColor7 << 8) + 
                                  longColor8;
                            
                  fblindex++; 
                  shade += 8;
              }

              /* Take care of the non-word-aligned section of the line at its end. */
              fbindex = fbindex + 8*fblindex;
              already_drawn += 8*fblindex;
              for (x = 0; x <= ((end-start)-already_drawn); x++) {
                  fb[fbindex] = startcolor + (shade*dcolor)/dx;
                  fbindex++;
                  shade++;
              }
	  }
      }

      else if (dx == 0) {
          /* This is a vertical line. */
          if (y1 < y2) {
              start      = y1;
              end        = y2;
              startcolor = color1;
              dcolor     = color2-color1;
	  }
          else {
              start      = y2;
              end        = y1;
              startcolor = color2;
              dcolor     = color1-color2;
	  }

          for (i = start; i <= end; i++) {
              fb[screen_width*i + x1] = startcolor + (shade*dcolor)/dy;
              shade++;
	  }
      }

      else if (dx == dy) {
         /* This is a perfectly diagonal line. Cannot be a one-pixel line;
            this case was handled earlier. Always start at leftmost endpoint. */
          if ((x1 < x2) && (y1 < y2)) {
              j          = y1;
              start      = x1; 
              end        = x2;
              step       = 1;
              startcolor = color1;
              dcolor     = color2-color1;
	  }
          else if ((x1 < x2) && (y1 > y2)) {
              j          = y1;
              start      = x1;
              end        = x2;
              step       = -1;
              startcolor = color1;
              dcolor     = color2-color1;
          }               
          else if ((x1 > x2) && (y1 < y2)) {
              j          = y2;
              start      = x2;
              end        = x1;
              step       = -1;
              startcolor = color2;
              dcolor     = color1-color2;
          }               
          else if ((x1 > x2) && (y1 > y2)) {
              j          = y2;
              start      = x2;
              end        = x1;
              step       = 1;
              startcolor = color2;
              dcolor     = color1-color2;
	  }

          for (i = start; i <= end; i++) {
              fb[screen_width*j+i] = startcolor + (shade*dcolor)/dx;
              j = j + step;
              shade++;
          } 
      }

      else if (dx > dy) {
          /* This line has slope less than 1. */

          /* Determine which point to use as start and which to use as end. */
          if (x1 < x2) {
              x          = x1;
              y          = y1;
              xEnd       = x2;
              stepy      = y2-y1;
              startcolor = color1;
              dcolor     = color2-color1;
          }
          else {
              x          = x2;
              y          = y2;
              xEnd       = x1;
              stepy      = y1-y2;
              startcolor = color2;
              dcolor     = color1-color2;
          }   
   
          if (stepy < 0)
              step = -1;
          else
              step = 1;

          p  = (2*dy) - dx;
  
          fb[screen_width*y + x] = startcolor;
          shade++;
          while (x < xEnd) {
              x = x + 1;
              if (p<0) 
                  p = p + (2*dy);
              else {
                  y = y + step;
                  p = p + (2 * (dy-dx));
              }
              fb[screen_width*y + x] = startcolor + (shade*dcolor)/dx;
              shade++;
          }
      }

      else {
          /* This line has slope greater than 1. */

          /* Determine which point to use as start and which to use as end. */
          if (y1 < y2) {
              x          = x1;
              y          = y1;
              yEnd       = y2;
              stepx      = x2-x1;
              startcolor = color1;
              dcolor     = color2-color1;
          }
          else {
              x          = x2;
              y          = y2;
              yEnd       = y1;
              stepx      = x1-x2;
              startcolor = color2;
              dcolor     = color1-color2;
          }   
   
          if (stepx < 0)
              step = -1;
          else
              step = 1;

          p  = (2*dx) - dy;
  
          fb[screen_width*y + x] = startcolor;
          shade++;
          while (y < yEnd) {
              y = y + 1;
              if (p<0) 
                  p = p + (2*dx);
              else {
                  x = x + step;
                  p = p + (2 * (dx-dy));
              }
              fb[screen_width*y + x] = startcolor + (shade*dcolor)/dy;
              shade++;
          }
      }

      return 1;
  }

  /* Must have been passed an unsupported raster operation. Return failure. */
  else
    return 0;
}

int Point (video_t dpy, int x1, int y1, int color) {
    unsigned char * fb = GetDirect(dpy);
    int screen_width = GetScreenWidth(dpy);  
    int offset = screen_width*y1 + x1;

    fb[offset] = color;
}

/* Set the color map to mimic 8-bit(3 bits red, 3 bits green, 2 bits blue) 
   RGB color; in other words, the color table entries are equally spread out 
   across the color spectrum. */
void SetRGBColorMap (video_t dpy)
{
    int i, j, k, rc;
    ws_color_cell cell;
    ws_color_map_data cd;
    int index;
    
    cd.cells  = &cell;
    cd.map    = 0;
    cd.start  = 0;
    cd.ncells = 1;
    index     = 0;

    for (i = 0; i < 8; i++) {
      for (j = 0; j < 8; j++) {
        for (k = 0; k < 4; k++) {
          cd.cells->index  = index;
	  cd.cells->red    = (unsigned short) (255/7)*i << 8;
	  cd.cells->green  = (unsigned short) (255/7)*j << 8;
	  cd.cells->blue   = (unsigned short) (255/3)*k << 8; 
 	
	  cd.cells->pad    = 0; 

	  SetColorMapEntry(dpy, &cd);
          index++;
	}
      }
    }

}

/* Sets a grayscale color map. */
void SetGrayColorMap (video_t dpy)
{
    int i, rc;
    ws_color_cell cell;
    ws_color_map_data cd;
    
    cd.cells  = &cell;
    cd.map    = 0;
    cd.start  = 0;
    cd.ncells = 1;

    for (i = 0; i < 256; i++) {
        cd.cells->index  = i;
	cd.cells->red    = (unsigned short) i << 8;
	cd.cells->green  = (unsigned short) i << 8;
	cd.cells->blue   = (unsigned short) i << 8; 
 	
	cd.cells->pad    = 0; 

	SetColorMapEntry(dpy, &cd);
    }
}



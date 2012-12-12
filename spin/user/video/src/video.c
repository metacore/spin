/* 
 * HISTORY
 * 16-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *	Modified to work from within the kernel.
 *
 * 19-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *	Tried to abstract any mach dependencies away and make video_xxx
 *	operations safe.  The video structure is handed out as an opaque
 *	reference, to export directly to M3 world.
 *	
 *	The only interface that exposes the underlying hardware is
 *	video_framebuffer(), which returns a pointer to the framebuffer.
 *
 */


/* #include <kern/kalloc.h> */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/workstation.h>
#include <sys/inputdevice.h>
#include <sys/wsdevice.h>
#include <sys/fbinfo.h>

#include "video.h"

struct video {
        unsigned short frame_width;   /* width of frame buffer */
        unsigned short frame_height;  /* height of frame buffer */
        unsigned short frame_bitpxel; /* bits per pixel */
	unsigned short unit;          /* frame buffer unit */
        char *framebuffer;             /* beginning of frame buffer */
	char *frame;
	ws_info wsinfo;
};

char *
video_framebuffer(video_t refany)
{
	struct video * dpy = (struct video*) refany;
	return(dpy->frame);
}

unsigned short 
video_frame_width(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_width);
}

unsigned short 
video_frame_height(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_height);
}

unsigned short 
video_frame_bitpxel(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->frame_bitpxel);
}

unsigned short 
video_frame_unit(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	return(dpy->unit);
}

void
video_set_screen_pos(video_t refany, unsigned short xoff, unsigned short yoff)
{
	struct video * dpy = (struct video*) refany;	
	dpy->frame = (dpy->framebuffer+((yoff*dpy->frame_width+xoff)*dpy->frame_bitpxel)/8);
	return;
}

void
video_frame_write(video_t refany, unsigned char byte)
{
	struct video * dpy = (struct video*) refany;	
	*(dpy->frame)++ = byte;
	return;
}

void
video_clear_screen(video_t refany)
{
	struct video * dpy = (struct video*) refany;	
	unsigned short unit = dpy->unit;
	/* XXX fix me... need to go through high layer interface */
	return;
}

void
video_set_cmap(video_t refany,
	       ws_color_map_data *cd)
{
	struct video * dpy = (struct video*) refany;	
	unsigned short unit = dpy->unit;
	int rc;
	unsigned long size;

	cd->screen = dpy->wsinfo.ws.console_screen;
/* PICCO */
	rc = wssetstat(unit,WRITE_COLOR_MAP, cd, size);
	if(rc){
		printf("WRITE_COLOR_MAP failed with %x.",rc);
	}
	return;
}

char *
video_new(unsigned short unit)
{
	/* screen_hw_info_t *fb_info; */
        extern struct fb_info fb_softc[];
	struct fb_info *fbp = &fb_softc[unit];
	ws_screen_descriptor *sp = &fbp->screen;
 	ws_depth_descriptor *dp = &fbp->depth[sp->root_depth];

	struct video * dpy = (struct video*) sal_malloc(sizeof(struct video));
	unsigned long count;
	int rc;
	
	printf("video_new() ");
	dpy->framebuffer = dp->physaddr + 0x1000 /* MEF */;
	dpy->frame       = dpy->framebuffer;
	dpy->frame_width = sp->width;
        dpy->frame_height = sp->height;
        dpy->frame_bitpxel = 0x8; /* sc->frame_scanline_width? */
	dpy->unit = unit;
	count = 0;
	rc = wsgetstat(unit,GET_WORKSTATION_INFO,&(dpy->wsinfo),&count);
	if (rc) {
		printf("GET_WORKSTATION_INFO failed with %x\n",rc);
		video_dealloc(dpy);
		dpy = (struct video*)0;
	}
	return((char*) dpy);
}

void
video_dealloc(video_t refany)
{
	kfree(refany,sizeof(struct video));
}

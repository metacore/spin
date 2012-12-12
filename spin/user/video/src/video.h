/*
 * HISTORY
 * 17-Feb-95  Marc Fiuczynski (mef) at the University of Washington
 *	Inkernel framebuffer interface.
 *
 */

#ifndef _video_h
#define _video_h

typedef char* video_t;

void video_set_cmap_entry(video_t refany,
			  unsigned short index,
			  unsigned short red,
			  unsigned short green,
			  unsigned short blue);

void video_clear_screen(video_t refany);

void video_set_screen_pos(video_t refany, 
			  unsigned short xoff,
			  unsigned short yoff);

void video_frame_write(video_t refany,
		       unsigned char byte);

unsigned short video_frame_width(video_t refany);
unsigned short video_frame_height(video_t refany);
unsigned short video_frame_bitpxel(video_t refany);
unsigned short video_frame_unit(video_t refany);
char * video_framebuffer(video_t refany);
video_t video_new(unsigned short unit);
void video_dealloc(video_t refany);
#endif _video_h

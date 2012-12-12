/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989,1988,1987 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes. */
/*
 *	Changed to include new rt_thread deinfition
 *	[92/06/10]	savage]
 *	Created
 *	[92/02/01	savage]
 */

#include <mach/boolean.h>
#include <sys/types.h>
#include <sys/workstation.h>
#include <sys/inputdevice.h>
#include <sys/wsdevice.h>
#include <arch/machine/endian.h>
#define swap_bytes(x) ntohs(x)

#include "video.h"
#include "fli.h"
#include "../ALPHA_OSF/RealTime.h"

void
fli_clr(video_t dpy, unsigned short cbits, char *chunk_buff, boolean_t lsb)
{
	unsigned short i,j;
	unsigned short packets;
 	unsigned short change;
	unsigned short colorpos = 0;
	unsigned short c_index;
	unsigned short mask;
	char skip;

		ws_color_cell cell;
		ws_color_map_data cd;


	mask = (0x01 << cbits) - 1;
	c_index = 0;
	
	packets = *((unsigned short *)chunk_buff)++;
	if(lsb) packets = swap_bytes(packets);
	
	for (i=0;i<packets;i++)
	{
		skip = *chunk_buff++;
		colorpos += skip;

		change = *chunk_buff++;
		if (change == 0)
			change = FLI_MAX_COLORS;

		/* 
		 * left shift increases intensity; 2 bits because coming
		 * from 6-bit world to 8-bit world
		 */
#if 0
		for (j=0,c_index+=skip;j<colors;j++,c_index++)
		{
			register unsigned short red, green, blue;
			/* 
			   red = (unsigned short)(*chunk_buff++ << 2);
			   green = (unsigned short)(*chunk_buff++ << 2);
			   blue = (unsigned short)(*chunk_buff++ << 2);
			   */

			red = (unsigned short)(*chunk_buff++ & mask) << 2;
			green = (unsigned short)(*chunk_buff++ & mask) << 2;
			blue = (unsigned short)(*chunk_buff++ & mask) << 2;
			video_set_cmap_entry(dpy, c_index, red, green, blue);
		}

#else

		
		cd.cells = &cell;
		/* 
		 * left shift increases intensity; we normally shift 8 bits,
		 * now + 2 bits because coming from 6-bit world to 8-bit world
		 */
		for (j=colorpos/* , k=0 */;j<(colorpos+change);j++/* , k++ */)
		{
			cd.cells/* [k] .*/->index = j;
			cd.cells/* [k] .*/->red = (unsigned short)(*chunk_buff++ & mask) << (2/*+8*/ /* 8 more for OSF bt driver */);
			cd.cells/* [k] .*/->green = (unsigned short)(*chunk_buff++ & mask) << (2/*+8*/ /* 8 more for OSF bt driver */);
			cd.cells/* [k] .*/->blue = (unsigned short)(*chunk_buff++ & mask) << (2/*+8*/ /* 8 more for OSF bt driver */);
			cd.cells/* [k] .*/->pad = 07; /* DoRed|DoBlue|DoGreen from X.h */
		}
		cd.map = 0;
		cd.start = 0;
		cd.ncells = 1;

		video_set_cmap(dpy,&cd);
#endif

	}
}

void
fli_lc(video_t dpy, char *chunk_buff, boolean_t lsb)
{
	unsigned short	i,j,k,l;
	unsigned short	skiplines;
	unsigned short changelines;
	unsigned char	packets;
	unsigned char	skip;
	char size;
	char *save_buff;

	/* XXX Fix me: the video_framebuffer() interface will
	 * go away.  Need to compute the screen buffer position
	 * based on (x,y) coordinates 
	 * 
	 */

	char *screen_buff = video_framebuffer(dpy);

	unsigned short frame_width = video_frame_width(dpy);

	skiplines = *((unsigned short *)chunk_buff)++;
	if(lsb) skiplines = swap_words(skiplines);
	changelines = *((unsigned short *)chunk_buff)++;
	if(lsb) changelines = swap_words(changelines);
	
	screen_buff += (skiplines * frame_width);
	save_buff = screen_buff;
	for (i=0;i<changelines;i++)
	{

		packets = *chunk_buff++;
		for (l =0;l<packets;l++)
		{
			skip = *chunk_buff++;
			screen_buff += skip;
			size = *chunk_buff++;
			if (size > 0)
			{
				bcopy(chunk_buff,screen_buff,size);
				chunk_buff += size;
				screen_buff+= size;
				/* for (k=0;k<size;k++)
				 * *screen_buff++ = *chunk_buff++;
				 */
			}
			else if (size < 0)
			{
				size = -size;
				/* if we just had a fast memset. */
				memset(screen_buff,*chunk_buff,size);
				/* for(k=0;k<size;k++)
				 * *screen_buff++ = *chunk_buff; 
				 */
				screen_buff += size;
				chunk_buff  ++; 
			}
		}
		save_buff += frame_width;
		screen_buff = save_buff;
	}
}

void
fli_brun(video_t dpy, unsigned short height, unsigned short width, char *chunk_buff, boolean_t lsb)
{
	unsigned short	i,j,k;
	unsigned char	packets;
	char	size;

	char *screen_buff = video_framebuffer(dpy); 
	unsigned short frame_width = video_frame_width(dpy);

	for (i=0;i<height;i++)
	{
		packets = *chunk_buff++;
		for (j=0;j<packets;j++)
		{
			size = *chunk_buff++;
			if (size < 0)
			{
				size = -size;
				bcopy(chunk_buff,screen_buff,size);
				chunk_buff += size;
				screen_buff+= size;
				/* for(k=0;k<size;k++)
				 * *screen_buff++ = *chunk_buff++;
				 */
			}
			else if (size > 0)
			{
				/* if we just had a fast memset. */
				
				memset(screen_buff,*chunk_buff,size);
				/* for(k=0;k<size;k++)
				 * *screen_buff++ = *chunk_buff; 
				 */
				screen_buff += size;
				chunk_buff++;
			}
		}
		/* skip to next line */
		screen_buff += (frame_width - width);
	}
}

void
fli_display_frame(struct fli_frame_header *fh, video_t dpy,
		  unsigned short height, unsigned short width,
		  char *buff, boolean_t lsb)
{
	struct fli_chunk_header	*ch;
	char 			*chunk_buff;
	int 			i;
#if	__alpha
	int			size;
#endif	__alpha
	unsigned short 		chunks;
	
	chunks = fh->chunks;
	ch = (struct fli_chunk_header *)buff;
   
	while (chunks--)
	{
		unsigned short type;
		if(lsb) type = swap_bytes(ch->type);
		else    type = ch->type;

		chunk_buff = (char *)((long)ch + (long)FLI_CHUNK_HEADER_SIZE);
		switch (type)
		{
		case FLI_CHUNK_4:
			fli_clr(dpy,0x8,chunk_buff,lsb);
			break;
		case FLI_CLR:
			fli_clr(dpy,0x6,chunk_buff,lsb);
			break;
		case FLI_LC:
			fli_lc(dpy, chunk_buff,lsb);
			break;
		case FLI_BLK:
			printf("fli_blk not implemented\n");
			/* bzero(screen_buff,width * height);*/
			break;
		case FLI_BRUN:

			fli_brun(dpy, height, width, chunk_buff,lsb);
			break;
		case FLI_CPY:		
			printf("fli_cpy not implemented\n");
			/* bcopy(chunk_buff,screen_buff, (* fli_image_size = *) fli_image_width * fli_image_height); */
			break;
		case FLI_MINI:
			printf("fli_mini not implemented\n");
			break;
		default:
			printf("Bad chunk with type = %d\n",type);
		}
	   (long)ch += fli_size((char*)ch,FLI_CHUNK_HEADER_SIZE_OFFSET,lsb);
	}
}

void
fli_play(char *buff, char* dpy, 
	 unsigned short height, unsigned short width, 
	 unsigned short frames, unsigned short repeat,
	 unsigned short speed)
{
	char *save_buff;
	int i,j;
	int rc;

	unsigned short offset;
	boolean_t lsb;
	struct fli_frame_header frame_hdr;
	
	/* only do colormap frame once */
	if(!fli_read_frame_header(buff,&frame_hdr,&offset,&lsb))
	{
		fli_dump((char*) buff);
		lsb = TRUE; /* should just return */
	} 
	else 
	{
		buff += offset;
		fli_display_frame(&frame_hdr, dpy, height, width,buff+FLI_FRAME_HEADER_SIZE,lsb);
	}

	save_buff = buff;
	
	RealTime__SetStartTime(speed * 12);
	(void)fli_display_frame(&frame_hdr,dpy,height,width,buff+FLI_FRAME_HEADER_SIZE,lsb);
	(long)save_buff+=fli_size((char*)&frame_hdr,FLI_FRAME_HEADER_SIZE_OFFSET,lsb);

	for(j = 0; j < repeat; j++)
	{
		buff=save_buff;
		for(i=0; i<frames; i++)
		{
		        RealTime__WaitUntilStartTime();
		        RealTime__SetStartTime(speed * 12);
			if(!fli_read_frame_header(buff,&frame_hdr,&offset,&lsb))
			{
				printf("frame %d corrupt: ",i);
				fli_dump(buff);
				lsb = TRUE; /* should just return */
			} 
			else 
			{
			    (long)buff += offset;
			    fli_display_frame(&frame_hdr, dpy, height, width,buff+FLI_FRAME_HEADER_SIZE,lsb);
			}
			(long)buff += fli_size((char*)&frame_hdr,FLI_FRAME_HEADER_SIZE_OFFSET,lsb);
		}
	}
	RealTime__WaitUntilStartTime();
}

unsigned short
fli_frames(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;
	return fh->frames;
}

unsigned short
fli_width(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;
	return fh->width;
}

unsigned short
fli_speed(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;
	return fh->speed;
}

unsigned short
fli_height(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;
	return fh->height;
}

unsigned int
fli_total_size(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;	
	return fh->size;
}

boolean_t
fli_magic(char *refany)
{
	/* fh = REVEAL(refany,struct fli_file_header*) */
	struct fli_file_header	*fh = (struct fli_file_header*) refany;	
	boolean_t r = (fh->magic == FLI_FILE_HEADER_MAGIC);
	if(!r){
		printf("fli_magic() magic %x != %x\n",fh->magic,FLI_FRAME_HEADER_MAGIC);
	}
	return (r);
}

boolean_t
fli_read_frame_header(char *refany, struct fli_frame_header *frame_hdr, unsigned short *offset, boolean_t *lsb)
{
	struct fli_frame_header *fh;
	unsigned short magic;
	int i;

	for(i=0; i<6; i++){
		bcopy(refany+sizeof(int)+i, (char*) &magic, sizeof(unsigned short));
		if((magic  == FLI_FRAME_HEADER_MAGIC) || 
		   (magic == FLI_FRAME_HEADER_MAGIC_V26976))
		{
			*lsb = FALSE;
			*offset = i;
			bcopy(refany+i,(char*)frame_hdr,sizeof(struct fli_frame_header));
			return TRUE;
		} 
		else if ((swap_bytes(magic) == FLI_FRAME_HEADER_MAGIC) || 
			 (swap_bytes(magic) == FLI_FRAME_HEADER_MAGIC_V26976))
		{
			*lsb = TRUE;
			*offset = i;
			bcopy(refany+i,(char*)frame_hdr,sizeof(struct fli_frame_header));
			return TRUE;
		} 
		else		
		{
			printf("fli_read_frame_header() magic %x != %x\n",magic,FLI_FRAME_HEADER_MAGIC);
		}
	}
	return FALSE;
}

unsigned int
fli_size(char *refany, unsigned short offset, boolean_t lsb)
{
	/* fh = REVEAL(refany,struct fli_frame_header*) */
	struct fli_frame_header	*fh = (struct fli_frame_header*) refany;		
	int size;
#if	__alpha
	/* XXX This is strange stuff. mef */
	/* detect and deal with unaligned access */
	if ((long)refany % sizeof(int))  {
		/* unaligned */
		bcopy((char*)(((long)refany)+offset),(char*)&size,sizeof(int));
	}  else  {
		size = fh->size;
	}
#else	__alpha
	size = frame_hdr->size;
#endif	__alpha
	if (lsb) size = swap_words(size);
	return (size);
}

void
fli_dump(char *refany)
{
	char *p;
	printf("[fli_dump: ");
	for ( p = refany; p < refany + sizeof(struct fli_frame_header) + 16; p++)
	{
		printf("%x ",*p);
	}
	printf("]\n");
}

boolean_t
fli_get_frame(char* buff, /* IN */
	      unsigned short buffsize, /* IN */
	      char *frame, /* OUT */
	      unsigned short *framesize /* OUT */)
{
	long size;
	char * b;
	boolean_t lsb;
	struct fli_frame_header frame_header;
	unsigned short offset;

	if(buffsize==0)
		return FALSE;

	if(fli_read_frame_header(buff,&frame_header,&offset,&lsb)==FALSE)
		return FALSE;

	buff += offset;
	size = fli_size(buff,FLI_FRAME_HEADER_SIZE_OFFSET,lsb);

	/* XXX this is wrong */


	if(size >= buffsize){
		frame = 0;
		*framesize = 0;
		return FALSE;
	} 
	else
	{
		frame = (char*)((long)buff + size);
		*framesize = size;
		return TRUE;
	}

}

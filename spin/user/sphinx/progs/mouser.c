/*
 * Standalone program to open the display and
 * and track the mouse.  Mainly stolen from
 * the X11R6 sources Xserver/hw/dec/ws/cfbinit.c.
 *
 * Tests frame buffer mapping and ioctl path
 * for graphics driver.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/tty.h>
#include <errno.h>
#include <sys/devio.h>
#include <sys/workstation.h>
#include <sys/inputdevice.h>

#ifdef NOPRINTF
#define printf Xprintf
#endif

/*
 * Globals
 */
ws_event_queue *queue;  	/* Mouse/kbd events */
int wsFd; 			/* Mouse file  */
ws_screen_descriptor screenDesc;
ws_depth_descriptor  depthDesc;
ws_descriptor wsinfo;
ws_map_control mc;
/*
 * keyboard stuff.
 */
ws_keyboard_definition kbddef;
unsigned char rawcodes[256];
ws_keycode_modifiers mods[32];
unsigned int rawsyms[256];
ws_keysyms_and_modifiers km;

#define NCOLORS 5
ws_color_cell colors[NCOLORS] = {
#define BLACK 0
    { 0, 0, 0, 0, 0 }, 			/* black */
#define WHITE 1
    { 1, 0xffff, 0xffff, 0xffff, 0 },	/* white */
#define RED 2
    { 2, 0xffff, 0, 0, 0 },		/* red */
#define GREEN 3
    { 3, 0, 0xffff, 0, 0 },		/* green */
#define BLUE 4
    { 4, 0, 0, 0xffff, 0 }
};

ws_color_map_data color_map = {
    0, 0, colors, 0, NCOLORS
};

/*
 * Our cursor, featuring a notable man about town.
 */
unsigned int cursorbits[16] = {
    0x3f00, 0x1080, 0xc840, 0xeaa0, 0xc820,
    0xcba0, 0xf83c, 0x383f, 0x0827, 0x0827,
    0x092f, 0x0927, 0x0920, 0x1110, 0x2108,
    0x3ef8
};

unsigned int cursormask[16] = {
    0x3f00, 0xdf80, 0xefc0, 0xffe0, 0xefe0,
    0xeffc, 0xfffe, 0xffff, 0x3fef, 0x0fef,
    0x0fff, 0x0fef, 0x0fe7, 0x1ff0, 0x3ff8,
    0x3ef8
};

ws_cursor_data cursorData = {
    0, 16, 16, 8, 8,
    cursorbits, cursormask
};

ws_cursor_control cursorControl = {
    0, CURSOR_ON
};

ws_cursor_color cursorColor;

ws_pointer_position pointerPosition;
ws_pointer_control pointerControl;

ws_descriptor wsinfo;

/*
 * Print error message and errno.  Then bail..
 */
Cant( msg )
    char *msg;
{
#ifndef NOPRINTF
    fprintf(stderr,"Can't %s: %d\n",msg,errno);
#endif
    exit(1);
}

int use_select = 0;
int trap_bads = 0;

main(argc, argv)
    int  argc;
    char *argv[];
{
    int i;
    int quickexit = 0;
    
printf("argc is %d\n",argc);
    if ( argc > 1 ) {
      for ( i = 1; i < argc; i++ ) {
printf("arg %d: %s\n",i,argv[i]);
	if (argv[i][0] == 'q') quickexit = 1;
	if (argv[i][0] == 's') use_select = 1;
	if (argv[i][0] == 't') trap_bads = 1;
      }
    }
    
    wsFd = open("/dev/mouse", O_RDWR, 0);
    if (wsFd < 0) {
	Cant("open /dev/mouse");
    }
    else {
	printf("wsFd is %d\n",wsFd);
    }
    
    if ( ioctl(wsFd, GET_WORKSTATION_INFO, &wsinfo ) < 0 ) {
	Cant("GET_WORKSTATION_INFO");
    }
    Print_wsinfo(&wsinfo);

    if ( ioctl(wsFd, GET_SCREEN_INFO, &screenDesc) < 0 ) {
	Cant("GET_SCREEN_INFO");
    }
    Print_screenDesc(&screenDesc);
    depthDesc.screen = 0;
    depthDesc.which_depth = 0;
    
    if ( ioctl(wsFd, GET_DEPTH_INFO, &depthDesc) < 0 ) {
	Cant("GET_DEPTH_INFO initially");
    }
    Print_depthDesc(&depthDesc);
    if ( depthDesc.bits_per_pixel != 8 ) {
	Cant("get an 8-bit screen");
    }
    /*
     * Load, color, turn on and position the cursor...
     */
    printf("Loading cursor...\n");
    
    if ( ioctl(wsFd, LOAD_CURSOR, &cursorData) < 0 ) {
	Cant("LOAD_CURSOR");
    }

    printf("Set cursor color...\n");
    cursorColor.screen = 0;
    cursorColor.foreground = /* struct assign */ colors[RED];
    cursorColor.background = /* struct assign */ colors[BLUE];
    if ( ioctl(wsFd, RECOLOR_CURSOR, &cursorColor) < 0 ) {
	Cant("RECOLOR_CURSOR");
    }
    
    printf("Turn cursor on...\n");
    if ( ioctl(wsFd, CURSOR_ON_OFF, &cursorControl) < 0 ) {
	Cant("CURSOR_ON_OFF -> on");
    }

    /*
     * Map in the frame buffer...
     */
    mc.screen = 0;
    mc.which_depth = 0;
    mc.map_unmap = MAP_SCREEN;
    printf("Mapping the screen...\n");
    
    if ( ioctl(wsFd, MAP_SCREEN_AT_DEPTH, &mc) < 0 ) {
	Cant("MAP_SCREEN_AT_DEPTH");
    }
    
    /*
     * Re-get depth info.  It now contains the user-mapped bitmap
     * address.
     */
    printf("It's miracle, let's see what we got...\n");
    
    if (ioctl(wsFd, GET_DEPTH_INFO, &depthDesc) == -1) {
	Cant("GET_DEPTH_INFO after mapping in");
    }
    Print_depthDesc( &depthDesc );
    
    if ( !strncmp(screenDesc.moduleID,"PMAGD",5) ) {
	depthDesc.pixmap = depthDesc.plane_mask;
	printf("  PMAGD\n");
    }
    /*
     * PCI TGA
     */
    if ( !strncmp(screenDesc.moduleID,"p0004"/*1011*/,5) ) {
	depthDesc.pixmap = depthDesc.plane_mask;
	printf("  TGA\n");
    }
    
    if ( !strncmp(screenDesc.moduleID,"PMAGB",5) ) {
	depthDesc.pixmap -= 0x1000;
	printf("  PMAGB\n");
    }
/**********
    printf("Let's see some sparkle...\n");
    BlitSome(2);
 **********/

    /*
     * Give ourselves some colors....
     */
    printf("Write color map...\n");
    if ( ioctl(wsFd, WRITE_COLOR_MAP, &color_map) < 0 ) {
	Cant("WRITE_COLOR_MAP");
    }

    printf("Setting pointer control...\n");
    pointerControl.device_number = wsinfo.console_pointer;
    pointerControl.numerator = 4;
    pointerControl.denominator = 1;
    pointerControl.threshold = 4;
    if ( ioctl(wsFd, SET_POINTER_CONTROL, &pointerControl) < 0 ) {
	Cant("SET_POINTER_CONTROL");
    }
    
    printf("Warp cursor...\n");
    pointerPosition.screen = 0;
    pointerPosition.device_number = wsinfo.console_pointer;
    pointerPosition.x = 640;
    pointerPosition.y = 512;
    if ( ioctl(wsFd, SET_POINTER_POSITION, &pointerPosition) < 0 ) {
	Cant("SET_POINTER_POSITION initially");
    }
    
    /*
     * Get the mouse/kdb event queue....
     */
    printf("Map event queue...\n");
    if ( ioctl(wsFd, GET_AND_MAP_EVENT_QUEUE, &queue) < 0 ) {
	Cant("GET_AND_MAP_EVENT_QUEUE");
    }

    /*
     * Get keysyms, etc.
     */
    kbddef.device_number = wsinfo.console_keyboard;
    if ( ioctl(wsFd, GET_KEYBOARD_DEFINITION, &kbddef) < 0 ) {
        Cant("GET_KEYBOARD_DEFINITION");
    }
    printf("syms/code %d, syms present %d, mod count %d\n",
        kbddef.keysyms_per_keycode, kbddef.keysyms_present,
        kbddef.modifier_keycode_count);
    km.device_number = wsinfo.console_keyboard;
    km.modifiers = mods;
    km.keysyms = rawsyms;
    km.keycodes = rawcodes;
    if (ioctl (wsFd, GET_KEYSYMS_AND_MODIFIERS, &km) == -1) {
        Cant("GET_KEYSYMS_AND_MODIFIERS");
    }
    if ( (rawcodes[1] != 0x56) || (rawsyms[0] != 0xff1b) ) {
        Cant("get expected K_and_M");
    }

    /*
     * Ready to rock...
     */
    if (quickexit) {
	close(wsFd);
	exit(0);
    }
    
    printf("Trying to track mouse using %s %s\n",
	use_select ? "select" : "polling",
        trap_bads ? "(will trap bad events)" : "");
    TrackMouse();
}

int bad_selres = 0;
int bad_times = 0;
int bad_overruns = 0;

PrintBads()
{
    printf("Bad selres %d\n",bad_selres);
    printf("Bad times %d\n",bad_times);
    printf("Overruns %d\n",bad_overruns);
}

TrackMouse()
{
    register ws_event *e;
    fd_set inputs;
    register int i;
    char key;
    /* forward */
    extern char TranslateKey();
    EQTime lt;
    int nev;
    int lastt, lasth;
    struct timeval tv;
    int selres;
    
    FD_ZERO(&inputs);
    FD_SET(wsFd, &inputs);
    tv.tv_sec = 10;
    tv.tv_usec = 0;
    
    lt = 0;
    do {
	if ( use_select ) {
	    selres = select(8, &inputs, (fd_set *) 0, (fd_set *) 0, &tv);
	    if ( selres == 0 ) {
		printf("select timed out\n");
		FD_SET(wsFd,&inputs);
		continue;
	    }
	    if (selres < 0) {
		printf("select failed: %d\n",errno);
		close(wsFd);
		exit(1);
	    }
	    if ( selres != 1 ) bad_selres++;
	}
	
	i = queue->head;
	lastt = queue->tail;

if ( i > 100 ) printf("bad head index %d\n",i);
if ( queue->tail == (i-1) ) printf("big events %d\n",i);
	nev = 0;
	while ( i != queue->tail ) {
/*
if ( queue->tail != lastt )
    printf("tail changed in loop %d (%d)\n",queue->tail,queue->tail-lastt);
 */
lasth = i;

	    nev++;
	    e = (ws_event *)((unsigned long)(queue->events) +
		   queue->event_size * i);
	    if (e->time < lt ) {
		wsinfo.version = (short) i;
		wsinfo.cpu = (short) queue->tail;
		wsinfo.num_screens_exist = 97;
		bad_times++;
		if (trap_bads ) {
		    printf("bad time on event: %lx %d\n",e->time,
			lt - e->time);
		    ioctl(wsFd,GET_WORKSTATION_INFO, &wsinfo );
		}
	    }
	    lt = e->time;
	    
	    if ( e->screen != 0 ) {
		printf("got event on screen %d\n",e->screen);
		goto bump_it;
	    }
	    switch ( e->device_type ) {
		case KEYBOARD_DEVICE:
		    key = TranslateKey( e->e.key.key & 0xff );
		    printf("keyboard button %s, key ",
			e->type == BUTTON_DOWN_TYPE ? "down" : "up");
		    if ( key == '\0' ) printf("unknown\n");
		    else {
			printf("'%c'\n",key);
			if (key == 'q') {
			    printf("exiting program\n");
			    close(wsFd);
			    exit(0);
		        }
			if ((key == 's') && (e->type == BUTTON_DOWN_TYPE)) {
			  PrintBads();
			}
			if ((key == 't') && (e->type == BUTTON_DOWN_TYPE)) {
			    trap_bads ^= 1;
			}
		    }
		    break;

		case MOUSE_DEVICE:
		    switch ( e->type ) {
			case BUTTON_DOWN_TYPE:
			case BUTTON_UP_TYPE:
			    printf("button event %d\n",e->e.button.button);
			    break;

			case MOTION_TYPE:
			    DoBlit(e->e.button.x, e->e.button.y);
			    break;

			default:
			    printf("Bad mouse event %d\n",e->type);
			    break;
		    }
		    break;
		default:
		    printf("Bad device type %d\n",e->device_type);
		    wsinfo.version = (short) i;
		    wsinfo.cpu = (short) queue->tail;
		    wsinfo.num_screens_exist = 98;
		    if (trap_bads) ioctl(wsFd,GET_WORKSTATION_INFO, &wsinfo );
		    break;
	    }
bump_it:
	    if ( i >= (queue->size - 1) )
		i = queue->head = 0;
	    else i = ++queue->head;
	    if ( (i != 0) && (i != (lasth+1)) ) {
		bad_overruns++;
		printf("bad i increment %d\n",i-lasth);
		wsinfo.version = i;
		wsinfo.cpu = lasth;
		wsinfo.num_screens_exist = 99;
		if (trap_bads) ioctl(wsFd,GET_WORKSTATION_INFO, &wsinfo );
	    }
	    lasth = i;
	    
	}

	if (nev > 1) printf("nevents %d\n",nev);
	
	FD_SET(wsFd, &inputs);
    } while (1);
}

DoBlit( x, y )
    int x, y;
{
    char *byte;
    
    byte = (y * depthDesc.fb_width) + x + (char *)depthDesc.pixmap;
    *byte = (*byte ^ 1);
}

BlitSome( times )
{
    int x, y, n;
    
    n = 0;
    while ( n < times ) {
	for(y = 0; y <depthDesc.fb_height; y++ ) {
	    for( x = 0; x <depthDesc.fb_width; x++ ) {
		DoBlit(x,y);
	    }
	}
	n++;
    }
}

    
Print_wsinfo( wsp )
    ws_descriptor *wsp;
{
    printf("WS descriptor....\n");
    printf("  vers %d, cpu %d, num_screens %d, num_devs %d\n",
	wsp->version, wsp->cpu, wsp->num_screens_exist,
	wsp->num_devices_exist);
    printf("  con_screen %d, con_kbd %d, con_ptr %d\n",
	wsp->console_screen,wsp->console_keyboard, wsp->console_pointer);
}

Print_screenDesc( sdp )
    ws_screen_descriptor *sdp;
{
    printf("Screen descriptor....\n");
/****
    printf("  screen %d, monitor %6.6s, pixels %dx%d\n",
	sdp->screen, sdp->monitor_type.type,
	sdp->width, sdp->height);
 ****/
    printf("  screen %d, pixels %dx%d\n",
	sdp->screen, sdp->width, sdp->height);
    printf("  font %dx%d, cursor %dx%d\n",
	sdp->f_width, sdp->f_height, sdp->cursor_width, sdp->cursor_height);
}

Print_depthDesc( ddp )
    ws_depth_descriptor *ddp;
{
   printf("Depth Descriptor...\n");
   printf("  screen %d, depth %d, bits/pixel %d, scanline pad %d\n",
	ddp->screen, ddp->which_depth, ddp->bits_per_pixel,
	ddp->scanline_pad);
   printf("  pixmap @ %lx, plane_mask @ %lx, plane_mask_phys @ %lx\n",
	ddp->pixmap, ddp->plane_mask, ddp->plane_mask_phys);
}

/*
 * codes 0xc0->0xfb, from /sys/data/lk201_data.c
 */
char xlate[] = {
'1', 'q', 'a', 'z', 0, '2', 'w', 's', 'x', '<', 0, '3',
'e', 'd', 'c',  0,  '4', 'r', 'f', 'v', ' ', 0, '5', 't',
'g', 'b', 0, '6', 'y', 'h', 'n', 0, '7', 'u', 'j', 'm', 0,
'8', 'i', 'k', ',', 0, '9', 'o', 'l', '.', 0, '0', 'p', 0,
';', '/', 0, '=', ']', '\\', 0, '-', '[', '\''
};

char TranslateKey(key)
    unsigned char key;
{
    if ( (key < 0xc0) || (key > 0xfb) ) {
	return 0;
    }
    return ( xlate[key-0xc0] );
}

Xprintf()
{
    return 0;
}

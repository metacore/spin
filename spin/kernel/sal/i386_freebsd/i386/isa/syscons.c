/*-
 * Copyright (c) 1992-1995 S�ren Schmidt
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer
 *    in this position and unchanged.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software withough specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: syscons.c,v 1.2 1997/01/31 16:00:56 mef Exp $
 */

#include "sc.h"
#include "apm.h"
#if NSC > 0
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/conf.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/tty.h>
#include <sys/uio.h>
#include <sys/callout.h>
#include <sys/kernel.h>
#include <sys/syslog.h>
#include <sys/errno.h>
#include <sys/malloc.h>
#include <sys/devconf.h>

#include <machine/clock.h>
#include <machine/console.h>
#include <machine/psl.h>
#include <machine/frame.h>
#include <machine/pc/display.h>
#include <machine/apm_bios.h>
#include <machine/random.h>

#include <i386/isa/isa.h>
#include <i386/isa/isa_device.h>
#include <i386/isa/timerreg.h>
#include <i386/isa/kbdtables.h>
#include <i386/i386/cons.h>
#include <i386/isa/syscons.h>

#if !defined(MAXCONS)
#define MAXCONS 16
#endif

/* this may break on older VGA's but is usefull on real 32 bit systems */
#define bcopyw  bcopy

static default_attr user_default = {
    (FG_LIGHTGREY | BG_BLACK) << 8,
    (FG_BLACK | BG_LIGHTGREY) << 8
};

static default_attr kernel_default = {
    (FG_WHITE | BG_BLACK) << 8,
    (FG_BLACK | BG_LIGHTGREY) << 8
};

static  scr_stat    	main_console;
static  scr_stat    	*console[MAXCONS];
	scr_stat    	*cur_console;
static  scr_stat    	*new_scp, *old_scp;
static  term_stat   	kernel_console;
static  default_attr    *current_default;
static  char        	init_done = FALSE;
static  int     	configuration = 0;
static  char        	switch_in_progress = FALSE;
static  char        	blink_in_progress = FALSE;
static  char        	write_in_progress = FALSE;
	u_int       	crtc_addr = MONO_BASE;
static  char        	crtc_vga = FALSE;
static  u_char      	shfts = 0, ctls = 0, alts = 0, agrs = 0, metas = 0;
static  u_char      	nlkcnt = 0, clkcnt = 0, slkcnt = 0, alkcnt = 0;
static  char        	*font_8 = NULL, *font_14 = NULL, *font_16 = NULL;
static  int     	fonts_loaded = 0;
	char        	palette[3*256];
static  const u_int     n_fkey_tab = sizeof(fkey_tab) / sizeof(*fkey_tab);
static  int     	delayed_next_scr = FALSE;
static  long        	scrn_blank_time = 0;    /* screen saver timeout value */
	int     	scrn_blanked = FALSE;   /* screen saver active flag */
static  int     	scrn_saver = 0;    	/* screen saver routine */
static  long       	scrn_time_stamp;
	u_char      	scr_map[256];
static  char        	*video_mode_ptr = NULL;
#if ASYNCH
static  u_char      	kbd_reply = 0;
#endif
#ifdef SPIN
static  int             scrn_timer_started = 0;
#endif

static  u_short mouse_and_mask[16] = {
	0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00, 0xff80,
	0xfe00, 0x1e00, 0x1f00, 0x0f00, 0x0f00, 0x0000, 0x0000, 0x0000
};
static  u_short mouse_or_mask[16] = {
	0x0000, 0x4000, 0x6000, 0x7000, 0x7800, 0x7c00, 0x7e00, 0x6800,
	0x0c00, 0x0c00, 0x0600, 0x0600, 0x0000, 0x0000, 0x0000, 0x0000
};

void    none_saver(int blank) { }
void    (*current_saver)() = none_saver;

/* OS specific stuff */
#ifdef not_yet_done
#define VIRTUAL_TTY(x)  (sccons[x] = ttymalloc(sccons[x]))
struct  CONSOLE_TTY 	(sccons[MAXCONS] = ttymalloc(sccons[MAXCONS]))
struct  tty         	*sccons[MAXCONS+1];
#else
#define VIRTUAL_TTY(x)  &sccons[x]
#define CONSOLE_TTY 	&sccons[MAXCONS]
struct  tty         	sccons[MAXCONS+1];
int			nsccons = MAXCONS+1;
#endif
#define MONO_BUF    	pa_to_va(0xB0000)
#define CGA_BUF     	pa_to_va(0xB8000)
u_short         	*Crtat = (u_short *)MONO_BUF;

#define WRAPHIST(scp, pointer, offset)\
    ((scp->history) + ((((pointer) - (scp->history)) + (scp->history_size)\
    + (offset)) % (scp->history_size)))

struct  isa_driver scdriver = {
    scprobe, scattach, "sc", 1
};

int
scprobe(struct isa_device *dev)
{
    int i, j, retries = 5;
    unsigned char val;
    int xt_keyboard = 0;

    /* Enable interrupts and keyboard controller */
    kbd_wait();
    outb(KB_STAT, KB_WRITE);
    kbd_wait();
    outb(KB_DATA, KB_MODE);

    /* flush any noise in the buffer */
    while (inb(KB_STAT) & KB_BUF_FULL) {
	DELAY(10);
	(void) inb(KB_DATA);
    }

    /* Reset keyboard hardware */
    while (retries--) {
	kbd_wait();
	outb(KB_DATA, KB_RESET);
	for (i=0; i<100000; i++) {
	    DELAY(10);
	    val = inb(KB_DATA);
	    if (val == KB_ACK || val == KB_ECHO)
		goto gotres;
	    if (val == KB_RESEND)
		break;
	}
    }
gotres:
    if (retries < 0)
	printf("scprobe: keyboard won't accept RESET command\n");
    else {
	i = 10;			/* At most 10 retries. */
gotack:
	DELAY(100);
	j = 1000;		/* Wait at most 1 s. */
	while ((inb(KB_STAT) & KB_BUF_FULL) == 0 && --j > 0) DELAY(1000);
	DELAY(1000);
	val = inb(KB_DATA);
	if (val == KB_ACK && --i > 0)
	    goto gotack;
	if (val != KB_RESET_DONE)
	    printf("scprobe: keyboard RESET failed (result = 0x%02x)\n", val);
    }
    /*
     * Allow us to set the XT_KEYBD flag in UserConfig so that keyboards
     * such as those on the IBM ThinkPad laptop computers can be used
     * with the standard console driver.
     */
    if ( dev->id_flags & XT_KEYBD )
        xt_keyboard = 1;
#ifdef XT_KEYBOARD
    xt_keyboard = 1;
#endif
    if ( xt_keyboard ) {
        kbd_wait();
        outb(KB_DATA, 0xF0);
        kbd_wait();
        outb(KB_DATA, 1);
        kbd_wait();
    }
    return (IO_KBDSIZE);
}

static struct kern_devconf kdc_sc[NSC] = {
    0, 0, 0,        		/* filled in by dev_attach */
    "sc", 0, { MDDT_ISA, 0, "tty" },
    isa_generic_externalize, 0, 0, ISA_EXTERNALLEN,
    &kdc_isa0,      		/* parent */
    0,          		/* parentdata */
    DC_BUSY,        		/* the console is almost always busy */
    "Graphics console",
    DC_CLS_DISPLAY		/* class */
};

static inline void
sc_registerdev(struct isa_device *id)
{
    if(id->id_unit)
	kdc_sc[id->id_unit] = kdc_sc[0];
    kdc_sc[id->id_unit].kdc_unit = id->id_unit;
    kdc_sc[id->id_unit].kdc_isa = id;
    dev_attach(&kdc_sc[id->id_unit]);
}

#if NAPM > 0
static int
scresume(void *dummy)
{
	shfts = 0;
	ctls = 0;
	alts = 0;
	agrs = 0;
	metas = 0;
	return 0;
}
#endif

int
scattach(struct isa_device *dev)
{
    scr_stat *scp;

    scinit();
    configuration = dev->id_flags;

    scp = console[0];

    if (crtc_vga) {
	font_8 = (char *)malloc(8*256, M_DEVBUF, M_NOWAIT);
	font_14 = (char *)malloc(14*256, M_DEVBUF, M_NOWAIT);
	font_16 = (char *)malloc(16*256, M_DEVBUF, M_NOWAIT);
	copy_font(SAVE, FONT_16, font_16);
	fonts_loaded = FONT_16;
	scp->font = FONT_16;
	save_palette();
    }

    scp->scr_buf = (u_short *)malloc(scp->xsize*scp->ysize*sizeof(u_short),
				     M_DEVBUF, M_NOWAIT);
    /* copy screen to buffer */
    bcopyw(Crtat, scp->scr_buf, scp->xsize * scp->ysize * sizeof(u_short));
    scp->cursor_pos = scp->scr_buf + scp->xpos + scp->ypos * scp->xsize;
    scp->mouse_pos = scp->scr_buf;

    /* initialize history buffer & pointers */
    scp->history_head = scp->history_pos = scp->history =
	(u_short *)malloc(scp->history_size*sizeof(u_short),
			  M_DEVBUF, M_NOWAIT);
    bzero(scp->history_head, scp->history_size*sizeof(u_short));

    /* initialize cursor stuff */
    draw_cursor(scp, TRUE);
    if (crtc_vga && (configuration & CHAR_CURSOR))
	set_destructive_cursor(scp, TRUE);

#ifndef SPIN
    /* get screen update going */
    scrn_timer();
#endif

    update_leds(scp->status);
    sc_registerdev(dev);

    printf("sc%d: ", dev->id_unit);
    if (crtc_vga)
	if (crtc_addr == MONO_BASE)
	    printf("VGA mono");
	else
	    printf("VGA color");
    else
	if (crtc_addr == MONO_BASE)
	    printf("MDA/hercules");
	else
	    printf("CGA/EGA");
    printf(" <%d virtual consoles, flags=0x%x>\n", MAXCONS, configuration);

#if NAPM > 0
    scp->r_hook.ah_fun = scresume;
    scp->r_hook.ah_arg = NULL;
    scp->r_hook.ah_name = "system keyboard";
    scp->r_hook.ah_order = APM_MID_ORDER;
    apm_hook_establish(APM_HOOK_RESUME , &scp->r_hook);
#endif
    return 0;
}

struct tty
*scdevtotty(dev_t dev)
{
    int unit = minor(dev);

    if (!init_done)
	return(NULL);
    if (unit > MAXCONS || unit < 0)
	return(NULL);
    if (unit == MAXCONS)
	return CONSOLE_TTY;
    return VIRTUAL_TTY(unit);
}

static scr_stat
*get_scr_stat(dev_t dev)
{
    int unit = minor(dev);

    if (unit > MAXCONS || unit < 0)
	return(NULL);
    if (unit == MAXCONS)
	return console[0];
    return console[unit];
}

static int
get_scr_num()
{
    int i = 0;

    while ((i < MAXCONS) && (cur_console != console[i]))
	i++;
    return i < MAXCONS ? i : 0;
}

int
scopen(dev_t dev, int flag, int mode, struct proc *p)
{
    struct tty *tp = scdevtotty(dev);

    if (!tp)
	return(ENXIO);

#ifdef SPIN
    /* get screen update going */
    if(!scrn_timer_started) {
        scrn_timer();
        scrn_timer_started = 1;
    }
#endif

    tp->t_oproc = scstart;
    tp->t_param = scparam;
    tp->t_dev = dev;
    if (!(tp->t_state & TS_ISOPEN)) {
	ttychars(tp);
	tp->t_iflag = TTYDEF_IFLAG;
	tp->t_oflag = TTYDEF_OFLAG;
	tp->t_cflag = TTYDEF_CFLAG;
	tp->t_lflag = TTYDEF_LFLAG;
	tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
	scparam(tp, &tp->t_termios);
	ttsetwater(tp);
	(*linesw[tp->t_line].l_modem)(tp, 1);
    }
    else
	if (tp->t_state & TS_XCLUDE && p->p_ucred->cr_uid != 0)
	    return(EBUSY);
    if (!console[minor(dev)])
	console[minor(dev)] = alloc_scp();
    return((*linesw[tp->t_line].l_open)(dev, tp));
}

int
scclose(dev_t dev, int flag, int mode, struct proc *p)
{
    struct tty *tp = scdevtotty(dev);
    struct scr_stat *scp;

    if (!tp)
	return(ENXIO);
    if (minor(dev) < MAXCONS) {
	scp = get_scr_stat(tp->t_dev);
	if (scp->status & SWITCH_WAIT_ACQ)
	    wakeup((caddr_t)&scp->smode);
#if not_yet_done
	if (scp == &main_console) {
	    scp->pid = 0;
	    scp->proc = NULL;
	    scp->smode.mode = VT_AUTO;
	}
	else {
	    free(scp->scr_buf, M_DEVBUF);
	    free(scp->history, M_DEVBUF);
	    free(scp, M_DEVBUF);
	    console[minor(dev)] = NULL;
	}
#else
	scp->pid = 0;
	scp->proc = NULL;
	scp->smode.mode = VT_AUTO;
#endif
    }
    (*linesw[tp->t_line].l_close)(tp, flag);
    ttyclose(tp);
    return(0);
}

int
scread(dev_t dev, struct uio *uio, int flag)
{
    struct tty *tp = scdevtotty(dev);

    if (!tp)
	return(ENXIO);
    return((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

int
scwrite(dev_t dev, struct uio *uio, int flag)
{
    struct tty *tp = scdevtotty(dev);

    if (!tp)
	return(ENXIO);
    return((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

void
scintr(int unit)
{
    static struct tty *cur_tty;
    int c, len;
    u_char *cp;

    /* make screensaver happy */
    scrn_time_stamp = time.tv_sec;
    if (scrn_blanked) {
	(*current_saver)(FALSE);
	cur_console->start = 0;
	cur_console->end = cur_console->xsize * cur_console->ysize;
    }

    c = scgetc(1);

    cur_tty = VIRTUAL_TTY(get_scr_num());
    if (!(cur_tty->t_state & TS_ISOPEN))
	if (!((cur_tty = CONSOLE_TTY)->t_state & TS_ISOPEN))
	    return;

    switch (c & 0xff00) {
    case 0x0000: /* normal key */
	(*linesw[cur_tty->t_line].l_rint)(c & 0xFF, cur_tty);
	break;
    case NOKEY: /* nothing there */
	break;
    case FKEY:  /* function key, return string */
	if (cp = get_fstr((u_int)c, (u_int *)&len)) {
	    while (len-- >  0)
		(*linesw[cur_tty->t_line].l_rint)(*cp++ & 0xFF, cur_tty);
	}
	break;
    case MKEY:  /* meta is active, prepend ESC */
	(*linesw[cur_tty->t_line].l_rint)(0x1b, cur_tty);
	(*linesw[cur_tty->t_line].l_rint)(c & 0xFF, cur_tty);
	break;
    case BKEY:  /* backtab fixed sequence (esc [ Z) */
	(*linesw[cur_tty->t_line].l_rint)(0x1b, cur_tty);
	(*linesw[cur_tty->t_line].l_rint)('[', cur_tty);
	(*linesw[cur_tty->t_line].l_rint)('Z', cur_tty);
	break;
    }
}

int
scparam(struct tty *tp, struct termios *t)
{
    tp->t_ispeed = t->c_ispeed;
    tp->t_ospeed = t->c_ospeed;
    tp->t_cflag = t->c_cflag;
    return 0;
}

int
scioctl(dev_t dev, int cmd, caddr_t data, int flag, struct proc *p)
{
    int i, error;
    struct tty *tp;
    struct trapframe *fp;
    scr_stat *scp;

    tp = scdevtotty(dev);
    if (!tp)
	return ENXIO;
    scp = get_scr_stat(tp->t_dev);

    switch (cmd) {  		/* process console hardware related ioctl's */

    case GIO_ATTR:      	/* get current attributes */
	*(int*)data = scp->term.cur_attr;
	return 0;

    case GIO_COLOR:     	/* is this a color console ? */
	if (crtc_addr == COLOR_BASE)
	    *(int*)data = 1;
	else
	    *(int*)data = 0;
	return 0;

    case CONS_CURRENT:  	/* get current adapter type */
	if (crtc_vga)
	    *(int*)data = KD_VGA;
	else
	    if (crtc_addr == MONO_BASE)
		*(int*)data = KD_MONO;
	    else
		*(int*)data = KD_CGA;
	return 0;

    case CONS_GET:      	/* get current video mode */
	*(int*)data = scp->mode;
	return 0;

    case CONS_BLANKTIME:    	/* set screen saver timeout (0 = no saver) */
	scrn_blank_time = *(int*)data;
	return 0;

    case CONS_CURSORTYPE:   	/* set cursor type blink/noblink */
	if ((*(int*)data) & 0x01)
	    configuration |= BLINK_CURSOR;
	else
	    configuration &= ~BLINK_CURSOR;
	if ((*(int*)data) & 0x02) {
	    configuration |= CHAR_CURSOR;
	    set_destructive_cursor(scp, TRUE);
	} else
	    configuration &= ~CHAR_CURSOR;
	return 0;

    case CONS_BELLTYPE: 	/* set bell type sound/visual */
	if (*data)
	    configuration |= VISUAL_BELL;
	else
	    configuration &= ~VISUAL_BELL;
	return 0;

    case CONS_HISTORY:  	/* set history size */
	if (*data) {
	    free(scp->history, M_DEVBUF);
	    scp->history_size = *(int*)data;
	    if (scp->history_size < scp->ysize)
		scp->history = NULL;
	    else {
		scp->history_size *= scp->xsize;
		scp->history_head = scp->history_pos = scp->history =
		    (u_short *)malloc(scp->history_size*sizeof(u_short),
				      M_DEVBUF, M_WAITOK);
		bzero(scp->history_head, scp->history_size*sizeof(u_short));
	    }
	    return 0;
	}
	else
	    return EINVAL;

    case CONS_MOUSECTL:		/* control mouse arrow */
    {
	mouse_info_t *mouse = (mouse_info_t*)data;
	int fontsize;

	switch (scp->font) {
	default:
	case FONT_8:
	    fontsize = 8; break;
	case FONT_14:
	    fontsize = 14; break;
	case FONT_16:
	    fontsize = 16; break;
	}
	switch (mouse->operation) {
	case MOUSE_SHOW:
	    if (!(scp->status & MOUSE_ENABLED)) {
		scp->mouse_oldpos = Crtat + (scp->mouse_pos - scp->scr_buf);
		scp->status |= (UPDATE_MOUSE | MOUSE_ENABLED);
	    }
	    else
		return EINVAL;
	    break;

	case MOUSE_HIDE:
	    if (scp->status & MOUSE_ENABLED) {
		scp->status &= ~MOUSE_ENABLED;
		scp->status |= UPDATE_MOUSE;
	    }
	    else
		return EINVAL;
	    break;

	case MOUSE_MOVEABS:
	    scp->mouse_xpos = mouse->x;
	    scp->mouse_ypos = mouse->y;
	    goto set_mouse_pos;

	case MOUSE_MOVEREL:
	    scp->mouse_xpos += mouse->x;
	    scp->mouse_ypos += mouse->y;
set_mouse_pos:
	    if (scp->mouse_xpos < 0)
		scp->mouse_xpos = 0;
	    if (scp->mouse_ypos < 0)
		scp->mouse_ypos = 0;
	    if (scp->mouse_xpos >= scp->xsize*8)
		scp->mouse_xpos = (scp->xsize*8)-1;
	    if (scp->mouse_ypos >= scp->ysize*fontsize)
		scp->mouse_ypos = (scp->ysize*fontsize)-1;
	    scp->mouse_pos = scp->scr_buf +
		(scp->mouse_ypos/fontsize)*scp->xsize + scp->mouse_xpos/8;
	    if (scp->status & MOUSE_ENABLED)
		scp->status |= UPDATE_MOUSE;
	    break;

	case MOUSE_GETPOS:
	    mouse->x = scp->mouse_xpos;
	    mouse->y = scp->mouse_ypos;
	    return 0;

	default:
	    return EINVAL;
	}
	/* make screensaver happy */
	if (scp == cur_console) {
	    scrn_time_stamp = time.tv_sec;
	    if (scrn_blanked) {
		(*current_saver)(FALSE);
		cur_console->start = 0;
		cur_console->end = cur_console->xsize * cur_console->ysize;
	    }
	}
	return 0;
    }

    case CONS_GETINFO:  	/* get current (virtual) console info */
    {
	vid_info_t *ptr = (vid_info_t*)data;
	if (ptr->size == sizeof(struct vid_info)) {
	    ptr->m_num = get_scr_num();
	    ptr->mv_col = scp->xpos;
	    ptr->mv_row = scp->ypos;
	    ptr->mv_csz = scp->xsize;
	    ptr->mv_rsz = scp->ysize;
	    ptr->mv_norm.fore = (scp->term.std_attr & 0x0f00)>>8;
	    ptr->mv_norm.back = (scp->term.std_attr & 0xf000)>>12;
	    ptr->mv_rev.fore = (scp->term.rev_attr & 0x0f00)>>8;
	    ptr->mv_rev.back = (scp->term.rev_attr & 0xf000)>>12;
	    ptr->mv_grfc.fore = 0;      /* not supported */
	    ptr->mv_grfc.back = 0;      /* not supported */
	    ptr->mv_ovscan = scp->border;
	    ptr->mk_keylock = scp->status & LOCK_KEY_MASK;
	    return 0;
	}
	return EINVAL;
    }

    case CONS_GETVERS:  	/* get version number */
	*(int*)data = 0x200;    /* version 2.0 */
	return 0;

    /* VGA TEXT MODES */
    case SW_VGA_C40x25:
    case SW_VGA_C80x25: case SW_VGA_M80x25:
    case SW_VGA_C80x30: case SW_VGA_M80x30:
    case SW_VGA_C80x50: case SW_VGA_M80x50:
    case SW_VGA_C80x60: case SW_VGA_M80x60:
    case SW_B40x25:     case SW_C40x25:
    case SW_B80x25:     case SW_C80x25:
    case SW_ENH_B40x25: case SW_ENH_C40x25:
    case SW_ENH_B80x25: case SW_ENH_C80x25:
    case SW_ENH_B80x43: case SW_ENH_C80x43:

	if (!crtc_vga || video_mode_ptr == NULL)
	    return ENXIO;
	switch (cmd & 0xff) {
	case M_VGA_C80x60: case M_VGA_M80x60:
	    if (!(fonts_loaded & FONT_8))
		return EINVAL;
	    scp->xsize = 80;
	    scp->ysize = 60;
	    break;
	case M_VGA_C80x50: case M_VGA_M80x50:
	    if (!(fonts_loaded & FONT_8))
		return EINVAL;
	    scp->xsize = 80;
	    scp->ysize = 50;
	    break;
	case M_ENH_B80x43: case M_ENH_C80x43:
	    if (!(fonts_loaded & FONT_8))
		return EINVAL;
	    scp->xsize = 80;
	    scp->ysize = 43;
	    break;
	case M_VGA_C80x30: case M_VGA_M80x30:
	    scp->xsize = 80;
	    scp->ysize = 30;
	    break;
	default:
	    if ((cmd & 0xff) > M_VGA_CG320)
		return EINVAL;
	    else
		scp->xsize = *(video_mode_ptr+((cmd&0xff)*64));
		scp->ysize = *(video_mode_ptr+((cmd&0xff)*64)+1)+1;
	    break;
	}
	scp->mode = cmd & 0xff;
	scp->status &= ~UNKNOWN_MODE;   /* text mode */
	free(scp->scr_buf, M_DEVBUF);
	scp->scr_buf = (u_short *)malloc(scp->xsize*scp->ysize*sizeof(u_short),
					 M_DEVBUF, M_WAITOK);
	if (scp == cur_console)
	    set_mode(scp);
	clear_screen(scp);
	if (tp->t_winsize.ws_col != scp->xsize
	    || tp->t_winsize.ws_row != scp->ysize) {
	    tp->t_winsize.ws_col = scp->xsize;
	    tp->t_winsize.ws_row = scp->ysize;
	    pgsignal(tp->t_pgrp, SIGWINCH, 1);
	}
	return 0;

    /* GRAPHICS MODES */
    case SW_BG320:     case SW_BG640:
    case SW_CG320:     case SW_CG320_D:   case SW_CG640_E:
    case SW_CG640x350: case SW_ENH_CG640:
    case SW_BG640x480: case SW_CG640x480: case SW_VGA_CG320:

	if (!crtc_vga || video_mode_ptr == NULL)
	    return ENXIO;
	scp->mode = cmd & 0xFF;
	scp->status |= UNKNOWN_MODE;    /* graphics mode */
	scp->xsize = (*(video_mode_ptr + (scp->mode*64))) * 8;
	scp->ysize = (*(video_mode_ptr + (scp->mode*64) + 1) + 1) *
		     (*(video_mode_ptr + (scp->mode*64) + 2));
	set_mode(scp);
	/* clear_graphics();*/

	if (tp->t_winsize.ws_xpixel != scp->xsize
	    || tp->t_winsize.ws_ypixel != scp->ysize) {
	    tp->t_winsize.ws_xpixel = scp->xsize;
	    tp->t_winsize.ws_ypixel = scp->ysize;
	    pgsignal(tp->t_pgrp, SIGWINCH, 1);
	}
	return 0;

    case VT_SETMODE:    	/* set screen switcher mode */
	bcopy(data, &scp->smode, sizeof(struct vt_mode));
	if (scp->smode.mode == VT_PROCESS) {
	    scp->proc = p;
	    scp->pid = scp->proc->p_pid;
	}
	return 0;

    case VT_GETMODE:    	/* get screen switcher mode */
	bcopy(&scp->smode, data, sizeof(struct vt_mode));
	return 0;

    case VT_RELDISP:    	/* screen switcher ioctl */
	switch(*data) {
	case VT_FALSE:  	/* user refuses to release screen, abort */
	    if (scp == old_scp && (scp->status & SWITCH_WAIT_REL)) {
		old_scp->status &= ~SWITCH_WAIT_REL;
		switch_in_progress = FALSE;
		return 0;
	    }
	    return EINVAL;

	case VT_TRUE:   	/* user has released screen, go on */
	    if (scp == old_scp && (scp->status & SWITCH_WAIT_REL)) {
		scp->status &= ~SWITCH_WAIT_REL;
		exchange_scr();
		if (new_scp->smode.mode == VT_PROCESS) {
		    new_scp->status |= SWITCH_WAIT_ACQ;
		    psignal(new_scp->proc, new_scp->smode.acqsig);
		}
		else
		    switch_in_progress = FALSE;
		return 0;
	    }
	    return EINVAL;

	case VT_ACKACQ: 	/* acquire acknowledged, switch completed */
	    if (scp == new_scp && (scp->status & SWITCH_WAIT_ACQ)) {
		scp->status &= ~SWITCH_WAIT_ACQ;
		switch_in_progress = FALSE;
		return 0;
	    }
	    return EINVAL;

	default:
	    return EINVAL;
	}
	/* NOT REACHED */

    case VT_OPENQRY:    	/* return free virtual console */
	for (i = 0; i < MAXCONS; i++) {
	    tp = VIRTUAL_TTY(i);
	    if (!(tp->t_state & TS_ISOPEN)) {
		*data = i + 1;
		return 0;
	    }
	}
	return EINVAL;

    case VT_ACTIVATE:   	/* switch to screen *data */
	return switch_scr(scp, (*data) - 1);

    case VT_WAITACTIVE: 	/* wait for switch to occur */
	if (*data > MAXCONS || *data < 0)
	    return EINVAL;
	if (minor(dev) == (*data) - 1)
	    return 0;
	if (*data == 0) {
	    if (scp == cur_console)
		return 0;
	}
	else
	    scp = console[(*data) - 1];
	while ((error=tsleep((caddr_t)&scp->smode, PZERO|PCATCH,
			     "waitvt", 0)) == ERESTART) ;
	return error;

    case VT_GETACTIVE:
	*data = get_scr_num()+1;
	return 0;

    case KDENABIO:      	/* allow io operations */
	error = suser(p->p_ucred, &p->p_acflag);
	if (error != 0)
	    return error;
	fp = (struct trapframe *)p->p_md.md_regs;
	fp->tf_eflags |= PSL_IOPL;
	return 0;

    case KDDISABIO:     	/* disallow io operations (default) */
	fp = (struct trapframe *)p->p_md.md_regs;
	fp->tf_eflags &= ~PSL_IOPL;
	return 0;

    case KDSETMODE:     	/* set current mode of this (virtual) console */
	switch (*data) {
	case KD_TEXT:   	/* switch to TEXT (known) mode */
	    /* restore fonts & palette ! */
	    if (crtc_vga) {
		if (fonts_loaded & FONT_8)
		    copy_font(LOAD, FONT_8, font_8);
		if (fonts_loaded & FONT_14)
		    copy_font(LOAD, FONT_14, font_14);
		if (fonts_loaded & FONT_16)
		    copy_font(LOAD, FONT_16, font_16);
		if (configuration & CHAR_CURSOR)
		    set_destructive_cursor(scp, TRUE);
		load_palette();
	    }
	    /* FALL THROUGH */

	case KD_TEXT1:  	/* switch to TEXT (known) mode */
	    /* no restore fonts & palette */
	    scp->status &= ~UNKNOWN_MODE;
	    if (crtc_vga && video_mode_ptr)
		set_mode(scp);
	    clear_screen(scp);
	    return 0;

	case KD_GRAPHICS:	/* switch to GRAPHICS (unknown) mode */
	    scp->status |= UNKNOWN_MODE;
	    return 0;
	default:
	    return EINVAL;
	}
	/* NOT REACHED */

    case KDGETMODE:     	/* get current mode of this (virtual) console */
	*data = (scp->status & UNKNOWN_MODE) ? KD_GRAPHICS : KD_TEXT;
	return 0;

    case KDSBORDER:     	/* set border color of this (virtual) console */
	if (!crtc_vga)
	    return ENXIO;
	scp->border = *data;
	if (scp == cur_console)
	    set_border(scp->border);
	return 0;

    case KDSKBSTATE:    	/* set keyboard state (locks) */
	if (*data >= 0 && *data <= LOCK_KEY_MASK) {
	    scp->status &= ~LOCK_KEY_MASK;
	    scp->status |= *data;
	    if (scp == cur_console)
		update_leds(scp->status);
	    return 0;
	}
	return EINVAL;

    case KDGKBSTATE:    	/* get keyboard state (locks) */
	*data = scp->status & LOCK_KEY_MASK;
	return 0;

    case KDSETRAD:      	/* set keyboard repeat & delay rates */
	if (*data & 0x80)
	    return EINVAL;
	i = spltty();
	kbd_cmd(KB_SETRAD);
	kbd_cmd(*data);
	splx(i);
	return 0;

    case KDSKBMODE:     	/* set keyboard mode */
	switch (*data) {
	case K_RAW: 		/* switch to RAW scancode mode */
	    scp->status |= KBD_RAW_MODE;
	    return 0;

	case K_XLATE:   	/* switch to XLT ascii mode */
	    if (scp == cur_console && scp->status == KBD_RAW_MODE)
		shfts = ctls = alts = agrs = metas = 0;
	    scp->status &= ~KBD_RAW_MODE;
	    return 0;
	default:
	    return EINVAL;
	}
	/* NOT REACHED */

    case KDGKBMODE:     	/* get keyboard mode */
	*data = (scp->status & KBD_RAW_MODE) ? K_RAW : K_XLATE;
	return 0;

    case KDMKTONE:      	/* sound the bell */
	if (*(int*)data)
	    do_bell(scp, (*(int*)data)&0xffff,
		    (((*(int*)data)>>16)&0xffff)*hz/1000);
	else
	    do_bell(scp, scp->bell_pitch, scp->bell_duration);
	return 0;

    case KIOCSOUND:     	/* make tone (*data) hz */
	if (scp == cur_console) {
	    if (*(int*)data) {
		int pitch = TIMER_FREQ/(*(int*)data);

		/* set command for counter 2, 2 byte write */
		if (acquire_timer2(TIMER_16BIT|TIMER_SQWAVE))
		    return EBUSY;

		/* set pitch */
		outb(TIMER_CNTR2, pitch);
		outb(TIMER_CNTR2, (pitch>>8));

		/* enable counter 2 output to speaker */
		outb(IO_PPI, inb(IO_PPI) | 3);
	    }
	    else {
		/* disable counter 2 output to speaker */
		outb(IO_PPI, inb(IO_PPI) & 0xFC);
		release_timer2();
	    }
	}
	return 0;

    case KDGKBTYPE:     	/* get keyboard type */
	*data = 0;  		/* type not known (yet) */
	return 0;

    case KDSETLED:      	/* set keyboard LED status */
	if (*data >= 0 && *data <= LED_MASK) {
	    scp->status &= ~LED_MASK;
	    scp->status |= *data;
	    if (scp == cur_console)
		update_leds(scp->status);
	    return 0;
	}
	return EINVAL;

    case KDGETLED:      	/* get keyboard LED status */
	*data = scp->status & LED_MASK;
	return 0;

    case GETFKEY:       	/* get functionkey string */
	if (*(u_short*)data < n_fkey_tab) {
	    fkeyarg_t *ptr = (fkeyarg_t*)data;
	    bcopy(&fkey_tab[ptr->keynum].str, ptr->keydef,
		  fkey_tab[ptr->keynum].len);
	    ptr->flen = fkey_tab[ptr->keynum].len;
	    return 0;
	}
	else
	    return EINVAL;

    case SETFKEY:       	/* set functionkey string */
	if (*(u_short*)data < n_fkey_tab) {
	    fkeyarg_t *ptr = (fkeyarg_t*)data;
	    bcopy(ptr->keydef, &fkey_tab[ptr->keynum].str,
		  min(ptr->flen, MAXFK));
	    fkey_tab[ptr->keynum].len = min(ptr->flen, MAXFK);
	    return 0;
	}
	else
	    return EINVAL;

    case GIO_SCRNMAP:   	/* get output translation table */
	bcopy(&scr_map, data, sizeof(scr_map));
	return 0;

    case PIO_SCRNMAP:   	/* set output translation table */
	bcopy(data, &scr_map, sizeof(scr_map));
	return 0;

    case GIO_KEYMAP:    	/* get keyboard translation table */
	bcopy(&key_map, data, sizeof(key_map));
	return 0;

    case PIO_KEYMAP:    	/* set keyboard translation table */
	bcopy(data, &key_map, sizeof(key_map));
	return 0;

    case PIO_FONT8x8:   	/* set 8x8 dot font */
	if (!crtc_vga)
	    return ENXIO;
	bcopy(data, font_8, 8*256);
	fonts_loaded |= FONT_8;
	copy_font(LOAD, FONT_8, font_8);
	if (configuration & CHAR_CURSOR)
	    set_destructive_cursor(scp, TRUE);
	return 0;

    case GIO_FONT8x8:   	/* get 8x8 dot font */
	if (!crtc_vga)
	    return ENXIO;
	if (fonts_loaded & FONT_8) {
	    bcopy(font_8, data, 8*256);
	    return 0;
	}
	else
	    return ENXIO;

    case PIO_FONT8x14:  	/* set 8x14 dot font */
	if (!crtc_vga)
	    return ENXIO;
	bcopy(data, font_14, 14*256);
	fonts_loaded |= FONT_14;
	copy_font(LOAD, FONT_14, font_14);
	if (configuration & CHAR_CURSOR)
	    set_destructive_cursor(scp, TRUE);
	return 0;

    case GIO_FONT8x14:  	/* get 8x14 dot font */
	if (!crtc_vga)
	    return ENXIO;
	if (fonts_loaded & FONT_14) {
	    bcopy(font_14, data, 14*256);
	    return 0;
	}
	else
	    return ENXIO;

    case PIO_FONT8x16:  	/* set 8x16 dot font */
	if (!crtc_vga)
	    return ENXIO;
	bcopy(data, font_16, 16*256);
	fonts_loaded |= FONT_16;
	copy_font(LOAD, FONT_16, font_16);
	if (configuration & CHAR_CURSOR)
	    set_destructive_cursor(scp, TRUE);
	return 0;

    case GIO_FONT8x16:  	/* get 8x16 dot font */
	if (!crtc_vga)
	    return ENXIO;
	if (fonts_loaded & FONT_16) {
	    bcopy(font_16, data, 16*256);
	    return 0;
	}
	else
	    return ENXIO;
    default:
	break;
    }

    error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
    if (error >= 0)
	return(error);
    error = ttioctl(tp, cmd, data, flag);
    if (error >= 0)
	return(error);
    return(ENOTTY);
}

void
scstart(struct tty *tp)
{
    struct clist *rbp;
    int i, s, len;
    u_char buf[PCBURST];
    scr_stat *scp = get_scr_stat(tp->t_dev);

    /* XXX who repeats the call when the above flags are cleared? */
    if (scp->status & SLKED || blink_in_progress)
	return;
    s = spltty();
    if (!(tp->t_state & (TS_TIMEOUT | TS_BUSY | TS_TTSTOP))) {
	tp->t_state |= TS_BUSY;
	rbp = &tp->t_outq;
	while (rbp->c_cc) {
	    len = q_to_b(rbp, buf, PCBURST);
	    splx(s);
	    ansi_put(scp, buf, len);
	    s = spltty();
	}
	tp->t_state &= ~TS_BUSY;
	ttwwakeup(tp);
    }
    splx(s);
}

void
pccnprobe(struct consdev *cp)
{
    int maj;

    /*
     * If this has already been done, don't do it again
     * because cdevsw[maj] has been changed to cnopen and
     * we won't find the right table entry.
     */
    if (cp->cn_dev > 0)
	return;

    /* locate the major number */
    for (maj = 0; maj < nchrdev; maj++)
	if ((void*)cdevsw[maj].d_open == (void*)scopen)
	    break;
    /* XXX - we should panic() if we don't find it */

    /* initialize required fields */
    cp->cn_dev = makedev(maj, MAXCONS);
    cp->cn_pri = CN_INTERNAL;
}

void
pccninit(struct consdev *cp)
{
    scinit();
}

void
pccnputc(dev_t dev, int c)
{
    u_char buf[1];
    int s;
    scr_stat *scp = console[0];
    term_stat save = scp->term;

    scp->term = kernel_console;
    current_default = &kernel_default;
    if (scp->scr_buf == Crtat)
	draw_cursor(scp, FALSE);
    buf[0] = c;
    ansi_put(scp, buf, 1);
    kernel_console = scp->term;
    current_default = &user_default;
    scp->term = save;
    s = splclock();		/* XXX stop scrn_timer */
    if (scp == cur_console) {
	if (scp->scr_buf != Crtat && (scp->start <= scp->end)) {
	    bcopyw(scp->scr_buf + scp->start, Crtat + scp->start,
		   (1 + scp->end - scp->start) * sizeof(u_short));
	    scp->start = scp->xsize * scp->ysize;
	    scp->end = 0;
	    scp->status &= ~CURSOR_SHOWN;
	}
	draw_cursor(scp, TRUE);
    }
    splx(s);
}

int
pccngetc(dev_t dev)
{
    int s = spltty();       /* block scintr while we poll */
    int c = scgetc(0);
    splx(s);
    return(c);
}

int
pccncheckc(dev_t dev)
{
    return (scgetc(1) & 0xff);
}

static void
scrn_timer()
{
    static int cursor_blinkrate;
    scr_stat *scp = cur_console;

    /* should we just return ? */
    if ((scp->status&UNKNOWN_MODE) || blink_in_progress || switch_in_progress) {
	timeout((timeout_func_t)scrn_timer, 0, hz/10);
	return;
    }

    if (!scrn_blanked) {
	/* update screen image */
	if (scp->start <= scp->end) {
	    bcopyw(scp->scr_buf + scp->start, Crtat + scp->start,
		   (1 + scp->end - scp->start) * sizeof(u_short));
	    scp->status &= ~CURSOR_SHOWN;
	    scp->start = scp->xsize * scp->ysize;
	    scp->end = 0;
	}
	/* update "pseudo" mouse arrow */
	if ((scp->status & MOUSE_ENABLED) && (scp->status & UPDATE_MOUSE))
	    draw_mouse_image(scp);

	/* update cursor image */
	if (scp->status & CURSOR_ENABLED)
	    draw_cursor(scp,
		!(configuration&BLINK_CURSOR) || !(cursor_blinkrate++&0x04));
    }
    if (scrn_blank_time && (time.tv_sec>scrn_time_stamp+scrn_blank_time))
	(*current_saver)(TRUE);
    timeout((timeout_func_t)scrn_timer, 0, hz/25);
}

static void
clear_screen(scr_stat *scp)
{
    move_crsr(scp, 0, 0);
    fillw(scp->term.cur_attr | scr_map[0x20], scp->scr_buf,
	  scp->xsize * scp->ysize);
    mark_all(scp);
}

static int
switch_scr(scr_stat *scp, u_int next_scr)
{
    if (switch_in_progress && (cur_console->proc != pfind(cur_console->pid)))
	switch_in_progress = FALSE;

    if (next_scr >= MAXCONS || switch_in_progress ||
	(cur_console->smode.mode == VT_AUTO
	 && cur_console->status & UNKNOWN_MODE)) {
	do_bell(scp, BELL_PITCH, BELL_DURATION);
	return EINVAL;
    }

    /* is the wanted virtual console open ? */
    if (next_scr) {
	struct tty *tp = VIRTUAL_TTY(next_scr);
	if (!(tp->t_state & TS_ISOPEN)) {
	    do_bell(scp, BELL_PITCH, BELL_DURATION);
	    return EINVAL;
	}
    }
    /* delay switch if actively updating screen */
    if (write_in_progress || blink_in_progress) {
	delayed_next_scr = next_scr+1;
	return 0;
    }
    switch_in_progress = TRUE;
    old_scp = cur_console;
    new_scp = console[next_scr];
    wakeup((caddr_t)&new_scp->smode);
    if (new_scp == old_scp) {
	switch_in_progress = FALSE;
	delayed_next_scr = FALSE;
	return 0;
    }

    /* has controlling process died? */
    if (old_scp->proc && (old_scp->proc != pfind(old_scp->pid)))
	old_scp->smode.mode = VT_AUTO;
    if (new_scp->proc && (new_scp->proc != pfind(new_scp->pid)))
	new_scp->smode.mode = VT_AUTO;

    /* check the modes and switch approbiatly */
    if (old_scp->smode.mode == VT_PROCESS) {
	old_scp->status |= SWITCH_WAIT_REL;
	psignal(old_scp->proc, old_scp->smode.relsig);
    }
    else {
	exchange_scr();
	if (new_scp->smode.mode == VT_PROCESS) {
	    new_scp->status |= SWITCH_WAIT_ACQ;
	    psignal(new_scp->proc, new_scp->smode.acqsig);
	}
	else
	    switch_in_progress = FALSE;
    }
    return 0;
}

static void
exchange_scr(void)
{
    move_crsr(old_scp, old_scp->xpos, old_scp->ypos);
    cur_console = new_scp;
    if (old_scp->mode != new_scp->mode || (old_scp->status & UNKNOWN_MODE)){
	if (crtc_vga && video_mode_ptr)
	    set_mode(new_scp);
    }
    move_crsr(new_scp, new_scp->xpos, new_scp->ypos);
    if ((old_scp->status & UNKNOWN_MODE) && crtc_vga) {
	if (fonts_loaded & FONT_8)
	    copy_font(LOAD, FONT_8, font_8);
	if (fonts_loaded & FONT_14)
	    copy_font(LOAD, FONT_14, font_14);
	if (fonts_loaded & FONT_16)
	    copy_font(LOAD, FONT_16, font_16);
	if (configuration & CHAR_CURSOR)
	    set_destructive_cursor(new_scp, TRUE);
	load_palette();
    }
    if (old_scp->status & KBD_RAW_MODE || new_scp->status & KBD_RAW_MODE)
	shfts = ctls = alts = agrs = metas = 0;
    update_leds(new_scp->status);
    delayed_next_scr = FALSE;
    bcopyw(new_scp->scr_buf, Crtat,
	   (new_scp->xsize*new_scp->ysize)*sizeof(u_short));
    new_scp->status &= ~CURSOR_SHOWN;
}

static inline void
move_crsr(scr_stat *scp, int x, int y)
{
    if (x < 0 || y < 0 || x >= scp->xsize || y >= scp->ysize)
	return;
    scp->xpos = x;
    scp->ypos = y;
    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
    scp->cursor_pos = scp->scr_buf + scp->ypos * scp->xsize + scp->xpos;
    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
}

static void
scan_esc(scr_stat *scp, u_char c)
{
    static u_char ansi_col[16] =
	{0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11, 15};
    int i, n;
    u_short *src, *dst, count;

    if (scp->term.esc == 1) {
	switch (c) {

	case '[':   /* Start ESC [ sequence */
	    scp->term.esc = 2;
	    scp->term.last_param = -1;
	    for (i = scp->term.num_param; i < MAX_ESC_PAR; i++)
		scp->term.param[i] = 1;
	    scp->term.num_param = 0;
	    return;

	case 'M':   /* Move cursor up 1 line, scroll if at top */
	    if (scp->ypos > 0)
		move_crsr(scp, scp->xpos, scp->ypos - 1);
	    else {
		bcopyw(scp->scr_buf, scp->scr_buf + scp->xsize,
		       (scp->ysize - 1) * scp->xsize * sizeof(u_short));
		fillw(scp->term.cur_attr | scr_map[0x20],
		      scp->scr_buf, scp->xsize);
    		mark_all(scp);
	    }
	    break;
#if notyet
	case 'Q':
	    scp->term.esc = 4;
	    break;
#endif
	case 'c':   /* Clear screen & home */
	    clear_screen(scp);
	    break;
	}
    }
    else if (scp->term.esc == 2) {
	if (c >= '0' && c <= '9') {
	    if (scp->term.num_param < MAX_ESC_PAR) {
	    if (scp->term.last_param != scp->term.num_param) {
		scp->term.last_param = scp->term.num_param;
		scp->term.param[scp->term.num_param] = 0;
	    }
	    else
		scp->term.param[scp->term.num_param] *= 10;
	    scp->term.param[scp->term.num_param] += c - '0';
	    return;
	    }
	}
	scp->term.num_param = scp->term.last_param + 1;
	switch (c) {

	case ';':
	    if (scp->term.num_param < MAX_ESC_PAR)
		return;
	    break;

	case '=':
	    scp->term.esc = 3;
	    scp->term.last_param = -1;
	    for (i = scp->term.num_param; i < MAX_ESC_PAR; i++)
		scp->term.param[i] = 1;
	    scp->term.num_param = 0;
	    return;

	case 'A':   /* up n rows */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, scp->xpos, scp->ypos - n);
	    break;

	case 'B':   /* down n rows */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, scp->xpos, scp->ypos + n);
	    break;

	case 'C':   /* right n columns */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, scp->xpos + n, scp->ypos);
	    break;

	case 'D':   /* left n columns */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, scp->xpos - n, scp->ypos);
	    break;

	case 'E':   /* cursor to start of line n lines down */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, 0, scp->ypos + n);
	    break;

	case 'F':   /* cursor to start of line n lines up */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    move_crsr(scp, 0, scp->ypos - n);
	    break;

	case 'f':   /* Cursor move */
	case 'H':
	    if (scp->term.num_param == 0)
		move_crsr(scp, 0, 0);
	    else if (scp->term.num_param == 2)
		move_crsr(scp, scp->term.param[1] - 1, scp->term.param[0] - 1);
	    break;

	case 'J':   /* Clear all or part of display */
	    if (scp->term.num_param == 0)
		n = 0;
	    else
		n = scp->term.param[0];
	    switch (n) {
	    case 0: /* clear form cursor to end of display */
		fillw(scp->term.cur_attr | scr_map[0x20],
		      scp->cursor_pos,
		      scp->scr_buf + scp->xsize * scp->ysize - scp->cursor_pos);
    		mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
    		mark_for_update(scp, scp->xsize * scp->ysize);
		break;
	    case 1: /* clear from beginning of display to cursor */
		fillw(scp->term.cur_attr | scr_map[0x20],
		      scp->scr_buf,
		      scp->cursor_pos - scp->scr_buf);
    		mark_for_update(scp, 0);
    		mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
		break;
	    case 2: /* clear entire display */
		clear_screen(scp);
		break;
	    }
	    break;

	case 'K':   /* Clear all or part of line */
	    if (scp->term.num_param == 0)
		n = 0;
	    else
		n = scp->term.param[0];
	    switch (n) {
	    case 0: /* clear form cursor to end of line */
		fillw(scp->term.cur_attr | scr_map[0x20],
		      scp->cursor_pos,
		      scp->xsize - scp->xpos);
    		mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
    		mark_for_update(scp, scp->cursor_pos - scp->scr_buf +
				scp->xsize - scp->xpos);
		break;
	    case 1: /* clear from beginning of line to cursor */
		fillw(scp->term.cur_attr|scr_map[0x20],
		      scp->cursor_pos - (scp->xsize - scp->xpos),
		      (scp->xsize - scp->xpos) + 1);
    		mark_for_update(scp, scp->ypos * scp->xsize);
    		mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
		break;
	    case 2: /* clear entire line */
		fillw(scp->term.cur_attr|scr_map[0x20],
		      scp->cursor_pos - (scp->xsize - scp->xpos),
		      scp->xsize);
    		mark_for_update(scp, scp->ypos * scp->xsize);
    		mark_for_update(scp, (scp->ypos + 1) * scp->xsize);
		break;
	    }
	    break;

	case 'L':   /* Insert n lines */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    if (n > scp->ysize - scp->ypos)
		n = scp->ysize - scp->ypos;
	    src = scp->scr_buf + scp->ypos * scp->xsize;
	    dst = src + n * scp->xsize;
	    count = scp->ysize - (scp->ypos + n);
	    bcopyw(src, dst, count * scp->xsize * sizeof(u_short));
	    fillw(scp->term.cur_attr | scr_map[0x20], src,
		  n * scp->xsize);
	    mark_for_update(scp, scp->ypos * scp->xsize);
	    mark_for_update(scp, scp->xsize * scp->ysize);
	    break;

	case 'M':   /* Delete n lines */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    if (n > scp->ysize - scp->ypos)
		n = scp->ysize - scp->ypos;
	    dst = scp->scr_buf + scp->ypos * scp->xsize;
	    src = dst + n * scp->xsize;
	    count = scp->ysize - (scp->ypos + n);
	    bcopyw(src, dst, count * scp->xsize * sizeof(u_short));
	    src = dst + count * scp->xsize;
	    fillw(scp->term.cur_attr | scr_map[0x20], src,
		  n * scp->xsize);
	    mark_for_update(scp, scp->ypos * scp->xsize);
	    mark_for_update(scp, scp->xsize * scp->ysize);
	    break;

	case 'P':   /* Delete n chars */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    if (n > scp->xsize - scp->xpos)
		n = scp->xsize - scp->xpos;
	    dst = scp->cursor_pos;
	    src = dst + n;
	    count = scp->xsize - (scp->xpos + n);
	    bcopyw(src, dst, count * sizeof(u_short));
	    src = dst + count;
	    fillw(scp->term.cur_attr | scr_map[0x20], src, n);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf + n + count);
	    break;

	case '@':   /* Insert n chars */
	    n = scp->term.param[0]; if (n < 1) n = 1;
	    if (n > scp->xsize - scp->xpos)
		n = scp->xsize - scp->xpos;
	    src = scp->cursor_pos;
	    dst = src + n;
	    count = scp->xsize - (scp->xpos + n);
	    bcopyw(src, dst, count * sizeof(u_short));
	    fillw(scp->term.cur_attr | scr_map[0x20], src, n);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf + n + count);
	    break;

	case 'S':   /* scroll up n lines */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    if (n > scp->ysize)
		n = scp->ysize;
	    bcopyw(scp->scr_buf + (scp->xsize * n),
		   scp->scr_buf,
		   scp->xsize * (scp->ysize - n) * sizeof(u_short));
	    fillw(scp->term.cur_attr | scr_map[0x20],
		  scp->scr_buf + scp->xsize * (scp->ysize - n),
		  scp->xsize * n);
    	    mark_all(scp);
	    break;

	case 'T':   /* scroll down n lines */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    if (n > scp->ysize)
		n = scp->ysize;
	    bcopyw(scp->scr_buf,
		  scp->scr_buf + (scp->xsize * n),
		  scp->xsize * (scp->ysize - n) *
		  sizeof(u_short));
	    fillw(scp->term.cur_attr | scr_map[0x20],
		  scp->scr_buf, scp->xsize * n);
    	    mark_all(scp);
	    break;

	case 'X':   /* erase n characters in line */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    if (n > scp->xsize - scp->xpos)
		n = scp->xsize - scp->xpos;
	    fillw(scp->term.cur_attr | scr_map[0x20],
		  scp->scr_buf + scp->xpos +
		  ((scp->xsize*scp->ypos) * sizeof(u_short)), n);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf + n);
	    break;

	case 'Z':   /* move n tabs backwards */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    if ((i = scp->xpos & 0xf8) == scp->xpos)
		i -= 8*n;
	    else
		i -= 8*(n-1);
	    if (i < 0)
		i = 0;
	    move_crsr(scp, i, scp->ypos);
	    break;

	case '`':   /* move cursor to column n */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    move_crsr(scp, n - 1, scp->ypos);
	    break;

	case 'a':   /* move cursor n columns to the right */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    move_crsr(scp, scp->xpos + n, scp->ypos);
	    break;

	case 'd':   /* move cursor to row n */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    move_crsr(scp, scp->xpos, n - 1);
	    break;

	case 'e':   /* move cursor n rows down */
	    n = scp->term.param[0]; if (n < 1)  n = 1;
	    move_crsr(scp, scp->xpos, scp->ypos + n);
	    break;

	case 'm':   /* change attribute */
	    if (scp->term.num_param == 0) {
		scp->term.cur_attr = scp->term.std_attr;
		break;
	    }
	    for (i = 0; i < scp->term.num_param; i++) {
		switch (n = scp->term.param[i]) {
		case 0: /* back to normal */
		    scp->term.cur_attr = scp->term.std_attr;
		    break;
		case 1: /* highlight (bold) */
		    scp->term.cur_attr &= 0xFF00;
		    scp->term.cur_attr |= 0x0800;
		    break;
		case 4: /* highlight (underline) */
		    scp->term.cur_attr &= 0xFF00;
		    scp->term.cur_attr |= 0x0800;
		    break;
		case 5: /* blink */
		    scp->term.cur_attr &= 0xFF00;
		    scp->term.cur_attr |= 0x8000;
		    break;
		case 7: /* reverse video */
		    scp->term.cur_attr = scp->term.rev_attr;
		    break;
		case 30: case 31: /* set fg color */
		case 32: case 33: case 34:
		case 35: case 36: case 37:
		    scp->term.cur_attr =
			(scp->term.cur_attr&0xF8FF) | (ansi_col[(n-30)&7]<<8);
		    break;
		case 40: case 41: /* set bg color */
		case 42: case 43: case 44:
		case 45: case 46: case 47:
		    scp->term.cur_attr =
			(scp->term.cur_attr&0x8FFF) | (ansi_col[(n-40)&7]<<12);
		    break;
		}
	    }
	    break;

	case 'x':
	    if (scp->term.num_param == 0)
		n = 0;
	    else
		n = scp->term.param[0];
	    switch (n) {
	    case 0:     /* reset attributes */
		scp->term.cur_attr = scp->term.std_attr =
		    current_default->std_attr;
		scp->term.rev_attr = current_default->rev_attr;
		break;
	    case 1:     /* set ansi background */
		scp->term.cur_attr = scp->term.std_attr =
		    (scp->term.std_attr & 0x0F00) |
		    (ansi_col[(scp->term.param[1])&0x0F]<<12);
		break;
	    case 2:     /* set ansi foreground */
		scp->term.cur_attr = scp->term.std_attr =
		    (scp->term.std_attr & 0xF000) |
		    (ansi_col[(scp->term.param[1])&0x0F]<<8);
		break;
	    case 3:     /* set ansi attribute directly */
		scp->term.cur_attr = scp->term.std_attr =
		    (scp->term.param[1]&0xFF)<<8;
		break;
	    case 5:     /* set ansi reverse video background */
		scp->term.rev_attr =
		    (scp->term.rev_attr & 0x0F00) |
		    (ansi_col[(scp->term.param[1])&0x0F]<<12);
		break;
	    case 6:     /* set ansi reverse video foreground */
		scp->term.rev_attr =
		    (scp->term.rev_attr & 0xF000) |
		    (ansi_col[(scp->term.param[1])&0x0F]<<8);
		break;
	    case 7:     /* set ansi reverse video directly */
		scp->term.rev_attr =
		    (scp->term.param[1]&0xFF)<<8;
		break;
	    }
	    break;

	case 'z':   /* switch to (virtual) console n */
	    if (scp->term.num_param == 1)
		switch_scr(scp, scp->term.param[0]);
	    break;
	}
    }
    else if (scp->term.esc == 3) {
	if (c >= '0' && c <= '9') {
	    if (scp->term.num_param < MAX_ESC_PAR) {
	    if (scp->term.last_param != scp->term.num_param) {
		scp->term.last_param = scp->term.num_param;
		scp->term.param[scp->term.num_param] = 0;
	    }
	    else
		scp->term.param[scp->term.num_param] *= 10;
	    scp->term.param[scp->term.num_param] += c - '0';
	    return;
	    }
	}
	scp->term.num_param = scp->term.last_param + 1;
	switch (c) {

	case ';':
	    if (scp->term.num_param < MAX_ESC_PAR)
		return;
	    break;

	case 'A':   /* set display border color */
	    if (scp->term.num_param == 1)
		scp->border=scp->term.param[0] & 0xff;
		if (scp == cur_console)
		    set_border(scp->border);
	    break;

	case 'B':   /* set bell pitch and duration */
	    if (scp->term.num_param == 2) {
		scp->bell_pitch = scp->term.param[0];
		scp->bell_duration = scp->term.param[1]*10;
	    }
	    break;

	case 'C':   /* set cursor type & shape */
	    if (scp->term.num_param == 1) {
		if (scp->term.param[0] & 0x01)
		    configuration |= BLINK_CURSOR;
		else
		    configuration &= ~BLINK_CURSOR;
		if (scp->term.param[0] & 0x02) {
		    configuration |= CHAR_CURSOR;
		    set_destructive_cursor(scp, TRUE);
		} else
		    configuration &= ~CHAR_CURSOR;
	    }
	    else if (scp->term.num_param == 2) {
		scp->cursor_start = scp->term.param[0] & 0x1F;
		scp->cursor_end = scp->term.param[1] & 0x1F;
		if (configuration & CHAR_CURSOR)
			set_destructive_cursor(scp, TRUE);
	    }
	    break;

	case 'F':   /* set ansi foreground */
	    if (scp->term.num_param == 1)
		scp->term.cur_attr = scp->term.std_attr =
		    (scp->term.std_attr & 0xF000)
		    | ((scp->term.param[0] & 0x0F) << 8);
	    break;

	case 'G':   /* set ansi background */
	    if (scp->term.num_param == 1)
		scp->term.cur_attr = scp->term.std_attr =
		    (scp->term.std_attr & 0x0F00)
		    | ((scp->term.param[0] & 0x0F) << 12);
	    break;

	case 'H':   /* set ansi reverse video foreground */
	    if (scp->term.num_param == 1)
		scp->term.rev_attr =
		    (scp->term.rev_attr & 0xF000)
		    | ((scp->term.param[0] & 0x0F) << 8);
	    break;

	case 'I':   /* set ansi reverse video background */
	    if (scp->term.num_param == 1)
		scp->term.rev_attr =
		    (scp->term.rev_attr & 0x0F00)
		    | ((scp->term.param[0] & 0x0F) << 12);
	    break;
	}
    }
    scp->term.esc = 0;
}

static inline void
draw_cursor(scr_stat *scp, int show)
{
    if (show && !(scp->status & CURSOR_SHOWN)) {
	u_short cursor_image = *(Crtat + (scp->cursor_pos - scp->scr_buf));

	scp->cursor_saveunder = cursor_image;
	if (configuration & CHAR_CURSOR) {
	    set_destructive_cursor(scp, FALSE);
	    cursor_image = (cursor_image & 0xff00) | DEAD_CHAR;
	}
	else {
	    if ((cursor_image & 0x7000) == 0x7000) {
		cursor_image &= 0x8fff;
		if(!(cursor_image & 0x0700))
		    cursor_image |= 0x0700;
	    } else {
		cursor_image |= 0x7000;
		if ((cursor_image & 0x0700) == 0x0700)
		    cursor_image &= 0xf0ff;
	    }
	}
	*(Crtat + (scp->cursor_pos - scp->scr_buf)) = cursor_image;
	mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	scp->status |= CURSOR_SHOWN;
    }
    if (!show && (scp->status & CURSOR_SHOWN)) {
	*(Crtat + (scp->cursor_pos - scp->scr_buf)) = scp->cursor_saveunder;
	mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	scp->status &= ~CURSOR_SHOWN;
    }
}

static void
ansi_put(scr_stat *scp, u_char *buf, int len)
{
    u_char *ptr = buf;

    if (scp->status & UNKNOWN_MODE)
	return;

    /* make screensaver happy */
    if (scp == cur_console) {
	scrn_time_stamp = time.tv_sec;
	if (scrn_blanked) {
	    (*current_saver)(FALSE);
	    cur_console->start = 0;
	    cur_console->end = cur_console->xsize * cur_console->ysize;
	}
    }
    write_in_progress++;
outloop:
    if (scp->term.esc) {
	scan_esc(scp, *ptr++);
	len--;
    }
    else if (PRINTABLE(*ptr)) {     /* Print only printables */
 	int cnt = len <= (scp->xsize-scp->xpos) ? len : (scp->xsize-scp->xpos);
 	u_short cur_attr = scp->term.cur_attr;
 	u_short *cursor_pos = scp->cursor_pos;
	do {
	    /*
	     * gcc-2.6.3 generates poor (un)sign extension code.  Casting the
	     * pointers in the following to volatile should have no effect,
	     * but in fact speeds up this inner loop from 26 to 18 cycles
	     * (+ cache misses) on i486's.
	     */
#define	UCVP(ucp)	((u_char volatile *)(ucp))
	    *cursor_pos++ = UCVP(scr_map)[*UCVP(ptr)] | cur_attr;
	    ptr++;
	    cnt--;
	} while (cnt && PRINTABLE(*ptr));
	len -= (cursor_pos - scp->cursor_pos);
	scp->xpos += (cursor_pos - scp->cursor_pos);
	mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	mark_for_update(scp, cursor_pos - scp->scr_buf);
	scp->cursor_pos = cursor_pos;
	if (scp->xpos >= scp->xsize) {
	    scp->xpos = 0;
	    scp->ypos++;
	}
    }
    else  {
	switch(*ptr) {
	case 0x07:
	    do_bell(scp, scp->bell_pitch, scp->bell_duration);
	    break;

	case 0x08:      /* non-destructive backspace */
	    if (scp->cursor_pos > scp->scr_buf) {
	    	mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
		scp->cursor_pos--;
	    	mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
		if (scp->xpos > 0)
		    scp->xpos--;
		else {
		    scp->xpos += scp->xsize - 1;
		    scp->ypos--;
		}
	    }
	    break;

	case 0x09:  /* non-destructive tab */
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    scp->cursor_pos += (8 - scp->xpos % 8u);
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    if ((scp->xpos += (8 - scp->xpos % 8u)) >= scp->xsize) {
	        scp->xpos = 0;
	        scp->ypos++;
	    }
	    break;

	case 0x0a:  /* newline, same pos */
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    scp->cursor_pos += scp->xsize;
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    scp->ypos++;
	    break;

	case 0x0c:  /* form feed, clears screen */
	    clear_screen(scp);
	    break;

	case 0x0d:  /* return, return to pos 0 */
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    scp->cursor_pos -= scp->xpos;
	    mark_for_update(scp, scp->cursor_pos - scp->scr_buf);
	    scp->xpos = 0;
	    break;

	case 0x1b:  /* start escape sequence */
	    scp->term.esc = 1;
	    scp->term.num_param = 0;
	    break;
	}
	ptr++; len--;
    }
    /* do we have to scroll ?? */
    if (scp->cursor_pos >= scp->scr_buf + scp->ysize * scp->xsize) {
	if (scp->history) {
	    bcopyw(scp->scr_buf, scp->history_head,
		   scp->xsize * sizeof(u_short));
	    scp->history_head += scp->xsize;
	    if (scp->history_head + scp->xsize >
		scp->history + scp->history_size)
		scp->history_head = scp->history;
	}
	bcopyw(scp->scr_buf + scp->xsize, scp->scr_buf,
	       scp->xsize * (scp->ysize - 1) * sizeof(u_short));
	fillw(scp->term.cur_attr | scr_map[0x20],
	      scp->scr_buf + scp->xsize * (scp->ysize - 1),
	      scp->xsize);
	scp->cursor_pos -= scp->xsize;
	scp->ypos--;
    	mark_all(scp);
    }
    if (len)
	goto outloop;
    write_in_progress--;
    if (delayed_next_scr)
	switch_scr(scp, delayed_next_scr - 1);
}

static void
scinit(void)
{
    u_short volatile *cp = Crtat + (CGA_BUF-MONO_BUF)/sizeof(u_short), was;
    unsigned hw_cursor;
    int i;

    if (init_done)
	return;
    init_done = TRUE;
    /*
     * Crtat initialized to point to MONO buffer, if not present change
     * to CGA_BUF offset. ONLY add the difference since locore.s adds
     * in the remapped offset at the "right" time
     */
    was = *cp;
    *cp = (u_short) 0xA55A;
    if (*cp != 0xA55A)
	crtc_addr = MONO_BASE;
    else {
	*cp = was;
	crtc_addr = COLOR_BASE;
	Crtat = Crtat + (CGA_BUF-MONO_BUF)/sizeof(u_short);
    }

    /* extract cursor location */
    outb(crtc_addr, 14);
    hw_cursor = inb(crtc_addr + 1) << 8;
    outb(crtc_addr, 15);
    hw_cursor |= inb(crtc_addr + 1);

    /* move hardware cursor out of the way */
    outb(crtc_addr, 14);
    outb(crtc_addr + 1, 0xff);
    outb(crtc_addr, 15);
    outb(crtc_addr + 1, 0xff);

    /* is this a VGA or higher ? */
    outb(crtc_addr, 7);
    if (inb(crtc_addr) == 7) {
	u_long  pa;
	u_long  segoff;

	crtc_vga = TRUE;
	/*
	 * Get the BIOS video mode pointer.
	 */
	segoff = *(u_long *)pa_to_va(0x4a8);
	pa = (((segoff & 0xffff0000) >> 12) + (segoff & 0xffff));
	if (ISMAPPED(pa, sizeof(u_long))) {
	    segoff = *(u_long *)pa_to_va(pa);
	    pa = (((segoff & 0xffff0000) >> 12) + (segoff & 0xffff));
	    if (ISMAPPED(pa, 64))
		video_mode_ptr = (char *)pa_to_va(pa);
	}
    }
    current_default = &user_default;
    console[0] = &main_console;
    init_scp(console[0]);
    console[0]->scr_buf = console[0]->mouse_pos =  Crtat;
    console[0]->cursor_pos = Crtat + hw_cursor;
    console[0]->xpos = hw_cursor % COL;
    console[0]->ypos = hw_cursor / COL;
    cur_console = console[0];
    for (i=1; i<MAXCONS; i++)
	console[i] = NULL;
    kernel_console.esc = 0;
    kernel_console.std_attr = kernel_default.std_attr;
    kernel_console.rev_attr = kernel_default.rev_attr;
    kernel_console.cur_attr = kernel_default.std_attr;
    /* initialize mapscrn array to a one to one map */
    for (i=0; i<sizeof(scr_map); i++)
	scr_map[i] = i;
}

static scr_stat
*alloc_scp()
{
    scr_stat *scp;

    scp = (scr_stat *)malloc(sizeof(scr_stat), M_DEVBUF, M_WAITOK);
    init_scp(scp);
    scp->scr_buf = scp->cursor_pos = scp->scr_buf = scp->mouse_pos =
	(u_short *)malloc(scp->xsize*scp->ysize*sizeof(u_short),
			  M_DEVBUF, M_WAITOK);
    scp->history_head = scp->history_pos = scp->history =
	(u_short *)malloc(scp->history_size*sizeof(u_short),
			  M_DEVBUF, M_WAITOK);
    bzero(scp->history_head, scp->history_size*sizeof(u_short));
    if (crtc_vga && video_mode_ptr)
	set_mode(scp);
    clear_screen(scp);
    return scp;
}

static void
init_scp(scr_stat *scp)
{
    scp->mode = M_VGA_C80x25;
    scp->font = FONT_16;
    scp->xsize = COL;
    scp->ysize = ROW;
    scp->start = COL * ROW;
    scp->end = 0;
    scp->term.esc = 0;
    scp->term.std_attr = current_default->std_attr;
    scp->term.rev_attr = current_default->rev_attr;
    scp->term.cur_attr = scp->term.std_attr;
    scp->border = BG_BLACK;
    scp->cursor_start = *(char *)pa_to_va(0x461);
    scp->cursor_end = *(char *)pa_to_va(0x460);
    scp->mouse_xpos = scp->mouse_ypos = 0;
    scp->bell_pitch = BELL_PITCH;
    scp->bell_duration = BELL_DURATION;
    scp->status = (*(char *)pa_to_va(0x417) & 0x20) ? NLKED : 0;
    scp->status |= CURSOR_ENABLED;
    scp->pid = 0;
    scp->proc = NULL;
    scp->smode.mode = VT_AUTO;
    scp->history_head = scp->history_pos = scp->history = NULL;
    scp->history_size = HISTORY_SIZE;
}

static u_char
*get_fstr(u_int c, u_int *len)
{
    u_int i;

    if (!(c & FKEY))
	return(NULL);
    i = (c & 0xFF) - F_FN;
    if (i > n_fkey_tab)
	return(NULL);
    *len = fkey_tab[i].len;
    return(fkey_tab[i].str);
}

static void
update_leds(int which)
{
    int s;
    static u_char xlate_leds[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };

    /* replace CAPS led with ALTGR led for ALTGR keyboards */
    if (key_map.n_keys > ALTGR_OFFSET) {
	if (which & ALKED)
	    which |= CLKED;
	else
	    which &= ~CLKED;
    }
    s = spltty();
    kbd_cmd(KB_SETLEDS);
    kbd_cmd(xlate_leds[which & LED_MASK]);
    splx(s);
}

static void
history_to_screen(scr_stat *scp)
{
    int i;

    for (i=0; i<scp->ysize; i++)
	bcopyw(scp->history + (((scp->history_pos - scp->history) +
	       scp->history_size-((i+1)*scp->xsize))%scp->history_size),
	       scp->scr_buf + (scp->xsize * (scp->ysize-1 - i)),
	       scp->xsize * sizeof(u_short));
    mark_all(scp);
}

static int
history_up_line(scr_stat *scp)
{
    if (WRAPHIST(scp, scp->history_pos, -(scp->xsize*scp->ysize)) !=
	scp->history_head) {
	scp->history_pos = WRAPHIST(scp, scp->history_pos, -scp->xsize);
	history_to_screen(scp);
	return 0;
    }
    else
	return -1;
}

static int
history_down_line(scr_stat *scp)
{
    if (scp->history_pos != scp->history_head) {
	scp->history_pos = WRAPHIST(scp, scp->history_pos, scp->xsize);
	history_to_screen(scp);
	return 0;
    }
    else
	return -1;
}

/*
 * scgetc(noblock) - get character from keyboard.
 * If noblock = 0 wait until a key is pressed.
 * Else return NOKEY.
 */
u_int
scgetc(int noblock)
{
    u_char scancode, keycode;
    u_int state, action;
    struct key_t *key;
    static u_char esc_flag = 0, compose = 0;
    static u_int chr = 0;

next_code:
    kbd_wait();
    /* First see if there is something in the keyboard port */
    if (inb(KB_STAT) & KB_BUF_FULL)
	scancode = inb(KB_DATA);
    else if (noblock)
	return(NOKEY);
    else
	goto next_code;

    add_keyboard_randomness(scancode);

    if (cur_console->status & KBD_RAW_MODE)
	return scancode;
#if ASYNCH
    if (scancode == KB_ACK || scancode == KB_RESEND) {
	kbd_reply = scancode;
	if (noblock)
	    return(NOKEY);
	goto next_code;
    }
#endif
    keycode = scancode & 0x7F;
    switch (esc_flag) {
    case 0x00:      /* normal scancode */
	switch(scancode) {
	case 0xB8:  /* left alt  (compose key) */
	    if (compose) {
		compose = 0;
		if (chr > 255) {
		    do_bell(cur_console,
			BELL_PITCH, BELL_DURATION);
		    chr = 0;
		}
	    }
	    break;
	case 0x38:
	    if (!compose) {
		compose = 1;
		chr = 0;
	    }
	    break;
	case 0xE0:
	case 0xE1:
	    esc_flag = scancode;
	    goto next_code;
	}
	break;
    case 0xE0:      /* 0xE0 prefix */
	esc_flag = 0;
	switch (keycode) {
	case 0x1C:  /* right enter key */
	    keycode = 0x59;
	    break;
	case 0x1D:  /* right ctrl key */
	    keycode = 0x5A;
	    break;
	case 0x35:  /* keypad divide key */
	    keycode = 0x5B;
	    break;
	case 0x37:  /* print scrn key */
	    keycode = 0x5C;
	    break;
	case 0x38:  /* right alt key (alt gr) */
	    keycode = 0x5D;
	    break;
	case 0x47:  /* grey home key */
	    keycode = 0x5E;
	    break;
	case 0x48:  /* grey up arrow key */
	    keycode = 0x5F;
	    break;
	case 0x49:  /* grey page up key */
	    keycode = 0x60;
	    break;
	case 0x4B:  /* grey left arrow key */
	    keycode = 0x61;
	    break;
	case 0x4D:  /* grey right arrow key */
	    keycode = 0x62;
	    break;
	case 0x4F:  /* grey end key */
	    keycode = 0x63;
	    break;
	case 0x50:  /* grey down arrow key */
	    keycode = 0x64;
	    break;
	case 0x51:  /* grey page down key */
	    keycode = 0x65;
	    break;
	case 0x52:  /* grey insert key */
	    keycode = 0x66;
	    break;
	case 0x53:  /* grey delete key */
	    keycode = 0x67;
	    break;

	/* the following 3 are only used on the MS "Natural" keyboard */
	case 0x5b:  /* left Window key */
	    keycode = 0x69;
	    break;
	case 0x5c:  /* right Window key */
	    keycode = 0x6a;
	    break;
	case 0x5d:  /* menu key */
	    keycode = 0x6b;
	    break;
	default:    /* ignore everything else */
	    goto next_code;
	}
	break;
    case 0xE1:      /* 0xE1 prefix */
	esc_flag = 0;
	if (keycode == 0x1D)
	    esc_flag = 0x1D;
	goto next_code;
	/* NOT REACHED */
    case 0x1D:      /* pause / break */
	esc_flag = 0;
	if (keycode != 0x45)
	    goto next_code;
	keycode = 0x68;
	break;
    }

    /* if scroll-lock pressed allow history browsing */
    if (cur_console->history && cur_console->status & SLKED) {
	int i;

	cur_console->status &= ~CURSOR_ENABLED;
	if (!(cur_console->status & BUFFER_SAVED)) {
	    cur_console->status |= BUFFER_SAVED;
	    cur_console->history_save = cur_console->history_head;

	    /* copy screen into top of history buffer */
	    for (i=0; i<cur_console->ysize; i++) {
		bcopyw(cur_console->scr_buf + (cur_console->xsize * i),
		       cur_console->history_head,
		       cur_console->xsize * sizeof(u_short));
		cur_console->history_head += cur_console->xsize;
		if (cur_console->history_head + cur_console->xsize >
		    cur_console->history + cur_console->history_size)
		    cur_console->history_head=cur_console->history;
	    }
	    cur_console->history_pos = cur_console->history_head;
	    history_to_screen(cur_console);
	}
	switch (scancode) {
	case 0x47:  /* home key */
	    cur_console->history_pos = cur_console->history_head;
	    history_to_screen(cur_console);
	    goto next_code;

	case 0x4F:  /* end key */
	    cur_console->history_pos =
		WRAPHIST(cur_console, cur_console->history_head,
			 cur_console->xsize*cur_console->ysize);
	    history_to_screen(cur_console);
	    goto next_code;

	case 0x48:  /* up arrow key */
	    if (history_up_line(cur_console))
		do_bell(cur_console, BELL_PITCH, BELL_DURATION);
	    goto next_code;

	case 0x50:  /* down arrow key */
	    if (history_down_line(cur_console))
		do_bell(cur_console, BELL_PITCH, BELL_DURATION);
	    goto next_code;

	case 0x49:  /* page up key */
	    for (i=0; i<cur_console->ysize; i++)
	    if (history_up_line(cur_console)) {
		do_bell(cur_console, BELL_PITCH, BELL_DURATION);
		break;
	    }
	    goto next_code;

	case 0x51:  /* page down key */
	    for (i=0; i<cur_console->ysize; i++)
	    if (history_down_line(cur_console)) {
		do_bell(cur_console, BELL_PITCH, BELL_DURATION);
		break;
	    }
	    goto next_code;
	}
    }

    if (compose) {
	switch (scancode) {
	/* key pressed process it */
	case 0x47: case 0x48: case 0x49:    /* keypad 7,8,9 */
	    chr = (scancode - 0x40) + chr*10;
	    goto next_code;
	case 0x4B: case 0x4C: case 0x4D:    /* keypad 4,5,6 */
	    chr = (scancode - 0x47) + chr*10;
	    goto next_code;
	case 0x4F: case 0x50: case 0x51:    /* keypad 1,2,3 */
	    chr = (scancode - 0x4E) + chr*10;
	    goto next_code;
	case 0x52:              /* keypad 0 */
	    chr *= 10;
	    goto next_code;

	/* key release, no interest here */
	case 0xC7: case 0xC8: case 0xC9:    /* keypad 7,8,9 */
	case 0xCB: case 0xCC: case 0xCD:    /* keypad 4,5,6 */
	case 0xCF: case 0xD0: case 0xD1:    /* keypad 1,2,3 */
	case 0xD2:              /* keypad 0 */
	    goto next_code;

	case 0x38:              /* left alt key */
	    break;
	default:
	    if (chr) {
		compose = chr = 0;
		do_bell(cur_console, BELL_PITCH, BELL_DURATION);
		goto next_code;
	    }
	    break;
	}
    }

    state = (shfts ? 1 : 0 ) | (2 * (ctls ? 1 : 0)) | (4 * (alts ? 1 : 0));
    if ((!agrs && (cur_console->status & ALKED))
	|| (agrs && !(cur_console->status & ALKED)))
	keycode += ALTGR_OFFSET;
    key = &key_map.key[keycode];
    if ( ((key->flgs & FLAG_LOCK_C) && (cur_console->status & CLKED))
	 || ((key->flgs & FLAG_LOCK_N) && (cur_console->status & NLKED)) )
	state ^= 1;

    /* Check for make/break */
    action = key->map[state];
    if (scancode & 0x80) {      /* key released */
	if (key->spcl & 0x80) {
	    switch (action) {
	    case LSH:
		shfts &= ~1;
		break;
	    case RSH:
		shfts &= ~2;
		break;
	    case LCTR:
		ctls &= ~1;
		break;
	    case RCTR:
		ctls &= ~2;
		break;
	    case LALT:
		alts &= ~1;
		break;
	    case RALT:
		alts &= ~2;
		break;
	    case NLK:
		nlkcnt = 0;
		break;
	    case CLK:
		clkcnt = 0;
		break;
	    case SLK:
		slkcnt = 0;
		break;
	    case ASH:
		agrs = 0;
		break;
	    case ALK:
		alkcnt = 0;
		break;
	    case META:
		metas = 0;
		break;
	    }
	}
	if (chr && !compose) {
	    action = chr;
	    chr = 0;
	    return(action);
	}
    } else {
	/* key pressed */
	if (key->spcl & (0x80>>state)) {
	    switch (action) {
	    /* LOCKING KEYS */
	    case NLK:
		if (!nlkcnt) {
		    nlkcnt++;
		    if (cur_console->status & NLKED)
			cur_console->status &= ~NLKED;
		    else
			cur_console->status |= NLKED;
		    update_leds(cur_console->status);
		}
		break;
	    case CLK:
		if (!clkcnt) {
		    clkcnt++;
		    if (cur_console->status & CLKED)
			cur_console->status &= ~CLKED;
		    else
			cur_console->status |= CLKED;
		    update_leds(cur_console->status);
		}
		break;
	    case SLK:
		if (!slkcnt) {
		    slkcnt++;
		    if (cur_console->status & SLKED) {
			cur_console->status &= ~SLKED;
			if (cur_console->status & BUFFER_SAVED){
			    int i;
			    u_short *ptr = cur_console->history_save;

			    for (i=0; i<cur_console->ysize; i++) {
				bcopyw(ptr,
				       cur_console->scr_buf +
				       (cur_console->xsize*i),
				       cur_console->xsize * sizeof(u_short));
				ptr += cur_console->xsize;
				if (ptr + cur_console->xsize >
				    cur_console->history +
				    cur_console->history_size)
				    ptr = cur_console->history;
			    }
			    cur_console->status &= ~BUFFER_SAVED;
			    cur_console->history_head=cur_console->history_save;
			    cur_console->status |= CURSOR_ENABLED;
			    mark_all(cur_console);
			}
			scstart(VIRTUAL_TTY(get_scr_num()));
		    }
		    else
			cur_console->status |= SLKED;
		    update_leds(cur_console->status);
		}
		break;
	    case ALK:
		if (!alkcnt) {
		    alkcnt++;
		    if (cur_console->status & ALKED)
			cur_console->status &= ~ALKED;
		    else
			cur_console->status |= ALKED;
		    update_leds(cur_console->status);
		}
		break;

	    /* NON-LOCKING KEYS */
	    case NOP:
		break;
	    case RBT:
		shutdown_nice();
		break;
	    case SUSP:
#if NAPM > 0
		apm_suspend();
#endif
		break;

	    case DBG:
#ifdef DDB          /* try to switch to console 0 */
		if (cur_console->smode.mode == VT_AUTO &&
		    console[0]->smode.mode == VT_AUTO)
		    switch_scr(cur_console, 0);
		Debugger("manual escape to debugger");
		return(NOKEY);
#else
		printf("No debugger in kernel\n");
#endif
		break;
	    case LSH:
		shfts |= 1;
		break;
	    case RSH:
		shfts |= 2;
		break;
	    case LCTR:
		ctls |= 1;
		break;
	    case RCTR:
		ctls |= 2;
		break;
	    case LALT:
		alts |= 1;
		break;
	    case RALT:
		alts |= 2;
		break;
	    case ASH:
		agrs = 1;
		break;
	    case META:
		metas = 1;
		break;
	    case NEXT:
		switch_scr(cur_console, (get_scr_num() + 1) % MAXCONS);
		break;
	    case BTAB:
		return(BKEY);
	    default:
		if (action >= F_SCR && action <= L_SCR) {
		    switch_scr(cur_console, action - F_SCR);
		    break;
		}
		if (action >= F_FN && action <= L_FN)
		    action |= FKEY;
		return(action);
	    }
	}
	else {
	    if (metas)
		action |= MKEY;
	    return(action);
	}
    }
    goto next_code;
}

int
scmmap(dev_t dev, int offset, int nprot)
{
    if (offset > 0x20000 - PAGE_SIZE)
	return -1;
    return i386_btop((VIDEOMEM + offset));
}

static void
kbd_wait(void)
{
    int i = 1000;

    while (i--) {
	if ((inb(KB_STAT) & KB_READY) == 0)
	    break;
	DELAY (10);
    }
}

static void
kbd_cmd(u_char command)
{
    int retry = 5;
    do {
	int i = 100000;

	kbd_wait();
#if ASYNCH
	kbd_reply = 0;
	outb(KB_DATA, command);
	while (i--) {
	    if (kbd_reply == KB_ACK)
		return;
	    if (kbd_reply == KB_RESEND)
		break;
	}
#else
	outb(KB_DATA, command);
	while (i--) {
	    if (inb(KB_STAT) & KB_BUF_FULL) {
		int val;
		DELAY(10);
		val = inb(KB_DATA);
		if (val == KB_ACK)
		    return;
		if (val == KB_RESEND)
		    break;
	    }
	}
#endif
    } while (retry--);
}

static void
set_mode(scr_stat *scp)
{
    char *modetable;
    char special_modetable[64];
    int mode, font_size;

    if (scp != cur_console)
	return;

    /* setup video hardware for the given mode */
    switch (scp->mode) {
    case M_VGA_M80x60:
	bcopyw(video_mode_ptr+(64*M_VGA_M80x25),&special_modetable, 64);
	goto special_80x60;

    case M_VGA_C80x60:
	bcopyw(video_mode_ptr+(64*M_VGA_C80x25),&special_modetable, 64);
special_80x60:
	special_modetable[2]  = 0x08;
	special_modetable[19] = 0x47;
	goto special_480l;

    case M_VGA_M80x30:
	bcopyw(video_mode_ptr+(64*M_VGA_M80x25),&special_modetable, 64);
	goto special_80x30;

    case M_VGA_C80x30:
	bcopyw(video_mode_ptr+(64*M_VGA_C80x25),&special_modetable, 64);
special_80x30:
	special_modetable[19] = 0x4f;
special_480l:
	special_modetable[9] |= 0xc0;
	special_modetable[16] = 0x08;
	special_modetable[17] = 0x3e;
	special_modetable[26] = 0xea;
	special_modetable[28] = 0xdf;
	special_modetable[31] = 0xe7;
	special_modetable[32] = 0x04;
	modetable = special_modetable;
	goto setup_mode;

    case M_ENH_B80x43:
	bcopyw(video_mode_ptr+(64*M_ENH_B80x25),&special_modetable, 64);
	goto special_80x43;

    case M_ENH_C80x43:
	bcopyw(video_mode_ptr+(64*M_ENH_C80x25),&special_modetable, 64);
special_80x43:
	special_modetable[28] = 87;
	goto special_80x50;

    case M_VGA_M80x50:
	bcopyw(video_mode_ptr+(64*M_VGA_M80x25),&special_modetable, 64);
	goto special_80x50;

    case M_VGA_C80x50:
	bcopyw(video_mode_ptr+(64*M_VGA_C80x25),&special_modetable, 64);
special_80x50:
	special_modetable[2] = 8;
	special_modetable[19] = 7;
	modetable = special_modetable;
	goto setup_mode;

    case M_VGA_C40x25: case M_VGA_C80x25:
    case M_VGA_M80x25:
    case M_B40x25:     case M_C40x25:
    case M_B80x25:     case M_C80x25:
    case M_ENH_B40x25: case M_ENH_C40x25:
    case M_ENH_B80x25: case M_ENH_C80x25:

	modetable = video_mode_ptr + (scp->mode * 64);
setup_mode:
	set_vgaregs(modetable);
	font_size = *(modetable + 2);

	/* set font type (size) */
	switch (font_size) {
	case 0x10:
	    outb(TSIDX, 0x03); outb(TSREG, 0x00);   /* font 0 */
	    scp->font = FONT_16;
	    break;
	case 0x0E:
	    outb(TSIDX, 0x03); outb(TSREG, 0x05);   /* font 1 */
	    scp->font = FONT_14;
	    break;
	default:
	case 0x08:
	    outb(TSIDX, 0x03); outb(TSREG, 0x0A);   /* font 2 */
	    scp->font = FONT_8;
	    break;
	}
	if (configuration & CHAR_CURSOR)
	    set_destructive_cursor(scp, TRUE);
	break;

    case M_BG320:     case M_CG320:     case M_BG640:
    case M_CG320_D:   case M_CG640_E:
    case M_CG640x350: case M_ENH_CG640:
    case M_BG640x480: case M_CG640x480: case M_VGA_CG320:

	set_vgaregs(video_mode_ptr + (scp->mode * 64));
	break;

    default:
	/* call user defined function XXX */
	break;
    }

    /* set border color for this (virtual) console */
    set_border(scp->border);
    return;
}

void
set_border(int color)
{
    inb(crtc_addr+6);               /* reset flip-flop */
    outb(ATC, 0x11); outb(ATC, color);
    inb(crtc_addr+6);               /* reset flip-flop */
    outb(ATC, 0x20);                /* enable Palette */
}

static void
set_vgaregs(char *modetable)
{
    int i, s = splhigh();

    outb(TSIDX, 0x00); outb(TSREG, 0x01);   	/* stop sequencer */
    outb(TSIDX, 0x07); outb(TSREG, 0x00);   	/* unlock registers */
    for (i=0; i<4; i++) {           		/* program sequencer */
	outb(TSIDX, i+1);
	outb(TSREG, modetable[i+5]);
    }
    outb(MISC, modetable[9]);       		/* set dot-clock */
    outb(TSIDX, 0x00); outb(TSREG, 0x03);   	/* start sequencer */
    outb(crtc_addr, 0x11);
    outb(crtc_addr+1, inb(crtc_addr+1) & 0x7F);
    for (i=0; i<25; i++) {          		/* program crtc */
	outb(crtc_addr, i);
	if (i == 14 || i == 15)     		/* no hardware cursor */
	    outb(crtc_addr+1, 0xff);
	else
	    outb(crtc_addr+1, modetable[i+10]);
    }
    inb(crtc_addr+6);           		/* reset flip-flop */
    for (i=0; i<20; i++) {          		/* program attribute ctrl */
	outb(ATC, i);
	outb(ATC, modetable[i+35]);
    }
    for (i=0; i<9; i++) {           		/* program graph data ctrl */
	outb(GDCIDX, i);
	outb(GDCREG, modetable[i+55]);
    }
    inb(crtc_addr+6);           		/* reset flip-flop */
    outb(ATC ,0x20);            		/* enable palette */
    splx(s);
}

static void
set_font_mode()
{
    /* setup vga for loading fonts (graphics plane mode) */
    inb(crtc_addr+6);
    outb(ATC, 0x30); outb(ATC, 0x01);
#if SLOW_VGA
    outb(TSIDX, 0x02); outb(TSREG, 0x04);
    outb(TSIDX, 0x04); outb(TSREG, 0x06);
    outb(GDCIDX, 0x04); outb(GDCREG, 0x02);
    outb(GDCIDX, 0x05); outb(GDCREG, 0x00);
    outb(GDCIDX, 0x06); outb(GDCREG, 0x05);
#else
    outw(TSIDX, 0x0402);
    outw(TSIDX, 0x0604);
    outw(GDCIDX, 0x0204);
    outw(GDCIDX, 0x0005);
    outw(GDCIDX, 0x0506);               /* addr = a0000, 64kb */
#endif
}

static void
set_normal_mode()
{
    int s = splhigh();

    /* setup vga for normal operation mode again */
    inb(crtc_addr+6);
    outb(ATC, 0x30); outb(ATC, 0x0C);
#if SLOW_VGA
    outb(TSIDX, 0x02); outb(TSREG, 0x03);
    outb(TSIDX, 0x04); outb(TSREG, 0x02);
    outb(GDCIDX, 0x04); outb(GDCREG, 0x00);
    outb(GDCIDX, 0x05); outb(GDCREG, 0x10);
    if (crtc_addr == MONO_BASE) {
	outb(GDCIDX, 0x06); outb(GDCREG, 0x0A); /* addr = b0000, 32kb */
    }
    else {
	outb(GDCIDX, 0x06); outb(GDCREG, 0x0E); /* addr = b8000, 32kb */
    }
#else
    outw(TSIDX, 0x0302);
    outw(TSIDX, 0x0204);
    outw(GDCIDX, 0x0004);
    outw(GDCIDX, 0x1005);
    if (crtc_addr == MONO_BASE)
	outw(GDCIDX, 0x0A06);           /* addr = b0000, 32kb */
    else
	outw(GDCIDX, 0x0E06);           /* addr = b8000, 32kb */
#endif
    splx(s);
}

static void
copy_font(int operation, int font_type, char* font_image)
{
    int ch, line, segment, fontsize;
    u_char val;

    switch (font_type) {
    default:
    case FONT_8:
	segment = 0x8000;
	fontsize = 8;
	break;
    case FONT_14:
	segment = 0x4000;
	fontsize = 14;
	break;
    case FONT_16:
	segment = 0x0000;
	fontsize = 16;
	break;
    }
    outb(TSIDX, 0x01); val = inb(TSREG);        /* disable screen */
    outb(TSIDX, 0x01); outb(TSREG, val | 0x20);
    set_font_mode();
    for (ch=0; ch < 256; ch++)
	for (line=0; line < fontsize; line++)
	if (operation)
	    *(char *)pa_to_va(VIDEOMEM+(segment)+(ch*32)+line) =
		    font_image[(ch*fontsize)+line];
	else
	    font_image[(ch*fontsize)+line] =
	    *(char *)pa_to_va(VIDEOMEM+(segment)+(ch*32)+line);
    set_normal_mode();
    outb(TSIDX, 0x01); outb(TSREG, val & 0xDF); /* enable screen */
}

static void
set_destructive_cursor(scr_stat *scp, int force)
{
    u_char cursor[32];
    caddr_t address;
    int i, font_size;
    char *font_buffer;
    static u_char old_saveunder = DEAD_CHAR;

    if (!force && (scp->cursor_saveunder & 0xFF) == old_saveunder)
	return;
    old_saveunder = force ? DEAD_CHAR : scp->cursor_saveunder & 0xFF;
    switch (scp->font) {
    default:
    case FONT_8:
	font_size = 8;
	font_buffer = font_8;
	address = (caddr_t)VIDEOMEM + 0x8000;
	break;
    case FONT_14:
	font_size = 14;
	font_buffer = font_14;
	address = (caddr_t)VIDEOMEM + 0x4000;
	break;
    case FONT_16:
	font_size = 16;
	font_buffer = font_16;
	address = (caddr_t)VIDEOMEM;
	break;
    }
    bcopyw(font_buffer + ((scp->cursor_saveunder & 0xff) * font_size),
	   cursor, font_size);
    for (i=0; i<32; i++)
	if ((i >= scp->cursor_start && i <= scp->cursor_end) ||
	    (scp->cursor_start >= font_size && i == font_size - 1))
	    cursor[i] |= 0xff;
    while (!(inb(crtc_addr+6) & 0x08)) /* wait for vertical retrace */ ;
    set_font_mode();
    bcopy(cursor, (char *)pa_to_va(address) + DEAD_CHAR * 32, 32);
    set_normal_mode();
}

static void
draw_mouse_image(scr_stat *scp)
{
    caddr_t address;
    int i, font_size;
    char *font_buffer;
    u_short buffer[32];
    u_short xoffset, yoffset;
    u_short *crt_pos = Crtat + (scp->mouse_pos - scp->scr_buf);

    xoffset = scp->mouse_xpos % 8;
    switch (scp->font) {
    default:
    case FONT_8:
	font_size = 8;
	font_buffer = font_8;
	yoffset = scp->mouse_ypos % 8;
	address = (caddr_t)VIDEOMEM + 0x8000;
	break;
    case FONT_14:
	font_size = 14;
	font_buffer = font_14;
	yoffset = scp->mouse_ypos % 14;
	address = (caddr_t)VIDEOMEM + 0x4000;
	break;
    case FONT_16:
	font_size = 16;
	font_buffer = font_16;
	yoffset = scp->mouse_ypos % 16;
	address = (caddr_t)VIDEOMEM;
	break;
    }

    bcopyw(font_buffer + ((*(scp->mouse_pos) & 0xff) * font_size),
	   &scp->mouse_cursor[0], font_size);
    bcopyw(font_buffer + ((*(scp->mouse_pos+1) & 0xff) * font_size),
	   &scp->mouse_cursor[32], font_size);
    bcopyw(font_buffer + ((*(scp->mouse_pos+scp->xsize) & 0xff) * font_size),
	   &scp->mouse_cursor[64], font_size);
    bcopyw(font_buffer + ((*(scp->mouse_pos+scp->xsize+1) & 0xff) * font_size),
	   &scp->mouse_cursor[96], font_size);

    for (i=0; i<font_size; i++) {
	buffer[i] = scp->mouse_cursor[i]<<8 | scp->mouse_cursor[i+32];
	buffer[i+font_size]=scp->mouse_cursor[i+64]<<8|scp->mouse_cursor[i+96];
    }
    for (i=0; i<16; i++) {
	buffer[i+yoffset] =
	    ( buffer[i+yoffset] & ~(mouse_and_mask[i] >> xoffset))
	    | (mouse_or_mask[i] >> xoffset);
    }
    for (i=0; i<font_size; i++) {
	scp->mouse_cursor[i] = (buffer[i] & 0xff00) >> 8;
	scp->mouse_cursor[i+32] = buffer[i] & 0xff;
	scp->mouse_cursor[i+64] = (buffer[i+font_size] & 0xff00) >> 8;
	scp->mouse_cursor[i+96] = buffer[i+font_size] & 0xff;
    }
    if (scp->status & UPDATE_MOUSE) {
	u_short *ptr = scp->scr_buf + (scp->mouse_oldpos - Crtat);

	if (crt_pos != scp->mouse_oldpos) {
	    *(scp->mouse_oldpos) = scp->mouse_saveunder[0];
	    *(scp->mouse_oldpos+1) = scp->mouse_saveunder[1];
	    *(scp->mouse_oldpos+scp->xsize) = scp->mouse_saveunder[2];
	    *(scp->mouse_oldpos+scp->xsize+1) = scp->mouse_saveunder[3];
	}
	scp->mouse_saveunder[0] = *(scp->mouse_pos);
	scp->mouse_saveunder[1] = *(scp->mouse_pos+1);
	scp->mouse_saveunder[2] = *(scp->mouse_pos+scp->xsize);
	scp->mouse_saveunder[3] = *(scp->mouse_pos+scp->xsize+1);
	if ((scp->cursor_pos == (ptr)) ||
	    (scp->cursor_pos == (ptr+1)) ||
	    (scp->cursor_pos == (ptr+scp->xsize)) ||
	    (scp->cursor_pos == (ptr+scp->xsize+1)) ||
	    (scp->cursor_pos == (scp->mouse_pos)) ||
	    (scp->cursor_pos == (scp->mouse_pos+1)) ||
	    (scp->cursor_pos == (scp->mouse_pos+scp->xsize)) ||
	    (scp->cursor_pos == (scp->mouse_pos+scp->xsize+1)))
	    scp->status &= ~CURSOR_SHOWN;
    }
    scp->mouse_oldpos = crt_pos;
    while (!(inb(crtc_addr+6) & 0x08)) /* wait for vertical retrace */ ;
    *(crt_pos) = *(scp->mouse_pos)&0xff00|0xd0;
    *(crt_pos+1) = *(scp->mouse_pos+1)&0xff00|0xd1;
    *(crt_pos+scp->xsize) = *(scp->mouse_pos+scp->xsize)&0xff00|0xd2;
    *(crt_pos+scp->xsize+1) = *(scp->mouse_pos+scp->xsize+1)&0xff00|0xd3;
    set_font_mode();
    bcopy(scp->mouse_cursor, (char *)pa_to_va(address) + 0xd0 * 32, 128);
    set_normal_mode();
}

static void
save_palette(void)
{
    int i;

    outb(PALRADR, 0x00);
    for (i=0x00; i<0x300; i++)
	palette[i] = inb(PALDATA);
    inb(crtc_addr+6);           /* reset flip/flop */
}

void
load_palette(void)
{
    int i;

    outb(PIXMASK, 0xFF);            /* no pixelmask */
    outb(PALWADR, 0x00);
    for (i=0x00; i<0x300; i++)
	 outb(PALDATA, palette[i]);
    inb(crtc_addr+6);           /* reset flip/flop */
    outb(ATC, 0x20);            /* enable palette */
}

static void
do_bell(scr_stat *scp, int pitch, int duration)
{
    if (scp == cur_console) {
	if (configuration & VISUAL_BELL) {
	    if (blink_in_progress)
		return;
	    blink_in_progress = 4;
	    blink_screen(scp);
	    timeout((timeout_func_t)blink_screen, scp, hz/10);
	}
	else
	    sysbeep(pitch, duration);
    }
}

static void
blink_screen(scr_stat *scp)
{
    if (blink_in_progress > 1) {
	if (blink_in_progress & 1)
	    fillw(kernel_default.std_attr | scr_map[0x20],
		  Crtat, scp->xsize * scp->ysize);
	else
	    fillw(kernel_default.rev_attr | scr_map[0x20],
		  Crtat, scp->xsize * scp->ysize);
	blink_in_progress--;
	timeout((timeout_func_t)blink_screen, scp, hz/10);
    }
    else {
	blink_in_progress = FALSE;
    	mark_all(scp);
	if (delayed_next_scr)
	    switch_scr(scp, delayed_next_scr - 1);
    }
}

#endif /* NSC */

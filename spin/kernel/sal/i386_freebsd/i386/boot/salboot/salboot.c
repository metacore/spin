/*
  	salboot.c

	salboot is netboot/boot.c modified to be operate off the serial line.
	This is the main file of a DOS program.  When run, it puts a
	prompt on the COM1 serial line.  Users enter the kernel to be
	built, boot options.  This version of netboot uses bootp to
	fetch the server and local ip or they can be entered on the
	command line.

	created by David Becker Wed Jun 25 09:34:50 PDT 1997
 */

/**************************************************************************
MAIN - Kick off routine
**************************************************************************/
#include "netboot.h"
#include <netinet/in.h>
#include <net/if.h>
#include <sys/mbuf.h>

#include <sal/salnet.h>
#include <salnet/bootp.h>

extern int serial;

struct	exec head;
char	*loadpoint;
char	kernel[BP_FILE_LEN];
void	(*kernelentry)();
int	hostnamelen;
struct	bootinfo bootinfo;

ipaddr_t server_ip;
int netmask;
int flags = 0;


/**************************************************************************
LOAD - Try to get booted
**************************************************************************/
void
load(void)
{
	char	*p,*q;
	int	err, offset, read_size;
	long	addr, broadcast, len;
	int     string_size;
	char	*file = 0x400000;
	char mount[32];
	unsigned int	ticks;
	char *slash;

	/* Load the kernel using TFTP */
	/*
	printf("Loading %s...\n",kernel);
        saltftp_fetch(server_ip, kernel, 0x800000, file, &len);
	*/

	ticks = cyclecounter();

	slash = index(kernel+1,'/');
	*slash = '\0';
	printf("mount %I:%s\n",ntohl(server_ip), kernel);
	err = salnfs_mount(server_ip, kernel, mount);
	*slash = '/';

	if (!err) {
		printf("fetch %s\n",slash+1);
		err = salnfs_fetch(server_ip, mount, slash+1, 0x800000,
			(void*)file, &len);
		}
	ticks = cycleminus(cyclecounter(),ticks);

        if (err) {
	    if (err==SALNET_NFSERROR)
		salnfs_err(*(int*)file);
	    else
		salnet_perror("fetch",err);
	    return;
	}

	printf("Got %s.  %d bytes in %d secs %d Kbps\n\n", kernel, len,
		ticks/hertz(),len*8/(ticks/(hertz()/1000)));

	bcopy(file,&head,sizeof(struct exec));
	if (N_BADMAG(head)) {
		printf("Bad executable format!\n");
		return;
	}
	loadpoint = (char *)0x100000;
	offset = N_TXTOFF(head);
	printf("text=0x%X, data=0x%X, ",head.a_text, head.a_data);

	bcopy(file+offset,loadpoint, head.a_text + head.a_data); 

	loadpoint += head.a_text + head.a_data;
	offset += head.a_text + head.a_data;

	printf("bss=0x%X, ",head.a_bss);
	while(head.a_bss--) *(loadpoint++) = 0;

	if(flags & RB_KDB) {
	    /* if the debug flag is set, then load the symbol table */
	    printf("syms=0x%X, ",head.a_syms);

	    /* first pad to a page boundary */
#define PAGESIZE 4096
	    loadpoint = (char *) ((unsigned int)(loadpoint + PAGESIZE - 1) &
				  ~(PAGESIZE-1));
	    bootinfo.bi_symtab = (unsigned int) loadpoint;

	    /* save the size of the symbols */
	    * (int *) loadpoint = head.a_syms;
	    loadpoint += sizeof(head.a_syms);

	    /* load the rest of the binary here */
	    bcopy(file+offset,loadpoint, len-offset); 

	    string_size =  * (int *) (loadpoint + head.a_syms);
		
	    bootinfo.bi_esymtab = loadpoint + head.a_syms + string_size;

	    printf("strings=0x%X, ", string_size);

	} else {
	    bootinfo.bi_symtab = 0;
	    bootinfo.bi_esymtab = 0;
	}
	
	printf("entry=0x%X.\n",head.a_entry);
	
	/* Jump to kernel */
	bootinfo.bi_version = BOOTINFO_VERSION;
	bootinfo.bi_kernelname = kernel;
	bootinfo.bi_nfs_diskless = 0;
	bootinfo.bi_size = sizeof(bootinfo);

	kernelentry = (void *)(head.a_entry & 0x00FFFFFF);
	(*kernelentry)(flags|RB_BOOTINFO,NODEV,0,0,0,&bootinfo,0,0,0);
	printf("*** %s execute failure ***\n",kernel);
}

void
main(void)
{
	gateA20();  /* turn on memory address line 20 */

	{ /* initialize BSS */
		extern char edata[], end[];
		char *p;
		for (p=edata; p<end; p++) *p = 0;	/* Zero BSS */
		
	}

	{ /* initialize i/o so we can print to either console/serial */
		extern void ioinit(void);        /* i/o put/get char initialization */
		ioinit();
		if(serial) {
		    flags |= RB_SERIAL;
		}

	}

	printf("\n\nSALBOOT.COM\n");

	{ /* malloc initialization */
		extern void malloc_init(unsigned long, unsigned long);
		/* XXX the address and size of malloc memory should come from config */
		malloc_init(0x50000,64*1024);
	}

	{ /* mbuf support */
		extern void mbinit(void);
		/* extern char *mclrefcnt; */
		extern void bzero(char*,int);

		/* XXX the size of mclrefcnt should come from config */
		mclrefcnt = (char *)malloc(512,0,0); /* XXX 512 is wrong & temp. */
		bzero(mclrefcnt, 512); /* XXX 512 is wrong & temp. */
		mbinit();
	}
	{ /* bus configuration */
		/* shut up compiler */
		extern void isa_configure(void); /* configure isa devices */
		extern void pci_configure(void); /* configure pci devices */
		isa_configure();
		pci_configure();
	}

	cmd_bootp();

	do { /* main loop */
		extern void bootmenu(void);
		bootmenu();
	} while(1);
}

void eth_reset(void) {};

getdec(ptr)
	char **ptr;
{
	char *p = *ptr;
	int ret=0;
	if ((*p < '0') || (*p > '9')) return(-1);
	while ((*p >= '0') && (*p <= '9')) {
		ret = ret*10 + (*p - '0');
		p++;
	}
	*ptr = p;
	return(ret);
}
/**************************************************************************
SETIP - Convert an ascii x.x.x.x to binary form
**************************************************************************/
setip(p, i)
	char *p;
	unsigned *i;
{
	unsigned ip = 0;
	int val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = (ip << 8) | val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = (ip << 8) | val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	*i = (ip << 8) | val;
	return(1);
}

int serial;

extern void kbd_putchar(int c);
extern int kbd_getchar(void);
extern int kbd_iskey(void);

extern void serial_putc(int c);
extern int serial_getc(void);
extern int serial_ischar(void);

extern void (*fputchar)(int c);
extern int (*fgetchar)(void);
extern int (*fiskey)(void);

void ioinit(void)
{
#ifdef FORCE_COMCONSOLE
	serial = 1;
#else
	serial = probe_keyboard();
#endif

    if(serial) {
	init_serial();
	fputchar = serial_putc;
	fgetchar = serial_getc;
	fiskey   = serial_ischar;
    } else {
	fputchar = kbd_putchar;
	fgetchar = kbd_getchar;
	fiskey   = kbd_iskey;
    }
}

#define K_RDWR 		0x60		/* keyboard data & cmds (read/write) */
#define K_STATUS 	0x64		/* keyboard status */
#define K_CMD	 	0x64		/* keybd ctlr command (write-only) */

#define K_OBUF_FUL 	0x01		/* output buffer full */
#define K_IBUF_FUL 	0x02		/* input buffer full */

#define KC_CMD_WIN	0xd0		/* read  output port */
#define KC_CMD_WOUT	0xd1		/* write output port */
#define KB_A20		0x9f		/* enable A20,
					   enable output buffer full interrupt
					   enable data line
					   disable clock line */

/*
 * Gate A20 for high memory
 */
unsigned char	x_20 = KB_A20;
gateA20()
{
#ifdef	IBM_L40
	outb(0x92, 0x2);
#else	IBM_L40
	while (inb(K_STATUS) & K_IBUF_FUL);
	while (inb(K_STATUS) & K_OBUF_FUL)
		(void)inb(K_RDWR);

	outb(K_CMD, KC_CMD_WOUT);
	while (inb(K_STATUS) & K_IBUF_FUL);
	outb(K_RDWR, x_20);
	while (inb(K_STATUS) & K_IBUF_FUL);
#endif	IBM_L40
}

/*
 * This routine uses an inb to an unused port, the time to execute that
 * inb is approximately 1.25uS.  This value is pretty constant across
 * all CPU's and all buses, with the exception of some PCI implentations
 * that do not forward this I/O adress to the ISA bus as they know it
 * is not a valid ISA bus address, those machines execute this inb in
 * 60 nS :-(.
 *
 * XXX we need to use BIOS timer calls or something more reliable to
 * produce timeouts in the boot code.
 */
void
delay1ms(void)
{
        int i = 800;
        while (--i >= 0)
                (void)inb(0x84);
}

/**************************************************************************
TWIDDLE
**************************************************************************/
twiddle()
{
	static int count=0;
	char tiddles[]="-\\|/";
	fputchar(tiddles[(count++)&3]);
	fputchar('\b');
}



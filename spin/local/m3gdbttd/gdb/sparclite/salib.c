/* Stand-alone library for SPARClite */

#include "sparclite.h"

#ifdef SL931
#define SDTR_BASE 0x200
#define SDTR_ASI 1
#define SDTR_SHIFT 0
#else
#define SDTR_BASE 0x10000000
#define SDTR_ASI 4
#define SDTR_SHIFT 16
#endif

#define get_uart_status(PORT) \
  ({unsigned long status; \
      read_asi (SDTR_ASI, SDTR_BASE + 0x24 + PORT * 0x10, status); \
	status >> SDTR_SHIFT; })

#define xmt_char(PORT, C) write_asi (SDTR_ASI, SDTR_BASE + 0x20 + PORT * 0x10, C << SDTR_SHIFT)

#define rcv_char(PORT) \
  ({unsigned long c; \
      read_asi (SDTR_ASI, SDTR_BASE + 0x20 + PORT * 0x10, c); \
	c >> SDTR_SHIFT; })

#if 0
void
set_uart (cmd)
     int cmd;
{
  write_asi (SDTR_ASI, SDTR_BASE + 0x24, cmd << SDTR_SHIFT);
}

void
set_timer_3 (val)
     int val;
{
  write_asi (SDTR_ASI, SDTR_BASE + 0x78, val << SDTR_SHIFT);
}
#endif

/* This cache code is known to work on both the 930 & 932 processors.  It just
   cheats and clears the all of the address space that could contain tags, as
   opposed to striding the tags at 8 or 16 word intervals, or using the cache
   flush registers, which don't exist on all processors.  */

void
cache_off ()
{
  write_asi (1, 0, 0);
}

void
cache_on ()
{
  unsigned long addr;

  cache_off ();			/* Make sure the cache is off */

  /* Reset all of the cache line valid bits */

  for (addr = 0; addr < 0x1000; addr += 8)
    {
      write_asi (0xc, addr, 0);	/* Clear bank 1, icache */
      write_asi (0xc, addr + 0x80000000, 0); /* Clear bank 2, icache */

      write_asi (0xe, addr, 0);	/* Clear bank 1, dcache */
      write_asi (0xe, addr + 0x80000000, 0); /* Clear bank 2, dcache */
    }

  /* turn on the cache */

  write_asi (1, 0, 0x35);	/* Write buf ena, prefetch buf ena, data
				   & inst caches enab */
}

/* Flush the instruction cache.  We need to do this for the debugger stub so
   that breakpoints, et. al. become visible to the instruction stream after
   storing them in memory.
 */

void
flush_i_cache ()
{
  int cache_reg;
  unsigned long addr;

  read_asi (1, 0, cache_reg);	/* Read cache/bus interface reg */

  if (!(cache_reg & 1))
    return;			/* Just return if cache is already off */

  for (addr = 0; addr < 0x1000; addr += 8)
    {
      write_asi (0xc, addr, 0);	/* Clear bank 1, icache */
      write_asi (0xc, addr + 0x80000000, 0); /* Clear bank 2, icache */
    }
}

asm("
	.text
	.align 4

! Register window overflow handler.  Come here when save would move us
! into the invalid window.  This routine runs with traps disabled, and
! must be careful not to touch the condition codes, as PSR is never
! restored.
!
! We are called with %l0 = wim, %l1 = pc, %l2 = npc

	.globl win_ovf
win_ovf:
	mov	%g1, %l3		! Save g1, we use it to hold the wim
	srl	%l0, 1, %g1		! Rotate wim right
	sll	%l0, 8-1, %l0
	or	%l0, %g1, %g1

	save	%g0, %g0, %g0		! Slip into next window
	mov	%g1, %wim		! Install the new wim

	std	%l0, [%sp + 0 * 4]	! save L & I registers
	std	%l2, [%sp + 2 * 4]
	std	%l4, [%sp + 4 * 4]
	std	%l6, [%sp + 6 * 4]

	std	%i0, [%sp + 8 * 4]
	std	%i2, [%sp + 10 * 4]
	std	%i4, [%sp + 12 * 4]
	std	%i6, [%sp + 14 * 4]

	restore				! Go back to trap window.
	mov	%l3, %g1		! Restore %g1

	jmpl	%l1,  %g0
	rett	%l2

! Register window underflow handler.  Come here when restore would move us
! into the invalid window.  This routine runs with traps disabled, and
! must be careful not to touch the condition codes, as PSR is never
! restored.
!
! We are called with %l0 = wim, %l1 = pc, %l2 = npc

	.globl win_unf
win_unf:
	sll	%l0, 1, %l3		! Rotate wim left
	srl	%l0, 8-1, %l0
	or	%l0, %l3, %l0

	mov	%l0, %wim		! Install the new wim

	restore				! User's window
	restore				! His caller's window

	ldd	[%sp + 0 * 4], %l0	! restore L & I registers
	ldd	[%sp + 2 * 4], %l2
	ldd	[%sp + 4 * 4], %l4
	ldd	[%sp + 6 * 4], %l6

	ldd	[%sp + 8 * 4], %i0
	ldd	[%sp + 10 * 4], %i2
	ldd	[%sp + 12 * 4], %i4
	ldd	[%sp + 14 * 4], %i6

	save	%g0, %g0, %g0		! Back to trap window
	save	%g0, %g0, %g0

	jmpl	%l1,  %g0
	rett	%l2

! Read the TBR.

	.globl _rdtbr
_rdtbr:
	retl
	mov	%tbr, %o0

");

extern unsigned long rdtbr();

void
die(val)
     int val;
{
  static unsigned char *leds = (unsigned char *)0x02000003;

  *leds = val;

  while (1) ;
}

void
__main()
{
  int x;

#if 0
  set_uart_stuff(0x4e);
  rdtbr();
  rdtbr();
  rdtbr();
  rdtbr();
  rdtbr();
  set_uart_stuff(0x35);
#endif
};

/* Each entry in the trap vector occupies four words. */

struct trap_entry
{
  unsigned sethi_filler:10;
  unsigned sethi_imm22:22;
  unsigned jmpl_filler:19;
  unsigned jmpl_simm13:13;
  unsigned long filler[2];
};

extern struct trap_entry fltr_proto;
asm ("
	.data
	.globl _fltr_proto
	.align 4
_fltr_proto:			! First level trap routine prototype
	sethi 0, %l0
	jmpl 0+%l0, %g0
	nop
	nop

	.text
	.align 4
");

void
exceptionHandler(tt, routine)
     int tt;
     unsigned long routine;
{
  struct trap_entry *tb;	/* Trap vector base address */

  tb = (struct trap_entry *)(rdtbr() & ~0xfff);

  tb[tt] = fltr_proto;

  tb[tt].sethi_imm22 = routine >> 10;
  tb[tt].jmpl_simm13 = routine & 0x3ff;
}

void
update_leds()
{
  static unsigned char *leds = (unsigned char *)0x02000003;
  static unsigned char curled = 1;
  static unsigned char dir = 0;

  *leds = ~curled;

  if (dir)
    curled <<= 1;
  else
    curled >>= 1;

  if (curled == 0)
    {
      if (dir)
	curled = 0x80;
      else
	curled = 1;
      dir = ~dir;
    }
}

 /* 1/5th of a second? */

#define LEDTIME (20000000 / 500)

int
getDebugChar()
{
  unsigned long countdown = LEDTIME;

  update_leds();

  while (1)
    {
      if ((get_uart_status(0) & 2) != 0) break;

      if (countdown-- == 0)
	{
	  countdown = LEDTIME;
	  update_leds();
	}
    }

  return rcv_char(0);
}

/* Output one character to the serial port */

void
putDebugChar(c)
     int c;
{
  update_leds();

  while ((get_uart_status(0) & 1) == 0) ;

  xmt_char(0, c);
}

int
write(fd, data, length)
     int fd;
     unsigned char *data;
     int length;
{
  int olength = length;

  while (length--)
    putDebugChar(*data++);

  return olength;
}

int
read(fd, data, length)
     int fd;
     unsigned char *data;
     int length;
{
  int olength = length;
  int c;

  while (length--)
    *data++ = getDebugChar();

  return olength;
}

/* Set the baud rate for the serial port, returns 0 for success,
   -1 otherwise */

#if 0
int
set_baud_rate(baudrate)
     int baudrate;
{
  /* Convert baud rate to uart clock divider */
  switch (baudrate)
    {
    case 38400:
      baudrate = 16;
      break;
    case 19200:
      baudrate = 33;
      break;
    case 9600:
      baudrate = 65;
      break;
    default:
      return -1;
    }

  set_timer_3(baudrate);	/* Set it */
}
#endif

/**************************************************************************
MISC Support Routines
**************************************************************************/

#include <sys/types.h>
#include <sys/param.h>

#define NO_SWITCH		/* saves space */
/* I/O initialization */
static void putchar_internal(int c){};
static int getchar_internal(void){return 0;};
static int iskey_internal(void){return 0;};
void (*fputchar)(int c) = putchar_internal;
int (*fgetchar)(void) = getchar_internal;
int (*fiskey)(void) = iskey_internal;

/**************************************************************************
PRINTF and friends

	Formats:
		%X	- 4 byte ASCII (8 hex digits)
		%x	- 2 byte ASCII (4 hex digits)
		%b	- 1 byte ASCII (2 hex digits)
		%d	- decimal
		%c	- ASCII char
		%s	- ASCII string
		%I	- Internet address in x.x.x.x notation
		%L	- Binary long
		%S	- String (multiple of 4 bytes) preceded with 4 byte
			  binary length
		%M	- Copy memory.  Takes two args, len and ptr
**************************************************************************/
static char hex[]="0123456789ABCDEF";
char *do_printf(buf, fmt, dp)
	char *buf, *fmt;
	int  *dp;
{
	register char *p;
	char tmp[16];
	while (*fmt) {
		if (*fmt == '%') {	/* switch() uses more space */
			do{
				fmt++;
			} while((*fmt>='0') && (*fmt<='9')); /* ignore */

			if (*fmt == 'l') {
				char tmpfmt = *fmt;
				fmt++;
				if(*fmt == 'x')
					*fmt = 'X';
				else
					*fmt = tmpfmt;
				
			}

			if (*fmt == 'L') {
				register int h = *(dp++);
				*(buf++) = h>>24;
				*(buf++) = h>>16;
				*(buf++) = h>>8;
				*(buf++) = h;
			}
			if (*fmt == 'S') {
				register int len = 0;
				char *lenptr = buf;
				p = (char *)*dp++;
				buf += 4;
				while (*p) {
					*(buf++) = *p++;
					len ++;
				}
				*(lenptr++) = len>>24;
				*(lenptr++) = len>>16;
				*(lenptr++) = len>>8;
				*lenptr = len;
				while (len & 3) {
					*(buf++) = 0;
					len ++;
				}
			}
			if (*fmt == 'M') {
				register int len = *(dp++);
				bcopy((char *)*dp++, buf, len);
				buf += len;
			}
			if ((*fmt == 'X')||(*fmt == 'x')) {
				register int h = *(dp++);
				*(buf++) = hex[(h>>28)& 0x0F];
				*(buf++) = hex[(h>>24)& 0x0F];
				*(buf++) = hex[(h>>20)& 0x0F];
				*(buf++) = hex[(h>>16)& 0x0F];
				*(buf++) = hex[(h>>12)& 0x0F];
				*(buf++) = hex[(h>>8)& 0x0F];
				*(buf++) = hex[(h>>4)& 0x0F];
				*(buf++) = hex[h& 0x0F];
			}
			if (*fmt == 'b') {
				register int h = *(dp++);
				*(buf++) = hex[(h>>4)& 0x0F];
				*(buf++) = hex[h& 0x0F];
			}
			if (*fmt == 'd') {
				register int dec = *(dp++);
				p = tmp;
				if (dec < 0) {
					*(buf++) = '-';
					dec = -dec;
				}
				do {
					*(p++) = '0' + (dec%10);
					dec = dec/10;
				} while(dec);
				while ((--p) >= tmp) *(buf++) = *p;
			}
			if (*fmt == 'I') {
				buf = sprintf(buf,"%d.%d.%d.%d",
					(*(dp)>>24) & 0x00FF,
					(*(dp)>>16) & 0x00FF,
					(*(dp)>>8) & 0x00FF,
					*dp & 0x00FF);
				dp++;
			}
			if (*fmt == 'c')
				*(buf++) = *(dp++);
			if (*fmt == 's') {
				p = (char *)*dp++;
				while (*p) *(buf++) = *p++;
			}
		} else if (*fmt == '\n') { /* hard coded cooking OCRNL */
			*(buf++) = *fmt;
			*(buf++) = '\r';
		} else if /*((*fmt == '\n')||*/(*fmt == '\t')/*)*/ {
			/* XXX TEMP HACK */
			*(buf++) = '|';
		} else {
			*(buf++) = *fmt;
		}
		fmt++;
	}
	*buf = 0;
	return(buf);
}

char *sprintf(char *buf, char *fmt, int data)
{
	return(do_printf(buf,fmt, &data));
}

#define	PRINTF_BUFMAX	128
printf(char *fmt, int data)
{
  char buf[PRINTF_BUFMAX],*p;
  p = buf;
  do_printf(buf,fmt,&data);
  while (*p) fputchar(*p++);
}

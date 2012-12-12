/*
 * EISA bus device definitions
 *
 * Copyright (c) 1995 Justin T. Gibbs.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice immediately at the beginning of the file, without modification,
 *    this list of conditions, and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Absolutely no warranty of function or purpose is made by the author
 *    Justin T. Gibbs.
 * 4. Modifications may be freely made to this file if the above conditions
 *    are met.
 *
 * $Id: eisaconf.h,v 1.2 1997/01/31 15:59:32 mef Exp $
 */

#ifndef _I386_EISA_EISACONF_H_
#define _I386_EISA_EISACONF_H_ 1

#include <sys/queue.h>

#define EISA_SLOTS 10   /* PCI clashes with higher ones.. fix later */
#define EISA_SLOT_SIZE 0x1000

#define EISA_MFCTR_CHAR0(ID) (char)(((ID>>26) & 0x1F) | '@')  /* Bits 26-30 */
#define EISA_MFCTR_CHAR1(ID) (char)(((ID>>21) & 0x1F) | '@')  /* Bits 21-25 */
#define EISA_MFCTR_CHAR2(ID) (char)(((ID>>16) & 0x1F) | '@')  /* Bits 16-20 */
#define EISA_MFCTR_ID(ID)    (short)((ID>>16) & 0xFF)	      /* Bits 16-31 */
#define EISA_PRODUCT_ID(ID)  (short)((ID>>4)  & 0xFFF)        /* Bits  4-15 */
#define EISA_REVISION_ID(ID) (u_char)(ID & 0x0F)              /* Bits  0-3  */

extern struct linker_set eisadriver_set;

typedef u_int32_t eisa_id_t;

typedef struct resvaddr {
        u_long	addr;				/* start address */
        u_long	size;				/* size of reserved area */
	int	flags;
#define		RESVADDR_NONE		0x00
#define		RESVADDR_BITMASK	0x01	/* size is a mask of reserved 
						 * bits at addr
						 */
#define		RESVADDR_RELOCATABLE	0x02
	LIST_ENTRY(resvaddr) links;		/* List links */
} resvaddr_t;

LIST_HEAD(resvlist, resvaddr);

struct eisa_ioconf {
	int		slot;
	struct resvlist	ioaddrs;	/* list of reserved I/O ranges */
	struct resvlist maddrs;		/* list of reserved memory ranges */
	u_short		irq;		/* bitmask of interrupt */
};

struct kern_devconf;
struct eisa_device;

struct eisa_driver {
	char*	name;			/* device name */
	int	(*probe) __P((void));
					/* test whether device is present */
	int	(*attach) __P((struct eisa_device *));
					/* setup driver for a device */
	int	(*shutdown) __P((struct kern_devconf *, int));
					/* Return the device to a safe
					 * state before shutdown
					 */
	u_long  *unit;			/* Next available unit */
};

/* To be replaced by the "super device" generic device structure... */
struct eisa_device {
	eisa_id_t		id;
	u_long			unit;
	char*			full_name; /* for use in the probe message */
	struct eisa_ioconf	ioconf;
	struct eisa_driver*	driver;
	struct kern_devconf*	kdc;
};

void eisa_configure __P((void));
struct eisa_device *eisa_match_dev __P((struct eisa_device *, char * (*)(eisa_id_t)));

void eisa_reg_start __P((struct eisa_device *));
void eisa_reg_end __P((struct eisa_device *));
int eisa_add_intr __P((struct eisa_device *, int));
int eisa_reg_intr __P((struct eisa_device *, int, void (*)(void *), void *, u_int *, int));
int eisa_release_intr __P((struct eisa_device *, int, void (*)(void *)));
int eisa_enable_intr __P((struct eisa_device *, int));
int eisa_add_iospace __P((struct eisa_device *, u_long, u_long, int));
int eisa_reg_iospace __P((struct eisa_device *, resvaddr_t *));
int eisa_add_mspace __P((struct eisa_device *, u_long, u_long, int));
int eisa_reg_mspace __P((struct eisa_device *, resvaddr_t *));
int eisa_registerdev __P((struct eisa_device *, struct eisa_driver *, struct kern_devconf *));


struct sysctl_req;

extern int eisa_generic_externalize __P(( struct proc *p,
					  struct kern_devconf *kdc,
					  void *userp,
					  size_t l));
extern struct kern_devconf kdc_eisa0;

#define EISA_EXTERNALLEN (sizeof(struct eisa_device))

#endif /* _I386_EISA_EISACONF_H_ */

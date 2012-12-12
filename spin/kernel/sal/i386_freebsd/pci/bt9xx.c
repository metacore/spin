/*
 * Product specific probe and attach routines for:
 *      Buslogic BT946 and BT956 SCSI controllers
 *
 * Copyright (c) 1995 Justin T. Gibbs
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
 *	$Id: bt9xx.c,v 1.1 1997/08/29 00:29:33 becker Exp $
 */

#include <pci.h>
#if NPCI > 0
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/malloc.h>
#include <sys/kernel.h>
#include <scsi/scsi_all.h>
#include <scsi/scsiconf.h>
#include <pci/pcireg.h>
#include <pci/pcivar.h>
#include <i386/scsi/btreg.h>

/* XXX Need more device IDs */
#define PCI_DEVICE_ID_BUSLOGIC_946	0x1040104Bul
#define PCI_DEVICE_ID_BUSLOGIC_946_OLD	0x0140104Bul

static char* bt_pci_probe __P((pcici_t tag, pcidi_t type));
static void bt_pci_attach __P((pcici_t config_id, int unit));
static int bt_pci_intr __P((void *arg));

static struct  pci_device bt_pci_driver = {
	"bt",
        bt_pci_probe,
        bt_pci_attach,
        &bt_unit,
	NULL
};

DATA_SET (pcidevice_set, bt_pci_driver);

static  char*
bt_pci_probe (pcici_t tag, pcidi_t type)
{
	switch(type) {
		case PCI_DEVICE_ID_BUSLOGIC_946_OLD:
		case PCI_DEVICE_ID_BUSLOGIC_946:
			return ("Buslogic 946 SCSI host adapter");
			break;
		default:
			break;
	}
	return (0);

}

static void
bt_pci_attach(config_id, unit)
	pcici_t config_id;
	int	unit;
{
	u_char reg;
	u_long io_port;
	unsigned opri = 0;
	struct bt_data *bt;

	for(reg = PCI_MAP_REG_START; reg < PCI_MAP_REG_END; reg+=4) {
	        io_port = pci_conf_read(config_id, reg);
		if ((io_port&~7)==0) continue;
		if(io_port & PCI_MAP_IO) {
			io_port &= ~PCI_MAP_IO;
			break;
		}
	}
	if(reg == PCI_MAP_REG_END)
		return;

	if(!(bt = bt_alloc(unit, io_port)))
		return;  /* XXX PCI code should take return status */

	if(!(pci_map_int(config_id, bt_pci_intr, (void *)bt, &bio_imask))) {
		bt_free(bt);
		return;
	}
	/*
	 * Protect ourself from spurrious interrupts during
	 * intialization and attach.  We should really rely
	 * on interrupts during attach, but we don't have
	 * access to our interrupts during ISA probes, so until
	 * that changes, we mask our interrupts during attach
	 * too.
	 */
	opri = splbio();

	if(bt_init(bt)){
		bt_free(bt);
		splx(opri);
		return; /* XXX PCI code should take return status */
	}

	bt_attach(bt);

	splx(opri);
	return;
}

/*             
 * Handle an PCI interrupt.
 * XXX should go away as soon as PCI interrupt handlers
 * return void.
 */
static int  
bt_pci_intr(arg)
	void *arg;
{     
	bt_intr(arg);
	return (1);  /* 
		      * XXX: Always say we handle the interrupt.
		      * won't work with edge-triggered ints.
		      */
}

#endif /* NPCI > 0 */

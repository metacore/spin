/*
 * Copyright (C) 1996 Naoki Hamada <nao@tom-yam.or.jp>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the author nor the names of any co-contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include "pci.h"
#if NPCI > 0

#include "vx.h"
#if NVX > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/devconf.h>
#include <sys/kernel.h>

#include <net/if.h>

#ifdef INET
#include <netinet/in.h>
#include <netinet/if_ether.h>
#endif

#ifdef NS
#include <netns/ns.h>
#include <netns/ns_if.h>
#endif

#include <machine/clock.h>

#include <pci/pcivar.h>

#include <dev/vx/if_vxreg.h>

static int vx_pci_shutdown(struct kern_devconf *, int);
static char *vx_pci_probe(pcici_t, pcidi_t);
static void vx_pci_attach(pcici_t, int);

static int
vx_pci_shutdown(
	struct kern_devconf * const kdc,
	int force)
{
   struct vx_softc *sc = vx_softc[kdc->kdc_unit];

   vxstop(sc); 
   (void) dev_detach(kdc);
   vxfree(sc);
   return 0;
}

static char*
vx_pci_probe(
	pcici_t config_id,
	pcidi_t device_id)
{
   if(device_id == 0x590010b7ul)
      return "3COM 3C590 Etherlink III PCI";
   if(device_id == 0x595010b7ul || device_id == 0x595110b7ul ||
        device_id == 0x595210b7ul)
      return "3COM 3C595 Fast Etherlink III PCI";
   if(device_id == 0x900010b7ul || device_id == 0x900110b7ul)
      return "3COM 3C900 Etherlink XL PCI";    
   if(device_id == 0x905010b7ul || device_id == 0x905110b7ul)  
      return "3COM 3C595 Fast Etherlink XL PCI"; 
   return NULL;
}

static void
vx_pci_attach(
	pcici_t config_id,
	int unit)
{
    struct vx_softc *sc;

    if (unit >= NVX) {
       printf("vx%d: not configured; kernel is built for only %d device%s.\n",
          unit, NVX, NVX == 1 ? "" : "s"); 
       return;
    }

    sc = vxalloc(unit);

    sc->vx_io_addr = pci_conf_read(config_id, 0x10) & 0xffffffe0;

    if (vxattach(sc) == 0) {
	return;
    }

    /* defect check for 3C590 */
    if ((pci_conf_read(config_id, 0) >> 16) == 0x5900) {
	GO_WINDOW(0);
	if (vxbusyeeprom(sc))
	    return;
	outw(BASE + VX_W0_EEPROM_COMMAND, EEPROM_CMD_RD | EEPROM_SOFT_INFO_2);
	if (vxbusyeeprom(sc))
	    return;
	if (!(inw(BASE + VX_W0_EEPROM_DATA) & NO_RX_OVN_ANOMALY)) {
	    printf("Warning! Defective early revision adapter!\n");
	}
    }

    pci_map_int(config_id, (void *) vxintr, (void *) sc, &net_imask);
}

static struct pci_device vxdevice = {
    "vx",
    vx_pci_probe,
    vx_pci_attach,
    &vx_count,
    vx_pci_shutdown,
};

DATA_SET (pcidevice_set, vxdevice);

#endif	/* NVX */
#endif	/* NPCI */

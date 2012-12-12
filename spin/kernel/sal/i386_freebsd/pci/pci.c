/**************************************************************************
**
**  $Id: pci.c,v 1.3 1997/06/25 18:26:37 becker Exp $
**
**  General subroutines for the PCI bus.
**  pci_configure ()
**
**  FreeBSD
**
**-------------------------------------------------------------------------
**
** Copyright (c) 1994 Wolfgang Stanglmeier.  All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
** 3. The name of the author may not be used to endorse or promote products
**    derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
** IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
** OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
** IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
** INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
** NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
** THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
***************************************************************************
*/

#include "pci.h"
#if NPCI > 0

/*========================================================
**
**	#includes  and  declarations
**
**========================================================
*/

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/malloc.h>
#include <sys/errno.h>
#include <sys/kernel.h>
#include <sys/proc.h> /* declaration of wakeup(), used by vm.h */
#include <sys/devconf.h>

#include <machine/cpu.h> /* bootverbose */

#include <vm/vm.h>
#include <vm/vm_param.h>

#include <machine/pmap.h>
#include <sys/devconf.h>

#include <pci/pcivar.h>
#include <pci/pcireg.h>
#include <pci/pcibus.h>

#define PCI_MAX_IRQ	(16)


/*========================================================
**
**	Structs and Functions
**
**========================================================
*/

struct pci_devconf {
	struct kern_devconf pdc_kdc;
	struct pci_info     pdc_pi;
};

struct pcicb {
	struct pcicb   *pcicb_next;
	struct pcicb   *pcicb_up;
	struct pcicb   *pcicb_down;
	pcici_t 	pcicb_bridge;

	u_long		pcicb_seen;
	u_char		pcicb_bus;
	u_char		pcicb_subordinate;
	u_char		pcicb_flags;
#define  PCICB_ISAMEM	0x01
	u_int		pcicb_mfrom;
	u_int		pcicb_mupto;
	u_int		pcicb_mamount;
	u_short		pcicb_pfrom;
	u_short		pcicb_pupto;
	u_short		pcicb_pamount;
	u_char		pcicb_bfrom;
	u_char		pcicb_bupto;

	u_long		pcicb_iobase;
	u_long		pcicb_iolimit;
	u_long		pcicb_membase;
	u_long		pcicb_memlimit;
	u_long		pcicb_p_membase;
	u_long		pcicb_p_memlimit;
};

static int
pci_externalize (struct proc *, struct kern_devconf *, void *, size_t);

static int
pci_internalize (struct proc *, struct kern_devconf *, void *, size_t);

static void
not_supported (pcici_t tag, u_long type);

static void
pci_bus_config (void);

static int
pci_bridge_config (void);

static int
pci_mfdev (int bus, int device);

/*========================================================
**
**	Variables
**
**========================================================
*/

/*
**      log2 of safe burst len (in words)
*/

unsigned pci_max_burst_len = 2; /* 2=16Byte, 3=32Byte, 4=64Byte, ... */
unsigned pci_mechanism     = 0;
unsigned pci_maxdevice     = 0;
struct pcibus* pcibus;

/*--------------------------------------------------------
**
**	Local variables.
**
**--------------------------------------------------------
*/

static	int		pci_conf_count;
static	int		pci_info_done;
static	struct pcicb	pcibus0 = {
    NULL, NULL, NULL,
    { 0 },
    0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,	/* real allocation */
    0, 0xFFFF,			/* iobase/limit */
    0x2000000, 0xFFFFFFFFu,	/* nonprefetch membase/limit */
    0x2000000, 0xFFFFFFFFu	/* prefetch membase/limit */
};
static	struct pcicb   *pcicb;

/*-----------------------------------------------------------------
**
**	The following functions are provided for the device driver
**	to read/write the configuration space.
**
**	pci_conf_read():
**		Read a long word from the pci configuration space.
**		Requires a tag (from pcitag) and the register
**		number (should be a long word alligned one).
**
**	pci_conf_write():
**		Writes a long word to the pci configuration space.
**		Requires a tag (from pcitag), the register number
**		(should be a long word alligned one), and a value.
**
**-----------------------------------------------------------------
*/

u_long
pci_conf_read  (pcici_t tag, u_long reg)
{
    return (pcibus->pb_read (tag, reg));
}

void
pci_conf_write (pcici_t tag, u_long reg, u_long data)
{
    pcibus->pb_write (tag, reg, data);
}

/*========================================================
**
**	Subroutines for configuration.
**
**========================================================
*/

static void
pci_register_io (struct pcicb * cb, u_int base, u_int limit)
{
#ifdef PCI_BRIDGE_DEBUG
	if (bootverbose)
		printf ("register_io:  bus=%d base=%x limit=%x\n",
			cb->pcicb_bus, base, limit);
#endif

	if (!cb->pcicb_pfrom || base < cb->pcicb_pfrom)
		cb->pcicb_pfrom = base;
	if (limit > cb->pcicb_pupto)
		cb->pcicb_pupto = limit;

	/*
	**	XXX should set bridge io mapping here
	**	but it can be mapped in 4k blocks only,
	**	leading to conflicts with isa/eisa ..
	*/
}

static void
pci_register_memory (struct pcicb * cb, u_int base, u_int limit)
{
#ifdef PCI_BRIDGE_DEBUG
	if (bootverbose)
		printf ("register_mem: bus=%d base=%x limit=%x\n",
			cb->pcicb_bus, base, limit);
#endif

	if (!cb->pcicb_mfrom || base < cb->pcicb_mfrom)
		cb->pcicb_mfrom = base;
	if (limit > cb->pcicb_mupto)
		cb->pcicb_mupto = limit;
	/*
	**	set the bridges mapping
	**
	**	XXX should handle the 1Mb granularity.
	*/
	if (cb->pcicb_bridge.tag) {
		pci_conf_write(cb->pcicb_bridge,
			PCI_PCI_BRIDGE_MEM_REG,
			(cb->pcicb_memlimit & 0xffff0000) |
			(cb->pcicb_membase >> 16));
		if (bootverbose)
			printf ("\t[pci%d uses memory from %x to %x]\n",
				cb->pcicb_bus,
				(unsigned) cb->pcicb_membase,
				(unsigned) cb->pcicb_memlimit);
	}
}

/*
**	XXX This function is neither complete nor tested.
**	It's only used if the bios hasn't done it's job
**	of mapping the pci devices in the physical memory.
*/

static u_int
pci_memalloc (struct pcicb * cb, u_int addr, u_int size)
{
	u_int result = 0, limit=0, newbase=0;
#ifdef PCI_BRIDGE_DEBUG
	if (bootverbose)
		printf ("memalloc:  bus=%d addr=%x size=%x ..\n",
			cb->pcicb_bus, addr, size);
#endif

	if (!cb) goto done;

	if (!cb->pcicb_membase) {
		printf ("memalloc: bus%d: membase not set.\n",
			cb->pcicb_bus);
		goto done;
	}

	/*
	**	get upper allocation limit
	*/
	limit = cb->pcicb_memlimit;
	if (cb->pcicb_mfrom && cb->pcicb_mfrom <= limit)
		limit  = cb->pcicb_mfrom-1;

	/*
	**	address fixed, and impossible to allocate ?
	*/
	if (addr && addr+size-1 > limit)
		goto done;

	/*
	**	get possible address
	*/

	result = addr;
	if (!result) result = ((limit + 1) / size - 1) * size;

	/*
	**	if not local available, request from parent.
	*/

	if (result < cb->pcicb_membase) {
		newbase = pci_memalloc (cb->pcicb_up, result, size);
		if (newbase) cb->pcicb_membase = result;
		else result=0;
	}
done:
	if (result)
		pci_register_memory (cb, result, result+size-1);

#ifdef PCI_BRIDGE_DEBUG
	printf ("memalloc:  bus=%d addr=%x size=%x --> %x (limit=%x).\n",
		cb->pcicb_bus, addr, size, result, limit);
#endif

	return (result);
}

/*========================================================
**
**	pci_bridge_config()
**
**	Configuration of a pci bridge.
**
**========================================================
*/

static int
pci_bridge_config (void)
{
	pcici_t tag;
	struct pcicb* parent;

	tag = pcicb->pcicb_bridge;
	if (tag.tag) {

	    if (!pcicb->pcicb_bus) {
		u_int data;
		/*
		**	Get the lowest available bus number.
		*/
		pcicb->pcicb_bus = ++pcibus0.pcicb_subordinate;

		/*
		**	and configure the bridge
		*/
		data = pci_conf_read (tag, PCI_PCI_BRIDGE_BUS_REG);
		data = PCI_PRIMARY_BUS_INSERT(data, pcicb->pcicb_up->pcicb_bus);
		data = PCI_SECONDARY_BUS_INSERT(data, pcicb->pcicb_bus);
		data = PCI_SUBORDINATE_BUS_INSERT(data, pcicb->pcicb_bus);
		pci_conf_write (tag, PCI_PCI_BRIDGE_BUS_REG, data);

		/*
		**	Propagate the new upper bus number limit.
		*/
		for (parent = pcicb->pcicb_up; parent != NULL;
			parent = parent->pcicb_up)
		{
			if (parent->pcicb_subordinate >= pcicb->pcicb_bus)
				continue;
			parent->pcicb_subordinate = pcicb->pcicb_bus;
			if (!parent->pcicb_bridge.tag)
				continue;
			data = pci_conf_read
				(parent->pcicb_bridge, PCI_PCI_BRIDGE_BUS_REG);
			data = PCI_SUBORDINATE_BUS_INSERT
				(data, pcicb->pcicb_bus);
			pci_conf_write (parent->pcicb_bridge,
				PCI_PCI_BRIDGE_BUS_REG, data);
		}
	    }

	    if (!pcicb->pcicb_membase) {
		u_int size = 0x100000;
		pcicb->pcicb_membase = pci_memalloc (pcicb->pcicb_up, 0, size);
		if (pcicb->pcicb_membase)
			pcicb->pcicb_memlimit = pcicb->pcicb_membase+size-1;
	    }
	}
	return pcicb->pcicb_bus;
}

/*========================================================
**
**	pci_bus_config()
**
**	Autoconfiguration of one pci bus.
**
**========================================================
*/

static int
pci_mfdev (int bus, int device)
{
    pcici_t tag0,tag1;
    int	pci_id0, pci_id1;

    /*
    ** Detect a multi-function device that complies to the PCI 2.0 spec
    */
    tag0 = pcibus->pb_tag  (bus, device, 0);
    if (pci_conf_read (tag0, PCI_HEADER_MISC) & PCI_HEADER_MULTIFUNCTION)
	return 1;

    /*
    ** Well, as always: Theory and implementation of PCI ...
    **
    ** If there is a valid device ID returned for function 1 AND
    **		the device ID of function 0 and 1 is different OR
    **		the first mapping register of 0 and 1 differs,
    ** then assume a multi-function device anyway ...
    **
    ** Example of such a broken device: ISA and IDE chip i83371FB (Triton)
    */
    tag1 = pcibus->pb_tag  (bus, device, 1);
    pci_id1 = pci_conf_read (tag1, PCI_ID_REG);

    if (pci_id1 != 0xffffffff) {

	pci_id0 = pci_conf_read (tag0, PCI_ID_REG);

	if (pci_id0 != pci_id1)
	    return 1;

	if (pci_conf_read (tag0, PCI_MAP_REG_START) 
			!= pci_conf_read (tag1, PCI_MAP_REG_START))
	    return 1;
    }
    return 0;
}

static void
pci_bus_config (void)
{
	int	bus_no;
	u_char  device;
	u_char	reg;
	pcici_t tag, mtag;
	pcidi_t type;
	u_long  data;
	int     unit;
	u_char	pciint;
	u_char	irq;

	struct	pci_device *dvp;

	struct	pci_devconf *pdcp;

	/*
	**	first initialize the bridge (bus controller chip)
	*/
	bus_no = pci_bridge_config ();

	printf ("Probing for devices on PCI bus %d:\n", bus_no);
#ifndef PCI_QUIET
	if (bootverbose && !pci_info_done) {
		pci_info_done=1;
		printf ("\tconfiguration mode %d allows %d devices.\n",
			pci_mechanism, pci_maxdevice);
	};
#endif
	for (device=0; device<pci_maxdevice; device ++) {
	    char *name = NULL;
	    struct pci_device **dvpp;
	    int func, maxfunc = 0;

	    if ((pcicb->pcicb_seen >> device) & 1)
	    	continue;

	    for (func=0; func <= maxfunc; func++) {
		tag  = pcibus->pb_tag  (bus_no, device, func);
		type = pci_conf_read (tag, PCI_ID_REG);

		if ((!type) || (type==0xfffffffful)) continue;

		/*
		**	lookup device in ioconfiguration:
		*/

		dvpp = (struct pci_device **)pcidevice_set.ls_items;

		while (dvp = *dvpp++) {
			if (dvp->pd_probe) {
				if (name=(*dvp->pd_probe)(tag, type))
					break;
			}
		};
		/*
		**	check for mirrored devices.
		*/
		if (func != 0) {
			goto real_device;
		}
		if (device & 0x10) {
			mtag=pcibus->pb_tag (bus_no,
				(u_char)(device & ~0x10), 0);
		} else if (device & 0x08) {
			mtag=pcibus->pb_tag (bus_no,
				(u_char)(device & ~0x08), 0);
		} else goto real_device;

		if (type!=pci_conf_read (mtag, PCI_ID_REG))
			goto real_device;

		for (reg=PCI_MAP_REG_START;reg<PCI_MAP_REG_END;reg+=4)
			if (pci_conf_read(tag,reg)!=pci_conf_read(mtag,reg))
				goto real_device;

#ifndef PCI_QUIET
		if (dvp==NULL) continue;
		if (bootverbose)
			printf ("%s? <%s> mirrored on pci%d:%d\n",
				dvp->pd_name, name, bus_no, device);
#endif
		continue;

	real_device:

#ifndef PCI_QUIET
#ifdef PCI_BRIDGE_DEBUG
		if (bootverbose) {
		    printf ("\tconfig header: 0x%08x 0x%08x 0x%08x 0x%08x\n",
			    pci_conf_read (tag, 0),
			    pci_conf_read (tag, 4),
			    pci_conf_read (tag, 8),
			    pci_conf_read (tag, 12));
		}
#endif
#endif

		if (func == 0 && pci_mfdev (bus_no, device)) {
			maxfunc = 7;
		}

		if (dvp==NULL) {
#ifndef PCI_QUIET
			if (pci_conf_count)
				continue;

			if (maxfunc == 0)
				printf("%s%d:%d:    ", 
				       pcibus->pb_name, bus_no, device);
			else
				printf("%s%d:%d:%d: ", 
				       pcibus->pb_name, bus_no, device, func);
			not_supported (tag, type);
#endif
			continue;
		};

		pcicb->pcicb_seen |= (1ul << device);

		/*
		**	Get and increment the unit.
		*/

		unit = (*dvp->pd_count)++;

		/*
		**	ignore device ?
		*/

		if (!*name) continue;

		/*
		**	Announce this device
		*/

		printf ("%s%d <%s> rev %d", dvp->pd_name, unit, name,
			(unsigned) pci_conf_read (tag, PCI_CLASS_REG) & 0xff);

		/*
		**	Get the int pin number (pci interrupt number a-d)
		**	from the pci configuration space.
		*/

		data = pci_conf_read (tag, PCI_INTERRUPT_REG);
		pciint = PCI_INTERRUPT_PIN_EXTRACT(data);

		if (pciint) {

			printf (" int %c irq ", 0x60+pciint);

			irq = PCI_INTERRUPT_LINE_EXTRACT(data);

			/*
			**	If it's zero, the isa irq number is unknown,
			**	and we cannot bind the pci interrupt.
			*/

			if (irq && (irq != 0xff))
				printf ("%d", irq);
			else
				printf ("??");
		};

		if (maxfunc == 0)
			printf (" on pci%d:%d\n", bus_no, device);
		else
			printf (" on pci%d:%d:%d\n", bus_no, device, func);

		/*
		**	Read the current mapping,
		**	and update the pcicb fields.
		*/

		for (reg=PCI_MAP_REG_START;reg<PCI_MAP_REG_END;reg+=4) {
			u_int map, addr, size;

			data = pci_conf_read(tag, PCI_CLASS_REG);
			switch (data & (PCI_CLASS_MASK|PCI_SUBCLASS_MASK)) {
			case PCI_CLASS_BRIDGE|PCI_SUBCLASS_BRIDGE_PCI:
				continue;
			};

			map = pci_conf_read (tag, reg);
			if (!(map & PCI_MAP_MEMORY_ADDRESS_MASK))
				continue;

			pci_conf_write (tag, reg, 0xffffffff);
			data = pci_conf_read (tag, reg);
			pci_conf_write (tag, reg, map);

			switch (data & 7) {

			default:
				continue;
			case 1:
			case 5:
				addr = map & PCI_MAP_IO_ADDRESS_MASK;
				size = -(data & PCI_MAP_IO_ADDRESS_MASK);
				size &= ~(addr ^ -addr);

				pci_register_io (pcicb, addr, addr+size-1);
				pcicb->pcicb_pamount += size;
				break;

			case 0:
			case 2:
			case 4:
				size = -(data & PCI_MAP_MEMORY_ADDRESS_MASK);
				addr = map & PCI_MAP_MEMORY_ADDRESS_MASK;
				if (addr >= 0x100000) {
					pci_register_memory
						(pcicb, addr, addr+size-1);
					pcicb->pcicb_mamount += size;
				} else {
					pcicb->pcicb_flags |= PCICB_ISAMEM;
				};
				break;
			};
			if (bootverbose)
				printf ("\tmapreg[%02x] type=%d addr=%08x size=%04x.\n",
					reg, map&7, addr, size);
		};

		/*
		**	Allocate a devconf structure
		**	We should, and eventually will, set the
		**	parent pointer to a pci bus devconf structure,
		**	and arrange to set the state field dynamically.
		*/

		pdcp = (struct pci_devconf *)
			malloc (sizeof (struct pci_devconf),M_DEVBUF,M_WAITOK);
		bzero(pdcp, sizeof(struct pci_devconf));

		pdcp -> pdc_pi.pi_bus    = bus_no;
		pdcp -> pdc_pi.pi_device = device;
		pdcp -> pdc_pi.pi_func   = func;

		pdcp -> pdc_kdc.kdc_name = dvp->pd_name;
		pdcp -> pdc_kdc.kdc_unit = unit;

		pdcp -> pdc_kdc.kdc_md.mddc_devtype = MDDT_PCI;

		pdcp -> pdc_kdc.kdc_externalize = pci_externalize;
		pdcp -> pdc_kdc.kdc_internalize = pci_internalize;

		pdcp -> pdc_kdc.kdc_datalen     = PCI_EXTERNAL_LEN;
		pdcp -> pdc_kdc.kdc_parentdata  = &pdcp->pdc_pi;
		pdcp -> pdc_kdc.kdc_state       = DC_UNKNOWN;
		pdcp -> pdc_kdc.kdc_description = name;
		pdcp -> pdc_kdc.kdc_shutdown	= dvp->pd_shutdown;

		/*
		**	And register this device
		*/

		dev_attach (&pdcp->pdc_kdc);

		/*
		**	attach device
		**	may produce additional log messages,
		**	i.e. when installing subdevices.
		*/

		(*dvp->pd_attach) (tag, unit);

		/*
		**	Special processing of certain classes
		*/

		data = pci_conf_read(tag, PCI_CLASS_REG);

		switch (data & (PCI_CLASS_MASK|PCI_SUBCLASS_MASK)) {
			struct pcicb *this, **link;
			unsigned char primary, secondary, subordinate;
			u_int command;

		case PCI_CLASS_BRIDGE|PCI_SUBCLASS_BRIDGE_PCI:

			/*
			**	get current configuration of the bridge.
			*/
			data = pci_conf_read (tag, PCI_PCI_BRIDGE_BUS_REG);
			primary     = PCI_PRIMARY_BUS_EXTRACT  (data);
			secondary   = PCI_SECONDARY_BUS_EXTRACT(data);
			subordinate = PCI_SUBORDINATE_BUS_EXTRACT(data);
#ifndef PCI_QUIET
			if (bootverbose) {
			    printf ("\tbridge from pci%d to pci%d through %d.\n",
				primary, secondary, subordinate);
			    printf ("\tmapping regs: io:%08lx mem:%08lx pmem:%08lx\n",
				pci_conf_read (tag, PCI_PCI_BRIDGE_IO_REG),
				pci_conf_read (tag, PCI_PCI_BRIDGE_MEM_REG),
				pci_conf_read (tag, PCI_PCI_BRIDGE_PMEM_REG));
			}
#endif
			/*
			**	check for uninitialized bridge.
			*/
			if (!(primary < secondary 
			      && secondary <= subordinate
			      && bus_no == primary))
			{
				printf ("\tINCORRECTLY or NEVER CONFIGURED.\n");
				/*
				**	disable this bridge
				*/
				pci_conf_write (tag, PCI_COMMAND_STATUS_REG,
							0xffff0000);
				secondary   = 0;
				subordinate = 0;
			};

			/*
			**  allocate bus descriptor for bus behind the bridge
			*/
			link = &pcicb->pcicb_down;
			while (*link && (*link)->pcicb_bus < secondary)
				link = &(*link)->pcicb_next;

			this = malloc (sizeof (*this), M_DEVBUF, M_WAITOK);

			/*
			**	Initialize this descriptor so far.
			**	(the initialization is completed just before
			**	scanning the bus behind the bridge.
			*/
			bzero (this, sizeof(*this));
			this->pcicb_next        = *link;
			this->pcicb_up		= pcicb;
			this->pcicb_bridge      = tag;
			this->pcicb_bus 	= secondary;
			this->pcicb_subordinate = subordinate;

			command = pci_conf_read(tag,PCI_COMMAND_STATUS_REG);

			if (command & PCI_COMMAND_IO_ENABLE){
				/*
				**	Bridge was configured by the bios.
				**	Read out the mapped io region.
				*/
				unsigned reg;

				reg = pci_conf_read (tag,
					PCI_PCI_BRIDGE_IO_REG);
				this->pcicb_iobase  =
					PCI_PPB_IOBASE_EXTRACT (reg);
				this->pcicb_iolimit =
					PCI_PPB_IOLIMIT_EXTRACT(reg);

				/*
				**	Note the used io space.
				*/
				pci_register_io (pcicb, this->pcicb_iobase,
						this->pcicb_iolimit);

			};

			if (command & PCI_COMMAND_MEM_ENABLE) {
				/*
				**	Bridge was configured by the bios.
				**	Read out the mapped memory regions.
				*/
				unsigned reg;

				/*
				**	non prefetchable memory
				*/
				reg = pci_conf_read (tag,
					PCI_PCI_BRIDGE_MEM_REG);
				this->pcicb_membase  =
					PCI_PPB_MEMBASE_EXTRACT (reg);
				this->pcicb_memlimit =
					PCI_PPB_MEMLIMIT_EXTRACT(reg);

				/*
				**	Register used memory space.
				*/
				pci_register_memory (pcicb,
					this->pcicb_membase,
					this->pcicb_memlimit);

				/*
				**	prefetchable memory
				*/
				reg = pci_conf_read (tag,
					PCI_PCI_BRIDGE_PMEM_REG);
				this->pcicb_p_membase=
					PCI_PPB_MEMBASE_EXTRACT (reg);
				this->pcicb_p_memlimit=
					PCI_PPB_MEMLIMIT_EXTRACT(reg);

				/*
				**	Register used memory space.
				*/
				pci_register_memory (pcicb,
					this->pcicb_p_membase,
					this->pcicb_p_memlimit);
			}

			/*
			**	Link it in chain.
			*/
			*link=this;

			/*
			**	Update mapping info of parent bus.
			*/
			if (!pcicb->pcicb_bfrom||secondary< pcicb->pcicb_bfrom)
				pcicb->pcicb_bfrom = secondary;
			if (subordinate > pcicb->pcicb_bupto)
				pcicb->pcicb_bupto = subordinate;

			break;
		}
	    }
	}

#ifndef PCI_QUIET
	if (bootverbose) {
	    if (pcicb->pcicb_mamount)
		printf ("%s%d: uses %d bytes of memory from %x upto %x.\n",
			pcibus->pb_name, bus_no,
			pcicb->pcicb_mamount,
			pcicb->pcicb_mfrom, pcicb->pcicb_mupto);
	    if (pcicb->pcicb_pamount)
		printf ("%s%d: uses %d bytes of I/O space from %x upto %x.\n",
			pcibus->pb_name, bus_no,
			pcicb->pcicb_pamount,
			pcicb->pcicb_pfrom, pcicb->pcicb_pupto);
	    if (pcicb->pcicb_bfrom)
		printf ("%s%d: subordinate busses from %x upto %x.\n",
			pcibus->pb_name, bus_no,
			pcicb->pcicb_bfrom, pcicb->pcicb_bupto);
	}
#endif
}

/*========================================================
**
**	pci_configure ()
**
**      Autoconfiguration of pci devices.
**
**      May be called more than once.
**      Any device is attached only once.
**
**      Has to take care of mirrored devices, which are
**      entailed by incomplete decoding of pci address lines.
**
**========================================================
*/

void pci_configure()
{
	struct pcibus **pbp = (struct pcibus**) pcibus_set.ls_items;

	/*
	**	check pci bus present
	*/

	while (!pci_maxdevice && (pcibus = *pbp++)) {
		(*pcibus->pb_setup)();
	}

	if (!pci_maxdevice) return;

	/*
	**	hello world ..
	*/

	for (pcicb = &pcibus0; pcicb != NULL;) {
		pci_bus_config ();

		if (pcicb->pcicb_down) {
			pcicb = pcicb->pcicb_down;
			continue;
		};

		while (pcicb && !pcicb->pcicb_next)
			pcicb = pcicb->pcicb_up;

		if (pcicb)
			pcicb = pcicb->pcicb_next;
	}
	pci_conf_count++;
}

#ifndef SALBOOT
/* salboot is space constrained and does not use this section */

/*-----------------------------------------------------------------------
**
**	Map device into port space.
**
**	Actually the device should have been mapped by the bios.
**	This function only reads and verifies the value.
**
**	PCI-Specification:  6.2.5.1: address maps
**
**-----------------------------------------------------------------------
*/

int pci_map_port (pcici_t tag, u_long reg, u_short* pa)
{
	unsigned data, ioaddr, iosize;
	struct pcicb *link = pcicb;

	/*
	**	sanity check
	*/

	if (reg < PCI_MAP_REG_START || reg >= PCI_MAP_REG_END || (reg & 3)) {
		printf ("pci_map_port failed: bad register=0x%x\n",
			(unsigned)reg);
		return (0);
	};

	/*if (pcicb->pcicb_flags & PCICB_NOIOSET) {
		printf ("pci_map_port failed: pci%d has not been configured for I/O access\n",
			pcicb->pcicb_bus);
		return (0);
	}*/

	/*
	**	get size and type of port
	**
	**	type is in the lowest two bits.
	**	If device requires 2^n bytes, the next
	**	n-2 bits are hardwired as 0.
	*/

	ioaddr = pci_conf_read (tag, reg) & PCI_MAP_IO_ADDRESS_MASK;
	if (!ioaddr) {
		printf ("pci_map_port failed: not configured by bios.\n");
		return (0);
	};

	pci_conf_write (tag, reg, 0xfffffffful);
	data = pci_conf_read (tag, reg);
	pci_conf_write (tag, reg, ioaddr);

	if ((data & 0x03) != PCI_MAP_IO) {
		printf ("pci_map_port failed: bad port type=0x%x\n",
			(unsigned) data);
		return (0);
	};
	iosize = -(data &  PCI_MAP_IO_ADDRESS_MASK);
	iosize &= ~(ioaddr ^ -ioaddr);
	if (ioaddr < pcicb->pcicb_iobase
		|| ioaddr + iosize -1 > pcicb->pcicb_iolimit) {
		printf ("pci_map_port failed: device's iorange 0x%x-0x%x "
			"is incompatible with its bridge's range 0x%x-0x%x\n",
			(unsigned) ioaddr, (unsigned) ioaddr + iosize - 1,
			(unsigned) pcicb->pcicb_iobase,
			(unsigned) pcicb->pcicb_iolimit);
		return (0);
	}

#ifndef PCI_QUIET
	if (bootverbose)
		printf ("\treg%d: ioaddr=0x%x size=0x%x\n",
			(unsigned) reg, (unsigned) ioaddr, (unsigned) iosize);
#endif
	/*
	**	set the configuration register of and
	**      return the address to the driver.
	**	Make sure to enable each upstream bridge
	**	so I/O and DMA can go all the way.
	*/

	for (;;) {
		data =	pci_conf_read (tag, PCI_COMMAND_STATUS_REG) & 0xffff;
		data |= PCI_COMMAND_IO_ENABLE | PCI_COMMAND_MASTER_ENABLE;
		(void)	pci_conf_write(tag, PCI_COMMAND_STATUS_REG, data);
		if ((link = link->pcicb_up) == NULL)
			break;
		tag = link->pcicb_bridge;
	}

	*pa = ioaddr;

	return (1);
}

/*-----------------------------------------------------------------------
**
**	Map device into virtual and physical space
**
**	Actually the device should have been mapped by the bios.
**	This function only reads and verifies the value.
**
**      PCI-Specification:  6.2.5.1: address maps
**
**-----------------------------------------------------------------------
*/

int pci_map_mem (pcici_t tag, u_long reg, vm_offset_t* va, vm_offset_t* pa)
{
	struct pcicb *link = pcicb;
	unsigned    data ,paddr;
	vm_size_t   psize, poffs;
	vm_offset_t vaddr;

	/*
	**	sanity check
	*/

	if (reg < PCI_MAP_REG_START || reg >= PCI_MAP_REG_END || (reg & 3)) {
		printf ("pci_map_mem failed: bad register=0x%x\n",
			(unsigned)reg);
		return (0);
	};

	/*
	**	save old mapping, get size and type of memory
	**
	**	type is in the lowest four bits.
	**	If device requires 2^n bytes, the next
	**	n-4 bits are read as 0.
	*/

	paddr = pci_conf_read (tag, reg) & PCI_MAP_MEMORY_ADDRESS_MASK;
	pci_conf_write (tag, reg, 0xfffffffful);
	data = pci_conf_read (tag, reg);
	pci_conf_write (tag, reg, paddr);

	/*
	**	check the type
	*/

	if ((data & PCI_MAP_MEMORY_TYPE_MASK) != PCI_MAP_MEMORY_TYPE_32BIT) {
		printf ("pci_map_mem failed: bad memory type=0x%x\n",
			(unsigned) data);
		return (0);
	};

	/*
	**	get the size.
	*/

	psize = -(data & PCI_MAP_MEMORY_ADDRESS_MASK);

	if (!paddr || paddr == PCI_MAP_MEMORY_ADDRESS_MASK) {
		paddr = pci_memalloc (pcicb, 0, psize);
		if (!paddr) {
			printf ("pci_map_mem: not configured by bios.\n");
			return (0);
		};
		pci_register_memory (pcicb, paddr, paddr+psize-1);
	};

	if (paddr < pcicb->pcicb_membase ||
		paddr + psize - 1 > pcicb->pcicb_memlimit) {
		printf ("pci_map_mem failed: device's memrange 0x%x-0x%x is "
			"incompatible with its bridge's memrange 0x%x-0x%x\n",
			(unsigned) paddr,
			(unsigned) (paddr + psize - 1),
			(unsigned) pcicb->pcicb_membase,
			(unsigned) pcicb->pcicb_memlimit);
/*		return (0);*/
/* ACHTUNG: Ist der Code richtig, wenn eine PCI-PCI-Bridge fuer
 * die PCI-Slots verwendet wird, aber die Onboard-Devices direkt 
 * an der CPU-PCI-Bridge haengen (Siehe Compaq Prolinea Problem) ???
 */
	}
	pci_conf_write (tag, reg, paddr);

	/*
	**	Truncate paddr to page boundary.
	**	(Or does pmap_mapdev the job?)
	*/

	poffs = paddr - trunc_page (paddr);
	vaddr = (vm_offset_t) pmap_mapdev (paddr-poffs, psize+poffs);

	if (!vaddr) return (0);

	vaddr += poffs;

#ifndef PCI_QUIET
	/*
	**	display values.
	*/

	if (bootverbose)
		printf ("\treg%d: virtual=0x%lx physical=0x%lx size=0x%lx\n",
		 (unsigned) reg, (u_long)vaddr, (u_long)paddr, (u_long)psize);
#endif
	/*
	**      set the configuration register and
	**      return the address to the driver
	**      Make sure to enable each upstream bridge
	**      so memory and DMA can go all the way.
	*/

	for (;;) {
		data =  pci_conf_read (tag, PCI_COMMAND_STATUS_REG) & 0xffff;
		data |= PCI_COMMAND_MEM_ENABLE | PCI_COMMAND_MASTER_ENABLE;
		(void)  pci_conf_write(tag, PCI_COMMAND_STATUS_REG, data);
		if ((link = link->pcicb_up) == NULL)
			break;
		tag = link->pcicb_bridge;
	}

	*va = vaddr;
	*pa = paddr;

	return (1);
}
#endif /* SALBOOT */

/*------------------------------------------------------------
**
**	Interface functions for the devconf module.
**
**------------------------------------------------------------
*/

static int
pci_externalize (struct proc *p, struct kern_devconf *kdcp, void *u, size_t l)
{
	struct pci_externalize_buffer buffer;
	struct pci_info * pip = kdcp->kdc_parentdata;
	pcici_t tag;
	int	i;

	if (l < sizeof buffer) {
		return ENOMEM;
	};

	tag = pcibus->pb_tag (pip->pi_bus, pip->pi_device, pip->pi_func);

	buffer.peb_pci_info	= *pip;

	for (i=0; i<PCI_EXT_CONF_LEN; i++) {
		buffer.peb_config[i] = pci_conf_read (tag, i*4);
	};

	return copyout(&buffer, u, sizeof buffer);
}


static int
pci_internalize (struct proc *p, struct kern_devconf *kdcp, void *u, size_t s)
{
	return EOPNOTSUPP;
}

/*-----------------------------------------------------------------------
**
**	Pci meta interrupt handler
**
**	This handler assumes level triggered interrupts.
**	It's possible to build a kernel which handles shared
**	edge triggered interrupts by the options "PCI_EDGE_INT".
**	But there is a performance penalty.
**
**	(Of course you can delete the #ifdef PCI_EDGE_INT bracketed
**	code at all :-) :-) :-)
**
**-----------------------------------------------------------------------
*/

struct pci_int_desc*
	pci_int_desc [PCI_MAX_IRQ];

#ifndef NO_SHARED_IRQ

static inline unsigned
splq (unsigned mask)
{
	unsigned temp=cpl;
	cpl |= mask;
	return temp;
}

static void
pci_int (int irq)
{
	struct pci_int_desc * p;
	int c, s;
#ifdef PCI_EDGE_INT
	int i, n;
#endif
	if (irq<0 || irq >= PCI_MAX_IRQ) {
		printf ("pci_int: irq %d out of range, ignored\n", irq);
		return;
	};

#ifdef PCI_EDGE_INT
	for (i=0; i<1000; i++) {
		n = 0;
#endif
		for (p = pci_int_desc[irq]; p!=NULL; p=p->pcid_next) {
			s = splq (*p->pcid_maskptr);
			c= (*p->pcid_handler) (p->pcid_argument);
			p-> pcid_tally += c;
			splx (s);
#ifdef PCI_EDGE_INT
			n += c;
#endif
#if 0
			if (c && p->pcid_tally<20)
			printf ("PCI_INT: irq=%d h=%p cpl o=%x n=%x val=%d\n",
					irq, p->pcid_handler, s, cpl, c);
#endif
		};
#ifdef PCI_EDGE_INT
		if (!n) return;
	};
	printf ("pci_int(%d): permanent interrupt request.\n", irq);
#endif
}
#endif

/*-----------------------------------------------------------------------
**
**	Auxiliary function for interrupt (un)mapping.
**
**-----------------------------------------------------------------------
*/

static u_int
getirq (pcici_t tag)
{
	u_int irq;

	irq = PCI_INTERRUPT_LINE_EXTRACT(
		pci_conf_read (tag, PCI_INTERRUPT_REG));

	if (irq <= 0) {
		printf ("\tint line register not set by bios\n");
		return (0);
	}

	if (irq >= pcibus->pb_maxirq || irq >= PCI_MAX_IRQ) {
		printf ("\tirq %d invalid.\n", irq);
		return (0);
	}

	return (irq);
}

static struct pci_int_desc **
getintdescbytag (u_int irq, pcici_t tag)
{
	struct pci_int_desc *p, **pp;

	pp=&pci_int_desc[irq];
	while (((p=*pp)) && !sametag(p->pcid_tag,tag))
		pp=&p->pcid_next;

	if (!p) return (NULL);

	return (pp);
}

static struct pci_int_desc *
getintdescbymptr (u_int irq, unsigned * mptr)
{
	struct pci_int_desc *p;

	for (p=pci_int_desc[irq];p;p=p->pcid_next)
		if (p->pcid_maskptr == mptr) break;
	return (p);
}

/*-----------------------------------------------------------------------
**
**	Map pci interrupt.
**
**-----------------------------------------------------------------------
*/

static unsigned pci_mask0 = 0;

int pci_map_int (pcici_t tag, int(*func)(), void* arg, unsigned* maskptr)
{
	u_int irq;
	int result, oldspl;
	unsigned  mask;
	struct pci_int_desc *tail, *mdp=NULL, *new=NULL;

	/*
	**	Get irq line from configuration space,
	**	and check for consistency.
	*/

	irq = getirq (tag);
	if (irq >= PCI_MAX_IRQ) {
		printf ("\tillegal irq %d.\n", irq);
		return (0);
	};
	mask= 1ul << irq;

	/*
	**      disable this interrupt.
	*/

	oldspl = splq (mask);

	/*
	**	If handler for this tag already installed,
	**	remove it first.
	*/

	if (getintdescbytag (irq, tag) != NULL)
		pci_unmap_int (tag);

	/*
	**	If this irq not yet included in the mask, include it.
	*/

	mdp = getintdescbymptr (irq, maskptr);
	if (!mdp) {
		result = pcibus->pb_imaskinc (irq, maskptr);
		if (result)
			goto conflict;
	};

	/*
	**	Allocate descriptor and initialize it.
	*/

	tail = pci_int_desc[irq];

	new = malloc (sizeof (*new), M_DEVBUF, M_WAITOK);
	bzero (new, sizeof (*new));

	new->pcid_next	   = tail;
	new->pcid_tag      = tag;
	new->pcid_handler  = func;
	new->pcid_argument = arg;
	new->pcid_maskptr  = maskptr;
	new->pcid_tally    = 0;
	new->pcid_mask	   = mask;

	/*
	**	If first handler:   install it.
	**	If second handler: install shared-int-handler.
	*/

	if (!tail) {
		/*
		**	first handler for this irq.
		*/

		result = pcibus->pb_iattach
			(irq, (void(*)()) func, (int) arg, maskptr);
		if (result) goto conflict;

#ifdef NO_SHARED_IRQ
	} else goto conflict;
#else
	} else if (!tail->pcid_next) {
		/*
		**	Second handler for this irq.
		*/

		if (bootverbose)
			printf ("\tusing shared irq %d.\n", irq);

		/*
		**	replace old handler by shared-int-handler.
		*/

		result = pcibus->pb_idetach (irq,(void(*)())tail->pcid_handler);
		if (result)
			printf ("\tCANNOT DETACH INT HANDLER.\n");

		result = pcibus->pb_iattach (irq, pci_int, irq, &pci_mask0);
		if (result) {
			printf ("\tCANNOT ATTACH SHARED INT HANDLER.\n");
			goto fail;
		};
	}
#endif
	/*
	**	Link new descriptor, reenable ints and done.
	*/

	pci_int_desc[irq]  = new;
	splx (oldspl);
	return (1);

	/*
	**	Handle some problems.
	*/

conflict:
	printf ("\tirq %d already in use.\n", irq);
fail:
	/*
	**	If descriptor allocated, free it.
	**	If included in mask, remove it.
	*/

	if (new) free(new, M_DEVBUF);
	if (!mdp) (void) pcibus->pb_imaskexc (irq, maskptr);
	splx (oldspl);
	return (0);
}

/*-----------------------------------------------------------------------
**
**	Unmap pci interrupt.
**
**-----------------------------------------------------------------------
*/

int pci_unmap_int (pcici_t tag)
{
	int result, oldspl;
	struct pci_int_desc *this, **hook, *tail;
	unsigned irq;

	/*
	**	Get irq line from configuration space,
	**	and check for consistency.
	*/

	irq = getirq (tag);
	if (irq >= PCI_MAX_IRQ) {
		printf ("\tillegal irq %d.\n", irq);
		return (0);
	};

	/*
	**	Search and unlink interrupt descriptor.
	*/

	hook = getintdescbytag (irq, tag);
	if (hook == NULL) {
		printf ("\tno irq %d handler for pci %x\n",
			irq, tag.tag);
		return (0);
	};

	this = *hook;
	*hook= this->pcid_next;

	/*
	**	Message
	*/

	printf ("\tirq %d handler %p(%p) unmapped for pci %x after %d ints.\n",
		irq, this->pcid_handler, this->pcid_argument,
		this->pcid_tag.tag, this->pcid_tally);

	/*
	**	If this irq no longer included in the mask, remove it.
	*/

	if (!getintdescbymptr (irq, this->pcid_maskptr))
		(void) pcibus->pb_imaskexc (irq, this->pcid_maskptr);

	tail = pci_int_desc[irq];

	if (tail == NULL) {

		/*
		**	Remove the old handler.
		*/

		result = pcibus->pb_idetach (irq,(void(*)())this->pcid_handler);
		if (result)
			printf ("\tirq %d: cannot remove handler.\n", irq);

	} else if (tail->pcid_next == NULL) {

		/*
		**	Remove the shared int handler.
		**	Install the last remaining handler.
		*/

		oldspl = splq (1ul << irq);

		result = pcibus->pb_idetach (irq, pci_int);
		if (result)
			printf ("\tirq %d: cannot remove handler.\n", irq);

		result = pcibus->pb_iattach (irq,
				(void(*)()) tail->pcid_handler,
				(int) tail->pcid_argument,
				tail->pcid_maskptr);

		if (result)
			printf ("\tirq %d: cannot install handler.\n", irq);

		splx (oldspl);
	};

	free (this, M_DEVBUF);
	return (1);
}

/*-----------------------------------------------------------
**
**	Display of unknown devices.
**
**-----------------------------------------------------------
*/
struct vt {
	u_short	ident;
	char*	name;
};

static struct vt VendorTable[] = {
	{0x0e11, "Compaq"},
	{0x1000, "NCR/Symbios"},
	{0x1002, "ATI Technologies Inc."},
	{0x1004, "VLSI"},
	{0x100B, "National Semiconductor"},
	{0x100E, "Weitek"},
	{0x1011, "Digital Equipment Corporation"},
	{0x1013, "Cirrus Logic"},
	{0x101A, "NCR"},
	{0x1022, "AMD"},
	{0x102B, "Matrox"},
	{0x102C, "Chips & Technologies"},
	{0x1039, "Silicon Integrated Systems"},
	{0x1042, "SMC"},
	{0x1044, "DPT"},
	{0x1045, "OPTI"},
	{0x104B, "Bus Logic"},
	{0x1060, "UMC"},
	{0x1080, "Contaq"},
	{0x1095, "CMD"},
	{0x1106, "VIA Technologies"},
	{0x5333, "S3 Inc."},
	{0x8086, "Intel Corporation"},
	{0x9004, "Adaptec"},
	{0,0}
};

typedef struct {
	const int	subclass;
	const char	*name;
} subclass_name;

/* 0x00 prehistoric subclasses */
static const subclass_name old_subclasses[] =
{
	{ 0x00, "misc"	},
	{ 0x01, "vga"	},
	{ 0x00, NULL	}
};

/* 0x01 mass storage subclasses */
static const subclass_name storage_subclasses[] =
{
	{ 0x00, "scsi"	},
	{ 0x01, "ide"	},
	{ 0x02, "floppy"},
	{ 0x03, "ipi"	},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

/* 0x02 network subclasses */
static const subclass_name network_subclasses[] =
{
	{ 0x00, "ethernet"	},
	{ 0x01, "tokenring"	},
	{ 0x02, "fddi"	},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

/* 0x03 display subclasses */
static const subclass_name display_subclasses[] =
{
	{ 0x00, "vga"	},
	{ 0x01, "xga"	},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

/* 0x04 multimedia subclasses */
static const subclass_name multimedia_subclasses[] =
{
	{ 0x00, "video"	},
	{ 0x01, "audio"	},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

/* 0x05 memory subclasses */
static const subclass_name memory_subclasses[] =
{
	{ 0x00, "ram"	},
	{ 0x01, "flash"	},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

/* 0x06 bridge subclasses */
static const subclass_name bridge_subclasses[] =
{
	{ 0x00, "host"	},
	{ 0x01, "isa"	},
	{ 0x02, "eisa"	},
	{ 0x03, "mc"	},
	{ 0x04, "pci"	},
	{ 0x05, "pcmcia"},
	{ 0x80, "misc"	},
	{ 0x00, NULL	}
};

static const subclass_name *const subclasses[] = {
	old_subclasses, 
	storage_subclasses, 
	network_subclasses, 
	display_subclasses,
	multimedia_subclasses,
	memory_subclasses, 
	bridge_subclasses,
};

static const char *const majclasses[] = {
	"old", 
	"storage", 
	"network", 
	"display",
	"multimedia", 
	"memory", 
	"bridge"
};


void not_supported (pcici_t tag, u_long type)
{
	u_long	reg;
	u_long	data;
	u_char	class;
	u_char	subclass;
	struct vt * vp;
	int	pciint;
	int	irq;

	/*
	**	lookup the names.
	*/

	for (vp=VendorTable; vp->ident; vp++)
		if (vp->ident == (type & 0xffff))
			break;

	/*
	**	and display them.
	*/

	if (vp->ident) printf (vp->name);
		else   printf ("vendor=0x%04lx", type & 0xffff);

	printf (", device=0x%04lx", type >> 16);

	data = pci_conf_read(tag, PCI_CLASS_REG);
	class = (data >> 24) & 0xff;
	subclass = (data >> 16) & 0xff;

	if (class < sizeof(majclasses) / sizeof(majclasses[0])) {
		printf(", class=%s", majclasses[class]);
	} else {
		printf(", class=0x%02x", class);
	}

	if (class < sizeof(subclasses) / sizeof(subclasses[0])) {
		const subclass_name *p = subclasses[class];
		while (p->name && (p->subclass != subclass)) 
			p++;
		if (p->name) {
			printf(" (%s)", p->name);
		} else {
			printf(" (unknown subclass 0x%02lx)", subclass);
		}
	} else {
		printf(", subclass=0x%02x", subclass);
	}

	data = pci_conf_read (tag, PCI_INTERRUPT_REG);
	pciint = PCI_INTERRUPT_PIN_EXTRACT(data);

	if (pciint) {

		printf (" int %c irq ", 0x60+pciint);

		irq = PCI_INTERRUPT_LINE_EXTRACT(data);

		/*
		**	If it's zero, the isa irq number is unknown,
		**	and we cannot bind the pci interrupt.
		*/

		if (irq && (irq != 0xff))
			printf ("%d", irq);
		else
			printf ("??");
	};

	if (class != (PCI_CLASS_BRIDGE >> 24))
	    printf (" [no driver assigned]");
	printf ("\n");

	if (bootverbose) {
	    if (class == (PCI_CLASS_BRIDGE >> 24)) {
		printf ("configuration space registers:");
		for (reg = 0; reg < 0x100; reg+=4) {
		    if ((reg & 0x0f) == 0) printf ("\n%02x:\t", reg);
		    printf ("%08x ", pci_conf_read (tag, reg));
		}
		printf ("\n");
	    } else {
		for (reg=PCI_MAP_REG_START; reg<PCI_MAP_REG_END; reg+=4) {
		    data = pci_conf_read (tag, reg);
		    if ((data&~7)==0) continue;
		    switch (data&7) {

		case 1:
		case 5:
			printf ("\tmap(%x): io(%04lx)\n",
				reg, data & ~3);
			break;
		case 0:
			printf ("\tmap(%x): mem32(%08lx)\n",
				reg, data & ~7);
			break;
		case 2:
			printf ("\tmap(%x): mem20(%05lx)\n",
				reg, data & ~7);
			break;
		case 4:
			printf ("\tmap(%x): mem64(%08x%08lx)\n",
				reg, pci_conf_read (tag, reg +4), data & ~7);
			reg += 4;
			break;
		    }
		}
	    }
	}
}
#endif /* NPCI */

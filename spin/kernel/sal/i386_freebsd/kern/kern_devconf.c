/*
 * Copyright (c) 1994, Garrett A. Wollman.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
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
 *	$Id: kern_devconf.c,v 1.1.1.1 1996/08/15 03:23:31 fgray Exp $
 */

/*
 * kern_devconf.c - manage device configuration table
 *
 * Garrett A. Wollman, October 1994.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/proc.h>
#include <vm/vm.h>		/* needed for sysctl.h */
#include <sys/sysctl.h>
#include <sys/devconf.h>

struct kern_devconf *dc_list = 0;
static unsigned dc_lastnum = 0;

int
dev_attach(struct kern_devconf *kdc)
{
	int s = splclock();
	kdc->kdc_next = dc_list;
	if(kdc->kdc_next)
		kdc->kdc_next->kdc_rlink = &kdc->kdc_next;
	kdc->kdc_rlink = &dc_list;
	kdc->kdc_number = ++dc_lastnum;
	dc_list = kdc;
	splx(s);
	return 0;
}

int
dev_detach(struct kern_devconf *kdc)
{
	int s;

	s = splclock();
	*kdc->kdc_rlink = kdc->kdc_next;
	if(kdc->kdc_next)
		kdc->kdc_next->kdc_rlink = kdc->kdc_rlink;
	splx(s);

	return 0;
}

/*
 * NB: the device must do a dev_detach inside its shutdown routine, if it
 * succeeds.  If no routine is specified, we assume no special treatment is
 * required.
 */
int
dev_shutdownall(int force)
{
	int rv = 0;
	struct kern_devconf *kdc = dc_list;

	while(kdc) {
		if(!kdc->kdc_shutdown) {
			dev_detach(kdc);
			kdc = dc_list;
			continue;
		}

		if(kdc->kdc_shutdown(kdc, force)) {
			rv++;
			kdc = kdc->kdc_next;
		} else {
			kdc = dc_list;
		}
	}
	return rv;
}

static void
make_devconf(struct kern_devconf *kdc, struct devconf *dc)
{
	strncpy(dc->dc_name, kdc->kdc_name, sizeof dc->dc_name);
	dc->dc_name[(sizeof dc->dc_name) - 1] = '\0';
	dc->dc_unit = kdc->kdc_unit;
	dc->dc_number = kdc->kdc_number;

	if(kdc->kdc_parent) {
		strncpy(dc->dc_pname, kdc->kdc_parent->kdc_name, sizeof dc->dc_pname);
		dc->dc_pname[(sizeof dc->dc_pname) - 1] = '\0';
		dc->dc_punit = kdc->kdc_parent->kdc_unit;
		dc->dc_pnumber = kdc->kdc_parent->kdc_number;
	} else {
		bzero(dc->dc_pname, sizeof dc->dc_pname);
		dc->dc_punit = -1;
		dc->dc_pnumber = -1;
	}

	MACHDEP_COPYDEV(dc, kdc);
	dc->dc_state = kdc->kdc_state;
	dc->dc_class = kdc->kdc_class;
	dc->dc_datalen = kdc->kdc_datalen;

	strncpy(dc->dc_descr, kdc->kdc_description, sizeof dc->dc_descr);
	dc->dc_descr[(sizeof dc->dc_descr) - 1] = '\0';
}

int
dev_sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp,
	   void *newp, size_t newlen, struct proc *p)
{
	struct kern_devconf *kdc;
	struct devconf dc;
	int rv;
	size_t len;

	/* all sysctl names at this level are terminal */
	if (namelen != 1)
		return ENOTDIR;

	switch(name[0]) {
#if 0
	case DEVCONF_NUMBER:
		return (sysctl_rdint(oldp, oldlenp, newp, dc_lastnum));
#endif
	default:
		for(kdc = dc_list; kdc; kdc = kdc->kdc_next) {
			if(kdc->kdc_number == name[0])
				break;
		}

		if(!kdc)
			return ENXIO;

		if(!oldp) {
			*oldlenp = sizeof(struct devconf) - 1;

			*oldlenp += kdc->kdc_datalen;
			return 0;
		}

		len = *oldlenp;
		make_devconf(kdc, &dc);
		*oldlenp = (sizeof dc) - 1 + dc.dc_datalen;

		if(len < *oldlenp) {
			return ENOMEM;
		}

		rv = copyout(&dc, oldp, (sizeof dc) - 1);
		if(rv)
			return rv;

		if(kdc->kdc_externalize)
			rv = kdc->kdc_externalize(p, kdc,
				            &((struct devconf *)oldp)->dc_data,
						  len - ((sizeof dc) - 1));
		if(rv)
			return rv;

		if(!newp)
			return 0;

		if(!kdc->kdc_internalize)
			return EOPNOTSUPP;

		rv = kdc->kdc_internalize(p, kdc,
				     &((struct devconf *)newp)->dc_data,
				     newlen - ((sizeof dc) - 1));
		return rv;
	}
}

/*
 * APM (Advanced Power Management) BIOS Device Driver
 *
 * Copyright (c) 1994 UKAI, Fumitoshi.
 * Copyright (c) 1994-1995 by HOSOKAWA, Tatsumi <hosokawa@mt.cs.keio.ac.jp>
 * Copyright (c) 1996 Nate Williams <nate@FreeBSD.org>
 *
 * This software may be used, modified, copied, and distributed, in
 * both source and binary form provided that the above copyright and
 * these terms are retained. Under no circumstances is the author
 * responsible for the proper functioning of this software, nor does
 * the author assume any responsibility for damages incurred with its
 * use.
 *
 * Sep, 1994	Implemented on FreeBSD 1.1.5.1R (Toshiba AVS001WD)
 *
 *	$Id: apm.c,v 1.2 1997/01/31 15:59:24 mef Exp $
 */

#include "apm.h"

#if NAPM > 1
#error only one APM device may be configured
#endif

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/kernel.h>
#include <sys/systm.h>
#include <sys/malloc.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include "i386/isa/isa.h"
#include "i386/isa/isa_device.h"
#include <machine/apm_bios.h>
#include <machine/segments.h>
#include <machine/clock.h>
#include <vm/vm.h>
#include <vm/vm_param.h>
#include <vm/pmap.h>
#include <sys/syslog.h>
#include <sys/devconf.h>
#include "apm_setup.h"

static int apm_display_off __P((void));
static int apm_int __P((u_long *eax, u_long *ebx, u_long *ecx));
static void apm_resume __P((void));

/* static data */
struct apm_softc {
	int	initialized, active;
	int	always_halt_cpu, slow_idle_cpu;
	int	disabled, disengaged;
	u_int	minorversion, majorversion;
	u_int	cs32_base, cs16_base, ds_base;
	u_int	cs_limit, ds_limit;
	u_int	cs_entry;
	u_int	intversion;
	struct apmhook sc_suspend;
	struct apmhook sc_resume;
};

static struct kern_devconf kdc_apm = {
	0, 0, 0,		/* filled in by dev_attach */
	"apm", 0, { MDDT_ISA, 0 },
	isa_generic_externalize, 0, 0, ISA_EXTERNALLEN,
	&kdc_isa0,		/* parent */
	0,			/* parentdata */
	DC_UNCONFIGURED,	/* state */
	"Advanced Power Management BIOS",
	DC_CLS_MISC		/* class */
};


static struct apm_softc apm_softc;
static struct apmhook	*hook[NAPM_HOOK];		/* XXX */

#define is_enabled(foo) ((foo) ? "enabled" : "disabled")

/* Map version number to integer (keeps ordering of version numbers) */
#define INTVERSION(major, minor)	((major)*100 + (minor))

static timeout_t apm_timeout;

static void
apm_registerdev(struct isa_device *id)
{
	if (kdc_apm.kdc_isa)
		return;
	kdc_apm.kdc_state = DC_UNCONFIGURED;
	kdc_apm.kdc_unit = 0;
	kdc_apm.kdc_isa = id;
	dev_attach(&kdc_apm);
}

/* setup APM GDT discriptors */
static void
setup_apm_gdt(u_int code32_base, u_int code16_base, u_int data_base, u_int code_limit, u_int data_limit)
{
	/* setup 32bit code segment */
	gdt_segs[GAPMCODE32_SEL].ssd_base  = code32_base;
	gdt_segs[GAPMCODE32_SEL].ssd_limit = code_limit;

	/* setup 16bit code segment */
	gdt_segs[GAPMCODE16_SEL].ssd_base  = code16_base;
	gdt_segs[GAPMCODE16_SEL].ssd_limit = code_limit;

	/* setup data segment */
	gdt_segs[GAPMDATA_SEL  ].ssd_base  = data_base;
	gdt_segs[GAPMDATA_SEL  ].ssd_limit = data_limit;

	/* reflect these changes on physical GDT */
	ssdtosd(gdt_segs + GAPMCODE32_SEL, &gdt[GAPMCODE32_SEL].sd);
	ssdtosd(gdt_segs + GAPMCODE16_SEL, &gdt[GAPMCODE16_SEL].sd);
	ssdtosd(gdt_segs + GAPMDATA_SEL  , &gdt[GAPMDATA_SEL  ].sd);
}

/* 48bit far pointer */
static struct addr48 {
	u_long		offset;
	u_short		segment;
} apm_addr;

static int apm_errno;

inline
int
apm_int(u_long *eax, u_long *ebx, u_long *ecx)
{
	u_long cf;
	__asm __volatile("
		pushfl
		cli
		lcall	_apm_addr
		movl	$0, %3
		jnc	1f
		incl	%3
	1:
		popfl
		"
		: "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=D" (cf)
		: "0" (*eax),  "1" (*ebx),  "2" (*ecx)
		: "dx", "si", "memory"
		);
	apm_errno = ((*eax) >> 8) & 0xff;
	return cf;
}


/* enable/disable power management */
static int
apm_enable_disable_pm(struct apm_softc *sc, int enable)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_ENABLEDISABLEPM;

	if (sc->intversion >= INTVERSION(1, 1)) {
		ebx  = PMDV_ALLDEV;
	} else {
		ebx  = 0xffff;	/* APM version 1.0 only */
	}
	ecx  = enable;
	return apm_int(&eax, &ebx, &ecx);
}

/* Tell APM-BIOS that WE will do 1.1 and see what they say... */
static void
apm_driver_version(void)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_DRVVERSION;
	ebx  = 0x0;
	ecx  = 0x0101;
	if(!apm_int(&eax, &ebx, &ecx))
		apm_version = eax & 0xffff;
}

/* engage/disengage power management (APM 1.1 or later) */
static int
apm_engage_disengage_pm(struct apm_softc *sc, int engage)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_ENGAGEDISENGAGEPM;
	ebx = PMDV_ALLDEV;
	ecx = engage;
	return(apm_int(&eax, &ebx, &ecx));
}

/* get PM event */
static u_int
apm_getevent(struct apm_softc *sc)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_GETPMEVENT;

	ebx = 0;
	ecx = 0;
	if (apm_int(&eax, &ebx, &ecx))
		return PMEV_NOEVENT;

	return ebx & 0xffff;
}

/* suspend entire system */
static int
apm_suspend_system(struct apm_softc *sc)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_SETPWSTATE;
	ebx = PMDV_ALLDEV;
	ecx = PMST_SUSPEND;

	if (apm_int(&eax, &ebx, &ecx)) {
		printf("Entire system suspend failure: errcode = %ld\n",
			0xff & (eax >> 8));
		return 1;
	}
	return 0;
}

/* Display control */
/*
 * Experimental implementation: My laptop machine can't handle this function
 * If your laptop can control the display via APM, please inform me.
 *                            HOSOKAWA, Tatsumi <hosokawa@mt.cs.keio.ac.jp>
 */
static int
apm_display_off(void)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_SETPWSTATE;
	ebx = PMDV_2NDSTORAGE0;
	ecx = PMST_STANDBY;
	if (apm_int(&eax, &ebx, &ecx)) {
		printf("Display off failure: errcode = %ld\n",
			0xff & (eax >> 8));
		return 1;
	}

	return 0;
}

/* APM Battery low handler */
static void
apm_battery_low(struct apm_softc *sc)
{
	printf("\007\007 * * * BATTERY IS LOW * * * \007\007");
}

/* APM hook manager */
static struct apmhook *
apm_add_hook(struct apmhook **list, struct apmhook *ah)
{
	int s;
	struct apmhook *p, *prev;

#ifdef APM_DEBUG
	printf("Add hook \"%s\"\n", ah->ah_name);
#endif

	s = splhigh();
	if (ah == NULL) {
		panic("illegal apm_hook!");
	}
	prev = NULL;
	for (p = *list; p != NULL; prev = p, p = p->ah_next) {
		if (p->ah_order > ah->ah_order) {
			break;
		}
	}

	if (prev == NULL) {
		ah->ah_next = *list;
		*list = ah;
	} else {
		ah->ah_next = prev->ah_next;
		prev->ah_next = ah;
	}
	splx(s);
	return ah;
}

static void
apm_del_hook(struct apmhook **list, struct apmhook *ah)
{
	int s;
	struct apmhook *p, *prev;

	s = splhigh();
	prev = NULL;
	for (p = *list; p != NULL; prev = p, p = p->ah_next) {
		if (p == ah) {
			goto deleteit;
		}
	}
	panic("Tried to delete unregistered apm_hook.");
	goto nosuchnode;
deleteit:
	if (prev != NULL) {
		prev->ah_next = p->ah_next;
	} else {
		*list = p->ah_next;
	}
nosuchnode:
	splx(s);
}


/* APM driver calls some functions automatically */
static void
apm_execute_hook(struct apmhook *list)
{
	struct apmhook *p;

	for (p = list; p != NULL; p = p->ah_next) {
#ifdef APM_DEBUG
		printf("Execute APM hook \"%s.\"\n", p->ah_name);
#endif
		if ((*(p->ah_fun))(p->ah_arg)) {
			printf("Warning: APM hook \"%s\" failed", p->ah_name);
		}
	}
}


/* establish an apm hook */
struct apmhook *
apm_hook_establish(int apmh, struct apmhook *ah)
{
	if (apmh < 0 || apmh >= NAPM_HOOK)
		return NULL;

	return apm_add_hook(&hook[apmh], ah);
}

/* disestablish an apm hook */
void
apm_hook_disestablish(int apmh, struct apmhook *ah)
{
	if (apmh < 0 || apmh >= NAPM_HOOK)
		return;

	apm_del_hook(&hook[apmh], ah);
}


static struct timeval suspend_time;
static struct timeval diff_time;

static int
apm_default_resume(void *arg)
{
	int pl;
	u_int second, minute, hour;
	struct timeval resume_time, tmp_time;

	/* modified for adjkerntz */
	pl = splsoftclock();
	inittodr(0);			/* adjust time to RTC */
	microtime(&resume_time);
	tmp_time = time;		/* because 'time' is volatile */
	timevaladd(&tmp_time, &diff_time);
	time = tmp_time;
	splx(pl);
	second = resume_time.tv_sec - suspend_time.tv_sec;
	hour = second / 3600;
	second %= 3600;
	minute = second / 60;
	second %= 60;
	log(LOG_NOTICE, "resumed from suspended mode (slept %02d:%02d:%02d)\n",
		hour, minute, second);
	return 0;
}

static int
apm_default_suspend(void *arg)
{
	int	pl;

	pl = splsoftclock();
	microtime(&diff_time);
	inittodr(0);
	microtime(&suspend_time);
	timevalsub(&diff_time, &suspend_time);
	splx(pl);
	return 0;
}

static void apm_processevent(struct apm_softc *);

/*
 * Public interface to the suspend/resume:
 *
 * Execute suspend and resume hook before and after sleep, respectively.
 *
 */

void
apm_suspend(void)
{
	struct apm_softc *sc = &apm_softc;

	if (!sc)
		return;

	if (sc->initialized) {
		apm_execute_hook(hook[APM_HOOK_SUSPEND]);
		apm_suspend_system(sc);
		apm_processevent(sc);
	}
}

void
apm_resume(void)
{
	struct apm_softc *sc = &apm_softc;

	if (!sc)
		return;

	if (sc->initialized) {
		apm_execute_hook(hook[APM_HOOK_RESUME]);
	}
}


/* get APM information */
static int
apm_get_info(struct apm_softc *sc, apm_info_t aip)
{
	u_long eax, ebx, ecx;

	eax = (APM_BIOS << 8) | APM_GETPWSTATUS;
	ebx = PMDV_ALLDEV;
	ecx = 0;

	if (apm_int(&eax, &ebx, &ecx))
		return 1;

	aip->ai_acline    = (ebx >> 8) & 0xff;
	aip->ai_batt_stat = ebx & 0xff;
	aip->ai_batt_life = ecx & 0xff;
	aip->ai_major     = (u_int)sc->majorversion;
	aip->ai_minor     = (u_int)sc->minorversion;
	aip->ai_status    = (u_int)sc->active;

	return 0;
}


/* inform APM BIOS that CPU is idle */
void
apm_cpu_idle(void)
{
	struct apm_softc *sc = &apm_softc;

	if (sc->active) {
		u_long eax, ebx, ecx;

		eax = (APM_BIOS <<8) | APM_CPUIDLE;
		ecx = ebx = 0;
		apm_int(&eax, &ebx, &ecx);
	}
	/*
	 * Some APM implementation halts CPU in BIOS, whenever
	 * "CPU-idle" function are invoked, but swtch() of
	 * FreeBSD halts CPU, therefore, CPU is halted twice
	 * in the sched loop. It makes the interrupt latency
	 * terribly long and be able to cause a serious problem
	 * in interrupt processing. We prevent it by removing
	 * "hlt" operation from swtch() and managed it under
	 * APM driver.
	 */
	if (!sc->active || sc->always_halt_cpu) {
		__asm("hlt");	/* wait for interrupt */
	}
}

/* inform APM BIOS that CPU is busy */
void
apm_cpu_busy(void)
{
	struct apm_softc *sc = &apm_softc;

	/*
	 * The APM specification says this is only necessary if your BIOS
	 * slows down the processor in the idle task, otherwise it's not
	 * necessary.
	 */
	if (sc->slow_idle_cpu && sc->active) {
		u_long eax, ebx, ecx;

		eax = (APM_BIOS <<8) | APM_CPUBUSY;
		ecx = ebx = 0;
		apm_int(&eax, &ebx, &ecx);
	}
}


/*
 * APM timeout routine:
 *
 * This routine is automatically called by timer once per second.
 */

static void
apm_timeout(void *arg)
{
	struct apm_softc *sc = arg;

	apm_processevent(sc);
	if (sc->active == 1) {
		timeout(apm_timeout, (void *)sc, hz - 1 );  /* More than 1 Hz */
	}
}

/* enable APM BIOS */
static void
apm_event_enable(struct apm_softc *sc)
{
#ifdef APM_DEBUG
	printf("called apm_event_enable()\n");
#endif
	if (sc->initialized) {
		sc->active = 1;
		apm_timeout(sc);
	}
}

/* disable APM BIOS */
static void
apm_event_disable(struct apm_softc *sc)
{
#ifdef APM_DEBUG
	printf("called apm_event_disable()\n");
#endif
	if (sc->initialized) {
		untimeout(apm_timeout, NULL);
		sc->active = 0;
	}
}

/* halt CPU in scheduling loop */
static void
apm_halt_cpu(struct apm_softc *sc)
{
	if (sc->initialized) {
		sc->always_halt_cpu = 1;
	}
}

/* don't halt CPU in scheduling loop */
static void
apm_not_halt_cpu(struct apm_softc *sc)
{
	if (sc->initialized) {
		sc->always_halt_cpu = 0;
	}
}

/* device driver definitions */
static int apmprobe (struct isa_device *);
static int apmattach(struct isa_device *);
struct isa_driver apmdriver = {
	apmprobe, apmattach, "apm" };

/*
 * probe APM (dummy):
 *
 * APM probing routine is placed on locore.s and apm_init.S because
 * this process forces the CPU to turn to real mode or V86 mode.
 * Current version uses real mode, but on future version, we want
 * to use V86 mode in APM initialization.
 */

static int
apmprobe(struct isa_device *dvp)
{
	if ( dvp->id_unit > 0 ) {
		printf("apm: Only one APM driver supported.\n");
		return 0;
	}
	apm_registerdev(dvp);
	switch (apm_version) {
	case APMINI_CANTFIND:
		/* silent */
		return 0;
	case APMINI_NOT32BIT:
		printf("apm: 32bit connection is not supported.\n");
		return 0;
	case APMINI_CONNECTERR:
		printf("apm: 32-bit connection error.\n");
		return 0;
	}
#ifdef APM_BROKEN_STATCLOCK
	statclock_disable = 1;
#endif

	return -1;
}


/* Process APM event */
static void
apm_processevent(struct apm_softc *sc)
{
	int apm_event;

#ifdef APM_DEBUG
#  define OPMEV_DEBUGMESSAGE(symbol) case symbol: \
	printf("Received APM Event: " #symbol "\n");
#else
#  define OPMEV_DEBUGMESSAGE(symbol) case symbol:
#endif
	do {
		apm_event = apm_getevent(sc);
		switch (apm_event) {
		    OPMEV_DEBUGMESSAGE(PMEV_STANDBYREQ);
			apm_suspend();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_SUSPENDREQ);
			apm_suspend();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_USERSUSPENDREQ);
			apm_suspend();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_CRITSUSPEND);
			apm_suspend();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_NORMRESUME);
			apm_resume();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_CRITRESUME);
			apm_resume();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_STANDBYRESUME);
			apm_resume();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_BATTERYLOW);
			apm_battery_low(sc);
			apm_suspend();
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_POWERSTATECHANGE);
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_UPDATETIME);
			inittodr(0);	/* adjust time to RTC */
			break;
		    OPMEV_DEBUGMESSAGE(PMEV_NOEVENT);
			break;
		    default:
			printf("Unknown Original APM Event 0x%x\n", apm_event);
			    break;
		}
	} while (apm_event != PMEV_NOEVENT);
}

/*
 * Attach APM:
 *
 * Initialize APM driver (APM BIOS itself has been initialized in locore.s)
 */

static int
apmattach(struct isa_device *dvp)
{
#define APM_KERNBASE	KERNBASE
	struct apm_softc	*sc = &apm_softc;
#ifdef APM_DSVALUE_BUG
	caddr_t apm_bios_work;

	apm_bios_work = (caddr_t)malloc(apm_ds_limit, M_DEVBUF, M_NOWAIT);
	bcopy((caddr_t)((apm_ds_base << 4) + APM_KERNBASE), apm_bios_work,
		apm_ds_limit);
#endif /* APM_DSVALUE_BUG */

	sc->initialized = 0;

	/* Must be externally enabled */
	sc->active = 0;

	/* setup APM parameters */
	sc->cs16_base = (apm_cs32_base << 4) + APM_KERNBASE;
	sc->cs32_base = (apm_cs16_base << 4) + APM_KERNBASE;
	sc->ds_base = (apm_ds_base << 4) + APM_KERNBASE;
	sc->cs_limit = apm_cs_limit;
	sc->ds_limit = apm_ds_limit;
	sc->cs_entry = apm_cs_entry;

#ifdef APM_DSVALUE_BUG
	sc->ds_base = (u_int)apm_bios_work;
#endif /* APM_DSVALUE_BUG */

	/* Always call HLT in idle loop */
	sc->always_halt_cpu = 1;

	sc->slow_idle_cpu = ((apm_flags & APM_CPUIDLE_SLOW) != 0);
	sc->disabled = ((apm_flags & APM_DISABLED) != 0);
	sc->disengaged = ((apm_flags & APM_DISENGAGED) != 0);

	/* print bootstrap messages */
#ifdef APM_DEBUG
	printf("apm: APM BIOS version %04x\n",  apm_version);
	printf("apm: Code32 0x%08x, Code16 0x%08x, Data 0x%08x\n",
		sc->cs32_base, sc->cs16_base, sc->ds_base);
	printf("apm: Code entry 0x%08x, Idling CPU %s, Management %s\n",
		sc->cs_entry, is_enabled(sc->slow_idle_cpu),
		is_enabled(!sc->disabled));
	printf("apm: CS_limit=%x, DS_limit=%x\n", sc->cs_limit, sc->ds_limit);
#endif /* APM_DEBUG */

#if 0
	/* Workaround for some buggy APM BIOS implementations */
	sc->cs_limit = 0xffff;
	sc->ds_limit = 0xffff;
#endif

	/* setup GDT */
	setup_apm_gdt(sc->cs32_base, sc->cs16_base, sc->ds_base,
			sc->cs_limit, sc->ds_limit);

	/* setup entry point 48bit pointer */
	apm_addr.segment = GSEL(GAPMCODE32_SEL, SEL_KPL);
	apm_addr.offset  = sc->cs_entry;

#ifdef FORCE_APM10
	apm_version = 0x100;
	sc->majorversion = 1;
	sc->minorversion = 0;
	sc->intversion = INTVERSION(sc->majorversion, sc->minorversion);
	printf("apm: running in APM 1.0 compatible mode\n");
	kcd_apm.kdc_description =
		"Advanced Power Management BIOS (1.0 compatability mode)",
#else
	/* Try to kick bios into 1.1 or greater mode */
	apm_driver_version();
	sc->minorversion = ((apm_version & 0x00f0) >>  4) * 10 +
			((apm_version & 0x000f) >> 0);
	sc->majorversion = ((apm_version & 0xf000) >> 12) * 10 +
			((apm_version & 0x0f00) >> 8);

	sc->intversion = INTVERSION(sc->majorversion, sc->minorversion);

	if (sc->intversion >= INTVERSION(1, 1)) {
#ifdef APM_DEBUG
		printf("apm: Engaged control %s\n", is_enabled(!sc->disengaged));
#endif
	}

	printf("apm: found APM BIOS version %d.%d\n",
		sc->majorversion, sc->minorversion);
#endif /* FORCE_APM10 */

#ifdef APM_DEBUG
	printf("apm: Slow Idling CPU %s\n", is_enabled(sc->slow_idle_cpu));
#endif

	/* enable power management */
	if (sc->disabled) {
		if (apm_enable_disable_pm(sc, 1)) {
#ifdef APM_DEBUG
			printf("apm: *Warning* enable function failed! [%x]\n",
				apm_errno);
#endif
		}
	}

	/* engage power managment (APM 1.1 or later) */
	if (sc->intversion >= INTVERSION(1, 1) && sc->disengaged) {
		if (apm_engage_disengage_pm(sc, 1)) {
#ifdef APM_DEBUG
			printf("apm: *Warning* engage function failed err=[%x]",
				apm_errno);
			printf(" (Docked or using external power?).\n");
#endif
		}
	}

        /* default suspend hook */
        sc->sc_suspend.ah_fun = apm_default_suspend;
        sc->sc_suspend.ah_arg = sc;
        sc->sc_suspend.ah_name = "default suspend";
        sc->sc_suspend.ah_order = APM_MAX_ORDER;

        /* default resume hook */
        sc->sc_resume.ah_fun = apm_default_resume;
        sc->sc_resume.ah_arg = sc;
        sc->sc_resume.ah_name = "default resume";
        sc->sc_resume.ah_order = APM_MIN_ORDER;

        apm_hook_establish(APM_HOOK_SUSPEND, &sc->sc_suspend);
        apm_hook_establish(APM_HOOK_RESUME , &sc->sc_resume);

	apm_event_enable(sc);
	kdc_apm.kdc_state = DC_IDLE;

	sc->initialized = 1;

	return 0;
}

int
apmopen(dev_t dev, int flag, int fmt, struct proc *p)
{
	struct apm_softc *sc = &apm_softc;

	if (minor(dev) != 0 || !sc->initialized)
		return (ENXIO);

	return 0;
}

int
apmclose(dev_t dev, int flag, int fmt, struct proc *p)
{
	return 0;
}

int
apmioctl(dev_t dev, int cmd, caddr_t addr, int flag, struct proc *p)
{
	struct apm_softc *sc = &apm_softc;
	int error = 0;

	if (minor(dev) != 0 || !sc->initialized)
		return (ENXIO);
#ifdef APM_DEBUG
	printf("APM ioctl: cmd = 0x%x\n", cmd);
#endif
	switch (cmd) {
	case APMIO_SUSPEND:
		if ( sc->active) {
			apm_suspend();
		} else {
			error = EINVAL;
		}
		break;
	case APMIO_GETINFO:
		if (apm_get_info(sc, (apm_info_t)addr)) {
			error = ENXIO;
		}
		break;
	case APMIO_ENABLE:
		kdc_apm.kdc_state = DC_BUSY;
		apm_event_enable(sc);
		break;
	case APMIO_DISABLE:
		kdc_apm.kdc_state = DC_IDLE;
		apm_event_disable(sc);
		break;
	case APMIO_HALTCPU:
		apm_halt_cpu(sc);
		break;
	case APMIO_NOTHALTCPU:
		apm_not_halt_cpu(sc);
		break;
	case APMIO_DISPLAYOFF:
		if (apm_display_off()) {
			error = ENXIO;
		}
		break;
	default:
		error = EINVAL;
		break;
	}
	return error;
}

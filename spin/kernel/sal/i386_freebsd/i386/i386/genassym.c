/*-
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	from: @(#)genassym.c	5.11 (Berkeley) 5/10/91
 *	$Id: genassym.c,v 1.2 1997/01/31 15:59:42 mef Exp $
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
/* XXX This is just real ugly to have to do, but it is what you have to do! */
#ifndef	NFS
#define	NFS
#include <sys/mount.h>
#undef	NFS
#else
#include <sys/mount.h>
#endif
#include <sys/mbuf.h>
#include <sys/msgbuf.h>
#include <machine/cpu.h>
#include <machine/trap.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/bootinfo.h>
#include <sys/syscall.h>
#include <vm/vm.h>
#include <sys/user.h>
#include <net/if.h>
#include <netinet/in.h>
#include <nfs/nfsv2.h>
#include <nfs/nfsdiskless.h>

int
main()
{
	struct proc *p = (struct proc *)0;
	struct vmmeter *vm = (struct vmmeter *)0;
	struct user *up = (struct user *)0;
	struct rusage *rup = (struct rusage *)0;
	struct uprof *uprof = (struct uprof *)0;
	struct vmspace *vms = (struct vmspace *)0;
	struct pcb *pcb = (struct pcb *)0;
	struct trapframe *tf = (struct trapframe *)0;
	struct sigframe *sigf = (struct sigframe *)0;
	struct bootinfo *bootinfo = (struct bootinfo *)0;

	printf("#define\tUDOT_SZ %d\n", sizeof(struct user));
	printf("#define\tP_FORW %#lx\n", &p->p_forw);
	printf("#define\tP_BACK %#lx\n", &p->p_back);
	printf("#define\tP_VMSPACE %#lx\n", &p->p_vmspace);
	printf("#define\tVM_PMAP %#lx\n", &vms->vm_pmap);
	printf("#define\tP_ADDR %#lx\n", &p->p_addr);
	printf("#define\tP_PRI %#lx\n", &p->p_priority);
	printf("#define\tP_RTPRIO_TYPE %#lx\n", &p->p_rtprio.type);
	printf("#define\tP_RTPRIO_PRIO %#lx\n", &p->p_rtprio.prio);
	printf("#define\tP_STAT %#lx\n", &p->p_stat);
	printf("#define\tP_WCHAN %#lx\n", &p->p_wchan);
	printf("#define\tP_FLAG %#lx\n", &p->p_flag);
	printf("#define\tP_PID %#lx\n", &p->p_pid);
	printf("#define\tSSLEEP %d\n", SSLEEP);
	printf("#define\tSRUN %d\n", SRUN);
	printf("#define\tV_TRAP %#lx\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %#lx\n", &vm->v_syscall);
	printf("#define\tV_INTR %#lx\n", &vm->v_intr);
	printf("#define\tUPAGES %d\n", UPAGES);
	printf("#define\tCLSIZE %d\n", CLSIZE);
	printf("#define\tNBPG %d\n", NBPG);
	printf("#define\tNPTEPG %d\n", NPTEPG);
	printf("#define\tPDESIZE %d\n", PDESIZE);
	printf("#define\tPTESIZE %d\n", PTESIZE);
	printf("#define\tNKPDE %d\n", NKPDE);
	printf("#define\tNKPT %d\n", NKPT);
	printf("#define\tKPTDI 0x%x\n", KPTDI);
	printf("#define\tKSTKPTDI 0x%x\n", KSTKPTDI);
	printf("#define\tKSTKPTEOFF 0x%x\n", KSTKPTEOFF);
	printf("#define\tPTDPTDI 0x%x\n", PTDPTDI);
	printf("#define\tAPTDPTDI 0x%x\n", APTDPTDI);
	printf("#define\tPGSHIFT %d\n", PGSHIFT);
	printf("#define\tPDRSHIFT %d\n", PDRSHIFT);
	printf("#define\tSYSPTSIZE %d\n", SYSPTSIZE);
	printf("#define\tUSRPTSIZE %d\n", USRPTSIZE);
	printf("#define\tUSRIOSIZE %d\n", USRIOSIZE);
#ifdef SYSVSHM
	printf("#define\tSHMMAXPGS %d\n", SHMMAXPGS);
#endif
	printf("#define\tUSRSTACK 0x%lx\n", USRSTACK);
	printf("#define\tVM_MAXUSER_ADDRESS 0x%lx\n", VM_MAXUSER_ADDRESS);
	printf("#define\tKERNBASE 0x%x\n", KERNBASE);
	printf("#define\tMSGBUFPTECNT %d\n", btoc(sizeof (struct msgbuf)));
	printf("#define\tMCLBYTES %d\n", MCLBYTES);
	printf("#define\tPCB_LINK %#lx\n", &pcb->pcb_tss.tss_link);
	printf("#define\tPCB_ESP0 %#lx\n", &pcb->pcb_tss.tss_esp0);
	printf("#define\tPCB_SS0 %#lx\n", &pcb->pcb_tss.tss_ss0);
	printf("#define\tPCB_ESP1 %#lx\n", &pcb->pcb_tss.tss_esp1);
	printf("#define\tPCB_SS1 %#lx\n", &pcb->pcb_tss.tss_ss1);
	printf("#define\tPCB_ESP2 %#lx\n", &pcb->pcb_tss.tss_esp2);
	printf("#define\tPCB_SS2 %#lx\n", &pcb->pcb_tss.tss_ss2);
	printf("#define\tPCB_CR3 %#lx\n", &pcb->pcb_tss.tss_cr3);
	printf("#define\tPCB_EIP %#lx\n", &pcb->pcb_tss.tss_eip);
	printf("#define\tPCB_EFLAGS %#lx\n", &pcb->pcb_tss.tss_eflags);
	printf("#define\tPCB_EAX %#lx\n", &pcb->pcb_tss.tss_eax);
	printf("#define\tPCB_ECX %#lx\n", &pcb->pcb_tss.tss_ecx);
	printf("#define\tPCB_EDX %#lx\n", &pcb->pcb_tss.tss_edx);
	printf("#define\tPCB_EBX %#lx\n", &pcb->pcb_tss.tss_ebx);
	printf("#define\tPCB_ESP %#lx\n", &pcb->pcb_tss.tss_esp);
	printf("#define\tPCB_EBP %#lx\n", &pcb->pcb_tss.tss_ebp);
	printf("#define\tPCB_ESI %#lx\n", &pcb->pcb_tss.tss_esi);
	printf("#define\tPCB_EDI %#lx\n", &pcb->pcb_tss.tss_edi);
	printf("#define\tPCB_ES %#lx\n", &pcb->pcb_tss.tss_es);
	printf("#define\tPCB_CS %#lx\n", &pcb->pcb_tss.tss_cs);
	printf("#define\tPCB_SS %#lx\n", &pcb->pcb_tss.tss_ss);
	printf("#define\tPCB_DS %#lx\n", &pcb->pcb_tss.tss_ds);
	printf("#define\tPCB_FS %#lx\n", &pcb->pcb_tss.tss_fs);
	printf("#define\tPCB_GS %#lx\n", &pcb->pcb_tss.tss_gs);
	printf("#define\tPCB_LDT %#lx\n", &pcb->pcb_tss.tss_ldt);
	printf("#define\tPCB_USERLDT %#lx\n", &pcb->pcb_ldt);
	printf("#define\tPCB_IOOPT %#lx\n", &pcb->pcb_tss.tss_ioopt);
	printf("#define\tU_PROF %#lx\n", &up->u_stats.p_prof);
	printf("#define\tU_PROFSCALE %#lx\n", &up->u_stats.p_prof.pr_scale);
	printf("#define\tPR_BASE %#lx\n", &uprof->pr_base);
	printf("#define\tPR_SIZE %#lx\n", &uprof->pr_size);
	printf("#define\tPR_OFF %#lx\n", &uprof->pr_off);
	printf("#define\tPR_SCALE %#lx\n", &uprof->pr_scale);
	printf("#define\tRU_MINFLT %#lx\n", &rup->ru_minflt);
	printf("#define\tPCB_FLAGS %#lx\n", &pcb->pcb_flags);
	printf("#define\tPCB_SAVEFPU %#lx\n", &pcb->pcb_savefpu);
	printf("#define\tFP_USESEMC %d\n", FP_USESEMC);
	printf("#define\tPCB_SAVEEMC %#lx\n", &pcb->pcb_saveemc);
	printf("#define\tPCB_INL %#lx\n", &pcb->pcb_inl);
	printf("#define\tPCB_ONFAULT %#lx\n", &pcb->pcb_onfault);

#ifdef SPIN
        /* we would like all stack frames to look alike */
	printf("#define\tTF_VEC %#lx\n", &tf->tf_vec);
	printf("#define\tTF_PPL %#lx\n", &tf->tf_ppl);	
#endif /* SPIN */
	printf("#define\tTF_ES %#lx\n", &tf->tf_es);
	printf("#define\tTF_DS %#lx\n", &tf->tf_ds);
	printf("#define\tTF_EDI %#lx\n", &tf->tf_edi);
	printf("#define\tTF_ESI %#lx\n", &tf->tf_esi);
	printf("#define\tTF_EBP %#lx\n", &tf->tf_ebp);
	printf("#define\tTF_ISP %#lx\n", &tf->tf_isp);
	printf("#define\tTF_EBX %#lx\n", &tf->tf_ebx);
	printf("#define\tTF_EDX %#lx\n", &tf->tf_edx);
	printf("#define\tTF_ECX %#lx\n", &tf->tf_ecx);
	printf("#define\tTF_EAX %#lx\n", &tf->tf_eax);
	printf("#define\tTF_TRAPNO %#lx\n", &tf->tf_trapno);
	printf("#define\tTF_ERR %#lx\n", &tf->tf_err);
	printf("#define\tTF_EIP %#lx\n", &tf->tf_eip);
	printf("#define\tTF_CS %#lx\n", &tf->tf_cs);
	printf("#define\tTF_EFLAGS %#lx\n", &tf->tf_eflags);
	printf("#define\tTF_ESP %#lx\n", &tf->tf_esp);
	printf("#define\tTF_SS %#lx\n", &tf->tf_ss);
	printf("#define\tTF_SIZE %#lx\n", sizeof(struct trapframe));

	printf("#define\tSIGF_SIGNUM %#lx\n", &sigf->sf_signum);
	printf("#define\tSIGF_CODE %#lx\n", &sigf->sf_code);
	printf("#define\tSIGF_SCP %#lx\n", &sigf->sf_scp);
	printf("#define\tSIGF_HANDLER %#lx\n", &sigf->sf_handler);
	printf("#define\tSIGF_SC %#lx\n", &sigf->sf_sc);

	printf("#define\tB_READ %d\n", B_READ);
	printf("#define\tENOENT %d\n", ENOENT);
	printf("#define\tEFAULT %d\n", EFAULT);
	printf("#define\tENAMETOOLONG %d\n", ENAMETOOLONG);
	printf("#define\tMAXPATHLEN %d\n", MAXPATHLEN);

	printf("#define\tBOOTINFO_SIZE %d\n", sizeof *bootinfo);
	printf("#define\tBI_VERSION %#lx\n", &bootinfo->bi_version);
	printf("#define\tBI_KERNELNAME %#lx\n", &bootinfo->bi_kernelname);
	printf("#define\tBI_NFS_DISKLESS %#lx\n", &bootinfo->bi_nfs_diskless);
	printf("#define\tBI_ENDCOMMON %#lx\n", &bootinfo->bi_endcommon);
	printf("#define\tNFSDISKLESS_SIZE %d\n", sizeof(struct nfs_diskless));
	printf("#define\tBI_SIZE %#lx\n", &bootinfo->bi_size);
	printf("#define\tBI_SYMTAB %#lx\n", &bootinfo->bi_symtab);
	printf("#define\tBI_ESYMTAB %#lx\n", &bootinfo->bi_esymtab);

	return (0);
}

/*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 */
/*
 * HISTORY
 * 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 *	Added code by Przemek to support boot flags on PCI boxes.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Rename run_program salhook_main_program
 *	Moved incredible_pal_hack() here.
 *	Renamed NumMallocPages to salhook_mallocpages and brought it
 *	back here
 *
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Moved SpinNumMallocPages to sal/TARGET/standalone/SAL.c and
 *	renamed it to NumMallocPages.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Moved nonpreemptible region setup support to machine/alpha/sal.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Fixed to use REAL physical addresses and print less as
 *	per pardy's request.
 *
 * 06-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Clean up and added hack to put memory constants in one place
 *
 * 23-Jan-96  David Dion (ddion) at the University of Washington
 *	Bumped up SPINPHYSFREE and compensated in SPINTRACED for Space
 *	allocation.
 * 
 * ??-Jan-95  David Becker (becker) at the University of Washington
 *    Removed newkernel and call (they weren't used)
 *	Cchanged to cnopen() for console open and added nifty
 *	comments at the top
 *
 * 29-Dec-95  Marc Fiuczynski (mef) at the University of Washington
 *	Increased SPINTRACED size from 16MB to 20MB.
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *    Removed newkernel and call (they weren't used)
 *
 * 24-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to install a SPIN custom pal entry to set the ptbr without
 *	having to do an insane thread context switch.
 *
 * 30-Jul-95  Stefan Savage (savage) at the University of Washington
 *	Updated to give extra memory to PhysFreeMem
 *
 * 28-Apr-28  David Becker (becker) at the University of Washington
 *	Created
 *
 */


typedef unsigned long vm_offset_t;
typedef unsigned long vm_size_t;
typedef unsigned long spl_t;
#define PGSHIFT             13
#define UNITY_BASE  ((~0L) << (4 * PGSHIFT - 10))
#define PHYS_TO_KSEG(addr) ((vm_offset_t)(addr) + UNITY_BASE)
#define PAGE_SIZE 8192


/*
 * These variables control how much memory each pool gets.
 */

#ifdef REFCOUNT
int InitSpinNumRefCountPages = 400;
int PermSpinNumRefCountPages = 100;
#else
int InitSpinNumRefCountPages = 0;
int PermSpinNumRefCountPages = 0;
#endif

int salhook_mallocpages = 3650; /* # of untraced pages */

int SpinNumPhysFreePages = 850; /* # of SPIN physical pages */
int SpinNumTracedPages;          /* Set dynamically to use all excess memory */
int SpinNumTracedPagesMin = 1024;/* min # of traced pages */

extern vm_offset_t virtual_avail, virtual_end;

/*
 * variables in SAL that tell us where memory is
 */
extern vm_offset_t	avail_start, avail_end;

/* These are <*EXTERNAL*> in RTMemExternal.i3 and PhysFreeMemExtern.i3 */
vm_offset_t     refcount_start,refcount_end;
vm_offset_t     prefcount_start,prefcount_end;
vm_offset_t     irefcount_start,irefcount_end;
vm_offset_t     pirefcount_start,pirefcount_end;
vm_offset_t     traced_start,traced_end,traced_current;
vm_offset_t     ptraced_start,ptraced_end,ptraced_current;
vm_size_t       traced_size, refcount_size, irefcount_size;
vm_offset_t     pmem_start,pmem_end;
vm_size_t	pmem_size;

#define DB /* printf  */


/*
 * Macros to convert bytes to pages and maegabytes
 */
#define PAGES(x)	((x)/PAGE_SIZE)
#define MEGABYTES(x)	((x)/(1024*1024))

salhook_main_program()
{
	extern vm_offset_t kmembase;	/* start of untraced mem */
	extern vm_offset_t kmemlimit;	/* end of  untraced mem  */
	vm_size_t kmemsize; /* size of untraced heap */

	/* zero all that memory (does not include untraced heap) */
	DB("bzero %dMB..",MEGABYTES(avail_end- avail_start));
        bzero(PHYS_TO_KSEG(avail_start), avail_end-avail_start);
	DB("\n");
	
        /* 
	 * Report size of traced heap
         */
	SpinNumTracedPages = ((avail_end-avail_start)/PAGE_SIZE)
		- SpinNumPhysFreePages;
	if (SpinNumTracedPages < SpinNumTracedPagesMin) {
	   if (SpinNumPhysFreePages <=  
	      (SpinNumTracedPagesMin - SpinNumTracedPages)) {
	      printf("      Traced heap below minimum.  No recovery possible.\n");
	      printf("      Don't expect this to work.\n");
           } else {
	      printf("      Traced heap below min. Overriding SpinNumPhysFreePages directive.\n"); 
	      printf("      Moving %d pages to traced heap.\n",
		     (SpinNumTracedPagesMin - SpinNumTracedPages));
	      SpinNumPhysFreePages -= (SpinNumTracedPagesMin
				       - SpinNumTracedPages);
	      SpinNumTracedPages += (SpinNumTracedPagesMin
				       - SpinNumTracedPages);
           }
        }

	/*
	 * Report size of untraced heap
	 */
	kmemsize = kmemlimit-kmembase;
        printf("SPIN: Untraced Heap(%ld.%ld%ld MB), ",
	       MEGABYTES(kmemsize), MEGABYTES(10*kmemsize)%10,
	       MEGABYTES(100*kmemsize)%10);
	DB("      mapped at 0x%lx to 0x%lx\n",kmembase, kmemlimit);

        refcount_size = PAGE_SIZE * PermSpinNumRefCountPages;
	if(refcount_size != 0) {
	  prefcount_start = avail_start;
	  prefcount_end = prefcount_start+refcount_size;
	  refcount_start = vmap(prefcount_start, refcount_size);  
	  refcount_end = refcount_start + refcount_size;
	  avail_start += refcount_size;
	  printf("Refcount Area (%ld.%ld%ld MB), ",
		 MEGABYTES(refcount_size), MEGABYTES(10*refcount_size)%10,
		 MEGABYTES(100*refcount_size)%10);
	}
	
        traced_size = PAGE_SIZE * SpinNumTracedPages;
        ptraced_start = avail_start;
        ptraced_end = ptraced_start+traced_size;
	ptraced_current = ptraced_start;
        printf("Traced Heap(%ld.%ld%ld MB), ",
	       MEGABYTES(traced_size), MEGABYTES(10*traced_size)%10,
	       MEGABYTES(100*traced_size)%10);

	traced_start = vmap(ptraced_start, traced_size);  

	/* 
         * replace the above line with:
	 *	traced_start =  PHYS_TO_KSEG(ptraced_start);
	 * to go back to the unmapped heap
	 */
	traced_end = traced_start + traced_size;
	traced_current = traced_start;
        avail_start += traced_size;
	
        irefcount_size = PAGE_SIZE * InitSpinNumRefCountPages;
	if(irefcount_size != 0) {
	  pirefcount_start = avail_start;
	  pirefcount_end = pirefcount_start+irefcount_size;
	  irefcount_start = vmap(pirefcount_start, irefcount_size);  
	  irefcount_end = irefcount_start + irefcount_size;
	  avail_start += irefcount_size;
	  printf("Initial Refcount Area (%ld.%ld%ld MB), ",
		 MEGABYTES(irefcount_size), MEGABYTES(10*irefcount_size)%10,
		 MEGABYTES(100*irefcount_size)%10);
	}
	
        /* 
	 * Report size of basic physical pages
         */
        pmem_size = PAGE_SIZE * SpinNumPhysFreePages;
        pmem_start = avail_start; 
        pmem_end = pmem_start+pmem_size;
        printf("PhysFreeMem(%ld.%ld%ld MB)\n",
	       MEGABYTES(pmem_size), MEGABYTES(10*pmem_size)%10,
	       MEGABYTES(100*pmem_size)%10);
	DB("      mapped at 0x%lx to 0x%lx\n",pmem_start, pmem_end);
	avail_start += pmem_size;

	printf("SPIN: Traced heap mapped from 0x%lx to 0x%lx\n",
	       traced_start, traced_end);

	/* add new ptbr pointer modification trap */
	incredible_pal_hack();
	
	/* FIXME: will enable when more code merged in (Przemek)
	cycleclock();
	*/

	spin_start();
}

/* 
 * installs a PAL handler that sets the ptbr.
 * OSF PALcode does not contain any entrypoints
 * that allow one to set the ptbr directly. The
 * only way to do so is to do a heavyweight
 * swap context operation with a fake thread
 * struct. The whole operation is too costly.
 *
 * XXX the pal entry has to be a privileged one - egs
 */
static
incredible_pal_hack() {
    int palhack[] = {
	0x77FF0000,     /* hw_mtpr $31,0x00 */
	0x4A01B737,     /* sll $16,0x0d,$23 */
	0x6718009D,     /* hw_mfpr/p $24,$29 */
	0x46E03401,     /* or $23,0x01,$1 */
	0x47010297,     /* cmovlbs $24,$1,$23 */
	0x76F7009C,     /* hw_mtpr/p $23,$28 */
	0x7BFF8000,     /* hw_rei */
    };
    int *paloverwrite = (int*) PHYS_TO_KSEG(0x63c00);
    int *newpal = palhack;

    /*
     * XXX I am overwriting PAL entry 0xb0, which is
     * callable from unprivileged code. The entry point
     * should be moved to a privileged one post sosp.
     */
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    *paloverwrite++ = *newpal++;
    /* don't forget we only have 64 bytes (16 instrs) at our disposal. */
    mb();
}


void *boot_stack_bottom; 

#if 0
void *spin_stack_bottom; 
void *spin_stack_top;


void
set_spin_bottom(void)
{
	spin_stack_bottom = GetSP();
	/*
	printf ("spin_stack_bottom:: 0x%lx\n", spin_stack_bottom);
	*/
}

void
set_spin_top(void) 
{
	spin_stack_top = GetSP();
	/*
	printf ("spin_stack_top:: 0x%lx\n", spin_stack_top);
	*/
}
#endif

/******************************************************************************
 *
 * BOOT FLAGS
 *
 *****************************************************************************/

/*
 * Layout of flags:
 *       {osf-flags}{'#'{spin-single-char-flags}{' 'spin-word-argument}}
 *
 * - First go original osf flags, they will not be passed to SPIN and can
 *   be accessed by SPIN through a call to prom_getnev.
 * - '#' denotes the end of osf flags and beginning of SPIN flags, it will
 *   not be passed to SPIN.
 * - '#' is followed by a set of single character flags with no space between
 *   them and between '#' and the first flag.
 * - These are followed by a set of flags separated by words.
 * - All one character flags are translated through "flag_translation" array
 *   below.  If no translation is found, the original character is passed on.
 * - Multi-character flags are not interpretted and just passed on.
 * - All flags are packed into argv/argc data structure just like in 
 *   any C program.
 * - argv[0] will contain "spin".
 *
 * The rules are the way they for the following reasons:
 * - We want to separate SPIN and OSF flags so that they can be independently
 *   modified and still be correctly interpreted.  They only shared information
 *   is the meaning of '#'.  In the unlikely case that OSF decides to use
 *   '#' as a legal flag the character must be changed.
 * - We want to save space.  Currently there are only 23 characters available
 *   for flags.  Hence all the translation.
 *
 * Warning:
 *   Flags, which start with "@M3" are destined for M3 runtime and will be
 *   removed by it (they are invisible after the runtime comes up).  This
 *   is SRC M3 semantics and we preserve it.
 */

/*
 * Current set of translations:
 */
struct ftrans {
  char c;
  char *str;
} flag_translation[] = { { 'b', "-noboot" },
			 { 'c', "@M3nogc" },
			 { 'g', "@M3nogenerational" },
			 { 'z', "@M3noincremental" }, /* FIXME: i */
			 { 'v', "@M3novm" },
			 { 'p', "@M3paranoid" }
			 };

/*
 * Count the number of words in the flag string after the X flag.
 * Set cp pass the first '#' character
 */
int count_flag_words (char **cp)
{
  int n_words = 0;
  int in_word = 0;
  int x_found = 0;
  int in_x = 0;
  char *ptr = *cp;
  char c;

  /* iterate over the whole string */
  do {
    c = *ptr++;

    if(x_found) {
      /* already in spin flags */
      if(c == ' ' || c == '\000') {
	if(in_word) {
	  /* end of a word */
	  n_words++;
	  in_word = 0;
	} else if (in_x) {
	  /* found the end of continuous flags */
	  in_x = 0;
	}
      } else {
	if(in_x) {
	  /* count each character as a word */
	  n_words++;
	} else {
	  /* found new separate word */
	  in_word = 1;
	} 
      }
    } else {
      /* skip osf flags */
      if(c == '#') {
	/* offset to spin flags */
	x_found = 1;

	/* set the flag pointer to point to spin flags */
	*cp = ptr;

	/* we are in continuous set of flags after '#' */
/* FIXME: diabled because boot flags are processed differently on PCI boxes
	in_x = 1;
*/
      }
    }
  } while (c);

  return n_words;
}

/*
 * return a copy of a translated flag or NULL
 */
char *translate_flag(char c)
{
  char *word, *str;
  int i, size;

  word = 0;
  for(i = 0; i < ((sizeof(flag_translation)/sizeof(struct ftrans))-1); i++) {
    if(c == flag_translation[i].c) {
      word = flag_translation[i].str;
      break;
    }
  }
  if(word) {
    size = strlen(word);
    str = (char *)spin_malloc(size+1);
    memcpy(str, word, size+1);
    return str;
  } else {
    return 0;
  }
}

/*
 * copy flags from the flag string into argv and do the right translations
 */
void pack_boot_flags (char **argv, char *ptr)
{
  int   n_words = 1;
  int   in_word = 0;
/* FIXME
  int   in_x = 1;
*/
  int   in_x = 0;
  char *word = 0, *str;
  int   size;
  char  c;

  do {
    c = *ptr;
    if(c == ' ' || c == '\000') {
      if(in_word) {
	/* end of the word */
	size = ptr-word;
	str = 0;
	if(size == 1) {
	  str = translate_flag(word[0]);
	}
	if(str == 0) {
	  str  = (char *)spin_malloc(size+1);
	  memcpy(str, word, size);
	  str[size] = '\000';
	}
	argv[n_words] = str;
	n_words++;
	in_word = 0;
	word = 0;
      } else if (in_x) {
	/* end of continuous flags */
	in_x = 0;
      }
    } else {
      if(in_x) {
	/* single character flag */
	str = translate_flag(c);
	if(str == 0) {
	  size = 1;
	  str = (char *)spin_malloc(size+1);
	  str[0] = c;
	  str[size] = '\000';
	}
	argv[n_words] = str;
	n_words++;
      } else if(!in_word) {
	/* beginning of a separate word */
	word = ptr;
	in_word = 1;
      }
    }
    ptr++;
  } while (c);
}

/*
 * collect the SPIN boot flags
 */
void find_boot_flags(char ***argv, int *argc)
{
  char *cp;
  extern char *prom_getenv();
  int n_words, in_word, i;
  char *word, *ptr, *str;
  char c;
  int in_x, x_found, size;

  if (cp = prom_getenv("booted_osflags")) {
    printf("booted_osflags: %s\n", cp);
    n_words = count_flag_words(&cp);
  } else {
    printf("No booted_osflags\n");
    n_words = 0;
  }

  /* allocate and initialize the vector of text */
  *argc = n_words + 2;
  *argv = (char **)spin_malloc((*argc + 1) * sizeof(char *));
  (*argv)[0] = "spin";
  (*argv)[(*argc)] = 0;

  pack_boot_flags(*argv, cp);

  printf("SPIN boot flags:");
  for(i = 0; i < *argc-1; i++) {
    printf(" %s", (*argv)[i]);
  }
  printf("\n");
}

/*  
 * Starts up spin.
 */
spin_start(void)
{
  extern long hwrpb_addr;
  char **argv;
  char *envp[] = {0};
  int   argc;
  spl_t s;
  
  /* Remember the top of the stack at the point in which we entered SPIN.
     This is used by the GC so that it does not scan the stack below that
     pointer where no traced reference could possibly be. */
  boot_stack_bottom = (void *)getsp();

  /* Parse the boot flags and put them into argv/argc */
  find_boot_flags(&argv, &argc);
  
  /* Jump into SPIN */
  printf("Starting M3 runtime\n");
  spin_init(argc, argv, envp);  /* in start/${target}/_m3main.c */
  
  /* NOT REACHED */
  printf("ERROR >> SPIN exiting\n");
  panic("Spin exited.\n");
  halt();
}


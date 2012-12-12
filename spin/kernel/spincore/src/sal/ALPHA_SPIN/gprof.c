/* 
   HISTORY
 * 08-Jan-97  Charles Garrett (garrett) at the University of Washington
 *	Created. This procedure is the guts of the profiling code. It
 *	 should only be called from the assembly language code in Profile.S.
 *

*/

/* This file (the alpha version) represents the beginning of a new 
   implementation of the profiling data structure. I'll describe how
   it is supposed to work and hopefully keep it up-to-date as it changes.

   We don't want to restrict the range of valid PC's a priori, because
   we are dynamically linking code and also executing dynamically 
   generated stub code, whose addresses we cannot guess. Thus we are going
   to extend the original DEC gprof data structures by making the first
   level a hash table keyed by the PC value and mapping to a caller PC
   record. The caller PC record contains an index into the callee PC
   table.

    profile_hash         profile_caller           profile_arcs
   --------------      -------------------     ------------------
   | unsigned   | -    | struct CallerPC | --> | struct ArcInfo |
   |------------|  \   |-----------------|     |----------------|
   |            |   -> |                 |     |                |
   --------------      -------------------     ------------------

   This table is           This table            This table has
   keyed by a HBIT         remembers the         the count of the
   hash of the             whole caller PC.      number of times the
   caller PC value.                              call arc occured.
   
   Length = 2^HBIT      Length =                Length =
                        profile_caller_len      profile_arcs_len

			By convention, the      Same convention 
			first entry in this     applies here, 
			table is used to        with the "link"
			hold the count of       field.
			valid entries in its
			"next" field.

   *** All of these tables should remain fixed in position and size
       while profiling is on. They can change when profiling is turned
       off. ***
*/

#define HBIT 16
#define HSIZE (1 << HBIT)

/* This candidate hash function is just the XOR of 4 16-bit pieces
   of the PC. Go nuts on the parentheses. */
#define HASH(pc) ((((pc) >> 48) & 0xffff) ^ \
		  (((pc) >> 32) & 0xffff) ^ \
		  (((pc) >> 16) & 0xffff) ^ \
		   ((pc)        & 0xffff))

/* Expected alignment of this structure is 64 bits. */
struct CallerPC {
    unsigned int proc;          /* 4 bytes, at offset 0. */
    unsigned int next;          /* 4 bytes, at offset 4. */
    unsigned long pc;           /* 8 bytes, at offset 8. */
};

/* Expected alignment of this structure is 64 bits. */
struct ArcInfo {
    unsigned int link;          /* 4 bytes, at offset 0. */
    unsigned int count;         /* 4 bytes, at offset 4. */
    unsigned long time;         /* 8 bytes, at offset 8. */
    unsigned long selfPC;       /* 8 bytes, at offset 16. */
};

/* The elements of the auxiliary profiling stack. Used to save
   the real return address which is used by _mcount_epilogue. */

struct ProfileRec {
    void *sp;                    /* 8 bytes, at offset 0.  */
    void *ra;                    /* 8 bytes, at offset 8. */
    struct ArcInfo *arc;         /* 8 bytes, at offset 16.  */
};

/* This structure is designed to be 8 KB or one page long. 
   It should hold at least enough ProfileRec structures so
   that the kernel doesn't run off the stack while booting.
   The padding is inserted so that the distance between &cur
   and &data[0] is the same as the size of a ProfileRec. */

struct ProfileData {
    void *cur;                   /* 8 bytes, at offset 0. */
    long thread_flag;            /* 8 bytes, at offset 8. */
    long pad1;                   /* 8 bytes, at offset 16. */
    struct ProfileRec data[340]; /* 8160 bytes, at offset 24. */
    long pad2;                   /* 8 bytes, at offset 8184. */
};

struct ProfileData BootStack = {&BootStack, 1};
struct ProfileData *aux_stack = &BootStack;

extern unsigned long last_counter;

unsigned profile_hash[HSIZE];
struct CallerPC *profile_caller;
struct ArcInfo *profile_arcs;

unsigned long profile_flag = 1;
unsigned long profile_caller_len = 0;
unsigned long profile_arcs_len = 0;
unsigned long profile_count = 0;


/* Rather than calling the two procedures caller and callee which are easy
   to confuse when reading the source, we will call then caller and proc. */

void _gprof(unsigned long ra, unsigned long pc, unsigned long cycles,
	    struct ProfileRec *AuxPtr, struct ProfileData *AuxBegin) {
    unsigned hash;
    unsigned caller_ind;
    unsigned prev_ind = 0;
    unsigned int ProcIndex;
    unsigned int *CallerPtr = 0;
    unsigned int tmp;
    struct ArcInfo *Top, *PrevTop;

    /* If profile_flag is 1, then the profiling is turned off temporarily.
       If it is 2, then the profile_caller array has filled up and if it is
       3 then the profile_arcs array has filled up. */
    if (!profile_flag && AuxBegin->thread_flag) {
	/* If the caller was profiled, increment its time. */
	if (AuxPtr > &AuxBegin->data[0] && AuxPtr[-1].arc != 0) {
	    AuxPtr[-1].arc->time += cycles;
	}

        profile_count++;

	/* Hash the caller PC value to an index. */
	hash = HASH(ra);
	caller_ind = profile_hash[hash];

	/* Follow the hash chain until you find the PC value
	   or until it becomes 0. */
	while (caller_ind && 
	       profile_caller[caller_ind].pc != ra) {
	    prev_ind = caller_ind;
	    caller_ind = profile_caller[caller_ind].next;
	}

	/* If there is no entry, then create a new one. */
	if (!caller_ind) {
	    if (++profile_caller[0].next < profile_caller_len) {
		caller_ind = profile_caller[0].next;
		profile_caller[caller_ind].pc = ra;
		profile_caller[caller_ind].proc = 0;
		profile_caller[caller_ind].next = profile_hash[hash];
		profile_hash[hash] = caller_ind;
	    } else {
		/* There's no room. Give up. */
		profile_flag = 2;
		return;
	    }
	} else {
	    /* Pull the current caller record to the front 
	       of the list. */
	    if (prev_ind) {
		profile_caller[prev_ind].next = profile_caller[caller_ind].next;
		profile_caller[caller_ind].next = profile_hash[hash];
		profile_hash[hash] = caller_ind;
	    }
	}

	CallerPtr = &profile_caller[caller_ind].proc;

	ProcIndex = *CallerPtr;

	Top = &profile_arcs[ProcIndex];

	while (Top->selfPC != pc) {
	    if (!ProcIndex || !Top->link) {
		/* If ProcIndex is 0, or we reach the end of the list without
		   finding the callee PC, then we make a new arc. The first
		   entry in the profile_arcs array is really a dummy whose link
		   field is the total number of arcs seen so far. */
		if (++profile_arcs[0].link < profile_arcs_len) {
		    Top = &profile_arcs[profile_arcs[0].link];
		    Top->selfPC = pc;
		    Top->count = 0;
		    Top->time = 0;
		    Top->link = *CallerPtr;
		    *CallerPtr = profile_arcs[0].link;
		    break;
		} else {
		    profile_flag = 3;
		    return;
		    /* No room for a new link */
		}
	    } else {
		PrevTop = Top;
		Top = &profile_arcs[Top->link];
		if (Top->selfPC == pc) {
		    /* If we found the right arc. */
		    /* Switch it to the head. */
		    tmp = PrevTop->link;
		    PrevTop->link = Top->link;
		    Top->link = *CallerPtr;
		    *CallerPtr = tmp;
		}
	    }
	}

	Top->count++;
	AuxPtr->arc = Top; 
    }
}



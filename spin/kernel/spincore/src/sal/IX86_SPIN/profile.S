/* HISTORY
 *
 * 16-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Created for the X86. 
 *
 */

.data
	.align 4

	.globl _last_counter
_last_counter:
	.long 0

	# profile_call_graph is a flag indicating whether we are really counting
	# every call-arc.
	.globl _profile_call_graph
_profile_call_graph:
#if defined(CALL_GRAPH) 
	.long 1
#else
	.long 0
#endif

	# Keep these offsets in sync with the ArcInfo structure 
	# declared in Profile.m3
#define TOS_TIME	8

	# The Auxiliary stack records.
	# The situation when _mcount is called is:

	#		  --------------
	#		  | Callee Arc |
	#		  --------------
	#		  | Callee RA  |
	#		  --------------
	#		  | Callee BP  |
	#		  --------------
	#		  | Caller Arc |
	#		  --------------
	#		  | Caller RA  |
	#		  --------------
	#	AuxPtr -> | Caller BP  |
	#		  --------------

	# At the beginning of _mcount, AuxPtr is incremented to
	# point to the Callee Arc.
	
	# When _mcount_epilogue is called, the situation is 
	# reversed. That is, AuxPtr points to the callee arc
	# and is is decremented after all references which
	# use AuxPtr.

#define CALLEE_ARC 8
#define CALLEE_RA  4
#define CALLEE_BP  0
#define RECORD_SZ  12

#ifdef CALL_GRAPH

.text
	.align 4
.globl _mcount
.globl mcount
	.type	 mcount,@function
_mcount:
mcount:
	pushl %ebp
	movl %esp,%ebp

	# state of stack looks like
	#	caller return address
	#	caller base pointer	<< callee base pointer points here >>
	#	... arbitrary amount of state on stack ...
	#	callee return address
	#	callee base pointer     << esp points here >>
	
	# %eax is 0 for a save regs event and non-zero for a regular
 	# procedure or stub.

	pushl %eax			# save all regs
	pushl %ecx
	pushl %edx
	pushl %ebx
	pushl %esp
	pushl %esi
	pushl %edi

	# prevent interrupts
	pushl _cpl			# save old mask
	movl  $0xffffffff,_cpl		# disable all interrupts (splhigh)

	.byte	0x0f, 0x31		# RDTSC

	pushl %eax			# push counter on stack	
	pushl %edx

	# push the callers return address onto the profiling stack
	# The callers return address is found by dereferencing the
	# callee base pointer + 4 (see the picture of the stack above.)
	movl _aux_stack,%eax		# *_aux_stack is pointer to the next
	movl (%eax),%ebx		# record on the profiling stack

	addl $RECORD_SZ,%ebx		# increment the stack by one record
	movl %ebx,(%eax)
			
	movl (%ebp),%ecx		# We have to take **ebp since that
	movl (%ecx),%edx		# is the value that ebp will have
	movl %edx,CALLEE_BP(%ebx)	# inside of mcount_ep. The value of
	movl 4(%ecx),%edx		# ebp is switched before ret is 
	movl %edx,CALLEE_RA(%ebx)	# executed.
	movl $0,CALLEE_ARC(%ebx)

	# Call the procedure to record the profile info for this 
	# call arc.
	popl %edx
	popl %eax			# recover the time stamp counter
	subl _last_counter,%eax         # subtract the last TSC from this one

	pushl %ecx			# Save caller saved register
	movl _aux_stack,%ebx
	pushl %ebx			# The base address of the prof. stack
	pushl 0(%ebx)			# The current element of prof. stack
	pushl %eax			# The elapsed time in cycles
	pushl 4(%ebp)			# The pc value of the callee
	pushl 4(%ecx)			# The pc value of the caller
	call __gprof
	addl $20,%esp			# Discard 5 argument values.
	popl %ecx			# Restore caller saved register
	
	# overwrite the callers return address with the profiling
	# epilog.
	leal mcount_ep,%eax
	movl %eax,4(%ecx)

	.byte	0x0f, 0x31		# RDTSC
	movl %eax,_last_counter

	popl _cpl			# reenable interrupts
	
	popl %edi
	popl %esi
	popl %esp
	popl %ebx
	popl %edx
	popl %ecx
	popl %eax			# restore regs
	movl %ebp,%esp
	popl %ebp
	ret
	


.text
	.align 4
	.type	 mcount_ep,@function
mcount_ep:
	pushl $0
	pushl %ebp
	movl %esp,%ebp


	# state of stack looks like
	#	grand-callers return address
	#	zero			<< new  return address goes here >>
	#	caller base pointer     << esp points here >>

	# notice that in mcount_ep we dont have to worry about the
	# local variables of the callee, because the callee has already
	# removed its activation record from the stack.
	
	pushl %eax			# save all regs
	pushl %ecx
	pushl %edx
	pushl %ebx
	pushl %esp
	pushl %esi
	pushl %edi

	# prevent interrupts
	pushl _cpl			# save old mask
	movl  $0xffffffff,_cpl		# disable all interrupts (splhigh)

	.byte	0x0f, 0x31		# RDTSC
	pushl %eax			# push counter on stack	

	# walk up the profiling stack until you find a base pointer which
	# matches the one at (%ebp)
	movl _aux_stack,%eax		# *_aux_stack is pointer to the next
	movl (%eax),%ebx		# record on the profiling stack
	movl (%ebp),%ecx		# get the callers base pointer

UNWIND:
	cmpl %ecx,CALLEE_BP(%ebx)
	je   MATCH_FOUND
	subl $RECORD_SZ,%ebx
	jmp  UNWIND

MATCH_FOUND:
	movl CALLEE_RA(%ebx),%edx
	movl %edx,4(%ebp)
	movl CALLEE_ARC(%ebx),%edx	# Place to add time

	subl $RECORD_SZ,%ebx
	movl %ebx,(%eax)

	popl %eax			# retrieve counter
	subl _last_counter,%eax		# compute elapsed time

	cmpl $0,%edx			# check for non-existent caller
	je   NO_ARC

	addl %eax,TOS_TIME(%edx)	# add elapsed time

NO_ARC:
	.byte	0x0f, 0x31		# RDTSC
	movl %eax,_last_counter

	popl _cpl			# reenable interrupts

	popl %edi
	popl %esi
	popl %esp
	popl %ebx
	popl %edx
	popl %ecx
	popl %eax			# restore regs
	movl %ebp,%esp
	popl %ebp
	ret

#endif

	


	
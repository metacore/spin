/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *	IX86 version.
 */

	.text	
	.align	4

/******************************************************************************
 *	
 *				LEVEL 0
 *
 *****************************************************************************/

/*
 * trampoline_stub is a stub that saves callee saved registers and jumps 
 * to the right entry in the DispatcherPrivate jtable
 */


/*		
 * TRAMPOLINE_STUB_FRAME_SIZE is the size of the stack frame allocated
 * by this stub.  This constant has to by manually synchronized with 
 * Dispatcher.m3
 */
	.globl	_trampoline_stub
	.globl	_trampoline_stub_end
 
#ifdef CALL_GRAPH
	.globl	_trampoline_profile
#endif 

	.globl  _trampoline_long_patch
	.globl	_trampoline_offset_patch

_trampoline_stub:

	pushl	%ebp
	movl	%esp,%ebp


#ifdef CALL_GRAPH
 # A zero in the %eax register indicates a save regs event. Every other
 # event or procedure is expected to put a non-zero value in %eax.
_trampoline_profile:
	#call 	mcount
#endif


 # save callee saved registers to be able to access them in handlers for
 # Strand.Stop, stub doesn't have to restore it because the code that
 # gets called by this stub will do it
	pushl	%esi
	pushl	%edi
	pushl	%ebx
	
 # get the pointer to the dispatcher jtable
	movl	$_MI_MachineDispatcher,%eax
	
 # get the second argument (the original stack pointer)
	movl	%esp,%ecx
	addl	$16,%ecx
	pushl	%ecx

 # get the first argument (pointer to the alias descriptor)
_trampoline_long_patch:	
	movl	$0,%ecx	      /* to be filled in */
	pushl	%ecx

 # jump to Dispatcher.Dispatch 
_trampoline_offset_patch:	
	movl	$0,%ecx       /* to be filled in */
	addl	%ecx,%eax
	movl	(%eax),%eax
	call	*%eax
	movl	%ebp,%esp
	popl	%ebp
	ret
_trampoline_stub_end:	
	nop

/*	
 *
 *
 *
 */
	.globl	_argument_stub
	.globl	_argument_stub_end
	.globl	_argument_long_patch

_argument_stub:
	pushl	%ebp
	movl	%esp,%ebp

 # push arguments
	pushl	16(%ebp)
	pushl	12(%ebp)
	pushl	8(%ebp)
	movl	%esp, %ecx
	
 # push the open array header
	pushl	$3
	pushl	%ecx
	movl	%esp, %eax
	pushl	%eax

 # call the procedure
_argument_long_patch:	
	call	_CloneArgumentStub

 # epilog
	movl	%ebp,%esp
	popl	%ebp
	ret
_argument_stub_end:	
	nop

	.globl	_argument_cl_stub
	.globl	_argument_cl_stub_end
	.globl	_argument_cl_long_patch
	.globl	_argument_cl_cl_patch

_argument_cl_stub:
	pushl	%ebp
	movl	%esp,%ebp

 # push arguments
	pushl	16(%ebp)
	pushl	12(%ebp)
	pushl	8(%ebp)
	movl	%esp, %ecx
	
 # push the open array header
	pushl	$3
	pushl	%ecx
	movl	%esp, %eax

 # the array argument
	pushl	%eax

 # the closure
_argument_cl_cl_patch:	
	pushl	$12345
	
 # call the procedure
_argument_cl_long_patch:	
	call	_CloneArgumentStub

 # epilog
	movl	%ebp,%esp
	popl	%ebp
	ret
_argument_cl_stub_end:	
	nop



	.globl	_argument_prolog
	.globl	_argument_prolog_end
	.globl	_argument_push_arg
	.globl	_argument_push_arg_end
	.globl	_argument_push_ptr
	.globl	_argument_push_ptr_end
	.globl	_argument_push_array
	.globl	_argument_push_array_end
	.globl	_argument_push_closure
	.globl	_argument_push_closure_end
	.globl	_argument_call
	.globl	_argument_call_end
	.globl	_argument_epilog
	.globl	_argument_epilog_end

_argument_prolog:
	pushl	%ebp
	movl	%esp,%ebp
_argument_prolog_end:

_argument_push_arg:
	pushl	0(%ebp)
_argument_push_arg_end:

_argument_push_ptr:
	movl	%esp, %ecx
_argument_push_ptr_end:
	
_argument_push_array:
	pushl	$0xdeadbeef
	pushl	%ecx
	movl	%esp, %eax
	pushl	%eax
_argument_push_array_end:

_argument_push_closure:
	pushl	$0xdeadbeef
_argument_push_closure_end:
	
_argument_call:
	call	_CloneArgumentStub
_argument_call_end:

_argument_epilog:
	movl	%ebp,%esp
	popl	%ebp
	ret
_argument_epilog_end:

	nop
	nop
	nop


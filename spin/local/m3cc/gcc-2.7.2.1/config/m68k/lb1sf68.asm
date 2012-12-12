/* libgcc1 routines for 68000 w/o floating-point hardware. */
/* Copyright (C) 1994 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* Use this one for any 680x0; assumes no floating point hardware.
   The trailing " '" appearing on some lines is for ANSI preprocessors.  Yuk.
   Some of this code comes from MINIX, via the folks at ericsson.
   D. V. Henkel-Wallace (gumby@cygnus.com) Fete Bastille, 1992
*/

/* These are predefined by new versions of GNU cpp.  */

#ifndef __USER_LABEL_PREFIX__
#define __USER_LABEL_PREFIX__ _
#endif

#ifndef __REGISTER_PREFIX__
#define __REGISTER_PREFIX__
#endif

#ifndef __IMMEDIATE_PREFIX__
#define __IMMEDIATE_PREFIX__ #
#endif

/* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

/* Use the right prefix for registers.  */

#define REG(x) CONCAT1 (__REGISTER_PREFIX__, x)

/* Use the right prefix for immediate values.  */

#define IMM(x) CONCAT1 (__IMMEDIATE_PREFIX__, x)

#define d0 REG (d0)
#define d1 REG (d1)
#define d2 REG (d2)
#define d3 REG (d3)
#define d4 REG (d4)
#define d5 REG (d5)
#define d6 REG (d6)
#define d7 REG (d7)
#define a0 REG (a0)
#define a1 REG (a1)
#define a2 REG (a2)
#define a3 REG (a3)
#define a4 REG (a4)
#define a5 REG (a5)
#define a6 REG (a6)
#define fp REG (fp)
#define sp REG (sp)

#ifdef L_floatex

| This is an attempt at a decent floating point (single, double and 
| extended double) code for the GNU C compiler. It should be easy to
| adapt to other compilers (but beware of the local labels!).

| Starting date: 21 October, 1990

| It is convenient to introduce the notation (s,e,f) for a floating point
| number, where s=sign, e=exponent, f=fraction. We will call a floating
| point number fpn to abbreviate, independently of the precision.
| Let MAX_EXP be in each case the maximum exponent (255 for floats, 1023 
| for doubles and 16383 for long doubles). We then have the following 
| different cases:
|  1. Normalized fpns have 0 < e < MAX_EXP. They correspond to 
|     (-1)^s x 1.f x 2^(e-bias-1).
|  2. Denormalized fpns have e=0. They correspond to numbers of the form
|     (-1)^s x 0.f x 2^(-bias).
|  3. +/-INFINITY have e=MAX_EXP, f=0.
|  4. Quiet NaN (Not a Number) have all bits set.
|  5. Signaling NaN (Not a Number) have s=0, e=MAX_EXP, f=1.

|=============================================================================
|                                  exceptions
|=============================================================================

| This is the floating point condition code register (_fpCCR):
|
| struct {
|   short _exception_bits;	
|   short _trap_enable_bits;	
|   short _sticky_bits;
|   short _rounding_mode;
|   short _format;
|   short _last_operation;
|   union {
|     float sf;
|     double df;
|   } _operand1;
|   union {
|     float sf;
|     double df;
|   } _operand2;
| } _fpCCR;

	.data
	.even

	.globl	SYM (_fpCCR)
	
SYM (_fpCCR):
__exception_bits:
	.word	0
__trap_enable_bits:
	.word	0
__sticky_bits:
	.word	0
__rounding_mode:
	.word	ROUND_TO_NEAREST
__format:
	.word	NIL
__last_operation:
	.word	NOOP
__operand1:
	.long	0
	.long	0
__operand2:
	.long 	0
	.long	0

| Offsets:
EBITS  = __exception_bits - SYM (_fpCCR)
TRAPE  = __trap_enable_bits - SYM (_fpCCR)
STICK  = __sticky_bits - SYM (_fpCCR)
ROUND  = __rounding_mode - SYM (_fpCCR)
FORMT  = __format - SYM (_fpCCR)
LASTO  = __last_operation - SYM (_fpCCR)
OPER1  = __operand1 - SYM (_fpCCR)
OPER2  = __operand2 - SYM (_fpCCR)

| The following exception types are supported:
INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

| The allowed rounding modes are:
UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 | round result to nearest representable value
ROUND_TO_ZERO     = 1 | round result towards zero
ROUND_TO_PLUS     = 2 | round result towards plus infinity
ROUND_TO_MINUS    = 3 | round result towards minus infinity

| The allowed values of format are:
NIL          = 0
SINGLE_FLOAT = 1
DOUBLE_FLOAT = 2
LONG_FLOAT   = 3

| The allowed values for the last operation are:
NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

|=============================================================================
|                           __clear_sticky_bits
|=============================================================================

| The sticky bits are normally not cleared (thus the name), whereas the 
| exception type and exception value reflect the last computation. 
| This routine is provided to clear them (you can also write to _fpCCR,
| since it is globally visible).

	.globl  SYM (__clear_sticky_bit)

	.text
	.even

| void __clear_sticky_bits(void);
SYM (__clear_sticky_bit):		
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@(STICK)
	rts

|=============================================================================
|                           $_exception_handler
|=============================================================================

	.globl  $_exception_handler

	.text
	.even

| This is the common exit point if an exception occurs.
| NOTE: it is NOT callable from C!
| It expects the exception type in d7, the format (SINGLE_FLOAT,
| DOUBLE_FLOAT or LONG_FLOAT) in d6, and the last operation code in d5.
| It sets the corresponding exception and sticky bits, and the format. 
| Depending on the format if fills the corresponding slots for the 
| operands which produced the exception (all this information is provided
| so if you write your own exception handlers you have enough information
| to deal with the problem).
| Then checks to see if the corresponding exception is trap-enabled, 
| in which case it pushes the address of _fpCCR and traps through 
| trap FPTRAP (15 for the moment).

FPTRAP = 15

$_exception_handler:
	lea	SYM (_fpCCR),a0
	movew	d7,a0@(EBITS)	| set __exception_bits
	orw	d7,a0@(STICK)	| and __sticky_bits
	movew	d6,a0@(FORMT)	| and __format
	movew	d5,a0@(LASTO)	| and __last_operation

| Now put the operands in place:
	cmpw	IMM (SINGLE_FLOAT),d6
	beq	1f
	movel	a6@(8),a0@(OPER1)
	movel	a6@(12),a0@(OPER1+4)
	movel	a6@(16),a0@(OPER2)
	movel	a6@(20),a0@(OPER2+4)
	bra	2f
1:	movel	a6@(8),a0@(OPER1)
	movel	a6@(12),a0@(OPER2)
2:
| And check whether the exception is trap-enabled:
	andw	a0@(TRAPE),d7	| is exception trap-enabled?
	beq	1f		| no, exit
	pea	SYM (_fpCCR)	| yes, push address of _fpCCR
	trap	IMM (FPTRAP)	| and trap
1:	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts
#endif /* L_floatex */

#ifdef  L_mulsi3
	.text
	.proc
	.globl	SYM (__mulsi3)
SYM (__mulsi3):
	movew	sp@(4), d0	/* x0 -> d0 */
	muluw	sp@(10), d0	/* x0*y1 */
	movew	sp@(6), d1	/* x1 -> d1 */
	muluw	sp@(8), d1	/* x1*y0 */
	addw	d1, d0
	swap	d0
	clrw	d0
	movew	sp@(6), d1	/* x1 -> d1 */
	muluw	sp@(10), d1	/* x1*y1 */
	addl	d1, d0

	rts
#endif /* L_mulsi3 */

#ifdef  L_udivsi3
	.text
	.proc
	.globl	SYM (__udivsi3)
SYM (__udivsi3):
	movel	d2, sp@-
	movel	sp@(12), d1	/* d1 = divisor */
	movel	sp@(8), d0	/* d0 = dividend */

	cmpl	IMM (0x10000), d1 /* divisor >= 2 ^ 16 ?   */
	jcc	L3		/* then try next algorithm */
	movel	d0, d2
	clrw	d2
	swap	d2
	divu	d1, d2          /* high quotient in lower word */
	movew	d2, d0		/* save high quotient */
	swap	d0
	movew	sp@(10), d2	/* get low dividend + high rest */
	divu	d1, d2		/* low quotient */
	movew	d2, d0
	jra	L6

L3:	movel	d1, d2		/* use d2 as divisor backup */
L4:	lsrl	IMM (1), d1	/* shift divisor */
	lsrl	IMM (1), d0	/* shift dividend */
	cmpl	IMM (0x10000), d1 /* still divisor >= 2 ^ 16 ?  */
	jcc	L4
	divu	d1, d0		/* now we have 16 bit divisor */
	andl	IMM (0xffff), d0 /* mask out divisor, ignore remainder */

/* Multiply the 16 bit tentative quotient with the 32 bit divisor.  Because of
   the operand ranges, this might give a 33 bit product.  If this product is
   greater than the dividend, the tentative quotient was too large. */
	movel	d2, d1
	mulu	d0, d1		/* low part, 32 bits */
	swap	d2
	mulu	d0, d2		/* high part, at most 17 bits */
	swap	d2		/* align high part with low part */
	btst	IMM (0), d2	/* high part 17 bits? */
	jne	L5		/* if 17 bits, quotient was too large */
	addl	d2, d1		/* add parts */
	jcs	L5		/* if sum is 33 bits, quotient was too large */
	cmpl	sp@(8), d1	/* compare the sum with the dividend */
	jls	L6		/* if sum > dividend, quotient was too large */
L5:	subql	IMM (1), d0	/* adjust quotient */

L6:	movel	sp@+, d2
	rts
#endif /* L_udivsi3 */

#ifdef  L_divsi3
	.text
	.proc
	.globl	SYM (__divsi3)
SYM (__divsi3):
	movel	d2, sp@-

	moveb	IMM (1), d2	/* sign of result stored in d2 (=1 or =-1) */
	movel	sp@(12), d1	/* d1 = divisor */
	jpl	L1
	negl	d1
	negb	d2		/* change sign because divisor <0  */
L1:	movel	sp@(8), d0	/* d0 = dividend */
	jpl	L2
	negl	d0
	negb	d2

L2:	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__udivsi3)	/* divide abs(dividend) by abs(divisor) */
	addql	IMM (8), sp

	tstb	d2
	jpl	L3
	negl	d0

L3:	movel	sp@+, d2
	rts
#endif /* L_divsi3 */

#ifdef  L_umodsi3
	.text
	.proc
	.globl	SYM (__umodsi3)
SYM (__umodsi3):
	movel	sp@(8), d1	/* d1 = divisor */
	movel	sp@(4), d0	/* d0 = dividend */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__udivsi3)
	addql	IMM (8), sp
	movel	sp@(8), d1	/* d1 = divisor */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addql	IMM (8), sp
	movel	sp@(4), d1	/* d1 = dividend */
	subl	d0, d1		/* d1 = a - (a/b)*b */
	movel	d1, d0
	rts
#endif /* L_umodsi3 */

#ifdef  L_modsi3
	.text
	.proc
	.globl	SYM (__modsi3)
SYM (__modsi3):
	movel	sp@(8), d1	/* d1 = divisor */
	movel	sp@(4), d0	/* d0 = dividend */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__divsi3)
	addql	IMM (8), sp
	movel	sp@(8), d1	/* d1 = divisor */
	movel	d1, sp@-
	movel	d0, sp@-
	jbsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addql	IMM (8), sp
	movel	sp@(4), d1	/* d1 = dividend */
	subl	d0, d1		/* d1 = a - (a/b)*b */
	movel	d1, d0
	rts
#endif /* L_modsi3 */


#ifdef  L_double

	.globl	SYM (_fpCCR)
	.globl  $_exception_handler

QUIET_NaN      = 0xffffffff

D_MAX_EXP      = 0x07ff
D_BIAS         = 1022
DBL_MAX_EXP    = D_MAX_EXP - D_BIAS
DBL_MIN_EXP    = 1 - D_BIAS
DBL_MANT_DIG   = 53

INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

DOUBLE_FLOAT = 2

NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 | round result to nearest representable value
ROUND_TO_ZERO     = 1 | round result towards zero
ROUND_TO_PLUS     = 2 | round result towards plus infinity
ROUND_TO_MINUS    = 3 | round result towards minus infinity

| Entry points:

	.globl SYM (__adddf3)
	.globl SYM (__subdf3)
	.globl SYM (__muldf3)
	.globl SYM (__divdf3)
	.globl SYM (__negdf2)
	.globl SYM (__cmpdf2)

	.text
	.even

| These are common routines to return and signal exceptions.	

Ld$den:
| Return and signal a denormalized number
	orl	d7,d0
	movew	IMM (UNDERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (DOUBLE_FLOAT),d6
	jmp	$_exception_handler

Ld$infty:
Ld$overflow:
| Return a properly signed INFINITY and set the exception flags 
	movel	IMM (0x7ff00000),d0
	movel	IMM (0),d1
	orl	d7,d0
	movew	IMM (OVERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (DOUBLE_FLOAT),d6
	jmp	$_exception_handler

Ld$underflow:
| Return 0 and set the exception flags 
	movel	IMM (0),d0
	movel	d0,d1
	movew	IMM (UNDERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (DOUBLE_FLOAT),d6
	jmp	$_exception_handler

Ld$inop:
| Return a quiet NaN and set the exception flags
	movel	IMM (QUIET_NaN),d0
	movel	d0,d1
	movew	IMM (INVALID_OPERATION),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (DOUBLE_FLOAT),d6
	jmp	$_exception_handler

Ld$div$0:
| Return a properly signed INFINITY and set the exception flags
	movel	IMM (0x7ff00000),d0
	movel	IMM (0),d1
	orl	d7,d0
	movew	IMM (DIVIDE_BY_ZERO),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (DOUBLE_FLOAT),d6
	jmp	$_exception_handler

|=============================================================================
|=============================================================================
|                         double precision routines
|=============================================================================
|=============================================================================

| A double precision floating point number (double) has the format:
|
| struct _double {
|  unsigned int sign      : 1;  /* sign bit */ 
|  unsigned int exponent  : 11; /* exponent, shifted by 126 */
|  unsigned int fraction  : 52; /* fraction */
| } double;
| 
| Thus sizeof(double) = 8 (64 bits). 
|
| All the routines are callable from C programs, and return the result 
| in the register pair d0-d1. They also preserve all registers except 
| d0-d1 and a0-a1.

|=============================================================================
|                              __subdf3
|=============================================================================

| double __subdf3(double, double);
SYM (__subdf3):
	bchg	IMM (31),sp@(12) | change sign of second operand
				| and fall through, so we always add
|=============================================================================
|                              __adddf3
|=============================================================================

| double __adddf3(double, double);
SYM (__adddf3):
	link	a6,IMM (0)	| everything will be done in registers
	moveml	d2-d7,sp@-	| save all data registers and a2 (but d0-d1)
	movel	a6@(8),d0	| get first operand
	movel	a6@(12),d1	| 
	movel	a6@(16),d2	| get second operand
	movel	a6@(20),d3	| 

	movel	d0,d7		| get d0's sign bit in d7 '
	addl	d1,d1		| check and clear sign bit of a, and gain one
	addxl	d0,d0		| bit of extra precision
	beq	Ladddf$b	| if zero return second operand

	movel	d2,d6		| save sign in d6 
	addl	d3,d3		| get rid of sign bit and gain one bit of
	addxl	d2,d2		| extra precision
	beq	Ladddf$a	| if zero return first operand

	andl	IMM (0x80000000),d7 | isolate a's sign bit '
        swap	d6		| and also b's sign bit '
	andw	IMM (0x8000),d6	|
	orw	d6,d7		| and combine them into d7, so that a's sign '
				| bit is in the high word and b's is in the '
				| low word, so d6 is free to be used
	movel	d7,a0		| now save d7 into a0, so d7 is free to
                		| be used also

| Get the exponents and check for denormalized and/or infinity.

	movel	IMM (0x001fffff),d6 | mask for the fraction
	movel	IMM (0x00200000),d7 | mask to put hidden bit back

	movel	d0,d4		| 
	andl	d6,d0		| get fraction in d0
	notl	d6		| make d6 into mask for the exponent
	andl	d6,d4		| get exponent in d4
	beq	Ladddf$a$den	| branch if a is denormalized
	cmpl	d6,d4		| check for INFINITY or NaN
	beq	Ladddf$nf       | 
	orl	d7,d0		| and put hidden bit back
Ladddf$1:
	swap	d4		| shift right exponent so that it starts
	lsrw	IMM (5),d4	| in bit 0 and not bit 20
| Now we have a's exponent in d4 and fraction in d0-d1 '
	movel	d2,d5		| save b to get exponent
	andl	d6,d5		| get exponent in d5
	beq	Ladddf$b$den	| branch if b is denormalized
	cmpl	d6,d5		| check for INFINITY or NaN
	beq	Ladddf$nf
	notl	d6		| make d6 into mask for the fraction again
	andl	d6,d2		| and get fraction in d2
	orl	d7,d2		| and put hidden bit back
Ladddf$2:
	swap	d5		| shift right exponent so that it starts
	lsrw	IMM (5),d5	| in bit 0 and not bit 20

| Now we have b's exponent in d5 and fraction in d2-d3. '

| The situation now is as follows: the signs are combined in a0, the 
| numbers are in d0-d1 (a) and d2-d3 (b), and the exponents in d4 (a)
| and d5 (b). To do the rounding correctly we need to keep all the
| bits until the end, so we need to use d0-d1-d2-d3 for the first number
| and d4-d5-d6-d7 for the second. To do this we store (temporarily) the
| exponents in a2-a3.

	moveml	a2-a3,sp@-	| save the address registers

	movel	d4,a2		| save the exponents
	movel	d5,a3		| 

	movel	IMM (0),d7	| and move the numbers around
	movel	d7,d6		|
	movel	d3,d5		|
	movel	d2,d4		|
	movel	d7,d3		|
	movel	d7,d2		|

| Here we shift the numbers until the exponents are the same, and put 
| the largest exponent in a2.
	exg	d4,a2		| get exponents back
	exg	d5,a3		|
	cmpw	d4,d5		| compare the exponents
	beq	Ladddf$3	| if equal don't shift '
	bhi	9f		| branch if second exponent is higher

| Here we have a's exponent larger than b's, so we have to shift b. We do 
| this by using as counter d2:
1:	movew	d4,d2		| move largest exponent to d2
	subw	d5,d2		| and subtract second exponent
	exg	d4,a2		| get back the longs we saved
	exg	d5,a3		|
| if difference is too large we don't shift (actually, we can just exit) '
	cmpw	IMM (DBL_MANT_DIG+2),d2
	bge	Ladddf$b$small
	cmpw	IMM (32),d2	| if difference >= 32, shift by longs
	bge	5f
2:	cmpw	IMM (16),d2	| if difference >= 16, shift by words	
	bge	6f
	bra	3f		| enter dbra loop

4:	lsrl	IMM (1),d4
	roxrl	IMM (1),d5
	roxrl	IMM (1),d6
	roxrl	IMM (1),d7
3:	dbra	d2,4b
	movel	IMM (0),d2
	movel	d2,d3	
	bra	Ladddf$4
5:
	movel	d6,d7
	movel	d5,d6
	movel	d4,d5
	movel	IMM (0),d4
	subw	IMM (32),d2
	bra	2b
6:
	movew	d6,d7
	swap	d7
	movew	d5,d6
	swap	d6
	movew	d4,d5
	swap	d5
	movew	IMM (0),d4
	swap	d4
	subw	IMM (16),d2
	bra	3b
	
9:	exg	d4,d5
	movew	d4,d6
	subw	d5,d6		| keep d5 (largest exponent) in d4
	exg	d4,a2
	exg	d5,a3
| if difference is too large we don't shift (actually, we can just exit) '
	cmpw	IMM (DBL_MANT_DIG+2),d6
	bge	Ladddf$a$small
	cmpw	IMM (32),d6	| if difference >= 32, shift by longs
	bge	5f
2:	cmpw	IMM (16),d6	| if difference >= 16, shift by words	
	bge	6f
	bra	3f		| enter dbra loop

4:	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
3:	dbra	d6,4b
	movel	IMM (0),d7
	movel	d7,d6
	bra	Ladddf$4
5:
	movel	d2,d3
	movel	d1,d2
	movel	d0,d1
	movel	IMM (0),d0
	subw	IMM (32),d6
	bra	2b
6:
	movew	d2,d3
	swap	d3
	movew	d1,d2
	swap	d2
	movew	d0,d1
	swap	d1
	movew	IMM (0),d0
	swap	d0
	subw	IMM (16),d6
	bra	3b
Ladddf$3:
	exg	d4,a2	
	exg	d5,a3
Ladddf$4:	
| Now we have the numbers in d0--d3 and d4--d7, the exponent in a2, and
| the signs in a4.

| Here we have to decide whether to add or subtract the numbers:
	exg	d7,a0		| get the signs 
	exg	d6,a3		| a3 is free to be used
	movel	d7,d6		|
	movew	IMM (0),d7	| get a's sign in d7 '
	swap	d6              |
	movew	IMM (0),d6	| and b's sign in d6 '
	eorl	d7,d6		| compare the signs
	bmi	Lsubdf$0	| if the signs are different we have 
				| to subtract
	exg	d7,a0		| else we add the numbers
	exg	d6,a3		|
	addl	d7,d3		|
	addxl	d6,d2		|
	addxl	d5,d1		| 
	addxl	d4,d0           |

	movel	a2,d4		| return exponent to d4
	movel	a0,d7		| 
	andl	IMM (0x80000000),d7 | d7 now has the sign

	moveml	sp@+,a2-a3	

| Before rounding normalize so bit #DBL_MANT_DIG is set (we will consider
| the case of denormalized numbers in the rounding routine itself).
| As in the addition (not in the subtraction!) we could have set 
| one more bit we check this:
	btst	IMM (DBL_MANT_DIG+1),d0	
	beq	1f
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	addw	IMM (1),d4
1:
	lea	Ladddf$5,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Ladddf$5:
| Put back the exponent and check for overflow
	cmpw	IMM (0x7ff),d4	| is the exponent big?
	bge	1f
	bclr	IMM (DBL_MANT_DIG-1),d0
	lslw	IMM (4),d4	| put exponent back into position
	swap	d0		| 
	orw	d4,d0		|
	swap	d0		|
	bra	Ladddf$ret
1:
	movew	IMM (ADD),d5
	bra	Ld$overflow

Lsubdf$0:
| Here we do the subtraction.
	exg	d7,a0		| put sign back in a0
	exg	d6,a3		|
	subl	d7,d3		|
	subxl	d6,d2		|
	subxl	d5,d1		|
	subxl	d4,d0		|
	beq	Ladddf$ret$1	| if zero just exit
	bpl	1f		| if positive skip the following
	exg	d7,a0		|
	bchg	IMM (31),d7	| change sign bit in d7
	exg	d7,a0		|
	negl	d3		|
	negxl	d2		|
	negxl	d1              | and negate result
	negxl	d0              |
1:	
	movel	a2,d4		| return exponent to d4
	movel	a0,d7
	andl	IMM (0x80000000),d7 | isolate sign bit
	moveml	sp@+,a2-a3	|

| Before rounding normalize so bit #DBL_MANT_DIG is set (we will consider
| the case of denormalized numbers in the rounding routine itself).
| As in the addition (not in the subtraction!) we could have set 
| one more bit we check this:
	btst	IMM (DBL_MANT_DIG+1),d0	
	beq	1f
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	addw	IMM (1),d4
1:
	lea	Lsubdf$1,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Lsubdf$1:
| Put back the exponent and sign (we don't have overflow). '
	bclr	IMM (DBL_MANT_DIG-1),d0	
	lslw	IMM (4),d4	| put exponent back into position
	swap	d0		| 
	orw	d4,d0		|
	swap	d0		|
	bra	Ladddf$ret

| If one of the numbers was too small (difference of exponents >= 
| DBL_MANT_DIG+1) we return the other (and now we don't have to '
| check for finiteness or zero).
Ladddf$a$small:
	moveml	sp@+,a2-a3	
	movel	a6@(16),d0
	movel	a6@(20),d1
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts

Ladddf$b$small:
	moveml	sp@+,a2-a3	
	movel	a6@(8),d0
	movel	a6@(12),d1
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts

Ladddf$a$den:
	movel	d7,d4		| d7 contains 0x00200000
	bra	Ladddf$1

Ladddf$b$den:
	movel	d7,d5           | d7 contains 0x00200000
	notl	d6
	bra	Ladddf$2

Ladddf$b:
| Return b (if a is zero)
	movel	d2,d0
	movel	d3,d1
	bra	1f
Ladddf$a:
	movel	a6@(8),d0
	movel	a6@(12),d1
1:
	movew	IMM (ADD),d5
| Check for NaN and +/-INFINITY.
	movel	d0,d7         		|
	andl	IMM (0x80000000),d7	|
	bclr	IMM (31),d0		|
	cmpl	IMM (0x7ff00000),d0	|
	bge	2f			|
	movel	d0,d0           	| check for zero, since we don't  '
	bne	Ladddf$ret		| want to return -0 by mistake
	bclr	IMM (31),d7		|
	bra	Ladddf$ret		|
2:
	andl	IMM (0x000fffff),d0	| check for NaN (nonzero fraction)
	orl	d1,d0			|
	bne	Ld$inop         	|
	bra	Ld$infty		|
	
Ladddf$ret$1:
	moveml	sp@+,a2-a3	| restore regs and exit

Ladddf$ret:
| Normal exit.
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	orl	d7,d0		| put sign bit back
	moveml	sp@+,d2-d7
	unlk	a6
	rts

Ladddf$ret$den:
| Return a denormalized number.
	lsrl	IMM (1),d0	| shift right once more
	roxrl	IMM (1),d1	|
	bra	Ladddf$ret

Ladddf$nf:
	movew	IMM (ADD),d5
| This could be faster but it is not worth the effort, since it is not
| executed very often. We sacrifice speed for clarity here.
	movel	a6@(8),d0	| get the numbers back (remember that we
	movel	a6@(12),d1	| did some processing already)
	movel	a6@(16),d2	| 
	movel	a6@(20),d3	| 
	movel	IMM (0x7ff00000),d4 | useful constant (INFINITY)
	movel	d0,d7		| save sign bits
	movel	d2,d6		| 
	bclr	IMM (31),d0	| clear sign bits
	bclr	IMM (31),d2	| 
| We know that one of them is either NaN of +/-INFINITY
| Check for NaN (if either one is NaN return NaN)
	cmpl	d4,d0		| check first a (d0)
	bhi	Ld$inop		| if d0 > 0x7ff00000 or equal and
	bne	2f
	tstl	d1		| d1 > 0, a is NaN
	bne	Ld$inop		| 
2:	cmpl	d4,d2		| check now b (d1)
	bhi	Ld$inop		| 
	bne	3f
	tstl	d3		| 
	bne	Ld$inop		| 
3:
| Now comes the check for +/-INFINITY. We know that both are (maybe not
| finite) numbers, but we have to check if both are infinite whether we
| are adding or subtracting them.
	eorl	d7,d6		| to check sign bits
	bmi	1f
	andl	IMM (0x80000000),d7 | get (common) sign bit
	bra	Ld$infty
1:
| We know one (or both) are infinite, so we test for equality between the
| two numbers (if they are equal they have to be infinite both, so we
| return NaN).
	cmpl	d2,d0		| are both infinite?
	bne	1f		| if d0 <> d2 they are not equal
	cmpl	d3,d1		| if d0 == d2 test d3 and d1
	beq	Ld$inop		| if equal return NaN
1:	
	andl	IMM (0x80000000),d7 | get a's sign bit '
	cmpl	d4,d0		| test now for infinity
	beq	Ld$infty	| if a is INFINITY return with this sign
	bchg	IMM (31),d7	| else we know b is INFINITY and has
	bra	Ld$infty	| the opposite sign

|=============================================================================
|                              __muldf3
|=============================================================================

| double __muldf3(double, double);
SYM (__muldf3):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movel	a6@(8),d0		| get a into d0-d1
	movel	a6@(12),d1		| 
	movel	a6@(16),d2		| and b into d2-d3
	movel	a6@(20),d3		|
	movel	d0,d7			| d7 will hold the sign of the product
	eorl	d2,d7			|
	andl	IMM (0x80000000),d7	|
	movel	d7,a0			| save sign bit into a0 
	movel	IMM (0x7ff00000),d7	| useful constant (+INFINITY)
	movel	d7,d6			| another (mask for fraction)
	notl	d6			|
	bclr	IMM (31),d0		| get rid of a's sign bit '
	movel	d0,d4			| 
	orl	d1,d4			| 
	beq	Lmuldf$a$0		| branch if a is zero
	movel	d0,d4			|
	bclr	IMM (31),d2		| get rid of b's sign bit '
	movel	d2,d5			|
	orl	d3,d5			| 
	beq	Lmuldf$b$0		| branch if b is zero
	movel	d2,d5			| 
	cmpl	d7,d0			| is a big?
	bhi	Lmuldf$inop		| if a is NaN return NaN
	beq	Lmuldf$a$nf		| we still have to check d1 and b ...
	cmpl	d7,d2			| now compare b with INFINITY
	bhi	Lmuldf$inop		| is b NaN?
	beq	Lmuldf$b$nf 		| we still have to check d3 ...
| Here we have both numbers finite and nonzero (and with no sign bit).
| Now we get the exponents into d4 and d5.
	andl	d7,d4			| isolate exponent in d4
	beq	Lmuldf$a$den		| if exponent zero, have denormalized
	andl	d6,d0			| isolate fraction
	orl	IMM (0x00100000),d0	| and put hidden bit back
	swap	d4			| I like exponents in the first byte
	lsrw	IMM (4),d4		| 
Lmuldf$1:			
	andl	d7,d5			|
	beq	Lmuldf$b$den		|
	andl	d6,d2			|
	orl	IMM (0x00100000),d2	| and put hidden bit back
	swap	d5			|
	lsrw	IMM (4),d5		|
Lmuldf$2:				|
	addw	d5,d4			| add exponents
	subw	IMM (D_BIAS+1),d4	| and subtract bias (plus one)

| We are now ready to do the multiplication. The situation is as follows:
| both a and b have bit 52 ( bit 20 of d0 and d2) set (even if they were 
| denormalized to start with!), which means that in the product bit 104 
| (which will correspond to bit 8 of the fourth long) is set.

| Here we have to do the product.
| To do it we have to juggle the registers back and forth, as there are not
| enough to keep everything in them. So we use the address registers to keep
| some intermediate data.

	moveml	a2-a3,sp@-	| save a2 and a3 for temporary use
	movel	IMM (0),a2	| a2 is a null register
	movel	d4,a3		| and a3 will preserve the exponent

| First, shift d2-d3 so bit 20 becomes bit 31:
	rorl	IMM (5),d2	| rotate d2 5 places right
	swap	d2		| and swap it
	rorl	IMM (5),d3	| do the same thing with d3
	swap	d3		|
	movew	d3,d6		| get the rightmost 11 bits of d3
	andw	IMM (0x07ff),d6	|
	orw	d6,d2		| and put them into d2
	andw	IMM (0xf800),d3	| clear those bits in d3

	movel	d2,d6		| move b into d6-d7
	movel	d3,d7           | move a into d4-d5
	movel	d0,d4           | and clear d0-d1-d2-d3 (to put result)
	movel	d1,d5           |
	movel	IMM (0),d3	|
	movel	d3,d2           |
	movel	d3,d1           |
	movel	d3,d0	        |

| We use a1 as counter:	
	movel	IMM (DBL_MANT_DIG-1),a1		
	exg	d7,a1

1:	exg	d7,a1		| put counter back in a1
	addl	d3,d3		| shift sum once left
	addxl	d2,d2           |
	addxl	d1,d1           |
	addxl	d0,d0           |
	addl	d7,d7		|
	addxl	d6,d6		|
	bcc	2f		| if bit clear skip the following
	exg	d7,a2		|
	addl	d5,d3		| else add a to the sum
	addxl	d4,d2		|
	addxl	d7,d1		|
	addxl	d7,d0		|
	exg	d7,a2		| 
2:	exg	d7,a1		| put counter in d7
	dbf	d7,1b		| decrement and branch

	movel	a3,d4		| restore exponent
	moveml	sp@+,a2-a3

| Now we have the product in d0-d1-d2-d3, with bit 8 of d0 set. The 
| first thing to do now is to normalize it so bit 8 becomes bit 
| DBL_MANT_DIG-32 (to do the rounding); later we will shift right.
	swap	d0
	swap	d1
	movew	d1,d0
	swap	d2
	movew	d2,d1
	swap	d3
	movew	d3,d2
	movew	IMM (0),d3
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	
| Now round, check for over- and underflow, and exit.
	movel	a0,d7		| get sign bit back into d7
	movew	IMM (MULTIPLY),d5

	btst	IMM (DBL_MANT_DIG+1-32),d0
	beq	Lround$exit
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	addw	IMM (1),d4
	bra	Lround$exit

Lmuldf$inop:
	movew	IMM (MULTIPLY),d5
	bra	Ld$inop

Lmuldf$b$nf:
	movew	IMM (MULTIPLY),d5
	movel	a0,d7		| get sign bit back into d7
	tstl	d3		| we know d2 == 0x7ff00000, so check d3
	bne	Ld$inop		| if d3 <> 0 b is NaN
	bra	Ld$overflow	| else we have overflow (since a is finite)

Lmuldf$a$nf:
	movew	IMM (MULTIPLY),d5
	movel	a0,d7		| get sign bit back into d7
	tstl	d1		| we know d0 == 0x7ff00000, so check d1
	bne	Ld$inop		| if d1 <> 0 a is NaN
	bra	Ld$overflow	| else signal overflow

| If either number is zero return zero, unless the other is +/-INFINITY or
| NaN, in which case we return NaN.
Lmuldf$b$0:
	movew	IMM (MULTIPLY),d5
	exg	d2,d0		| put b (==0) into d0-d1
	exg	d3,d1		| and a (with sign bit cleared) into d2-d3
	bra	1f
Lmuldf$a$0:
	movel	a6@(16),d2	| put b into d2-d3 again
	movel	a6@(20),d3	|
	bclr	IMM (31),d2	| clear sign bit
1:	cmpl	IMM (0x7ff00000),d2 | check for non-finiteness
	bge	Ld$inop		| in case NaN or +/-INFINITY return NaN
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7
	unlk	a6
	rts

| If a number is denormalized we put an exponent of 1 but do not put the 
| hidden bit back into the fraction; instead we shift left until bit 21
| (the hidden bit) is set, adjusting the exponent accordingly. We do this
| to ensure that the product of the fractions is close to 1.
Lmuldf$a$den:
	movel	IMM (1),d4
	andl	d6,d0
1:	addl	d1,d1           | shift a left until bit 20 is set
	addxl	d0,d0		|
	subw	IMM (1),d4	| and adjust exponent
	btst	IMM (20),d0	|
	bne	Lmuldf$1        |
	bra	1b

Lmuldf$b$den:
	movel	IMM (1),d5
	andl	d6,d2
1:	addl	d3,d3		| shift b left until bit 20 is set
	addxl	d2,d2		|
	subw	IMM (1),d5	| and adjust exponent
	btst	IMM (20),d2	|
	bne	Lmuldf$2	|
	bra	1b


|=============================================================================
|                              __divdf3
|=============================================================================

| double __divdf3(double, double);
SYM (__divdf3):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movel	a6@(8),d0	| get a into d0-d1
	movel	a6@(12),d1	| 
	movel	a6@(16),d2	| and b into d2-d3
	movel	a6@(20),d3	|
	movel	d0,d7		| d7 will hold the sign of the result
	eorl	d2,d7		|
	andl	IMM (0x80000000),d7
	movel	d7,a0		| save sign into a0
	movel	IMM (0x7ff00000),d7 | useful constant (+INFINITY)
	movel	d7,d6		| another (mask for fraction)
	notl	d6		|
	bclr	IMM (31),d0	| get rid of a's sign bit '
	movel	d0,d4		|
	orl	d1,d4		|
	beq	Ldivdf$a$0	| branch if a is zero
	movel	d0,d4		|
	bclr	IMM (31),d2	| get rid of b's sign bit '
	movel	d2,d5		|
	orl	d3,d5		|
	beq	Ldivdf$b$0	| branch if b is zero
	movel	d2,d5
	cmpl	d7,d0		| is a big?
	bhi	Ldivdf$inop	| if a is NaN return NaN
	beq	Ldivdf$a$nf	| if d0 == 0x7ff00000 we check d1
	cmpl	d7,d2		| now compare b with INFINITY 
	bhi	Ldivdf$inop	| if b is NaN return NaN
	beq	Ldivdf$b$nf	| if d2 == 0x7ff00000 we check d3
| Here we have both numbers finite and nonzero (and with no sign bit).
| Now we get the exponents into d4 and d5 and normalize the numbers to
| ensure that the ratio of the fractions is around 1. We do this by
| making sure that both numbers have bit #DBL_MANT_DIG-32-1 (hidden bit)
| set, even if they were denormalized to start with.
| Thus, the result will satisfy: 2 > result > 1/2.
	andl	d7,d4		| and isolate exponent in d4
	beq	Ldivdf$a$den	| if exponent is zero we have a denormalized
	andl	d6,d0		| and isolate fraction
	orl	IMM (0x00100000),d0 | and put hidden bit back
	swap	d4		| I like exponents in the first byte
	lsrw	IMM (4),d4	| 
Ldivdf$1:			| 
	andl	d7,d5		|
	beq	Ldivdf$b$den	|
	andl	d6,d2		|
	orl	IMM (0x00100000),d2
	swap	d5		|
	lsrw	IMM (4),d5	|
Ldivdf$2:			|
	subw	d5,d4		| subtract exponents
	addw	IMM (D_BIAS),d4	| and add bias

| We are now ready to do the division. We have prepared things in such a way
| that the ratio of the fractions will be less than 2 but greater than 1/2.
| At this point the registers in use are:
| d0-d1	hold a (first operand, bit DBL_MANT_DIG-32=0, bit 
| DBL_MANT_DIG-1-32=1)
| d2-d3	hold b (second operand, bit DBL_MANT_DIG-32=1)
| d4	holds the difference of the exponents, corrected by the bias
| a0	holds the sign of the ratio

| To do the rounding correctly we need to keep information about the
| nonsignificant bits. One way to do this would be to do the division
| using four registers; another is to use two registers (as originally
| I did), but use a sticky bit to preserve information about the 
| fractional part. Note that we can keep that info in a1, which is not
| used.
	movel	IMM (0),d6	| d6-d7 will hold the result
	movel	d6,d7		| 
	movel	IMM (0),a1	| and a1 will hold the sticky bit

	movel	IMM (DBL_MANT_DIG-32+1),d5	
	
1:	cmpl	d0,d2		| is a < b?
	bhi	3f		| if b > a skip the following
	beq	4f		| if d0==d2 check d1 and d3
2:	subl	d3,d1		| 
	subxl	d2,d0		| a <-- a - b
	bset	d5,d6		| set the corresponding bit in d6
3:	addl	d1,d1		| shift a by 1
	addxl	d0,d0		|
	dbra	d5,1b		| and branch back
	bra	5f			
4:	cmpl	d1,d3		| here d0==d2, so check d1 and d3
	bhi	3b		| if d1 > d2 skip the subtraction
	bra	2b		| else go do it
5:
| Here we have to start setting the bits in the second long.
	movel	IMM (31),d5	| again d5 is counter

1:	cmpl	d0,d2		| is a < b?
	bhi	3f		| if b > a skip the following
	beq	4f		| if d0==d2 check d1 and d3
2:	subl	d3,d1		| 
	subxl	d2,d0		| a <-- a - b
	bset	d5,d7		| set the corresponding bit in d7
3:	addl	d1,d1		| shift a by 1
	addxl	d0,d0		|
	dbra	d5,1b		| and branch back
	bra	5f			
4:	cmpl	d1,d3		| here d0==d2, so check d1 and d3
	bhi	3b		| if d1 > d2 skip the subtraction
	bra	2b		| else go do it
5:
| Now go ahead checking until we hit a one, which we store in d2.
	movel	IMM (DBL_MANT_DIG),d5
1:	cmpl	d2,d0		| is a < b?
	bhi	4f		| if b < a, exit
	beq	3f		| if d0==d2 check d1 and d3
2:	addl	d1,d1		| shift a by 1
	addxl	d0,d0		|
	dbra	d5,1b		| and branch back
	movel	IMM (0),d2	| here no sticky bit was found
	movel	d2,d3
	bra	5f			
3:	cmpl	d1,d3		| here d0==d2, so check d1 and d3
	bhi	2b		| if d1 > d2 go back
4:
| Here put the sticky bit in d2-d3 (in the position which actually corresponds
| to it; if you don't do this the algorithm loses in some cases). '
	movel	IMM (0),d2
	movel	d2,d3
	subw	IMM (DBL_MANT_DIG),d5
	addw	IMM (63),d5
	cmpw	IMM (31),d5
	bhi	2f
1:	bset	d5,d3
	bra	5f
	subw	IMM (32),d5
2:	bset	d5,d2
5:
| Finally we are finished! Move the longs in the address registers to
| their final destination:
	movel	d6,d0
	movel	d7,d1
	movel	IMM (0),d3

| Here we have finished the division, with the result in d0-d1-d2-d3, with
| 2^21 <= d6 < 2^23. Thus bit 23 is not set, but bit 22 could be set.
| If it is not, then definitely bit 21 is set. Normalize so bit 22 is
| not set:
	btst	IMM (DBL_MANT_DIG-32+1),d0
	beq	1f
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	roxrl	IMM (1),d2
	roxrl	IMM (1),d3
	addw	IMM (1),d4
1:
| Now round, check for over- and underflow, and exit.
	movel	a0,d7		| restore sign bit to d7
	movew	IMM (DIVIDE),d5
	bra	Lround$exit

Ldivdf$inop:
	movew	IMM (DIVIDE),d5
	bra	Ld$inop

Ldivdf$a$0:
| If a is zero check to see whether b is zero also. In that case return
| NaN; then check if b is NaN, and return NaN also in that case. Else
| return zero.
	movew	IMM (DIVIDE),d5
	bclr	IMM (31),d2	|
	movel	d2,d4		| 
	orl	d3,d4		| 
	beq	Ld$inop		| if b is also zero return NaN
	cmpl	IMM (0x7ff00000),d2 | check for NaN
	bhi	Ld$inop		| 
	blt	1f		|
	tstl	d3		|
	bne	Ld$inop		|
1:	movel	IMM (0),d0	| else return zero
	movel	d0,d1		| 
	lea	SYM (_fpCCR),a0	| clear exception flags
	movew	IMM (0),a0@	|
	moveml	sp@+,d2-d7	| 
	unlk	a6		| 
	rts			| 	

Ldivdf$b$0:
	movew	IMM (DIVIDE),d5
| If we got here a is not zero. Check if a is NaN; in that case return NaN,
| else return +/-INFINITY. Remember that a is in d0 with the sign bit 
| cleared already.
	movel	a0,d7		| put a's sign bit back in d7 '
	cmpl	IMM (0x7ff00000),d0 | compare d0 with INFINITY
	bhi	Ld$inop		| if larger it is NaN
	tstl	d1		| 
	bne	Ld$inop		| 
	bra	Ld$div$0	| else signal DIVIDE_BY_ZERO

Ldivdf$b$nf:
	movew	IMM (DIVIDE),d5
| If d2 == 0x7ff00000 we have to check d3.
	tstl	d3		|
	bne	Ld$inop		| if d3 <> 0, b is NaN
	bra	Ld$underflow	| else b is +/-INFINITY, so signal underflow

Ldivdf$a$nf:
	movew	IMM (DIVIDE),d5
| If d0 == 0x7ff00000 we have to check d1.
	tstl	d1		|
	bne	Ld$inop		| if d1 <> 0, a is NaN
| If a is INFINITY we have to check b
	cmpl	d7,d2		| compare b with INFINITY 
	bge	Ld$inop		| if b is NaN or INFINITY return NaN
	tstl	d3		|
	bne	Ld$inop		| 
	bra	Ld$overflow	| else return overflow

| If a number is denormalized we put an exponent of 1 but do not put the 
| bit back into the fraction.
Ldivdf$a$den:
	movel	IMM (1),d4
	andl	d6,d0
1:	addl	d1,d1		| shift a left until bit 20 is set
	addxl	d0,d0
	subw	IMM (1),d4	| and adjust exponent
	btst	IMM (DBL_MANT_DIG-32-1),d0
	bne	Ldivdf$1
	bra	1b

Ldivdf$b$den:
	movel	IMM (1),d5
	andl	d6,d2
1:	addl	d3,d3		| shift b left until bit 20 is set
	addxl	d2,d2
	subw	IMM (1),d5	| and adjust exponent
	btst	IMM (DBL_MANT_DIG-32-1),d2
	bne	Ldivdf$2
	bra	1b

Lround$exit:
| This is a common exit point for __muldf3 and __divdf3. When they enter
| this point the sign of the result is in d7, the result in d0-d1, normalized
| so that 2^21 <= d0 < 2^22, and the exponent is in the lower byte of d4.

| First check for underlow in the exponent:
	cmpw	IMM (-DBL_MANT_DIG-1),d4		
	blt	Ld$underflow	
| It could happen that the exponent is less than 1, in which case the 
| number is denormalized. In this case we shift right and adjust the 
| exponent until it becomes 1 or the fraction is zero (in the latter case 
| we signal underflow and return zero).
	movel	d7,a0		|
	movel	IMM (0),d6	| use d6-d7 to collect bits flushed right
	movel	d6,d7		| use d6-d7 to collect bits flushed right
	cmpw	IMM (1),d4	| if the exponent is less than 1 we 
	bge	2f		| have to shift right (denormalize)
1:	addw	IMM (1),d4	| adjust the exponent
	lsrl	IMM (1),d0	| shift right once 
	roxrl	IMM (1),d1	|
	roxrl	IMM (1),d2	|
	roxrl	IMM (1),d3	|
	roxrl	IMM (1),d6	| 
	roxrl	IMM (1),d7	|
	cmpw	IMM (1),d4	| is the exponent 1 already?
	beq	2f		| if not loop back
	bra	1b              |
	bra	Ld$underflow	| safety check, shouldn't execute '
2:	orl	d6,d2		| this is a trick so we don't lose  '
	orl	d7,d3		| the bits which were flushed right
	movel	a0,d7		| get back sign bit into d7
| Now call the rounding routine (which takes care of denormalized numbers):
	lea	Lround$0,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Lround$0:
| Here we have a correctly rounded result (either normalized or denormalized).

| Here we should have either a normalized number or a denormalized one, and
| the exponent is necessarily larger or equal to 1 (so we don't have to  '
| check again for underflow!). We have to check for overflow or for a 
| denormalized number (which also signals underflow).
| Check for overflow (i.e., exponent >= 0x7ff).
	cmpw	IMM (0x07ff),d4
	bge	Ld$overflow
| Now check for a denormalized number (exponent==0):
	movew	d4,d4
	beq	Ld$den
1:
| Put back the exponents and sign and return.
	lslw	IMM (4),d4	| exponent back to fourth byte
	bclr	IMM (DBL_MANT_DIG-32-1),d0
	swap	d0		| and put back exponent
	orw	d4,d0		| 
	swap	d0		|
	orl	d7,d0		| and sign also

	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7
	unlk	a6
	rts

|=============================================================================
|                              __negdf2
|=============================================================================

| double __negdf2(double, double);
SYM (__negdf2):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movew	IMM (NEGATE),d5
	movel	a6@(8),d0	| get number to negate in d0-d1
	movel	a6@(12),d1	|
	bchg	IMM (31),d0	| negate
	movel	d0,d2		| make a positive copy (for the tests)
	bclr	IMM (31),d2	|
	movel	d2,d4		| check for zero
	orl	d1,d4		|
	beq	2f		| if zero (either sign) return +zero
	cmpl	IMM (0x7ff00000),d2 | compare to +INFINITY
	blt	1f		| if finite, return
	bhi	Ld$inop		| if larger (fraction not zero) is NaN
	tstl	d1		| if d2 == 0x7ff00000 check d1
	bne	Ld$inop		|
	movel	d0,d7		| else get sign and return INFINITY
	andl	IMM (0x80000000),d7
	bra	Ld$infty		
1:	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7
	unlk	a6
	rts
2:	bclr	IMM (31),d0
	bra	1b

|=============================================================================
|                              __cmpdf2
|=============================================================================

GREATER =  1
LESS    = -1
EQUAL   =  0

| int __cmpdf2(double, double);
SYM (__cmpdf2):
	link	a6,IMM (0)
	moveml	d2-d7,sp@- 	| save registers
	movew	IMM (COMPARE),d5
	movel	a6@(8),d0	| get first operand
	movel	a6@(12),d1	|
	movel	a6@(16),d2	| get second operand
	movel	a6@(20),d3	|
| First check if a and/or b are (+/-) zero and in that case clear
| the sign bit.
	movel	d0,d6		| copy signs into d6 (a) and d7(b)
	bclr	IMM (31),d0	| and clear signs in d0 and d2
	movel	d2,d7		|
	bclr	IMM (31),d2	|
	cmpl	IMM (0x7fff0000),d0 | check for a == NaN
	bhi	Ld$inop		| if d0 > 0x7ff00000, a is NaN
	beq	Lcmpdf$a$nf	| if equal can be INFINITY, so check d1
	movel	d0,d4		| copy into d4 to test for zero
	orl	d1,d4		|
	beq	Lcmpdf$a$0	|
Lcmpdf$0:
	cmpl	IMM (0x7fff0000),d2 | check for b == NaN
	bhi	Ld$inop		| if d2 > 0x7ff00000, b is NaN
	beq	Lcmpdf$b$nf	| if equal can be INFINITY, so check d3
	movel	d2,d4		|
	orl	d3,d4		|
	beq	Lcmpdf$b$0	|
Lcmpdf$1:
| Check the signs
	eorl	d6,d7
	bpl	1f
| If the signs are not equal check if a >= 0
	tstl	d6
	bpl	Lcmpdf$a$gt$b	| if (a >= 0 && b < 0) => a > b
	bmi	Lcmpdf$b$gt$a	| if (a < 0 && b >= 0) => a < b
1:
| If the signs are equal check for < 0
	tstl	d6
	bpl	1f
| If both are negative exchange them
	exg	d0,d2
	exg	d1,d3
1:
| Now that they are positive we just compare them as longs (does this also
| work for denormalized numbers?).
	cmpl	d0,d2
	bhi	Lcmpdf$b$gt$a	| |b| > |a|
	bne	Lcmpdf$a$gt$b	| |b| < |a|
| If we got here d0 == d2, so we compare d1 and d3.
	cmpl	d1,d3
	bhi	Lcmpdf$b$gt$a	| |b| > |a|
	bne	Lcmpdf$a$gt$b	| |b| < |a|
| If we got here a == b.
	movel	IMM (EQUAL),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts
Lcmpdf$a$gt$b:
	movel	IMM (GREATER),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts
Lcmpdf$b$gt$a:
	movel	IMM (LESS),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts

Lcmpdf$a$0:	
	bclr	IMM (31),d6
	bra	Lcmpdf$0
Lcmpdf$b$0:
	bclr	IMM (31),d7
	bra	Lcmpdf$1

Lcmpdf$a$nf:
	tstl	d1
	bne	Ld$inop
	bra	Lcmpdf$0

Lcmpdf$b$nf:
	tstl	d3
	bne	Ld$inop
	bra	Lcmpdf$1

|=============================================================================
|                           rounding routines
|=============================================================================

| The rounding routines expect the number to be normalized in registers
| d0-d1-d2-d3, with the exponent in register d4. They assume that the 
| exponent is larger or equal to 1. They return a properly normalized number
| if possible, and a denormalized number otherwise. The exponent is returned
| in d4.

Lround$to$nearest:
| We now normalize as suggested by D. Knuth ("Seminumerical Algorithms"):
| Here we assume that the exponent is not too small (this should be checked
| before entering the rounding routine), but the number could be denormalized.

| Check for denormalized numbers:
1:	btst	IMM (DBL_MANT_DIG-32),d0
	bne	2f		| if set the number is normalized
| Normalize shifting left until bit #DBL_MANT_DIG-32 is set or the exponent 
| is one (remember that a denormalized number corresponds to an 
| exponent of -D_BIAS+1).
	cmpw	IMM (1),d4	| remember that the exponent is at least one
 	beq	2f		| an exponent of one means denormalized
	addl	d3,d3		| else shift and adjust the exponent
	addxl	d2,d2		|
	addxl	d1,d1		|
	addxl	d0,d0		|
	dbra	d4,1b		|
2:
| Now round: we do it as follows: after the shifting we can write the
| fraction part as f + delta, where 1 < f < 2^25, and 0 <= delta <= 2.
| If delta < 1, do nothing. If delta > 1, add 1 to f. 
| If delta == 1, we make sure the rounded number will be even (odd?) 
| (after shifting).
	btst	IMM (0),d1	| is delta < 1?
	beq	2f		| if so, do not do anything
	orl	d2,d3		| is delta == 1?
	bne	1f		| if so round to even
	movel	d1,d3		| 
	andl	IMM (2),d3	| bit 1 is the last significant bit
	movel	IMM (0),d2	|
	addl	d3,d1		|
	addxl	d2,d0		|
	bra	2f		| 
1:	movel	IMM (1),d3	| else add 1 
	movel	IMM (0),d2	|
	addl	d3,d1		|
	addxl	d2,d0
| Shift right once (because we used bit #DBL_MANT_DIG-32!).
2:	lsrl	IMM (1),d0
	roxrl	IMM (1),d1		

| Now check again bit #DBL_MANT_DIG-32 (rounding could have produced a
| 'fraction overflow' ...).
	btst	IMM (DBL_MANT_DIG-32),d0	
	beq	1f
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	addw	IMM (1),d4
1:
| If bit #DBL_MANT_DIG-32-1 is clear we have a denormalized number, so we 
| have to put the exponent to zero and return a denormalized number.
	btst	IMM (DBL_MANT_DIG-32-1),d0
	beq	1f
	jmp	a0@
1:	movel	IMM (0),d4
	jmp	a0@

Lround$to$zero:
Lround$to$plus:
Lround$to$minus:
	jmp	a0@
#endif /* L_double */

#ifdef  L_float

	.globl	SYM (_fpCCR)
	.globl  $_exception_handler

QUIET_NaN    = 0xffffffff
SIGNL_NaN    = 0x7f800001
INFINITY     = 0x7f800000

F_MAX_EXP      = 0xff
F_BIAS         = 126
FLT_MAX_EXP    = F_MAX_EXP - F_BIAS
FLT_MIN_EXP    = 1 - F_BIAS
FLT_MANT_DIG   = 24

INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

SINGLE_FLOAT = 1

NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 | round result to nearest representable value
ROUND_TO_ZERO     = 1 | round result towards zero
ROUND_TO_PLUS     = 2 | round result towards plus infinity
ROUND_TO_MINUS    = 3 | round result towards minus infinity

| Entry points:

	.globl SYM (__addsf3)
	.globl SYM (__subsf3)
	.globl SYM (__mulsf3)
	.globl SYM (__divsf3)
	.globl SYM (__negsf2)
	.globl SYM (__cmpsf2)

| These are common routines to return and signal exceptions.	

	.text
	.even

Lf$den:
| Return and signal a denormalized number
	orl	d7,d0
	movew	IMM (UNDERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (SINGLE_FLOAT),d6
	jmp	$_exception_handler

Lf$infty:
Lf$overflow:
| Return a properly signed INFINITY and set the exception flags 
	movel	IMM (INFINITY),d0
	orl	d7,d0
	movew	IMM (OVERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (SINGLE_FLOAT),d6
	jmp	$_exception_handler

Lf$underflow:
| Return 0 and set the exception flags 
	movel	IMM (0),d0
	movew	IMM (UNDERFLOW),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (SINGLE_FLOAT),d6
	jmp	$_exception_handler

Lf$inop:
| Return a quiet NaN and set the exception flags
	movel	IMM (QUIET_NaN),d0
	movew	IMM (INVALID_OPERATION),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (SINGLE_FLOAT),d6
	jmp	$_exception_handler

Lf$div$0:
| Return a properly signed INFINITY and set the exception flags
	movel	IMM (INFINITY),d0
	orl	d7,d0
	movew	IMM (DIVIDE_BY_ZERO),d7
	orw	IMM (INEXACT_RESULT),d7
	movew	IMM (SINGLE_FLOAT),d6
	jmp	$_exception_handler

|=============================================================================
|=============================================================================
|                         single precision routines
|=============================================================================
|=============================================================================

| A single precision floating point number (float) has the format:
|
| struct _float {
|  unsigned int sign      : 1;  /* sign bit */ 
|  unsigned int exponent  : 8;  /* exponent, shifted by 126 */
|  unsigned int fraction  : 23; /* fraction */
| } float;
| 
| Thus sizeof(float) = 4 (32 bits). 
|
| All the routines are callable from C programs, and return the result 
| in the single register d0. They also preserve all registers except 
| d0-d1 and a0-a1.

|=============================================================================
|                              __subsf3
|=============================================================================

| float __subsf3(float, float);
SYM (__subsf3):
	bchg	IMM (31),sp@(8)	| change sign of second operand
				| and fall through
|=============================================================================
|                              __addsf3
|=============================================================================

| float __addsf3(float, float);
SYM (__addsf3):
	link	a6,IMM (0)	| everything will be done in registers
	moveml	d2-d7,sp@-	| save all data registers but d0-d1
	movel	a6@(8),d0	| get first operand
	movel	a6@(12),d1	| get second operand
	movel	d0,d6		| get d0's sign bit '
	addl	d0,d0		| check and clear sign bit of a
	beq	Laddsf$b	| if zero return second operand
	movel	d1,d7		| save b's sign bit '
	addl	d1,d1		| get rid of sign bit
	beq	Laddsf$a	| if zero return first operand

	movel	d6,a0		| save signs in address registers
	movel	d7,a1		| so we can use d6 and d7

| Get the exponents and check for denormalized and/or infinity.

	movel	IMM (0x00ffffff),d4	| mask to get fraction
	movel	IMM (0x01000000),d5	| mask to put hidden bit back

	movel	d0,d6		| save a to get exponent
	andl	d4,d0		| get fraction in d0
	notl 	d4		| make d4 into a mask for the exponent
	andl	d4,d6		| get exponent in d6
	beq	Laddsf$a$den	| branch if a is denormalized
	cmpl	d4,d6		| check for INFINITY or NaN
	beq	Laddsf$nf
	swap	d6		| put exponent into first word
	orl	d5,d0		| and put hidden bit back
Laddsf$1:
| Now we have a's exponent in d6 (second byte) and the mantissa in d0. '
	movel	d1,d7		| get exponent in d7
	andl	d4,d7		| 
	beq	Laddsf$b$den	| branch if b is denormalized
	cmpl	d4,d7		| check for INFINITY or NaN
	beq	Laddsf$nf
	swap	d7		| put exponent into first word
	notl 	d4		| make d4 into a mask for the fraction
	andl	d4,d1		| get fraction in d1
	orl	d5,d1		| and put hidden bit back
Laddsf$2:
| Now we have b's exponent in d7 (second byte) and the mantissa in d1. '

| Note that the hidden bit corresponds to bit #FLT_MANT_DIG-1, and we 
| shifted right once, so bit #FLT_MANT_DIG is set (so we have one extra
| bit).

	movel	d1,d2		| move b to d2, since we want to use
				| two registers to do the sum
	movel	IMM (0),d1	| and clear the new ones
	movel	d1,d3		|

| Here we shift the numbers in registers d0 and d1 so the exponents are the
| same, and put the largest exponent in d6. Note that we are using two
| registers for each number (see the discussion by D. Knuth in "Seminumerical 
| Algorithms").
	cmpw	d6,d7		| compare exponents
	beq	Laddsf$3	| if equal don't shift '
	bhi	5f		| branch if second exponent largest
1:
	subl	d6,d7		| keep the largest exponent
	negl	d7
	lsrw	IMM (8),d7	| put difference in lower byte
| if difference is too large we don't shift (actually, we can just exit) '
	cmpw	IMM (FLT_MANT_DIG+2),d7		
	bge	Laddsf$b$small
	cmpw	IMM (16),d7	| if difference >= 16 swap
	bge	4f
2:
	subw	IMM (1),d7
3:	lsrl	IMM (1),d2	| shift right second operand
	roxrl	IMM (1),d3
	dbra	d7,3b
	bra	Laddsf$3
4:
	movew	d2,d3
	swap	d3
	movew	d3,d2
	swap	d2
	subw	IMM (16),d7
	bne	2b		| if still more bits, go back to normal case
	bra	Laddsf$3
5:
	exg	d6,d7		| exchange the exponents
	subl	d6,d7		| keep the largest exponent
	negl	d7		|
	lsrw	IMM (8),d7	| put difference in lower byte
| if difference is too large we don't shift (and exit!) '
	cmpw	IMM (FLT_MANT_DIG+2),d7		
	bge	Laddsf$a$small
	cmpw	IMM (16),d7	| if difference >= 16 swap
	bge	8f
6:
	subw	IMM (1),d7
7:	lsrl	IMM (1),d0	| shift right first operand
	roxrl	IMM (1),d1
	dbra	d7,7b
	bra	Laddsf$3
8:
	movew	d0,d1
	swap	d1
	movew	d1,d0
	swap	d0
	subw	IMM (16),d7
	bne	6b		| if still more bits, go back to normal case
				| otherwise we fall through

| Now we have a in d0-d1, b in d2-d3, and the largest exponent in d6 (the
| signs are stored in a0 and a1).

Laddsf$3:
| Here we have to decide whether to add or subtract the numbers
	exg	d6,a0		| get signs back
	exg	d7,a1		| and save the exponents
	eorl	d6,d7		| combine sign bits
	bmi	Lsubsf$0	| if negative a and b have opposite 
				| sign so we actually subtract the
				| numbers

| Here we have both positive or both negative
	exg	d6,a0		| now we have the exponent in d6
	movel	a0,d7		| and sign in d7
	andl	IMM (0x80000000),d7
| Here we do the addition.
	addl	d3,d1
	addxl	d2,d0
| Note: now we have d2, d3, d4 and d5 to play with! 

| Put the exponent, in the first byte, in d2, to use the "standard" rounding
| routines:
	movel	d6,d2
	lsrw	IMM (8),d2

| Before rounding normalize so bit #FLT_MANT_DIG is set (we will consider
| the case of denormalized numbers in the rounding routine itself).
| As in the addition (not in the subtraction!) we could have set 
| one more bit we check this:
	btst	IMM (FLT_MANT_DIG+1),d0	
	beq	1f
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	addl	IMM (1),d2
1:
	lea	Laddsf$4,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Laddsf$4:
| Put back the exponent, but check for overflow.
	cmpw	IMM (0xff),d2
	bhi	1f
	bclr	IMM (FLT_MANT_DIG-1),d0
	lslw	IMM (7),d2
	swap	d2
	orl	d2,d0
	bra	Laddsf$ret
1:
	movew	IMM (ADD),d5
	bra	Lf$overflow

Lsubsf$0:
| We are here if a > 0 and b < 0 (sign bits cleared).
| Here we do the subtraction.
	movel	d6,d7		| put sign in d7
	andl	IMM (0x80000000),d7

	subl	d3,d1		| result in d0-d1
	subxl	d2,d0		|
	beq	Laddsf$ret	| if zero just exit
	bpl	1f		| if positive skip the following
	bchg	IMM (31),d7	| change sign bit in d7
	negl	d1
	negxl	d0
1:
	exg	d2,a0		| now we have the exponent in d2
	lsrw	IMM (8),d2	| put it in the first byte

| Now d0-d1 is positive and the sign bit is in d7.

| Note that we do not have to normalize, since in the subtraction bit
| #FLT_MANT_DIG+1 is never set, and denormalized numbers are handled by
| the rounding routines themselves.
	lea	Lsubsf$1,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Lsubsf$1:
| Put back the exponent (we can't have overflow!). '
	bclr	IMM (FLT_MANT_DIG-1),d0
	lslw	IMM (7),d2
	swap	d2
	orl	d2,d0
	bra	Laddsf$ret

| If one of the numbers was too small (difference of exponents >= 
| FLT_MANT_DIG+2) we return the other (and now we don't have to '
| check for finiteness or zero).
Laddsf$a$small:
	movel	a6@(12),d0
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts

Laddsf$b$small:
	movel	a6@(8),d0
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts

| If the numbers are denormalized remember to put exponent equal to 1.

Laddsf$a$den:
	movel	d5,d6		| d5 contains 0x01000000
	swap	d6
	bra	Laddsf$1

Laddsf$b$den:
	movel	d5,d7
	swap	d7
	notl 	d4		| make d4 into a mask for the fraction
				| (this was not executed after the jump)
	bra	Laddsf$2

| The rest is mainly code for the different results which can be 
| returned (checking always for +/-INFINITY and NaN).

Laddsf$b:
| Return b (if a is zero).
	movel	a6@(12),d0
	bra	1f
Laddsf$a:
| Return a (if b is zero).
	movel	a6@(8),d0
1:
	movew	IMM (ADD),d5
| We have to check for NaN and +/-infty.
	movel	d0,d7
	andl	IMM (0x80000000),d7	| put sign in d7
	bclr	IMM (31),d0		| clear sign
	cmpl	IMM (INFINITY),d0	| check for infty or NaN
	bge	2f
	movel	d0,d0		| check for zero (we do this because we don't '
	bne	Laddsf$ret	| want to return -0 by mistake
	bclr	IMM (31),d7	| if zero be sure to clear sign
	bra	Laddsf$ret	| if everything OK just return
2:
| The value to be returned is either +/-infty or NaN
	andl	IMM (0x007fffff),d0	| check for NaN
	bne	Lf$inop			| if mantissa not zero is NaN
	bra	Lf$infty

Laddsf$ret:
| Normal exit (a and b nonzero, result is not NaN nor +/-infty).
| We have to clear the exception flags (just the exception type).
	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	orl	d7,d0		| put sign bit
	moveml	sp@+,d2-d7	| restore data registers
	unlk	a6		| and return
	rts

Laddsf$ret$den:
| Return a denormalized number (for addition we don't signal underflow) '
	lsrl	IMM (1),d0	| remember to shift right back once
	bra	Laddsf$ret	| and return

| Note: when adding two floats of the same sign if either one is 
| NaN we return NaN without regard to whether the other is finite or 
| not. When subtracting them (i.e., when adding two numbers of 
| opposite signs) things are more complicated: if both are INFINITY 
| we return NaN, if only one is INFINITY and the other is NaN we return
| NaN, but if it is finite we return INFINITY with the corresponding sign.

Laddsf$nf:
	movew	IMM (ADD),d5
| This could be faster but it is not worth the effort, since it is not
| executed very often. We sacrifice speed for clarity here.
	movel	a6@(8),d0	| get the numbers back (remember that we
	movel	a6@(12),d1	| did some processing already)
	movel	IMM (INFINITY),d4 | useful constant (INFINITY)
	movel	d0,d2		| save sign bits
	movel	d1,d3
	bclr	IMM (31),d0	| clear sign bits
	bclr	IMM (31),d1
| We know that one of them is either NaN of +/-INFINITY
| Check for NaN (if either one is NaN return NaN)
	cmpl	d4,d0		| check first a (d0)
	bhi	Lf$inop		
	cmpl	d4,d1		| check now b (d1)
	bhi	Lf$inop		
| Now comes the check for +/-INFINITY. We know that both are (maybe not
| finite) numbers, but we have to check if both are infinite whether we
| are adding or subtracting them.
	eorl	d3,d2		| to check sign bits
	bmi	1f
	movel	d0,d7
	andl	IMM (0x80000000),d7	| get (common) sign bit
	bra	Lf$infty
1:
| We know one (or both) are infinite, so we test for equality between the
| two numbers (if they are equal they have to be infinite both, so we
| return NaN).
	cmpl	d1,d0		| are both infinite?
	beq	Lf$inop		| if so return NaN

	movel	d0,d7
	andl	IMM (0x80000000),d7 | get a's sign bit '
	cmpl	d4,d0		| test now for infinity
	beq	Lf$infty	| if a is INFINITY return with this sign
	bchg	IMM (31),d7	| else we know b is INFINITY and has
	bra	Lf$infty	| the opposite sign

|=============================================================================
|                             __mulsf3
|=============================================================================

| float __mulsf3(float, float);
SYM (__mulsf3):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movel	a6@(8),d0	| get a into d0
	movel	a6@(12),d1	| and b into d1
	movel	d0,d7		| d7 will hold the sign of the product
	eorl	d1,d7		|
	andl	IMM (0x80000000),d7
	movel	IMM (INFINITY),d6	| useful constant (+INFINITY)
	movel	d6,d5			| another (mask for fraction)
	notl	d5			|
	movel	IMM (0x00800000),d4	| this is to put hidden bit back
	bclr	IMM (31),d0		| get rid of a's sign bit '
	movel	d0,d2			|
	beq	Lmulsf$a$0		| branch if a is zero
	bclr	IMM (31),d1		| get rid of b's sign bit '
	movel	d1,d3		|
	beq	Lmulsf$b$0	| branch if b is zero
	cmpl	d6,d0		| is a big?
	bhi	Lmulsf$inop	| if a is NaN return NaN
	beq	Lmulsf$inf	| if a is INFINITY we have to check b
	cmpl	d6,d1		| now compare b with INFINITY
	bhi	Lmulsf$inop	| is b NaN?
	beq	Lmulsf$overflow | is b INFINITY?
| Here we have both numbers finite and nonzero (and with no sign bit).
| Now we get the exponents into d2 and d3.
	andl	d6,d2		| and isolate exponent in d2
	beq	Lmulsf$a$den	| if exponent is zero we have a denormalized
	andl	d5,d0		| and isolate fraction
	orl	d4,d0		| and put hidden bit back
	swap	d2		| I like exponents in the first byte
	lsrw	IMM (7),d2	| 
Lmulsf$1:			| number
	andl	d6,d3		|
	beq	Lmulsf$b$den	|
	andl	d5,d1		|
	orl	d4,d1		|
	swap	d3		|
	lsrw	IMM (7),d3	|
Lmulsf$2:			|
	addw	d3,d2		| add exponents
	subw	IMM (F_BIAS+1),d2 | and subtract bias (plus one)

| We are now ready to do the multiplication. The situation is as follows:
| both a and b have bit FLT_MANT_DIG-1 set (even if they were 
| denormalized to start with!), which means that in the product 
| bit 2*(FLT_MANT_DIG-1) (that is, bit 2*FLT_MANT_DIG-2-32 of the 
| high long) is set. 

| To do the multiplication let us move the number a little bit around ...
	movel	d1,d6		| second operand in d6
	movel	d0,d5		| first operand in d4-d5
	movel	IMM (0),d4
	movel	d4,d1		| the sums will go in d0-d1
	movel	d4,d0

| now bit FLT_MANT_DIG-1 becomes bit 31:
	lsll	IMM (31-FLT_MANT_DIG+1),d6		

| Start the loop (we loop #FLT_MANT_DIG times):
	movew	IMM (FLT_MANT_DIG-1),d3	
1:	addl	d1,d1		| shift sum 
	addxl	d0,d0
	lsll	IMM (1),d6	| get bit bn
	bcc	2f		| if not set skip sum
	addl	d5,d1		| add a
	addxl	d4,d0
2:	dbf	d3,1b		| loop back

| Now we have the product in d0-d1, with bit (FLT_MANT_DIG - 1) + FLT_MANT_DIG
| (mod 32) of d0 set. The first thing to do now is to normalize it so bit 
| FLT_MANT_DIG is set (to do the rounding).
	rorl	IMM (6),d1
	swap	d1
	movew	d1,d3
	andw	IMM (0x03ff),d3
	andw	IMM (0xfd00),d1
	lsll	IMM (8),d0
	addl	d0,d0
	addl	d0,d0
	orw	d3,d0

	movew	IMM (MULTIPLY),d5
	
	btst	IMM (FLT_MANT_DIG+1),d0
	beq	Lround$exit
	lsrl	IMM (1),d0
	roxrl	IMM (1),d1
	addw	IMM (1),d2
	bra	Lround$exit

Lmulsf$inop:
	movew	IMM (MULTIPLY),d5
	bra	Lf$inop

Lmulsf$overflow:
	movew	IMM (MULTIPLY),d5
	bra	Lf$overflow

Lmulsf$inf:
	movew	IMM (MULTIPLY),d5
| If either is NaN return NaN; else both are (maybe infinite) numbers, so
| return INFINITY with the correct sign (which is in d7).
	cmpl	d6,d1		| is b NaN?
	bhi	Lf$inop		| if so return NaN
	bra	Lf$overflow	| else return +/-INFINITY

| If either number is zero return zero, unless the other is +/-INFINITY, 
| or NaN, in which case we return NaN.
Lmulsf$b$0:
| Here d1 (==b) is zero.
	movel	d1,d0		| put b into d0 (just a zero)
	movel	a6@(8),d1	| get a again to check for non-finiteness
	bra	1f
Lmulsf$a$0:
	movel	a6@(12),d1	| get b again to check for non-finiteness
1:	bclr	IMM (31),d1	| clear sign bit 
	cmpl	IMM (INFINITY),d1 | and check for a large exponent
	bge	Lf$inop		| if b is +/-INFINITY or NaN return NaN
	lea	SYM (_fpCCR),a0	| else return zero
	movew	IMM (0),a0@	| 
	moveml	sp@+,d2-d7	| 
	unlk	a6		| 
	rts			| 

| If a number is denormalized we put an exponent of 1 but do not put the 
| hidden bit back into the fraction; instead we shift left until bit 23
| (the hidden bit) is set, adjusting the exponent accordingly. We do this
| to ensure that the product of the fractions is close to 1.
Lmulsf$a$den:
	movel	IMM (1),d2
	andl	d5,d0
1:	addl	d0,d0		| shift a left (until bit 23 is set)
	subw	IMM (1),d2	| and adjust exponent
	btst	IMM (FLT_MANT_DIG-1),d0
	bne	Lmulsf$1	|
	bra	1b		| else loop back

Lmulsf$b$den:
	movel	IMM (1),d3
	andl	d5,d1
1:	addl	d1,d1		| shift b left until bit 23 is set
	subw	IMM (1),d3	| and adjust exponent
	btst	IMM (FLT_MANT_DIG-1),d1
	bne	Lmulsf$2	|
	bra	1b		| else loop back

|=============================================================================
|                             __divsf3
|=============================================================================

| float __divsf3(float, float);
SYM (__divsf3):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movel	a6@(8),d0		| get a into d0
	movel	a6@(12),d1		| and b into d1
	movel	d0,d7			| d7 will hold the sign of the result
	eorl	d1,d7			|
	andl	IMM (0x80000000),d7	| 
	movel	IMM (INFINITY),d6	| useful constant (+INFINITY)
	movel	d6,d5			| another (mask for fraction)
	notl	d5			|
	movel	IMM (0x00800000),d4	| this is to put hidden bit back
	bclr	IMM (31),d0		| get rid of a's sign bit '
	movel	d0,d2			|
	beq	Ldivsf$a$0		| branch if a is zero
	bclr	IMM (31),d1		| get rid of b's sign bit '
	movel	d1,d3			|
	beq	Ldivsf$b$0		| branch if b is zero
	cmpl	d6,d0			| is a big?
	bhi	Ldivsf$inop		| if a is NaN return NaN
	beq	Ldivsf$inf		| if a is INFINITY we have to check b
	cmpl	d6,d1			| now compare b with INFINITY 
	bhi	Ldivsf$inop		| if b is NaN return NaN
	beq	Ldivsf$underflow
| Here we have both numbers finite and nonzero (and with no sign bit).
| Now we get the exponents into d2 and d3 and normalize the numbers to
| ensure that the ratio of the fractions is close to 1. We do this by
| making sure that bit #FLT_MANT_DIG-1 (hidden bit) is set.
	andl	d6,d2		| and isolate exponent in d2
	beq	Ldivsf$a$den	| if exponent is zero we have a denormalized
	andl	d5,d0		| and isolate fraction
	orl	d4,d0		| and put hidden bit back
	swap	d2		| I like exponents in the first byte
	lsrw	IMM (7),d2	| 
Ldivsf$1:			| 
	andl	d6,d3		|
	beq	Ldivsf$b$den	|
	andl	d5,d1		|
	orl	d4,d1		|
	swap	d3		|
	lsrw	IMM (7),d3	|
Ldivsf$2:			|
	subw	d3,d2		| subtract exponents
 	addw	IMM (F_BIAS),d2	| and add bias
 
| We are now ready to do the division. We have prepared things in such a way
| that the ratio of the fractions will be less than 2 but greater than 1/2.
| At this point the registers in use are:
| d0	holds a (first operand, bit FLT_MANT_DIG=0, bit FLT_MANT_DIG-1=1)
| d1	holds b (second operand, bit FLT_MANT_DIG=1)
| d2	holds the difference of the exponents, corrected by the bias
| d7	holds the sign of the ratio
| d4, d5, d6 hold some constants
	movel	d7,a0		| d6-d7 will hold the ratio of the fractions
	movel	IMM (0),d6	| 
	movel	d6,d7

	movew	IMM (FLT_MANT_DIG+1),d3
1:	cmpl	d0,d1		| is a < b?
	bhi	2f		|
	bset	d3,d6		| set a bit in d6
	subl	d1,d0		| if a >= b  a <-- a-b
	beq	3f		| if a is zero, exit
2:	addl	d0,d0		| multiply a by 2
	dbra	d3,1b

| Now we keep going to set the sticky bit ...
	movew	IMM (FLT_MANT_DIG),d3
1:	cmpl	d0,d1
	ble	2f
	addl	d0,d0
	dbra	d3,1b
	movel	IMM (0),d1
	bra	3f
2:	movel	IMM (0),d1
	subw	IMM (FLT_MANT_DIG),d3
	addw	IMM (31),d3
	bset	d3,d1
3:
	movel	d6,d0		| put the ratio in d0-d1
	movel	a0,d7		| get sign back

| Because of the normalization we did before we are guaranteed that 
| d0 is smaller than 2^26 but larger than 2^24. Thus bit 26 is not set,
| bit 25 could be set, and if it is not set then bit 24 is necessarily set.
	btst	IMM (FLT_MANT_DIG+1),d0		
	beq	1f              | if it is not set, then bit 24 is set
	lsrl	IMM (1),d0	|
	addw	IMM (1),d2	|
1:
| Now round, check for over- and underflow, and exit.
	movew	IMM (DIVIDE),d5
	bra	Lround$exit

Ldivsf$inop:
	movew	IMM (DIVIDE),d5
	bra	Lf$inop

Ldivsf$overflow:
	movew	IMM (DIVIDE),d5
	bra	Lf$overflow

Ldivsf$underflow:
	movew	IMM (DIVIDE),d5
	bra	Lf$underflow

Ldivsf$a$0:
	movew	IMM (DIVIDE),d5
| If a is zero check to see whether b is zero also. In that case return
| NaN; then check if b is NaN, and return NaN also in that case. Else
| return zero.
	andl	IMM (0x7fffffff),d1	| clear sign bit and test b
	beq	Lf$inop			| if b is also zero return NaN
	cmpl	IMM (INFINITY),d1	| check for NaN
	bhi	Lf$inop			| 
	movel	IMM (0),d0		| else return zero
	lea	SYM (_fpCCR),a0		|
	movew	IMM (0),a0@		|
	moveml	sp@+,d2-d7		| 
	unlk	a6			| 
	rts				| 
	
Ldivsf$b$0:
	movew	IMM (DIVIDE),d5
| If we got here a is not zero. Check if a is NaN; in that case return NaN,
| else return +/-INFINITY. Remember that a is in d0 with the sign bit 
| cleared already.
	cmpl	IMM (INFINITY),d0	| compare d0 with INFINITY
	bhi	Lf$inop			| if larger it is NaN
	bra	Lf$div$0		| else signal DIVIDE_BY_ZERO

Ldivsf$inf:
	movew	IMM (DIVIDE),d5
| If a is INFINITY we have to check b
	cmpl	IMM (INFINITY),d1	| compare b with INFINITY 
	bge	Lf$inop			| if b is NaN or INFINITY return NaN
	bra	Lf$overflow		| else return overflow

| If a number is denormalized we put an exponent of 1 but do not put the 
| bit back into the fraction.
Ldivsf$a$den:
	movel	IMM (1),d2
	andl	d5,d0
1:	addl	d0,d0		| shift a left until bit FLT_MANT_DIG-1 is set
	subw	IMM (1),d2	| and adjust exponent
	btst	IMM (FLT_MANT_DIG-1),d0
	bne	Ldivsf$1
	bra	1b

Ldivsf$b$den:
	movel	IMM (1),d3
	andl	d5,d1
1:	addl	d1,d1		| shift b left until bit FLT_MANT_DIG is set
	subw	IMM (1),d3	| and adjust exponent
	btst	IMM (FLT_MANT_DIG-1),d1
	bne	Ldivsf$2
	bra	1b

Lround$exit:
| This is a common exit point for __mulsf3 and __divsf3. 

| First check for underlow in the exponent:
	cmpw	IMM (-FLT_MANT_DIG-1),d2		
	blt	Lf$underflow	
| It could happen that the exponent is less than 1, in which case the 
| number is denormalized. In this case we shift right and adjust the 
| exponent until it becomes 1 or the fraction is zero (in the latter case 
| we signal underflow and return zero).
	movel	IMM (0),d6	| d6 is used temporarily
	cmpw	IMM (1),d2	| if the exponent is less than 1 we 
	bge	2f		| have to shift right (denormalize)
1:	addw	IMM (1),d2	| adjust the exponent
	lsrl	IMM (1),d0	| shift right once 
	roxrl	IMM (1),d1	|
	roxrl	IMM (1),d6	| d6 collect bits we would lose otherwise
	cmpw	IMM (1),d2	| is the exponent 1 already?
	beq	2f		| if not loop back
	bra	1b              |
	bra	Lf$underflow	| safety check, shouldn't execute '
2:	orl	d6,d1		| this is a trick so we don't lose  '
				| the extra bits which were flushed right
| Now call the rounding routine (which takes care of denormalized numbers):
	lea	Lround$0,a0	| to return from rounding routine
	lea	SYM (_fpCCR),a1	| check the rounding mode
	movew	a1@(6),d6	| rounding mode in d6
	beq	Lround$to$nearest
	cmpw	IMM (ROUND_TO_PLUS),d6
	bhi	Lround$to$minus
	blt	Lround$to$zero
	bra	Lround$to$plus
Lround$0:
| Here we have a correctly rounded result (either normalized or denormalized).

| Here we should have either a normalized number or a denormalized one, and
| the exponent is necessarily larger or equal to 1 (so we don't have to  '
| check again for underflow!). We have to check for overflow or for a 
| denormalized number (which also signals underflow).
| Check for overflow (i.e., exponent >= 255).
	cmpw	IMM (0x00ff),d2
	bge	Lf$overflow
| Now check for a denormalized number (exponent==0).
	movew	d2,d2
	beq	Lf$den
1:
| Put back the exponents and sign and return.
	lslw	IMM (7),d2	| exponent back to fourth byte
	bclr	IMM (FLT_MANT_DIG-1),d0
	swap	d0		| and put back exponent
	orw	d2,d0		| 
	swap	d0		|
	orl	d7,d0		| and sign also

	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7
	unlk	a6
	rts

|=============================================================================
|                             __negsf2
|=============================================================================

| This is trivial and could be shorter if we didn't bother checking for NaN '
| and +/-INFINITY.

| float __negsf2(float);
SYM (__negsf2):
	link	a6,IMM (0)
	moveml	d2-d7,sp@-
	movew	IMM (NEGATE),d5
	movel	a6@(8),d0	| get number to negate in d0
	bchg	IMM (31),d0	| negate
	movel	d0,d1		| make a positive copy
	bclr	IMM (31),d1	|
	tstl	d1		| check for zero
	beq	2f		| if zero (either sign) return +zero
	cmpl	IMM (INFINITY),d1 | compare to +INFINITY
	blt	1f		|
	bhi	Lf$inop		| if larger (fraction not zero) is NaN
	movel	d0,d7		| else get sign and return INFINITY
	andl	IMM (0x80000000),d7
	bra	Lf$infty		
1:	lea	SYM (_fpCCR),a0
	movew	IMM (0),a0@
	moveml	sp@+,d2-d7
	unlk	a6
	rts
2:	bclr	IMM (31),d0
	bra	1b

|=============================================================================
|                             __cmpsf2
|=============================================================================

GREATER =  1
LESS    = -1
EQUAL   =  0

| int __cmpsf2(float, float);
SYM (__cmpsf2):
	link	a6,IMM (0)
	moveml	d2-d7,sp@- 	| save registers
	movew	IMM (COMPARE),d5
	movel	a6@(8),d0	| get first operand
	movel	a6@(12),d1	| get second operand
| Check if either is NaN, and in that case return garbage and signal
| INVALID_OPERATION. Check also if either is zero, and clear the signs
| if necessary.
	movel	d0,d6
	andl	IMM (0x7fffffff),d0
	beq	Lcmpsf$a$0
	cmpl	IMM (0x7f800000),d0
	bhi	Lf$inop
Lcmpsf$1:
	movel	d1,d7
	andl	IMM (0x7fffffff),d1
	beq	Lcmpsf$b$0
	cmpl	IMM (0x7f800000),d1
	bhi	Lf$inop
Lcmpsf$2:
| Check the signs
	eorl	d6,d7
	bpl	1f
| If the signs are not equal check if a >= 0
	tstl	d6
	bpl	Lcmpsf$a$gt$b	| if (a >= 0 && b < 0) => a > b
	bmi	Lcmpsf$b$gt$a	| if (a < 0 && b >= 0) => a < b
1:
| If the signs are equal check for < 0
	tstl	d6
	bpl	1f
| If both are negative exchange them
	exg	d0,d1
1:
| Now that they are positive we just compare them as longs (does this also
| work for denormalized numbers?).
	cmpl	d0,d1
	bhi	Lcmpsf$b$gt$a	| |b| > |a|
	bne	Lcmpsf$a$gt$b	| |b| < |a|
| If we got here a == b.
	movel	IMM (EQUAL),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts
Lcmpsf$a$gt$b:
	movel	IMM (GREATER),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts
Lcmpsf$b$gt$a:
	movel	IMM (LESS),d0
	moveml	sp@+,d2-d7 	| put back the registers
	unlk	a6
	rts

Lcmpsf$a$0:	
	bclr	IMM (31),d6
	bra	Lcmpsf$1
Lcmpsf$b$0:
	bclr	IMM (31),d7
	bra	Lcmpsf$2

|=============================================================================
|                           rounding routines
|=============================================================================

| The rounding routines expect the number to be normalized in registers
| d0-d1, with the exponent in register d2. They assume that the 
| exponent is larger or equal to 1. They return a properly normalized number
| if possible, and a denormalized number otherwise. The exponent is returned
| in d2.

Lround$to$nearest:
| We now normalize as suggested by D. Knuth ("Seminumerical Algorithms"):
| Here we assume that the exponent is not too small (this should be checked
| before entering the rounding routine), but the number could be denormalized.

| Check for denormalized numbers:
1:	btst	IMM (FLT_MANT_DIG),d0
	bne	2f		| if set the number is normalized
| Normalize shifting left until bit #FLT_MANT_DIG is set or the exponent 
| is one (remember that a denormalized number corresponds to an 
| exponent of -F_BIAS+1).
	cmpw	IMM (1),d2	| remember that the exponent is at least one
 	beq	2f		| an exponent of one means denormalized
	addl	d1,d1		| else shift and adjust the exponent
	addxl	d0,d0		|
	dbra	d2,1b		|
2:
| Now round: we do it as follows: after the shifting we can write the
| fraction part as f + delta, where 1 < f < 2^25, and 0 <= delta <= 2.
| If delta < 1, do nothing. If delta > 1, add 1 to f. 
| If delta == 1, we make sure the rounded number will be even (odd?) 
| (after shifting).
	btst	IMM (0),d0	| is delta < 1?
	beq	2f		| if so, do not do anything
	tstl	d1		| is delta == 1?
	bne	1f		| if so round to even
	movel	d0,d1		| 
	andl	IMM (2),d1	| bit 1 is the last significant bit
	addl	d1,d0		| 
	bra	2f		| 
1:	movel	IMM (1),d1	| else add 1 
	addl	d1,d0		|
| Shift right once (because we used bit #FLT_MANT_DIG!).
2:	lsrl	IMM (1),d0		
| Now check again bit #FLT_MANT_DIG (rounding could have produced a
| 'fraction overflow' ...).
	btst	IMM (FLT_MANT_DIG),d0	
	beq	1f
	lsrl	IMM (1),d0
	addw	IMM (1),d2
1:
| If bit #FLT_MANT_DIG-1 is clear we have a denormalized number, so we 
| have to put the exponent to zero and return a denormalized number.
	btst	IMM (FLT_MANT_DIG-1),d0
	beq	1f
	jmp	a0@
1:	movel	IMM (0),d2
	jmp	a0@

Lround$to$zero:
Lround$to$plus:
Lround$to$minus:
	jmp	a0@
#endif /* L_float */

| gcc expects the routines __eqdf2, __nedf2, __gtdf2, __gedf2,
| __ledf2, __ltdf2 to all return the same value as a direct call to
| __cmpdf2 would.  In this implementation, each of these routines
| simply calls __cmpdf2.  It would be more efficient to give the
| __cmpdf2 routine several names, but separating them out will make it
| easier to write efficient versions of these routines someday.

#ifdef  L_eqdf2
LL0:
	.text
	.proc
|#PROC# 04
	LF18	=	4
	LS18	=	128
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__eqdf2)
SYM (__eqdf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_eqdf2 */

#ifdef  L_nedf2
LL0:
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__nedf2)
SYM (__nedf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_nedf2 */

#ifdef  L_gtdf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__gtdf2)
SYM (__gtdf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gtdf2 */

#ifdef  L_gedf2
LL0:
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__gedf2)
SYM (__gedf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gedf2 */

#ifdef  L_ltdf2
LL0:
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__ltdf2)
SYM (__ltdf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ltdf2 */

#ifdef  L_ledf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__ledf2)
SYM (__ledf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(20),sp@-
	movl	a6@(16),sp@-
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpdf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ledf2 */

| The comments above about __eqdf2, et. al., also apply to __eqsf2,
| et. al., except that the latter call __cmpsf2 rather than __cmpdf2.

#ifdef  L_eqsf2
	.text
	.proc
|#PROC# 04
	LF18	=	4
	LS18	=	128
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__eqsf2)
SYM (__eqsf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_eqsf2 */

#ifdef  L_nesf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__nesf2)
SYM (__nesf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_nesf2 */

#ifdef  L_gtsf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__gtsf2)
SYM (__gtsf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gtsf2 */

#ifdef  L_gesf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__gesf2)
SYM (__gesf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_gesf2 */

#ifdef  L_ltsf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__ltsf2)
SYM (__ltsf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_ltsf2 */

#ifdef  L_lesf2
	.text
	.proc
|#PROC# 04
	LF18	=	8
	LS18	=	132
	LFF18	=	0
	LSS18	=	0
	LV18	=	0
	.text
	.globl	SYM (__lesf2)
SYM (__lesf2):
|#PROLOGUE# 0
	link	a6,IMM (0)
|#PROLOGUE# 1
	movl	a6@(12),sp@-
	movl	a6@(8),sp@-
	jbsr	SYM (__cmpsf2)
|#PROLOGUE# 2
	unlk	a6
|#PROLOGUE# 3
	rts
#endif /* L_lesf2 */


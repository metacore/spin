/* inline functions for Z8KSIM
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of Z8KSIM

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef INLINE
#define INLINE
#endif
#define UGT 0x0b
#define ULE 0x03
#define ULT 0x07
#define UGE 0x0f
#define SLOW 0
#define T 0x8
#define F 0x0
#define LT 0x1
#define GT 0xa
#define LE 0x2
#define EQ 0x6
#define NE 0xe
#define GE 0x9

 static int is_cond_true PARAMS((sim_state_type *context, int c)); 
 static void makeflags PARAMS((sim_state_type *context, int mask)); 

INLINE
long
sitoptr (si)
long si;
{
  return ((si & 0xff000000) >> 8) | (si & 0xffff);
}
INLINE long
ptrtosi (ptr)
long ptr;
{
  return ((ptr & 0xff0000) << 8) | (ptr & 0xffff);
}

INLINE
void 
put_long_reg (context, reg, val)
     sim_state_type *context;
     int reg;
     int val;
{
  context->regs[reg].word = val >> 16;
  context->regs[reg + 1].word = val;
}

INLINE
void 
put_quad_reg (context, reg, val1, val2)
     sim_state_type *context;
     int reg;
     int val1;
     int val2;
{
  context->regs[reg].word = val2 >> 16;
  context->regs[reg + 1].word = val2;
  context->regs[reg + 2].word = val1 >> 16;
  context->regs[reg + 3].word = val1;
}

INLINE
void 
put_word_reg (context, reg, val)
     sim_state_type *context;
     int reg;
     int val;
{
  context->regs[reg].word = val;
}

INLINE
SItype get_long_reg (context, reg)
     sim_state_type *context;
     int reg;
{
  USItype lsw = context->regs[reg + 1].word;
  USItype msw = context->regs[reg].word;

  return (msw << 16) | lsw;
}

#ifdef __GNUC__
INLINE
struct UDIstruct
get_quad_reg (context, reg)
     sim_state_type *context;
     int reg;
{
  UDItype res;
  USItype lsw = get_long_reg (context, reg + 2);
  USItype msw = get_long_reg (context, reg);

  res.low = lsw;
  res.high = msw;
  return res;
}

#endif

INLINE
void 
put_byte_reg (context, reg, val)
     sim_state_type *context;
     int reg;
     int val;
{
  int old = context->regs[reg & 0x7].word;
  if (reg & 0x8)
    {
      old = old & 0xff00 | (val & 0xff);
    }
  else
    {
      old = old & 0x00ff | (val << 8);
    }
  context->regs[reg & 0x7].word = old;      
}

INLINE
int 
get_byte_reg (context, reg)
     sim_state_type *context;
     int reg;
{
  if (reg & 0x8)
    return  context->regs[reg & 0x7].word & 0xff;
  else
    return  (context->regs[reg & 0x7].word >> 8) & 0xff;
}

INLINE
void 
put_word_mem_da (context, addr, value)
     sim_state_type *context;
     int addr;
     int value;
{
  if (addr & 1)
    {
      context->exception = SIM_BAD_ALIGN;
      addr &= ~1;
    }
  put_byte_mem_da(context, addr, value>>8);
  put_byte_mem_da(context, addr+1, value);
}

INLINE unsigned char
get_byte_mem_da (context, addr)
     sim_state_type *context;
     int addr;
{
  return ((unsigned char *) (context->memory))[addr];
}

INLINE void
put_byte_mem_da (context, addr, value)
     sim_state_type *context;
     int addr;
     int value;
{
  ((unsigned char *) (context->memory))[addr] = value;
}

#if 0
#define get_word_mem_da(context,addr)\
 *((unsigned short*)((char*)((context)->memory)+(addr)))

#else
#define get_word_mem_da(context,addr) (get_byte_mem_da(context, addr) << 8) | (get_byte_mem_da(context,addr+1))
#endif

#define get_word_reg(context,reg) (context)->regs[reg].word

INLINE
SItype
get_long_mem_da (context, addr)
     sim_state_type *context;
     int addr;
{
  USItype lsw = get_word_mem_da(context,addr+2);
  USItype msw =  get_word_mem_da(context, addr);

  return (msw << 16) + lsw;
}

INLINE
void 
put_long_mem_da (context, addr, value)
     sim_state_type *context;
     int addr;
     int value;
{
  put_word_mem_da(context,addr, value>>16);
  put_word_mem_da(context,addr+2, value);
}

INLINE
int 
get_word_mem_ir (context, reg)
     sim_state_type *context;
     int reg;
{
  return get_word_mem_da (context, get_word_reg (context, reg));
}

INLINE
void 
put_word_mem_ir (context, reg, value)
     sim_state_type *context;
     int reg;
     int value;
{

  put_word_mem_da (context, get_word_reg (context, reg), value);
}

INLINE
int 
get_byte_mem_ir (context, reg)
     sim_state_type *context;
     int reg;
{
  return get_byte_mem_da (context, get_word_reg (context, reg));
}

INLINE
void 
put_byte_mem_ir (context, reg, value)
     sim_state_type *context;
     int reg;
     int value;
{
  put_byte_mem_da (context, get_word_reg (context, reg), value);
}

INLINE
int 
get_long_mem_ir (context, reg)
     sim_state_type *context;
     int reg;
{
  return get_long_mem_da (context, get_word_reg (context, reg));
}

INLINE
void 
put_long_mem_ir (context, reg, value)
     sim_state_type *context;
     int reg;
     int value;
{

  put_long_mem_da (context, get_word_reg (context, reg), value);
}

INLINE
void 
put_long_mem_x (context, base, reg, value)
     sim_state_type *context;
     int base;
     int reg;
     int value;
{
  put_long_mem_da (context, get_word_reg (context, reg) + base, value);
}

INLINE
void 
put_word_mem_x (context, base, reg, value)
     sim_state_type *context;
     int base;
     int reg;
     int value;
{
  put_word_mem_da (context, get_word_reg (context, reg) + base, value);
}

INLINE
void 
put_byte_mem_x (context, base, reg, value)
     sim_state_type *context;
     int base;
     int reg;
     int value;
{
  put_byte_mem_da (context, get_word_reg (context, reg) + base, value);
}

INLINE
int 
get_word_mem_x (context, base, reg)
     sim_state_type *context;
     int base;
     int reg;
{
  return get_word_mem_da (context, base + get_word_reg (context, reg));
}

INLINE
int 
get_byte_mem_x (context, base, reg)
     sim_state_type *context;
     int base;
     int reg;
{
  return get_byte_mem_da (context, base + get_word_reg (context, reg));
}

INLINE
int 
get_long_mem_x (context, base, reg)
     sim_state_type *context;
     int base;
     int reg;
{
  return get_long_mem_da (context, base + get_word_reg (context, reg));
}

static int
is_cond_true (context, c)
     sim_state_type *context;
     int c;
{
  switch (c)
    {
    case T:
      return 1;
    case F:
      return 0;			/* F */
    case LE:
      return (PSW_ZERO | (PSW_SIGN ^ PSW_OVERFLOW)) & 1;	/*LE */
    case GT:
      return (~(PSW_ZERO | (PSW_SIGN ^ PSW_OVERFLOW))) & 1;	/*GT */
    case 0x5:
      return (PSW_SIGN & 1);	/* sign */
    case 0xd:
      return (~(PSW_SIGN)) & 1;	/* not sign */
    case 0x3:
      return ((PSW_CARRY | PSW_ZERO) & 1);	/* ule*/
    case UGT:
      return ((~(PSW_CARRY | PSW_ZERO)) & 1);	/* ugt */
    case 0x4:
      return (PSW_OVERFLOW & 1);/* overflow */
    case 0xc:
      return (~(PSW_OVERFLOW)) & 1;	/* not overflow */
    case LT:
      return (PSW_SIGN ^ PSW_OVERFLOW) & 1;	/* LT */
    case GE:
      return (~(PSW_SIGN ^ PSW_OVERFLOW)) & 1;	/* GE */
    case EQ:
      return (PSW_ZERO) & 1;	/* zero */
    case NE:
      return ((~PSW_ZERO) & 1);	/* not zero */
    case 0x7:
      return (PSW_CARRY) & 1;	/* carry */
    case 0xf:
      return (~PSW_CARRY) & 1;	/* not carry */
    default:
      abort ();
    }
}

static
void
makeflags (context, mask)
     sim_state_type *context;
     int mask;
{

  PSW_ZERO = (context->dst & mask) == 0;
  PSW_SIGN = (context->dst >> (context->size - 1));

  if (context->broken_flags == TST_FLAGS)
    {
      extern char the_parity[];

      if (context->size == 8)
	{
	  PSW_OVERFLOW = the_parity[context->dst & 0xff];
	}
    }
  else
    {
      /* Overflow is set if both operands have the same sign and the
         result is of different sign.

         V =  A==B && R!=B  jumping logic
         (~(A^B))&(R^B)
         V =  (A^B)^(R^B)   boolean
         */

      PSW_OVERFLOW =
	((
	   (~(context->srca ^ context->srcb)
	    & (context->srca ^ context->dst))
	 ) >> (context->size - 1)
	);

      if (context->size < 32)
	{
	  PSW_CARRY = ((context->dst >> context->size)) & 1;
	}
      else
	{
	  /* carry is set when the result is smaller than the first source */

	  PSW_CARRY = (unsigned) context->dst > (unsigned) context->srca;
	}
    }
  context->broken_flags = 0;
}

INLINE
int
COND (context, c)
     sim_state_type *context;
     int c;
{
  if (c == 8)
    return 1;

  /* We can calculate what the flags would have been by
     looking at the src and dst and size of the operation */

  if (context->broken_flags)
    {
      int slow = 0;
      int size;
      int dst;
      int srca;
      int srcb;
      int mask;
      int ans;

      /* see if we can short-cut the nasty flag calcs */

      switch (size = context->size)
	{
	 default:
	  abort();
	  return 0;
	case 8:
	  srca = (char) (context->srca);
	  srcb = (char) (context->srcb);
	  dst = (char) (context->dst);
	  mask = 0xff;
	  break;
	case 16:
	  srca = (short) (context->srca);
	  srcb = (short) (context->srcb);
	  dst = (short) (context->dst);
	  mask = 0xffff;
	  break;
	case 32:
	  srca = (long) (context->srca);
	  srcb = (long) (context->srcb);
	  dst = (long) (context->dst);
	  mask = 0xffffffff;
	  break;
	}

      switch (c)
	{
	case T:
	  return 1;
	case F:
	  return 0;
	case EQ:
	  return !dst;
	case NE:
	  return dst;
	case GT:
	  ans = ((dst)) > 0;
	  if (slow)
	    {
	      if (is_cond_true (context, c) != ans)
		abort ();
	    }
	  return ans;
	case LE:
	  ans = ((dst)) <= 0;
	  if (slow)
	    {
	      if (is_cond_true (context, c) != ans)
		abort ();
	    }
	  return ans;
	case GE:
	  ans = ((dst)) >= 0;
	  if (slow)
	    {
	      if (is_cond_true (context, c) != ans)
		abort ();
	    }
	  return ans;
	case LT:
	  ans = ((dst)) < 0;
	  if (slow)
	    {
	      if (is_cond_true (context, c) != ans)
		abort ();
	    }
	  return ans;
	default:
	  break;
	}

      /* Can't fake it, we'll have to work out the flags the
         hard way */

      makeflags (context, mask);
    }

  /* don't know how to fake a test, inspect the flags
     the hard way */

  return is_cond_true (context, c);
}

INLINE
void 
NORMAL_FLAGS (context, size, dst, srca, srcb)
     sim_state_type *context;
     int size;
     int dst;
     int srca;
     int srcb;
{
  context->srca = srca;
  context->srcb = srcb;
  context->dst = dst;
  context->size = size;
  context->broken_flags = CMP_FLAGS;
}

INLINE
void 
TEST_NORMAL_FLAGS (context, size, dst)
     sim_state_type *context;
     int size;
     int dst;
{
  context->dst = dst;
  context->size = size;
  context->broken_flags = TST_FLAGS;
}

INLINE
void 
put_ptr_long_reg (context, reg, val)
     sim_state_type *context;
     int reg;
     int val;
{
  context->regs[reg].word = (val >> 8) & 0x7f00;
  context->regs[reg + 1].word = val;
}

INLINE
long 
get_ptr_long_reg (context, reg)
     sim_state_type *context;
     int reg;
{
  int val;

  val = (context->regs[reg].word << 8) | context->regs[reg + 1].word;
  return val;
}

INLINE
long 
get_ptr_long_mem_ir (context, reg)
sim_state_type *context;
int reg;
{
  return sitoptr (get_long_mem_da (context, get_ptr_long_reg (context, reg)));
}

INLINE
long 
get_ptr_long_mem_da (context, addr)
sim_state_type *context;
long addr; 
{
  return sitoptr (get_long_mem_da (context, addr));
}

INLINE
void 
put_ptr_long_mem_da (context, addr, ptr)
sim_state_type *context;
long addr; 
long ptr;
{
  put_long_mem_da (context, addr, ptrtosi (ptr));

}

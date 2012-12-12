/* Instruction printing code for the Alpha
   Copyright (C) 1993 Free Software

Foundation, Inc. Contributed by Cygnus Support. 

Written by Steve Chamberlain (sac@cygnus.com) 

This file is part of libopcodes. 

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version. 

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details. 

You should have received a copy of the GNU General Public License along with
This program; if not, write to the Free Software Foundation, Inc., 675
 Mass Ave, Cambridge, MA 02139, USA.  
*/

#include "dis-asm.h"
#define DEFINE_TABLE
#include "alpha-opc.h"


/* Print one instruction from PC on INFO->STREAM.
   Return the size of the instruction (always 4 on alpha). */

int
print_insn_alpha(pc, info)
	bfd_vma         pc;
	struct disassemble_info *info;
{
  alpha_insn     *insn;
  unsigned  char            b[4];
  void           *stream = info->stream;
  fprintf_ftype   func = info->fprintf_func;
  int             given;
  int             status ;
  int found = 0;
  
  status = (*info->read_memory_func) (pc, (bfd_byte *) &b[0], 4, info);
  if (status != 0) {
    (*info->memory_error_func) (status, pc, info);
    return -1;
  }
  given = (b[0]) | (b[1] << 8) | (b[2] << 16) | (b[3] << 24);

  func (stream, "%08x %2x\t", given, (given>>26) & 0x3f);
  
  for (insn = alpha_insn_set;
       insn->name && !found;
       insn++)
    {
      switch (insn->type)
	{
	case MEMORY_FORMAT_CODE:
	  if ((insn->i & MEMORY_FORMAT_MASK) 
	      ==(given & MEMORY_FORMAT_MASK))
	    {
	      func (stream, "%s\t%s, %d(%s)",
		      insn->name,
		      alpha_regs[RA(given)],
		      OPCODE (given) == 9 ? DISP(given) * 65536 : DISP(given),
		      alpha_regs[RB(given)]);
	      found = 1;
	    }
	  break;
	case BRANCH_FORMAT_CODE:
	  if ((insn->i & BRANCH_FORMAT_MASK)
	      == (given & BRANCH_FORMAT_MASK))
	    {
	      func (stream, "%s\t%s, ",
		      insn->name,
		      alpha_regs[RA(given)]);
	      (*info->print_address_func) (BDISP(given) * 4 + pc + 4, info);
	      found = 1;
	    }
	  break;

	case MEMORY_BRANCH_FORMAT_CODE:
	  if ((insn->i & MEMORY_BRANCH_FORMAT_MASK) 
	      == (given & MEMORY_BRANCH_FORMAT_MASK) )
	    {
	      if (given & (1<<15)) 
		{
		  func (stream, "%s\t%s, (%s), %d", insn->name,
			  alpha_regs[RA(given)],
			  alpha_regs[RB(given)],
			  JUMP_HINT(given));
		} 
	      else 
		{
		  /* The displacement is a hint only, do not put out
		     a symbolic address.  */
		  func (stream, "%s\t%s, (%s), 0x%lx", insn->name,
			  alpha_regs[RA(given)],
			  alpha_regs[RB(given)],
		          JDISP(given) * 4 + pc + 4);
		}
	      found = 1;
	    }

	  break;
	case OPERATE_FORMAT_CODE:
	  if ((insn->i & OPERATE_FORMAT_MASK)
	      == (given & OPERATE_FORMAT_MASK)) 
	    {
	      if (OP_OPTYPE(insn->i) == OP_OPTYPE(given)) 
		{
		  if (OP_IS_CONSTANT(given)) {
		    func (stream, "%s\t%s, 0x%x, %s", insn->name,
			    alpha_regs[RA(given)],
			    LITERAL(given),
			    alpha_regs[RC(given)]);
		  } else {
		    func (stream, "%s\t%s, %s, %s", insn->name,
			    alpha_regs[RA(given)],
			    alpha_regs[RB(given)],
			    alpha_regs[RC(given)]);
		  }
		  found = 1;
		}
	    }
	      
	  break;
	case FLOAT_FORMAT_CODE:
	  if ((insn->i & OPERATE_FORMAT_MASK)
	      == (given & OPERATE_FORMAT_MASK)) 
	    {
	      func (stream, "%s\tf%d, f%d, f%d", insn->name,
		      RA(given),
		      RB(given),
		      RC(given));
	      found = 1;
	    }

	  break;
	case PAL_FORMAT_CODE:
	  if (insn->i == given)
	    {
	      func (stream, "call_pal %s", insn->name);
	      found = 1;
	    }

	  break;
	case FLOAT_MEMORY_FORMAT_CODE:
	  if ((insn->i & MEMORY_FORMAT_MASK) 
	      ==(given & MEMORY_FORMAT_MASK))
	    {
	      func (stream, "%s\tf%d, %d(%s)",
		      insn->name,
		      RA(given),
		      OPCODE (given) == 9 ? DISP(given) * 65536 : DISP(given),
		      alpha_regs[RB(given)]);
	      found = 1;
	    }
	  break;
	case FLOAT_BRANCH_FORMAT_CODE:
	  if ((insn->i & BRANCH_FORMAT_MASK)
	      == (given & BRANCH_FORMAT_MASK))
	    {
	      func (stream, "%s\tf%d, ",
		      insn->name,
		      RA(given));
	      (*info->print_address_func) (BDISP(given) * 4 + pc + 4, info);
	      found = 1;
	    }
	  break;
	}
    }
    
  return 4;
}

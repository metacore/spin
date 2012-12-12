/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Run-time code generation for the dispatcher
 *
 * HISTORY
 * 07-May-96  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Machine code representation, disassembler, assembler, 
 *	inlining.
 *
 */

#include <stdlib.h>
#include "Assembler.h"

static int asm_debug  = 0;
static int asm_broken = 0;

/******************************************************************************
 *
 * global area for clones
 *
 *****************************************************************************/

/*
 * maintain a single gpa for all cloned/assembled procedures
 * incrementally add global data and return pointers to a slot
 * in the area.
 */

#define      GPA_SIZE 32 * 1024 / sizeof(long)
inst_val_t **GPA_ptr;
int          GPA_size;
int          GPA_cnt;
int          GPA_total_cnt = 0;

/* 
 * add a pointer to the area, return its new location, NULL if no more space
 */
inst_val_t *add_to_gpa(inst_val_t *ptr)
{
  inst_val_t **where;

  if(GPA_cnt == GPA_SIZE) {
    printf("ERROR >> no space in the GPA area\n");
    asm_broken = 1;
    return NULL;
  }

  where = GPA_ptr + GPA_cnt;
  if(asm_debug > 8) {
    printf("adding to GPA: %d 0x%lx 0x%lx\n", GPA_cnt, where, ptr);
  }
  *where = ptr;
  GPA_cnt++;
  GPA_total_cnt++;
  return (inst_val_t *)where;
}

void init_gpa()
{
  /*
  printf(">>> allocating space for the GPA area: %d %d %d\n", 
	 GPA_total_cnt, GPA_cnt, GPA_size);
	 */
  GPA_ptr = (inst_val_t **)spin_malloc(GPA_SIZE * sizeof(inst_val_t *));
  if(GPA_ptr == NULL) {
    printf("ERROR >> could not allocate space in the GPA area\n");
    asm_broken = 1;
    return;
  }
  GPA_size = GPA_SIZE;
  GPA_cnt = 0;
}

static inst_val_t *get_gp(proc_t *proc_desc);

/******************************************************************************
 *
 * instructions descriptors and names
 *
 *****************************************************************************/

#define NUM_INST      119

inst_desc_t decode_table[120] = {
  { 0x00, 0x0000,      0,      NULL,              0     },
  { 0x08, 0xffff, OP_lda,     "lda",     FMT_MEMORY     },
  { 0x21, 0xffff, OP_ldg,     "ldg",     FMT_MEMORY     },
  { 0x29, 0xffff, OP_ldq,     "ldq",     FMT_MEMORY     },
  { 0x22, 0xffff, OP_lds,     "lds",     FMT_MEMORY     },
  { 0x25, 0xffff, OP_stg,     "stg",     FMT_MEMORY     },
  { 0x2d, 0xffff, OP_stq,     "stq",     FMT_MEMORY     },
  { 0x26, 0xffff, OP_sts,     "sts",     FMT_MEMORY     },
  { 0x09, 0xffff, OP_ldah,    "ldah",    FMT_MEMORY     },
  { 0x28, 0xffff, OP_ldl,     "ldl",     FMT_MEMORY     },
  { 0x2b, 0xffff, OP_ldq_l,   "ldq_l",   FMT_MEMORY     },
  { 0x23, 0xffff, OP_ldt,     "ldt",     FMT_MEMORY     },
  { 0x2c, 0xffff, OP_stl,     "stl",     FMT_MEMORY     },
  { 0x2f, 0xffff, OP_stq_c,   "stq_c",   FMT_MEMORY     },
  { 0x27, 0xffff, OP_stt,     "stt",     FMT_MEMORY     },
  { 0x20, 0xffff, OP_ldf,     "ldf",     FMT_MEMORY     },
  { 0x2a, 0xffff, OP_ldl_l,   "ldl_l",   FMT_MEMORY     },
  { 0x0b, 0xffff, OP_ldq_u,   "ldq_u",   FMT_MEMORY     },
  { 0x24, 0xffff, OP_stf,     "stf",     FMT_MEMORY     },
  { 0x2e, 0xffff, OP_stl_c,   "stl_c",   FMT_MEMORY     },
  { 0x0f, 0xffff, OP_stq_u,   "stq_u",   FMT_MEMORY     },
  { 0x18, 0x8000, OP_fetch,   "fetch",   FMT_MEMORY_FC  },
  { 0x18, 0xe000, OP_rc,      "rc",      FMT_MEMORY_FC  },
  { 0x18, 0x0000, OP_trapb,   "trapb",   FMT_MEMORY_FC  },
  { 0x18, 0xa000, OP_fetch_m, "fetch_m", FMT_MEMORY_FC  },
  { 0x18, 0xc000, OP_rpcc,    "rpcc",    FMT_MEMORY_FC  },
  { 0x18, 0x4000, OP_mb,      "mb",      FMT_MEMORY_FC  },
  { 0x18, 0xf000, OP_rs,      "rs",      FMT_MEMORY_FC  },
  { 0x1a,    0x0, OP_jmp,     "jmp",     FMT_MEMORY_JMP },
  { 0x1a,    0x2, OP_ret,     "ret",     FMT_MEMORY_JMP },
  { 0x1a,    0x1, OP_jsr,     "jsr",     FMT_MEMORY_JMP },
  { 0x1a,    0x3, OP_jsr_co,  "jsr_co",  FMT_MEMORY_JMP },
  { 0x30, 0xffff, OP_br,      "br",      FMT_BRANCH     },
  { 0x33, 0xffff, OP_fble,    "fble",    FMT_BRANCH     },
  { 0x36, 0xffff, OP_fbge,    "fbge",    FMT_BRANCH     },
  { 0x39, 0xffff, OP_beq,     "beq",     FMT_BRANCH     },
  { 0x3c, 0xffff, OP_blbs,    "blbs",    FMT_BRANCH     },
  { 0x3f, 0xffff, OP_bgt,     "bgt",     FMT_BRANCH     },
  { 0x31, 0xffff, OP_fbeq,    "fbeq",    FMT_BRANCH     },
  { 0x34, 0xffff, OP_bsr,     "bsr",     FMT_BRANCH     },
  { 0x37, 0xffff, OP_fbgt,    "fbgt",    FMT_BRANCH     },
  { 0x3a, 0xffff, OP_blt,     "blt",     FMT_BRANCH     },
  { 0x3d, 0xffff, OP_bne,     "bne",     FMT_BRANCH     },
  { 0x32, 0xffff, OP_fblt,    "fblt",    FMT_BRANCH     },
  { 0x35, 0xffff, OP_fbne,    "fbne",    FMT_BRANCH     },
  { 0x38, 0xffff, OP_blbc,    "blbc",    FMT_BRANCH     },
  { 0x3b, 0xffff, OP_ble,     "ble",     FMT_BRANCH     },
  { 0x3e, 0xffff, OP_bge,     "bge",     FMT_BRANCH     },
  { 0x10,   0x00, OP_addl,    "addl",    FMT_OPERATE    },
  { 0x10,   0x60, OP_addqv,   "addqv",   FMT_OPERATE    },
  { 0x10,   0x6d, OP_cmple,   "cmple",   FMT_OPERATE    },
  { 0x10,   0x1d, OP_cmpult,  "cmpult",  FMT_OPERATE    },
  { 0x10,   0x29, OP_subq,    "subq",    FMT_OPERATE    },
  { 0x10,   0x40, OP_addlv,   "addlv",   FMT_OPERATE    },
  { 0x10,   0x0f, OP_cmpbge,  "cmpbge",  FMT_OPERATE    },
  { 0x10,   0x4d, OP_cmplt,   "cmplt",   FMT_OPERATE    },
  { 0x10,   0x09, OP_subl,    "subl",    FMT_OPERATE    },
  { 0x10,   0x69, OP_subqv,   "subqv",   FMT_OPERATE    },
  { 0x10,   0x20, OP_addq,    "addq",    FMT_OPERATE    },
  { 0x10,   0x2d, OP_cmpeq,   "cmpeq",   FMT_OPERATE    },
  { 0x10,   0x3d, OP_cmpule,  "cmpule",  FMT_OPERATE    },
  { 0x10,   0x49, OP_sublv,   "sublv",   FMT_OPERATE    },
  { 0x10,   0x02, OP_s4addl,  "s4addl",  FMT_OPERATE    },
  { 0x10,   0x2b, OP_s4subq,  "s4subq",  FMT_OPERATE    },
  { 0x10,   0x1b, OP_s8subl,  "s8subl",  FMT_OPERATE    },
  { 0x10,   0x22, OP_s4addq,  "s4addq",  FMT_OPERATE    },
  { 0x10,   0x12, OP_s8addl,  "s8addl",  FMT_OPERATE    },
  { 0x10,   0x3b, OP_s8subq,  "s8subq",  FMT_OPERATE    },
  { 0x10,   0x0b, OP_s4subl,  "s4subl",  FMT_OPERATE    },
  { 0x10,   0x32, OP_s8addq,  "s8addq",  FMT_OPERATE    },
  { 0x11,   0x00, OP_and,     "and",     FMT_OPERATE    },
  { 0x11,   0x24, OP_cmoveq,  "cmoveq",  FMT_OPERATE    },
  { 0x11,   0x08, OP_bic,     "bic",     FMT_OPERATE    },
  { 0x11,   0x16, OP_cmovlbc, "cmovlbc", FMT_OPERATE    },
  { 0x11,   0x20, OP_bis,     "bis",     FMT_OPERATE    },
  { 0x11,   0x14, OP_cmovlbs, "cmovlbs", FMT_OPERATE    },
  { 0x11,   0x46, OP_cmovge,  "cmovge",  FMT_OPERATE    },
  { 0x11,   0x44, OP_cmovlt,  "cmovlt",  FMT_OPERATE    },
  { 0x11,   0x28, OP_ornot,   "ornot",   FMT_OPERATE    },
  { 0x11,   0x66, OP_cmovgt,  "cmovgt",  FMT_OPERATE    },
  { 0x11,   0x26, OP_cmovne,  "cmovne",  FMT_OPERATE    },
  { 0x11,   0x40, OP_xor,     "xor",     FMT_OPERATE    },
  { 0x11,   0x64, OP_cmovle,  "cmovle",  FMT_OPERATE    },
  { 0x11,   0x48, OP_eqv,     "eqv",     FMT_OPERATE    },
  { 0x12,   0x06, OP_extbl,   "extbl",   FMT_OPERATE    },
  { 0x12,   0x7a, OP_extqh,   "extqh",   FMT_OPERATE    },
  { 0x12,   0x16, OP_extwl,   "extwl",   FMT_OPERATE    },
  { 0x12,   0x2b, OP_insll,   "insll",   FMT_OPERATE    },
  { 0x12,   0x57, OP_inswh,   "inswh",   FMT_OPERATE    },
  { 0x12,   0x62, OP_msklh,   "msklh",   FMT_OPERATE    },
  { 0x12,   0x32, OP_mskql,   "mskql",   FMT_OPERATE    },
  { 0x12,   0x39, OP_sll,     "sll",     FMT_OPERATE    },
  { 0x12,   0x30, OP_zap,     "zap",     FMT_OPERATE    },
  { 0x12,   0x6a, OP_extlh,   "extlh",   FMT_OPERATE    },
  { 0x12,   0x36, OP_extql,   "extql",   FMT_OPERATE    },
  { 0x12,   0x0b, OP_insbl,   "insbl",   FMT_OPERATE    },
  { 0x12,   0x77, OP_insqh,   "insqh",   FMT_OPERATE    },
  { 0x12,   0x1b, OP_inswl,   "inswl",   FMT_OPERATE    },
  { 0x12,   0x22, OP_mskll,   "mskll",   FMT_OPERATE    },
  { 0x12,   0x52, OP_mskwh,   "mskwh",   FMT_OPERATE    },
  { 0x12,   0x3c, OP_sra,     "sra",     FMT_OPERATE    },
  { 0x12,   0x31, OP_zapnot,  "zapnot",  FMT_OPERATE    },
  { 0x12,   0x26, OP_extll,   "extll",   FMT_OPERATE    },
  { 0x12,   0x5a, OP_extwh,   "extwh",   FMT_OPERATE    },
  { 0x12,   0x67, OP_inslh,   "inslh",   FMT_OPERATE    },
  { 0x12,   0x3b, OP_insql,   "insql",   FMT_OPERATE    },
  { 0x12,   0x02, OP_mskbl,   "mskbl",   FMT_OPERATE    },
  { 0x12,   0x72, OP_mskqh,   "mskqh",   FMT_OPERATE    },
  { 0x12,   0x12, OP_mskwl,   "mskwl",   FMT_OPERATE    },
  { 0x12,   0x34, OP_srl,     "srl",     FMT_OPERATE    },
  { 0x13,   0x00, OP_mull,    "mull",    FMT_OPERATE    },
  { 0x13,   0x60, OP_mulqv,   "mulqv",   FMT_OPERATE    },
  { 0x13,   0x40, OP_mullv,   "mullv",   FMT_OPERATE    },
  { 0x13,   0x30, OP_umulh,   "umulh",   FMT_OPERATE    },
  { 0x13,   0x20, OP_mulq,    "mulq",    FMT_OPERATE    }
  };

char *reg_names[32] = {
  "v0",
  "t0",
  "t1",
  "t2",
  "t3",
  "t4",
  "t5",
  "t6",
  "t7",
  "s0",
  "s1",
  "s2",
  "s3",
  "s4",
  "s5",
  "fp",
  "a0",
  "a1",
  "a2",
  "a3",
  "a4",
  "a5",
  "t8",
  "t9",
  "t10",
  "t11",
  "ra",
  "t12",
  "at",
  "gp",
  "sp",
  "zero"
};

static void set_opcode(int *dst, int code)
{
  SET_INST_FIELD(dst, code, OPCODE_MASK, OPCODE_SHIFT);
}

static int get_opcode(int inst_val)
{
  return GET_INST_FIELD(inst_val, OPCODE_MASK, OPCODE_SHIFT);
}

static void set_branch_pred(int *dst, int pred)
{
  SET_INST_FIELD(dst, pred, BRANCH_PRED_MASK, BRANCH_PRED_SHIFT);
}

static int get_branch_pred(int inst_val)
{
  return GET_INST_FIELD(inst_val, BRANCH_PRED_MASK, BRANCH_PRED_SHIFT);
}

static void set_target(int *dst, int target)
{
  SET_INST_FIELD(dst, target, TARGET_MASK, TARGET_SHIFT);
}

static int get_target(int inst_val)
{
  return GET_INST_FIELD(inst_val, TARGET_MASK, TARGET_SHIFT);
}

static void set_lit_flag(int *dst, int flag)
{
  SET_INST_FIELD(dst, flag, LIT_FLAG_MASK, LIT_FLAG_SHIFT);
}

static int get_lit_flag(int inst_val)
{
  return GET_INST_FIELD(inst_val, LIT_FLAG_MASK, LIT_FLAG_SHIFT);
}

static void set_number(int *dst, int num)
{
  SET_INST_FIELD(dst, num, NUMBER_MASK, NUMBER_SHIFT);
}

static int get_number(int inst_val)
{
  return GET_INST_FIELD(inst_val, NUMBER_MASK, NUMBER_SHIFT);
}

static void set_function(int *dst, int fc)
{
  SET_INST_FIELD(dst, fc, FUNCTION_MASK, FUNCTION_SHIFT);
}

static int get_function(int inst_val)
{
  return GET_INST_FIELD(inst_val, FUNCTION_MASK, FUNCTION_SHIFT);
}

static void set_reg_a(int *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_A_MASK, REG_A_SHIFT);
}

static int get_reg_a(int inst_val)
{
  return GET_INST_FIELD(inst_val, REG_A_MASK, REG_A_SHIFT);
}

static void set_reg_b(int *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_B_MASK, REG_B_SHIFT);
}

static int get_reg_b(int inst_val)
{
  return GET_INST_FIELD(inst_val, REG_B_MASK, REG_B_SHIFT);
}

static void set_reg_c(int *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_C_MASK, REG_C_SHIFT);
}

static int get_reg_c(int inst_val)
{
  return GET_INST_FIELD(inst_val, REG_C_MASK, REG_C_SHIFT);
}

static void set_disp(int *dst, int disp)
{
  SET_INST_FIELD(dst, disp, DISP_MASK, DISP_SHIFT);
}

static int get_disp(int inst_val)
{
  return GET_INST_FIELD(inst_val, DISP_MASK, DISP_SHIFT);
}

static void set_br_disp(int *dst, int disp)
{
  SET_INST_FIELD(dst, (disp >> 2), BR_DISP_MASK, BR_DISP_SHIFT);
}

static int get_br_disp(int inst_val)
{
  int disp = GET_INST_FIELD(inst_val, BR_DISP_MASK, BR_DISP_SHIFT);
  disp = (int)(((int)(disp << 12)) >> 10);
  return disp;
}

static void set_lit(int *dst, int lit)
{
  SET_INST_FIELD(dst, lit, LIT_MASK, LIT_SHIFT);
}

static int get_lit(int inst_val)
{
  return GET_INST_FIELD(inst_val, LIT_MASK, LIT_SHIFT);
}

int find_by_opcode(int op_val)
{
  int i;

  for(i = 0; i < NUM_INST; i++) {
    if(decode_table[i].op_val == op_val) {
      return i;
    }
  }
  return -1;
}

int find_by_function(int idx, int op_val, int fc_val) 
{
  int i;
  
  for(i = idx; i < NUM_INST; i++) {
    if(decode_table[i].op_val != op_val) {
      break;
    }
    if(decode_table[i].fc_val == fc_val) {
      return i;
    }
  }

  return -1;
}

/*
 * print a machine instruction
 */

void print_inst(inst_t inst)
{
  int idx = inst.idx;

  printf("0x%lx 0x%lx : ", inst.pc, inst.orig_pc);
  if(idx == -1) {
    printf("ERROR >> assembler: illegal instruction");
  } else if(inst.cancel) {
    printf("        %s ", "<CANCEL>");
  } else if(inst.insert) {
    printf("        %s ", "<INSERT>");
  } else if(inst.end) {
    printf("        %s ", "<END>");
  } else {
    if(inst.label == 0) {
      printf("        %s ", decode_table[idx].name);
    } else {
      printf("L_%d  %s ", inst.label, decode_table[idx].name);
    }
    switch(decode_table[idx].format) {
    case FMT_PALCODE    :
      printf("%d", inst.u.palcode.number);
      break;
    case FMT_OPERATE    :
      if(inst.u.operate.lit_flag) {
	printf("%s, %d, %s", 
	       reg_names[inst.u.operate.ra],
	       inst.u.operate.lit,
	       reg_names[inst.u.operate.rc]);
      } else {
	printf("%s, %s, %s", 
	       reg_names[inst.u.operate.ra],
	       reg_names[inst.u.operate.rb],
	       reg_names[inst.u.operate.rc]);
      }
      break;
    case FMT_MEMORY     :
      printf("%s, %d(%s)", 
	     reg_names[inst.u.memory.ra],
	     inst.u.memory.disp,
	     reg_names[inst.u.memory.rb]);
      break;
    case FMT_MEMORY_FC  :
      printf("%s, %d(%s)", 
	     reg_names[inst.u.memory.ra],
	     inst.u.memory.disp,
	     reg_names[inst.u.memory.rb]);
      break;
    case FMT_MEMORY_JMP :
      printf("%s, (%s), %d [0x%lx]", 
	     reg_names[inst.u.jump.ra],
	     reg_names[inst.u.jump.rb], 
	     1,
	     inst.u.jump.ptr);
      break;
    case FMT_BRANCH     :
      if(inst.u.branch.i_target != NULL) {
	printf("%s, L_%d (0x%lx) ", reg_names[inst.u.branch.ra], 
	       inst.u.branch.i_target->label, inst.u.branch.i_target->pc);
      } else {
	printf("%s, %d", reg_names[inst.u.branch.ra], inst.u.branch.disp);
      }
      printf(" (int: %d off %d disp: %d orig_disp: %d orig: 0x%lx 0x%lx new: 0x%lx)", 
	     inst.u.branch.internal,
	     inst.u.branch.offset,
	     inst.u.branch.disp,
	     inst.u.branch.orig_disp,
	     inst.u.branch.ptr,
	     ((char *)inst.orig_pc) + sizeof(int) + inst.u.branch.orig_disp,
	     ((char *)inst.pc) + sizeof(int) + inst.u.branch.disp);
      break;
    default:
      printf("ERROR >> wrong instruction format");
    }
    /* if(inst.gp != NULL) { printf("  (gp: 0x%lx)", inst.gp); };*/
  }
}

int print_insts(inst_t *inst_ptr, char *name, int i)
{
  int j, k;

  j = 0;
  while(!inst_ptr->end) {
    if(inst_ptr->cancel) {
      print_inst(*inst_ptr); printf("\n");
    } else if(inst_ptr->insert) {
      printf("---> insert\n");
      k = print_insts(inst_ptr->u.insert.inst, name, i);
      printf("---> end insert\n");
      i += k;
      j++;
    } else {
      printf("0x%lx 0x%lx <%s+%d>:", 
	     inst_ptr->pc, inst_ptr->orig_pc, name, i*4);
      print_inst(*inst_ptr);
      printf("\n");
      i++;
      j++;
    }
    inst_ptr++;
  }
  return j;
}

void print_proc(proc_t *proc_desc)
{
  inst_t *inst_ptr = proc_desc->code;

  if(proc_desc == NULL) {
    printf("ERROR >> NULL instead of a  procedure descriptor\n");
    return;
  }

  printf("--- proc: 0x%lx (orig: 0x%lx); gp: 0x%lx; new gp: 0x%lx\n", 
	 proc_desc->proc, 
	 (proc_desc->orig != NULL) ? proc_desc->orig->proc : NULL,
	 proc_desc->gp, get_gp(proc_desc));
  printf("--- start\n");
  print_insts(inst_ptr, proc_desc->name, 0);
  printf("--- end\n");
}

static int dump_cnt = 0;

void dump(proc_t *desc) 
{
  if(asm_broken) {
    return;
  }

#ifdef DEBUG
  print_proc(desc);
  if(dump_cnt == 0) {
#ifndef USER
    gimmeabreak();
#else USER
    for(;;);
#endif USER
  }
#endif DEBUG
/*  dump_cnt++;*/
}


/******************************************************************************
 *
 * assembly/disassembly of instructions
 *
 *****************************************************************************/

/*
 * disassemble a single machine instruction
 */
inst_t disassemble_inst(inst_val_t inst_val, inst_val_t *pc)
{
  inst_t inst;
  int    op_val;
  int    fc_val;
  int    idx;
  int    fmt;
  int    flag;

  if(pc == NULL) {
    printf("ERROR >> NULL pc in disassemble_inst\n");
  }

  inst.label = 0;

  op_val = get_opcode(inst_val);
  idx = find_by_opcode(op_val);
  if(idx != -1) {
    fmt = decode_table[idx].format;
    inst.branch = 0;
    inst.jump   = 0;
    switch(fmt) {
    case FMT_PALCODE    :
      inst.u.palcode.number = get_number(inst_val);
      break;
    case FMT_OPERATE    :
      fc_val = get_function(inst_val);
      idx = find_by_function(idx, op_val, fc_val);
      if(idx != -1) {
	inst.u.operate.ra = get_reg_a(inst_val);
	inst.u.operate.rc = get_reg_c(inst_val);
	flag = get_lit_flag(inst_val);
	inst.u.operate.lit_flag = flag;
	if(flag) {
	  inst.u.operate.lit = get_lit(inst_val);
	} else {
	  inst.u.operate.rb = get_reg_b(inst_val);
	}
      }
      break;
    case FMT_MEMORY     :
      inst.u.memory.ra = get_reg_a(inst_val);
      inst.u.memory.rb = get_reg_b(inst_val);
      inst.u.memory.disp = (short)get_disp(inst_val);
      inst.u.memory.target = NULL;
      inst.u.memory.format = 0;
      break;
    case FMT_MEMORY_FC  :
      fc_val = get_disp(inst_val);
      idx = find_by_function(idx, op_val, fc_val);
      if(idx != -1) {
	inst.u.memory.ra = get_reg_a(inst_val);
	inst.u.memory.rb = get_reg_b(inst_val);
	inst.u.memory.disp = fc_val;
      }
      break;
    case FMT_MEMORY_JMP :
      fc_val = get_branch_pred(inst_val);
      idx = find_by_function(idx, op_val, fc_val);
      if(idx != -1) {
	inst.u.jump.ra = get_reg_a(inst_val);
	inst.u.jump.rb = get_reg_b(inst_val);
	inst.u.jump.disp = get_target(inst_val);
	inst.jump = 1;
      }      
      break;
    case FMT_BRANCH     :
      inst.u.branch.ra = get_reg_a(inst_val);
      inst.u.branch.disp = get_br_disp(inst_val);
      inst.u.branch.orig_disp = inst.u.branch.disp;
      if(inst.u.branch.disp % 4 != 0) {
	printf("ERROR >> incorrect displacement for branch\n");
      }
      inst.u.branch.internal = 0;
      inst.u.branch.i_target = NULL;
      inst.u.branch.ptr = NULL;
      inst.branch = 1;
      break;
    default:
      idx = -1;
    }
  }
  if(idx != -1) {
    inst.opcode = decode_table[idx].opcode;
  }
  inst.idx = idx;
  inst.pc  = NULL;
  inst.orig_pc = pc;
  inst.gp  = NULL;
  inst.end = 0;
  inst.insert = 0;
  inst.cancel = 0;
  return inst;
}

void set_ldah_lda(inst_t *inst, inst_val_t *target, inst_val_t *base)
{
  int disp, off_1, off_2;

  disp = (char *)target - (char *)base;
  off_1 = disp >> 16;
  off_2 = (((int)(disp - (off_1 << 16))) << 16) >> 16;
  if(off_2 < 0) {
    off_1 += 1;
  }
  inst->u.memory.disp = off_1;
  inst++;
  inst->u.memory.disp = off_2;
}


inst_t *find_next_not_cancelled(inst_t *inst) 
{
  inst_t *new;

  while(inst->cancel || inst->end) {
    if(inst->end) {
      /* no valid instruction in this chain */
      /* try the external chain if it exists */
      if(inst->u.end.inst != NULL) {
	new = find_next_not_cancelled(inst->u.end.inst);
	if(new != NULL) {
	  inst = new;
	  break;
	}
      } else {
	return NULL;
      }
    } else if (inst->insert) {
      new = find_next_not_cancelled(inst->u.insert.inst);
      if(new != NULL) {
	inst = new;
	break;
      }
    }

    /* try the next one */
    inst++;
  }
  
  return inst;
}

/*
 * assemble a single machine instruction
 */

int assemble_inst(inst_t *inst, inst_val_t *inst_val, inst_val_t *gp)
{
  int          fc_val;
  int          fmt;
  int          flag;
  inst_desc_t *desc;
  int          disp;
  inst_t      *target_inst;
  inst_val_t  *target;

  if(asm_debug > 8) {
    printf(">>> asm inst 0x%lx 0x%lx : ", inst, inst_val); 
    print_inst(*inst); printf("\n");
  }

  if(inst == NULL || inst->insert || inst->end || inst->cancel) {
    printf("ERROR >> not an instuction in assemble_inst\n");
    return 0;
  }

  if(inst->pc != inst_val) {
    printf("ERROR >> pc not set: 0x%lx 0x%lx\n", inst->pc, inst_val);
    print_inst(*inst);
    printf("\n");
    return 0;
  }
  desc = &decode_table[inst->idx];
  set_opcode(inst_val, desc->op_val);
  fmt = decode_table[inst->idx].format;

  switch(fmt) {
  case FMT_PALCODE    :
    set_number(inst_val, inst->u.palcode.number);
    break;
  case FMT_OPERATE    :
    set_function(inst_val, desc->fc_val);
    set_reg_a(inst_val, inst->u.operate.ra);
    set_reg_c(inst_val, inst->u.operate.rc);
    flag = inst->u.operate.lit_flag;
    set_lit_flag(inst_val, flag);
    if(flag) {
      set_lit(inst_val, inst->u.operate.lit);
    } else {
      set_reg_b(inst_val, inst->u.operate.rb);
    }
    break;
  case FMT_MEMORY     :
    set_reg_a(inst_val, inst->u.memory.ra);
    set_reg_b(inst_val, inst->u.memory.rb);
    switch(inst->u.memory.format) {
    case 1: 
      /* ldq off of gp */
      target = add_to_gpa(inst->u.memory.target);
      if(target == NULL) {
	return -1;
      }
      inst->u.memory.disp = (char *)target - (char *)gp;
      break;
    case 2:
      /* first instruction of a pair computing off of gp */
      set_ldah_lda(inst, inst->u.memory.target, gp);
      break;
    case 3:
      /* second instruction of a pair computing off of gp */
      /* its disp field is already set */
      break;
    case 4:
      /* no relocation needed */
      break;
    case 5:
      /* first instruction of loading of gp */
      set_ldah_lda(inst, gp, inst->pc);
      break;
    case 6:
      /* second instruction of loading of gp */
      break;
    default:
      printf("ERROR >> assemble_inst: memory format not recognized\n");
      print_inst(*inst);
      return 0;
    }
    set_disp(inst_val, inst->u.memory.disp);
    break;
  case FMT_MEMORY_FC  :
    set_reg_a(inst_val, inst->u.memory.ra);
    set_reg_b(inst_val, inst->u.memory.rb);
    set_disp(inst_val, inst->u.memory.disp);
    break;
  case FMT_MEMORY_JMP :
    set_branch_pred(inst_val, desc->fc_val);
    set_reg_a(inst_val, inst->u.memory.ra);
    set_reg_b(inst_val, inst->u.memory.rb);
    set_target(inst_val, inst->u.memory.disp);
    break;
  case FMT_BRANCH     :
    set_reg_a(inst_val, inst->u.branch.ra);
    if(inst->u.branch.internal) {
      /* we only know which instruction so now find its pc */
      /* the new pc of the original target instruction is the new target*/
      /*
      if((target_inst = inst->u.branch.i_target) == NULL) {
	printf("ERROR >> internal branch to unknown instruction\n");
	print_inst(*inst); printf("\n"); 
	return 0;
      }
      if(target_inst->pc == NULL) {
	printf("ERROR >> internal branch to an instruction without pc set\n");
	print_inst(*inst); printf("\n"); 
	print_inst(*target_inst); printf("\n"); 
	return 0;
      }
      */

      target_inst = inst + inst->u.branch.offset;
      if(target_inst->insert) {
	target_inst = target_inst->u.insert.inst;
      }
      if(target_inst->cancel) {
	target_inst = find_next_not_cancelled(target_inst);
	if(target_inst == NULL) {
	  printf("ERROR >> could not find the target of a branch\n");
	  return 0;
	}
      }
      if(target_inst->end) {
	printf("ERROR >> target of a branch is END instruction\n");
	return 0;
      }

      /* bullshit?
      if(target_inst->opcode != inst->u.branch.i_target->opcode &&
	 !target_inst->cancel) 
      {
	printf("ERROR >> branch inconsistency\n");
	print_inst(*inst); printf("\n");
	print_inst(*target_inst); printf("\n");
	print_inst(*inst->u.branch.i_target); printf("\n");
	return 0;
      }
      */
      inst->u.branch.ptr = target_inst->pc;
      inst->u.branch.disp = (((char *)inst->u.branch.ptr - (char *)inst->pc) -
			     sizeof(int));

      /*
      printf("internal branch: 0x%lx 0x%lx 0x%lx %d %d %d\n", 
	     inst->pc, 
	     inst_val,
	     target_inst->pc, 
	     inst->u.branch.offset,
	     inst->u.branch.disp, 
	     (((char *)inst->u.branch.ptr - (char *)inst_val) - sizeof(int)));
      print_inst(*target_inst); printf("\n");
      */
    }
    /*
       else
       printf("external branch: 0x%lx 0x%lx %d %d %d\n", 
       inst->pc, 
       inst_val,
       inst->u.branch.offset,
       inst->u.branch.disp, 
       (inst->u.branch.ptr - inst_val - sizeof(int)) >> 2);
       */

    disp = ((char *)inst->u.branch.ptr - (char *)inst_val) - sizeof(int);
    set_br_disp(inst_val, disp);

    /*
       print_inst(*inst);
       printf("\n");
       */

    break;
  default:
    printf("ERROR >> incorrect instruction format\n");
    inst_val = 0;
  }
}

/******************************************************************************
 *
 * procedures
 *
 *****************************************************************************/

/*
 * limit on a procedure size 
 */
#define MAX_PROC_SIZE 500

/*
 * set all the branch and label information
 */

int set_br_info(proc_t *desc, inst_t *inst_ptr)
{
  inst_t *i_target;
  inst_val_t *pc;
  int i, offset, size;

  i = 0;
  size = desc->size;
  while(!inst_ptr->end) {
    if(inst_ptr->insert) {
      printf("ERROR >> insert in set_br_info\n");
      return -1;
    } else if(inst_ptr->cancel) {
      printf("ERROR >> cancel in set_br_info\n");
      return -1;
    } else if(inst_ptr->branch) {
      offset = i + 1 + inst_ptr->u.branch.disp / sizeof(int);
      if(offset < 0 || offset > size-1) {
	/* external branch */
	if(inst_ptr->opcode != OP_bsr) {
	  printf("ERROR >> set_br_info: external branch not a BSR: ");
	  print_inst(*inst_ptr); printf("\n"); 
	  printf("0x%lx :",inst_ptr->orig_pc); 
	  print_inst(*inst_ptr); printf("\n"); 
	  printf("o %d; i %d; d %d; od %d; s %d; op %d; sp %d\n", 
		 offset, i, inst_ptr->u.branch.disp, 
		 inst_ptr->u.branch.orig_disp, 
		 size, offset * sizeof(int), size * sizeof(int));
	  return -1;
	}

	pc = (inst_val_t *)((char *)inst_ptr->orig_pc + 
			    sizeof(int) + inst_ptr->u.branch.disp);
	inst_ptr->u.branch.ptr = pc;
	if(asm_debug > 8) {
	  printf("set_br_info: 0x%lx branch to 0x%lx\n",
		 inst_ptr->orig_pc, inst_ptr->u.branch.ptr);
	}
      } else {
	/* internal branch */
	offset = i + 1 + inst_ptr->u.branch.orig_disp / sizeof(int);
	inst_ptr->u.branch.internal = 1;
	i_target = desc->code + offset;
	inst_ptr->u.branch.offset = offset - i;

	/*
	   printf("-- asm set branch 0x%lx:\n", inst_ptr);
	   print_inst(*inst_ptr);
	   printf("\n");
	   print_inst(*i_target);
	   printf("\n");
	   print_inst(*(inst_ptr+inst_ptr->u.branch.offset));
	   printf("\n");
	   printf("--\n");
	   */

	if(i_target == NULL) {
	  printf("ERROR >> set_br_info: target not found: %d %d %d\n",
		 offset, desc->size, (desc->orig)?desc->orig->size:0);
	  return -1;
	}
	inst_ptr->u.branch.i_target = i_target;
	if(i_target->label == 0) {
	  desc->label_cnt++;
	  i_target->label = desc->label_cnt;
	}
      }
      i++;
    } else {
      i++;
    }
    inst_ptr++;
  }
  return i;
}

void set_sentinel_inst(inst_t *inst, inst_t *external)
{
  inst->idx = 0;
  inst->end = 1;
  inst->u.end.inst = external;
}

void set_inst(inst_t *inst, int opcode, int fmt)
{
  int idx;

  idx = find_by_opcode(opcode);
  if(fmt) {
    idx = find_by_function(idx, opcode, fmt);
  }
  inst->opcode = decode_table[idx].opcode;
  inst->branch = 0;
  inst->jump   = 0;
  inst->idx = idx;
  inst->pc  = NULL;
  inst->orig_pc = NULL;
  inst->gp  = NULL;
  inst->end = 0;
  inst->insert = 0;
  inst->cancel = 0;
  inst->label = 0;
}


/*
 * Go through all instructions and make sure the can be reassambled
 * after cloning, replace branches, recognize what's impossible to
 * clone, put pointers into GPA.
 *
 * Since the code can grow, return the new size.
 */

int redo_insts(proc_t *desc)
{
  int i, idx;
  inst_val_t *target;
  int done_by_prev;
  inst_val_t *gp = desc->gp;
  int n_inst;
  inst_t *inst_ptr;
  inst_t *inst;
  
  if(asm_debug > 8) {
    printf(">>> redo_insts : 0x%lx\n", desc->proc);
  }

  i = 0;
  done_by_prev = FALSE;
  inst = desc->code;
  while(!inst->end) {
    if(asm_debug > 8) {
      printf(">>> redo insts 0x%lx (%d): ", inst->orig_pc, done_by_prev);
      print_inst(*inst); printf("\n");
    }

    if(inst->cancel) {
      printf("ERROR >> cancel in redo_insts\n");
      return -1;
    } else if(inst->insert) {
      printf("ERROR >> insert in redo_insts\n");
      return -1;
    } 

    if(!done_by_prev) {
      if(is_gp_set_inst(inst)) {
	inst->u.memory.format = 5;
	(inst+1)->u.memory.format = 6;
	done_by_prev = TRUE;
      } else if((n_inst = is_gp_access(inst, gp)) != 0) {
	if(n_inst == -1) {
	  dump(desc);
	  return -1;
	}
	if(n_inst == 2) {
	  done_by_prev = TRUE;
	} else {
	  done_by_prev = FALSE;
	}
      } else if(inst->branch && ! inst->u.branch.internal) {
	if(asm_debug > 8) {
	  printf("external branch: "); print_inst(*inst); printf("\n"); 
	}
	
	if(inst->opcode != OP_bsr) {	
	  printf("ERROR >> redo_insts: external branch not a BSR: ");
	  print_inst(*inst); printf("\n"); 
	  dump(desc);
	  return -1;
	}
	if(inst->u.branch.ra != REG_ra) {
	  printf("ERROR >> branch subroutine not using RA\n");
	  printf("0x%lx : ", inst->orig_pc); print_inst(*inst); printf("\n"); 
	  dump(desc);
	  
	  return -1;
	}

	/* this is an external branch, make it a jump to be conservative */
	inst_ptr = (inst_t *)spin_malloc(5 * sizeof(inst_t));
	desc->size += 4;
	
	/* ldq t12, xxx(gp) */
	set_inst(inst_ptr, 0x29, 0);
	inst_ptr[0].u.memory.ra = 27;
	inst_ptr[0].u.memory.rb = 29;

	if(inst->u.branch.ptr == NULL) {
	  printf("ERROR >> unknown target of branch\n");
	}

	/* FIXME: THIS IS AN UGLY HACK */
	/* check whether the pointer is to the beginning of the procedure
	   or skipping setting of gp, should use linker symbols instead */
 	target = inst->u.branch.ptr-2; 
	if(get_opcode(*target) != 0x09 || get_opcode(*(target+1)) != 0x08) {
	  target = inst->u.branch.ptr; 
	}
	inst_ptr[0].u.memory.target = target;
	inst_ptr[0].u.memory.format = 1;
	
	/* jsr ra, (t12), xxx */
	set_inst(inst_ptr+1, 0x1a, 0x1);
	inst_ptr[1].jump   = 1;
	inst_ptr[1].u.jump.ra = 26;
	inst_ptr[1].u.jump.rb = 27;
	inst_ptr[1].u.jump.disp = 0;
	inst_ptr[1].orig_pc = inst->orig_pc;
	inst_ptr[1].u.jump.ptr = target;

	/* ldah gp, xxx(ra) */
	set_inst(inst_ptr+2, 0x09, 0);
	inst_ptr[2].u.memory.ra = 29;
	inst_ptr[2].u.memory.rb = 26;
	inst_ptr[2].u.memory.format = 5;

	/* lda gp, xxx(gp) */
	set_inst(inst_ptr+3, 0x08, 0);
	inst_ptr[3].u.memory.ra = 29;
	inst_ptr[3].u.memory.rb = 29;
	inst_ptr[3].u.memory.format = 6;

	/* disp for 2 and 3 will be set in the recursive call */

	inst_ptr[4].idx = 0;
	inst_ptr[4].end = 1;
	inst_ptr[4].u.end.inst = inst+1;
	inst_ptr[4].label = 0;

	inst->idx = 0;
	inst->insert = 1;
	inst->u.insert.inst = inst_ptr;
	done_by_prev = FALSE;
      } else {
	done_by_prev = FALSE;
      }
    } else {
      done_by_prev = FALSE;
    }
      
    i++;
    inst++;
  }

  if(asm_debug > 8) {
    printf(">>> redo_insts : %d\n", i);
  }

  return i;
}
/*
 * disassemble a procedure
 */ 

proc_t *disassemble_proc(inst_val_t *proc, int orig_size, char *name)
{
  proc_t *desc;
  int i, size;
  inst_t *inst_ptr;
  inst_val_t *code_ptr;

  size = orig_size;
  if(size == 0) {
    size = MAX_PROC_SIZE;
  }
  if(size == 0) {
    printf("ERROR >> disassemble_proc: procedure size is zero\n");
    return NULL;
  }

  /* create the procedure descriptor */
  desc = (proc_t *)spin_malloc(sizeof(proc_t));
  desc->proc = proc;
  desc->orig = NULL;
  desc->size = size;
  desc->name = name;
  desc->clone = NULL;
  desc->label_cnt = 0;

  /* decode the instructions */
  code_ptr = proc;
  inst_ptr = (inst_t *)spin_malloc((size + 1)* sizeof(inst_t));
  desc->code = inst_ptr;
  for(i = 0; i < size; i++) {
    *inst_ptr = disassemble_inst(*code_ptr, code_ptr);
    inst_ptr++;
    code_ptr++;
  }

  /* set the temporary sentinel instruction */
  set_sentinel_inst(desc->code + i, NULL);

  /* find the gp for the procedure (NULL if not set) */
  desc->gp = get_gp(desc);

  /* find or verify the size */
  i = correct_proc_size(desc);
  if(i == 0) {
    return NULL;
  }
  desc->size = i;

  /* set the sentinel instruction */
  set_sentinel_inst(desc->code + i, NULL);

  if(desc == NULL) {
    return NULL;
  }

  if(orig_size != 0 && desc->size != orig_size) {
    printf("ERROR >> assembler: procedure size incorreclty specified: %d %d\n",
	   size, desc->size);
    return NULL;
  }

  /* find and annotate branches */
  if(set_br_info(desc, desc->code) == -1) {
    return NULL;
  }

  /* find and annotate memory access and relocation information */
  if(redo_insts(desc) == -1) {
    return NULL;
  }

  return desc;
}

/*
 * check whether the instruction sets the gp
 */

int is_gp_set_inst(inst_t *inst)
{
  if(inst->opcode == OP_ldah && inst->u.memory.ra == REG_gp && 
     (inst->u.memory.rb == REG_t12 || inst->u.memory.rb == REG_ra))
  {
    inst++;
    if(inst->opcode == OP_lda && 
       inst->u.memory.ra == REG_gp && inst->u.memory.rb == REG_gp)
    {  
      return 1;
    }
  }
  return 0;
}

static inst_val_t *get_gp(proc_t *proc_desc)
{
  inst_t *inst = proc_desc->code;
  char *gp = (char *)proc_desc->proc;

  if(! is_gp_set_inst(inst)) {
    return NULL;
  }

  gp += inst->u.memory.disp << 16;
  inst++;
  gp += inst->u.memory.disp;
  return (inst_val_t *)gp;
}

int is_gp_access(inst_t *inst, inst_val_t *gp)
{
  int idx = inst->idx;
  inst_desc_t *desc = &decode_table[idx];
  int fmt = decode_table[idx].format;
  inst_t *next_inst = NULL;
  inst_val_t *new_target;
  
  switch(fmt) {
  case FMT_MEMORY_FC  :
  case FMT_MEMORY_JMP :
    inst->u.memory.format = 4;
  case FMT_PALCODE    :
  case FMT_BRANCH     :
    return 0;
  case FMT_OPERATE    :
    if(inst->u.operate.rc == REG_gp) {
      printf("ERROR >> gp used as rc\n");
      return -1;
    }
    
    if(inst->u.operate.rc != REG_gp &&
       (inst->u.operate.lit_flag || inst->u.operate.rb != REG_gp))
    {
      return 0;
    }

    if(inst->opcode == OP_addq) {
      printf("ERROR >> addq gp directive (probably switch statement)\n");
      printf("0x%lx: \t", inst); print_inst(*inst); printf("\n");
      return -1;
    }

    printf("ERROR >> unrecognized gp directive (operate format)\n");
    printf("0x%lx: \t", inst); print_inst(*inst); printf("\n");
    return -1;

  case FMT_MEMORY     :
    if(inst->u.memory.rb != REG_gp) {
      inst->u.memory.format = 4;
      return 0;
    }

    if(inst->opcode == OP_ldq) {
      /*
      printf("??? %lx %d %lx\n", gp, inst->u.memory.disp, 
	     (((char *)gp) + inst->u.memory.disp));
	     */
      inst->u.memory.target =
	*((inst_val_t **)(((char *)gp) + inst->u.memory.disp));
      if(asm_debug > 8) {
	printf("\n\ntarget: 0x%lx\n", inst->u.memory.target);
      }
      inst->u.memory.format = 1;
      return 1;
    }

    if(inst->opcode == OP_ldah) {
      next_inst = inst + 1;
      if((next_inst->opcode == OP_lda || next_inst->opcode == OP_ldl) && 
	 inst->u.memory.ra == next_inst->u.memory.rb)
      {
	inst->u.memory.target = (inst_val_t *)(((char *)gp) + 
					       (inst->u.memory.disp << 16) +
					       (next_inst->u.memory.disp));
	inst->u.memory.format = 2;
	next_inst->u.memory.format = 3;
	return 2;
      } else {
	/* a single ldah, change it into a ldq from a GPA */
	if(inst->u.memory.ra != REG_at) {
	  printf("ERROR >> expected AT in is_gp_access\n");
	}

	/* ldq at, xxx(gp) */
	set_inst(inst, 0x29, 0);
	inst->u.memory.ra = 28;
	inst->u.memory.rb = 29;

	/* original value */
	(long)new_target  = ((long)gp & 0xffffffff00000000);
	(long)new_target |= (((long)gp + 
			      ((inst->u.memory.disp<<16) & 0xffff0000)) & 
			     0x00000000ffffffff);
	new_target = add_to_gpa(new_target);
	inst->u.memory.target = new_target;
	inst->u.memory.disp = (char *)new_target - (char *)gp;
	inst->u.memory.format = 1;
      }
      return 0;
    } else if(inst->opcode == OP_lda) {
      /* a single lda, change it into a ldq from a GPA */
      /* ldq ra, xxx(gp) */
      set_inst(inst, 0x29, 0);
      inst->u.memory.rb = 29;
      
      /* original value (gp plus sign extended old disp) */
      (long)new_target = (long)gp + ((inst->u.memory.disp << 16) >> 16);
      inst->u.memory.target = new_target;
      inst->u.memory.disp = (char *)new_target - (char *)gp;
      inst->u.memory.format = 1;
      return 0;
    }
  
    printf("ERROR >> unrecognized gp directive (memory format)\n");
    printf("0x%lx: \t", inst); print_inst(*inst); printf("\n");
    if(next_inst != NULL) {
      printf("0x%lx: \t", inst); print_inst(*inst); printf("\n");
    }
    return -1;

  default:
    printf("ERROR >> is_gp_access: unrecognized instruction format\n");
    return 0;
  }
}


/*
 * Generate code.  All instructions must be legal (after executing
 * redo_insts).  Get loading of gp which relative to the position
 * of code.
 */

int assemble_insts(proc_t *new_desc, inst_t *new_src, inst_val_t *new_code)
{
  int i, k;
  
  if(asm_debug > 8) {
    printf(">>> asm_insts\n");
  }

  i = 0;
  while(!new_src->end) {
    if(asm_debug > 8) {
      printf(">>> asm insts 0x%lx (0x%lx): ", new_code, new_src);
      print_inst(*new_src); printf("\n");
    }
    
    if(new_src->insert) {
      if((k=assemble_insts(new_desc, new_src->u.insert.inst, new_code)) == -1){
	return -1;
      }
      new_code += k;
      i += k;
    } else if(new_src->cancel) {
    } else {
      if(!assemble_inst(new_src, new_code, new_desc->gp)) {
	return -1;
      }
      new_code++;
      i++;
    }
    new_src++;
  }

  if(asm_debug > 8) {
    printf(">>> asm_insts: done\n");
  }
  
  return i;
}


int set_pc_values(inst_t *inst, inst_val_t *pc)
{
  int i = 0, k;

  while(!inst->end) {
    if(inst->cancel) {
    } else if(inst->insert) {
      k = set_pc_values(inst->u.insert.inst, pc);
      i += k;
      pc += k;
    } else {
      inst->pc = pc;
/*
printf(">>> set pc 0x%lx 0x%lx 0x%lx: ", inst, pc, inst->orig_pc);
print_inst(*inst); printf("\n");
*/
      i++;
      pc++;
    }
    inst++;
  }
  return i;
}
/*
 * assemble a procedure from its intermediate code 
 */
int assemble_proc(proc_t *new_desc)
{
  int size, new_size;

  size = new_desc->size;
  
  if(asm_debug > 8) {
    printf(">>> assemble_proc: 0x%lx\n", new_desc->proc);
    print_proc(new_desc);
  }

  /* check whether GPA area is enough */
  if(size > 2 * (GPA_size - GPA_cnt)) {
    init_gpa();
  }
  new_desc->gp = (inst_val_t *)GPA_ptr;

  /* allocate memory for the executable code */
  new_desc->proc = (inst_val_t *)spin_malloc(size * sizeof(inst_val_t));
  
  /* set all the pc-s */
  set_pc_values(new_desc->code, new_desc->proc);
  
  /* assemble instructions */
  new_size = assemble_insts(new_desc, new_desc->code, new_desc->proc);
  if(new_size == -1) {
    return 0;
  } else if(new_size > size) {
    printf("ERROR >> assemble_proc procedure size mismatch: %d %d\n",
	   size, new_size);
  }
  

  if(asm_debug > 8) {
    printf(">>> assemble_proc: done\n");
  }

  return 1;
}

int find_length(inst_t *inst)
{
  int n = 0;

  if(asm_debug > 8) {
    printf(">>> find_length: 0x%lx\n", inst);
  }

  if(inst == NULL) {
    return 0;
  }

  while(!inst->end) {
    n++;
    inst++;
  }

  if(asm_debug > 8) {
    printf(">>> find_length: %d\n", n);
  }

  return n;
}

/*
 * try to find the correct size of the procedure
 * be conservative and reject procedures that:
 *      are too long (no return found within the size limits)
 *      branch over the return instruction, 
 *      branch before their beginning
 *      calls a subroutine within its own code
 * branch to subroutine are OK
 * mark the procedure as leaf if no "bsr" found
 */

int correct_proc_size(proc_t *desc)
{
  inst_t *inst_ptr = desc->code;
  inst_t *i_target;
  inst_val_t *pc;
  int i, offset, size, max_offset, max_idx;
  int leaf = 1, ret = 0;

  size = desc->size;
  max_offset = 0;
  for(i = 0; i < size; i++) {
    if(inst_ptr->branch) {
      /* offset from the beginning of the procedure */
      offset = i + 1 + inst_ptr->u.branch.disp / sizeof(int);
      if(inst_ptr->opcode == OP_bsr) {
	leaf = 0;
      } else {
	if(offset < 0) {
	  /* branch to outside of procedure found */
	  printf("ERROR >> branch to before the beginning of procedure\n");
	  return 0;
	} else {
	  /* keep track of branch (non-subroutine) that jumps farthest */
	  if(offset > max_offset) {
	    max_offset = offset;
	    max_idx = i;
	  }
	}
      }
    } else if(inst_ptr->jump) {
      if(inst_ptr->opcode == OP_ret) {
	/* return instruction found */
	ret = 1;
	break;
      }
      /* call to outside of procedure found */
      leaf = 0;
    }
    inst_ptr++;
  }

  if(leaf) {
    if(max_offset > i && ret) {
      printf("WARNING >> procedure too long or too complicated to disassemble\n");
      return 0;
    }
  } 

  if(!ret) {
    /* procedure too long */
    return 0;
  }

  desc->leaf = leaf;
  return i+1;
}



void cancel_inst(inst_t *inst)
{
  int idx;

  /* bis zero, zero, zero */
  idx = find_by_opcode(0x11);
  idx = find_by_function(idx, 0x11, 0x20);
  
  inst->opcode = decode_table[idx].opcode;
  inst->branch = 0;
  inst->jump   = 0;
  inst->u.operate.ra = 31;
  inst->u.operate.rb = 31;
  inst->u.operate.rc = 31;
  inst->idx = idx;
  inst->cancel = 1;
}

/* 
 * remove the return instruction, there should be only 
 * one and it should be last 
 */
int remove_return(inst_t *inst, inst_t *external)
{
  int idx;

  while(!inst->end) {
    if(inst->insert) {
      if(remove_return(inst->u.insert.inst, NULL)) {
	printf("ERROR >> return instruction in the insert\n");
	return 0;
      }
    } else {
      if(inst->opcode == OP_ret) {
	cancel_inst(inst);
	inst++;
	if(!inst->end) {
	  printf("ERROR >> return should be the last instruction\n");
	  return 0;
	}
	inst->u.end.inst = external;
	return 1;
      }
    }
    inst++;
  }
  return 0;
}

int inline_insts(proc_t *new_desc, proc_t *desc_in, 
		 inst_t *inst, inst_val_t *inline_pc)
{
  inst_t *new_inst;

  if(asm_debug > 8) {
    printf(">>> inline_insts: 0x%lx 0x%lx\n", inst->orig_pc, inline_pc);
  }

  while(!inst->end) {
    if(asm_debug > 8) {
      printf(">> inline: 0x%lx 0x%lx ", inst->orig_pc, inline_pc);
      print_inst(*inst); printf("\n");
    }
    if(inst->insert) {
      if(inline_insts(new_desc, desc_in, inst->u.insert.inst, inline_pc)) {
	if(asm_debug > 8) {
	  printf(">>> inline_insts inlined in an insert\n");
	}
	return 1;
      }
    } else if(inst->orig_pc == inline_pc) {
      /* this is the instruction, check if it's a jump */
      if((inst->opcode == OP_bsr && ! inst->u.branch.internal) || 
	 inst->opcode == OP_jsr)
      {
	if(asm_debug > 8) {
	  printf(">>> inlining\n");
	}
	/* replace regeneration of gp with noops */
	if(is_gp_set_inst(inst+1)) {
	  /* FIXME: this is a problem if the inlined procedure does not
	     restore $gp after its last jump */
	  cancel_inst(inst+1);
	  cancel_inst(inst+2);
	}
	
	/* replace the jump instruction with insert */
	inst->insert = 1;

	/* clone the inlined procedure into the new one */
	new_inst = desc_in->code;

	/* do not recompute gp on entry to the inlined procedure */
	if(is_gp_set_inst(new_inst)) {
	  cancel_inst(new_inst);
	  cancel_inst(new_inst+1);
	}
	inst->u.insert.inst = new_inst;

	/* remove the return instruction of the inlined code */
	/* and add a back pointer to the end instruction */
	remove_return(new_inst, inst+1);

	/* increase the size */
	new_desc->size += desc_in->size;
	
	if(asm_debug > 8) {
	  printf(">>> inline_insts done inlining\n");
	}

	return 1;
      } else {
	printf("ERROR >> inline_insts: not a jump instruction\n");
	return 0;
      }
    } 
    inst++;
  }

  if(asm_debug > 8) {
    printf(">>> inline_insts did not find the target\n");
  }
  return 0;
}

proc_t *inline_proc(proc_t *desc_out, proc_t *desc_in, inst_val_t *inline_pc)
{
  int found = 0;
  
  if(asm_debug > 8) {
    printf(">>> inline_proc: 0x%lx 0x%lx 0x%lx\n", 
	   desc_out->proc, desc_in->proc);
    print_proc(desc_out);
    print_proc(desc_in);
  }

  /* find the instruction at which to inline and do the inlining there */
  if(!inline_insts(desc_out, desc_in, desc_out->code, inline_pc)) {
    printf("ERROR >> inlining failed\n");
    return NULL;
  }
  
  if(asm_debug > 8) {
    printf(">>> inline_proc done\n");
    print_proc(desc_out);
  }

  /* success, return the new procedure */
  return desc_out;
}

inst_val_t *find_bsr(inst_t *inst, inst_val_t *ptr)
{
  inst_val_t *tmp;

  while(!inst->end) {
    if(inst->insert) {
      tmp = find_bsr(inst->u.insert.inst, ptr);
      if(tmp != NULL) {
	return tmp;
      }
    } else {
      if(inst->opcode == OP_jsr && inst->u.jump.ptr == ptr) {
	return inst->orig_pc;
      }
    }
    inst++;
  }
  return NULL;
}

void free_insts(inst_t *inst)
{
  inst_t *ptr = inst;

  while(!inst->end) {
    if(inst->insert) {
      free_insts(inst->u.insert.inst);
    }
    inst++;
  }
  spin_free(ptr);
}
/*
static int scnt = 0;
*/

int l_debug = 0;

inst_val_t *AsmInline(inst_val_t *out_code, int out_size,
		      inst_val_t **code, inst_val_t **ptr, int cnt)
{
  proc_t *out_proc, **procs;
  int i, j;
  inst_val_t *res;

  /*
  scnt++;
  asm_debug = (scnt == 12);
  printf("$%d %d\n", scnt, asm_debug);
  */

  if(out_code == NULL || code == NULL || ptr == NULL || cnt == 0) {
    printf("ERROR >> NULL or zero passed to AsmInline\n");
    return NULL;
  }

  if(asm_broken) {
    printf("ERROR >> assembler failure\n");
    return NULL;
  }

  if(l_debug || asm_debug) {
    printf("--- AsmInline:\n");
    printf("    out   : 0x%lx %d\n", out_code, out_size);
  }
  for(i = 0; i < cnt; i++) {
    if(l_debug || asm_debug) {
      printf("    in %3d: 0x%lx at: 0x%lx\n", i, code[i], ptr[i]);
    }
    if(code[i] == NULL || ptr[i] == NULL) {
      printf("ERROR >> NULL passed to AsmInline\n");
      return NULL;
    }
  }

  if(cnt > 25) {
    if(l_debug || asm_debug) {
      printf("not inlining, too many procedures\n");
    }
    return NULL;
  }

  procs  = (proc_t **)spin_malloc(cnt * sizeof(proc_t *));
  for(i = 0; i < cnt; i++) {
    procs[i]  = NULL;
  }

  if((out_proc = disassemble_proc(out_code, out_size, "out")) == NULL) {
    return NULL;
  }

  if(l_debug || asm_debug > 8) {
    print_proc(out_proc);
  }

  /*
  printf("cnt: %d\n", cnt);
  cnt = (cnt > 10) ? 10 : cnt;
  */
  for(i = 0; i < cnt; i++) {
    if((procs[i]  = disassemble_proc(code[i], 0, "in")) == NULL) {
      printf("WARNING >>> could not disassemble procedure 0x%lx\n", code[i]);
      continue;
    }
    if(l_debug || asm_debug > 8) {
      print_proc(procs[i]);
    }
    if(procs[i]->size > 500) {
      printf(">>> not inlining 0x%lx of size %d\n", 
	     procs[i]->proc, procs[i]->size);
      continue;
    }

    if(l_debug || asm_debug) {
      printf(">>> inlining 0x%lx into 0x%lx\n",
	     procs[i]->proc, out_proc->proc);
    }

    if((out_proc = inline_proc(out_proc, procs[i], ptr[i])) == NULL) {
      printf("ERROR >>> inlining failed\n");
      continue;
    }

    if(l_debug || asm_debug > 8) {
      print_proc(out_proc);
    }
  }
  
  /* assemble the executable code */
  if(!assemble_proc(out_proc)) {
    printf("ERROR >>> assembling failed\n");
    return NULL;
  }

  res = out_proc->proc;

  /* free everything except the generated code */
  /* FIXME: should free inserted code */
  for(i = 0; i < cnt; i++) {
    if(procs[i] != NULL) {
      spin_free(procs[i]);
    }
  }
  free_insts(out_proc->code);
  spin_free(out_proc);
  spin_free(procs);

  if(l_debug || asm_debug) {
    printf(">>> inlined 0x%lx\n", res);
  }
  return res;
}


AsmTest() {}
AsmPrint() {}



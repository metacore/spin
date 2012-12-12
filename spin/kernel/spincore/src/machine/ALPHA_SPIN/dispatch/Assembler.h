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
 *      Created. Machine code representation.
 *
 */

/******************************************************************************
 *
 * instruction decoding
 *
 *****************************************************************************/

enum {
  REG_v0,
  REG_t0,
  REG_t1,
  REG_t2,
  REG_t3,
  REG_t4,
  REG_t5,
  REG_t6,
  REG_t7,
  REG_s0,
  REG_s1,
  REG_s2,
  REG_s3,
  REG_s4,
  REG_s5,
  REG_fp,
  REG_a0,
  REG_a1,
  REG_a2,
  REG_a3,
  REG_a4,
  REG_a5,
  REG_t8,
  REG_t9,
  REG_t10,
  REG_t11,
  REG_ra,
  REG_t12,
  REG_at,
  REG_gp,
  REG_sp,
  REG_zero
};

enum {
  OP_lda,
  OP_ldg,
  OP_ldq,
  OP_lds,
  OP_stg,
  OP_stq,
  OP_sts,
  OP_ldah,
  OP_ldl,
  OP_ldq_l,
  OP_ldt,
  OP_stl,
  OP_stq_c,
  OP_stt,
  OP_ldf,
  OP_ldl_l,
  OP_ldq_u,
  OP_stf,
  OP_stl_c,
  OP_stq_u,
  OP_fetch,
  OP_rc,
  OP_trapb,
  OP_fetch_m,
  OP_rpcc,
  OP_mb,
  OP_rs,
  OP_jmp,
  OP_ret,
  OP_jsr,
  OP_jsr_co,
  OP_br,
  OP_fble,
  OP_fbge,
  OP_beq,
  OP_blbs,
  OP_bgt,
  OP_fbeq,
  OP_bsr,
  OP_fbgt,
  OP_blt,
  OP_bne,
  OP_fblt,
  OP_fbne,
  OP_blbc,
  OP_ble,
  OP_bge,
  OP_addl,
  OP_addqv,
  OP_cmple,
  OP_cmpult,
  OP_subq,
  OP_addlv,
  OP_cmpbge,
  OP_cmplt,
  OP_subl,
  OP_subqv,
  OP_addq,
  OP_cmpeq,
  OP_cmpule,
  OP_sublv,
  OP_s4addl,
  OP_s4subq,
  OP_s8subl,
  OP_s4addq,
  OP_s8addl,
  OP_s8subq,
  OP_s4subl,
  OP_s8addq,
  OP_and,
  OP_cmoveq,
  OP_bic,
  OP_cmovlbc,
  OP_bis,
  OP_cmovlbs,
  OP_cmovge,
  OP_cmovlt,
  OP_ornot,
  OP_cmovgt,
  OP_cmovne,
  OP_xor,
  OP_cmovle,
  OP_eqv,
  OP_extbl,
  OP_extqh,
  OP_extwl,
  OP_insll,
  OP_inswh,
  OP_msklh,
  OP_mskql,
  OP_sll,
  OP_zap,
  OP_extlh,
  OP_extql,
  OP_insbl,
  OP_insqh,
  OP_inswl,
  OP_mskll,
  OP_mskwh,
  OP_sra,
  OP_zapnot,
  OP_extll,
  OP_extwh,
  OP_inslh,
  OP_insql,
  OP_mskbl,
  OP_mskqh,
  OP_mskwl,
  OP_srl,
  OP_mull,
  OP_mulqv,
  OP_mullv,
  OP_umulh,
  OP_mulq
};

enum {
  FMT_MEMORY,
  FMT_MEMORY_FC,
  FMT_MEMORY_JMP,
  FMT_BRANCH,
  FMT_OPERATE,
  FMT_PALCODE
};

/******************************************************************************
 *
 * operations on instruction fields
 *
 *****************************************************************************/

#define REG_A_MASK       0x03e00000
#define REG_B_MASK       0x001f0000
#define REG_C_MASK       0x0000001f
#define OPCODE_MASK      0xfc000000
#define FUNCTION_MASK    0x00000fe0
#define DISP_MASK        0x0000ffff
#define BR_DISP_MASK     0x001fffff
#define LIT_MASK         0x001fe000
#define NUMBER_MASK      0x03ffffff
#define LIT_FLAG_MASK    0x00001000
#define BRANCH_PRED_MASK 0x0000c000
#define TARGET_MASK      0x00003fff

#define REG_A_SHIFT        21
#define REG_B_SHIFT        16
#define REG_C_SHIFT         0
#define OPCODE_SHIFT       26
#define FUNCTION_SHIFT      5
#define DISP_SHIFT          0
#define BR_DISP_SHIFT       0
#define LIT_SHIFT          13 
#define NUMBER_SHIFT        0
#define LIT_FLAG_SHIFT     12
#define BRANCH_PRED_SHIFT  14  
#define TARGET_SHIFT        0

#define SET_INST_FIELD(dst, val, mask, shift) \
  *(int *)(dst) = (((*(int *)(dst)) & (~mask)) | (((val) << (shift)) & (mask)))

#define GET_INST_FIELD(dst, mask, shift) \
  ((dst & (mask)) >> (shift))

/******************************************************************************
 *
 * code representation
 *
 *****************************************************************************/

typedef struct inst_desc {
  int   op_val;
  int   fc_val;
  int   opcode;
  char *name;
  int   format;
} inst_desc_t;

typedef int inst_val_t;

typedef struct inst {
  int   opcode;
  int   insert;
  int   cancel;
  int   end;
  int   done;
  int   idx;
  int   label;
  int   branch;
  int   jump;
  inst_val_t *pc;
  inst_val_t *orig_pc;
  inst_val_t *gp;
  union u {
    struct insert {
      struct inst *inst;
    } insert;
    struct end {
      struct inst *inst;
    } end;
    struct pal {
      int number;
    } palcode;
    struct br {
      int ra;
      int disp;
      int orig_disp;
      int offset;
      int internal;
      inst_val_t *ptr;
      struct inst *i_target;
    } branch;
    struct jmp {
      int ra;
      int rb;
      int disp;
      inst_val_t *ptr;
    } jump;
    struct mem {
      int ra;
      int rb;
      int disp;
      int format;
      inst_val_t *target;
    } memory;
    struct op {
      int   lit_flag;
      int   ra;
      int   rc;
      int   rb;
      int   lit;
    } operate;
  } u;
} inst_t;

typedef struct proc {
  inst_val_t  *proc;
  inst_val_t  *gp;
  int          leaf;
  int          size;
  inst_t      *code;
  int          label_cnt;
  char        *name;
  struct proc *clone;
  struct proc *orig;
} proc_t;

typedef struct pentry {
  inst_val_t *proc;
  char       *name;
  proc_t     *proc_desc;
  proc_t     *clone_desc;
  long        arg1;
  long        arg2;
} proc_entry_t;



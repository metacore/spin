/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Run-time code generation for the dispatcher
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	All stub cloning procedures have been changed to take the
 *	 saveRegs argument, because the profiled versions all need to
 *	 use it. Stubs with saveRegs set to true are not profiled, because 
 *       they can return twice which hoses the profiler.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added stub profiling support. When profiling is on, the stitcher
 *	 tries to allocate stubs from a single block of memory. This
 *	 enables the profiler to discriminate stubs from other procedures.
 *
 * 21-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	AddPCBypass comparison was off by one.
 *	AddPCBypass procedure badly needs comments.
 *
 * 26-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added second level of optimization (stiching from code snippets).
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Changed Stitcher__Init to StitcherInitialize to avoid conflict
 *	 with Stitcher.m3 initialization function (also called
 *	 StitcherInit).
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *
 */

#include <spincore/ALPHA_SPIN/Dispatcher.h>
#include <m3core_sa/ALPHA_SPIN/RTHeapRep.h>

#define free   XXXXXXXXXXXXXXXXXXXXXXX 
#define malloc XXXXXXXXXXXXXXXXXXXXXXX 

#include <sys/types.h> /* for NULL TRUE and FALSE */
typedef void (func)();

typedef struct dispatch_desc {
 long defaultResult;
 long defaultHandler;
 long defaultClosure;
 long useDefaultClosure;
 long resultHandler;
 long resultClosure;
 long useResultClosure;
} dispatch_desc_t;

typedef struct handler_desc {
  long nImposed;
  long guard;
  long handler;
  long cancel;
  long useGuardClosure;
  long useHandlerClosure;
  long guardClosure;
  long handlerClosure;
} handler_desc_t;

typedef struct imposed_desc {
  long guard;
  long useGuardClosure;
  long guardClosure;
  long postGuard;
  long usePostGuardClosure;
  long postGuardClosure;
} imposed_desc_t;

#ifdef CALL_GRAPH

/*
 * the procedure to be called to do profiling
 */
extern func _mcount;
#endif CALL_GRAPH

static char* disp_malloc(int);

typedef struct gd {
  void *guard;
  void *closure;
  int   useClosure;
} guardDescT;

/* 
 * pointers to templates
 */

/* trampoline_stub symbols */
extern func trampoline_stub;
extern func trampoline_stub_end;
extern func trampoline_long_patch;
extern func trampoline_offset_patch;
extern func trampoline_gp_patch;
#ifdef CALL_GRAPH
extern func trampoline_profile_gp_patch;
#endif

/* pessimistic_stub symbols */
extern func pessimistic_stub;
extern func pessimistic_stub_end;
extern func pessimistic_handlers_gl_long;
extern func pessimistic_handlers_hl_long;
extern func pessimistic_gp_patch;
#ifdef CALL_GRAPH
extern func pessimistic_profile_gp_patch;
#endif CALL_GRAPH
extern func pessimistic_guard_gp_patch;
extern func pessimistic_handler_gp_patch;
extern func pessimistic_pass_callee_saved_1;
extern func pessimistic_pass_callee_saved_2;
extern func pessimistic_inc_stack;
extern func snippet_adjust_sp;
extern func snippet_adjust_sp_end;
extern func pessimistic_jmp_no_handler_1;
extern func pessimistic_jmp_no_handler_2;
extern func pessimistic_return;

/* pessimistic_1_stub symbols */
extern func pessimistic_1_stub;
extern func pessimistic_1_stub_end;
#ifdef SHORT
extern func pessimistic_1_handlers_gl_l;
extern func pessimistic_1_handlers_gl_h;
extern func pessimistic_1_handlers_hl_l;
extern func pessimistic_1_handlers_hl_h;
#endif SHORT
#ifdef LONG
extern func pessimistic_1_handlers_gl_long;
extern func pessimistic_1_handlers_hl_long;
#endif LONG
extern func pessimistic_1_gp_patch;
#ifdef CALL_GRAPH
extern func pessimistic_1_profile_gp_patch;
#endif CALL_GRAPH
extern func pessimistic_1_guard_gp_patch;
extern func pessimistic_1_handler_gp_patch;
extern func pessimistic_1_pass_callee_saved_1;
extern func pessimistic_1_copy_callee_saved_1;
extern func pessimistic_1_pass_callee_saved_2;
extern func pessimistic_1_copy_callee_saved_2;
extern func pessimistic_1_passed_callee_saved;
extern func pessimistic_1_inc_stack;

/* snippets */
extern func snippet_prolog; 
#ifdef CALL_GRAPH
extern func snippet_profile_gp_patch;
#endif CALL_GRAPH
extern func snippet_prolog_end; 
extern func snippet_prolog_1; 
#ifdef CALL_GRAPH
extern func snippet_profile_gp_patch_1;
#endif CALL_GRAPH
extern func snippet_prolog_1_end; 
extern func snippet_prolog_2; 
extern func snippet_prolog_2_end; 
extern func snippet_save_callee;
extern func snippet_save_callee_end;
extern func snippet_save_regs; 
extern func snippet_save_regs_end; 
extern func snippet_load_desc; 
extern func snippet_load_desc_end; 
extern func snippet_start_up;
extern func snippet_start_up_end;
extern func snippet_start_up_mh;
extern func snippet_start_up_mh_end;
extern func snippet_start_up_unrolled;
extern func snippet_start_up_unrolled_end;
extern func snippet_start_up_unrolled_2;
extern func snippet_start_up_unrolled_2_end;
extern func snippet_gl_loop_head;
extern func snippet_gl_loop_head_end;
extern func snippet_gl_loop_head_mh;
extern func snippet_gl_loop_head_mh_end;
extern func snippet_call_guard;
extern func snippet_call_guard_end;
extern func snippet_call_guard_mh;
extern func snippet_call_guard_mh_end;
extern func snippet_handler_loop;
extern func snippet_handler_loop_end;
extern func snippet_handler_loop_mh;
extern func snippet_handler_loop_mh_end;
extern func snippet_check_handlers;
extern func snippet_check_handlers_end;
extern func snippet_call_handler;
extern func snippet_call_handler_end;
extern func snippet_call_handler_mh;
extern func snippet_call_handler_mh_end;
extern func snippet_call_handler_dir_1;
extern func snippet_call_handler_dir_1_end;
extern func snippet_call_handler_dir_2;
extern func snippet_call_handler_dir_2_end;
extern func snippet_hl_loop_head;
extern func snippet_hl_loop_head_end;
extern func snippet_hl_loop_head_mh;
extern func snippet_hl_loop_head_mh_end;
extern func snippet_hl_loop_tail;
extern func snippet_hl_loop_tail_end;
extern func snippet_hl_loop_tail_mh;
extern func snippet_hl_loop_tail_mh_end;
extern func snippet_epilog;
extern func snippet_epilog_end;
extern func snippet_epilog_g;
extern func snippet_epilog_g_end;
extern func snippet_epilog_mh;
extern func snippet_epilog_mh_end;
extern func snippet_no_handler;
extern func snippet_no_handler_end;
extern func snippet_no_handler_gp;
extern func snippet_no_handler_gp_end;
extern func snippet_pass_callee_saved;
extern func snippet_pass_callee_saved_end;
extern func snippet_check_no_closure;
extern func snippet_check_no_closure_end;
extern func snippet_branch_closure;
extern func snippet_branch_closure_end;
	

extern func snippet_load_long;
extern func snippet_load_long_end;
extern func snippet_load_short;
extern func snippet_load_short_end;
extern func snippet_call_guard_dir;
extern func snippet_call_guard_dir_end;
extern func snippet_call_all_guard;
extern func snippet_call_all_guard_end;
extern func snippet_call_imposed_dir;
extern func snippet_call_imposed_dir_end;
extern func snippet_bis_value;
extern func snippet_bis_value_end;
extern func snippet_call_handler_dir;
extern func snippet_call_handler_dir_end;
extern func snippet_check_call;
extern func snippet_check_call_end;
extern func snippet_shift_value;
extern func snippet_shift_value_end;
extern func snippet_branch_fwd;
extern func snippet_branch_fwd_end;

extern func snippet_return;
extern func snippet_return_end;
extern func snippet_beq_v0;
extern func snippet_beq_v0_end;
extern func snippet_call_dir;
extern func snippet_call_dir_end;
extern func snippet_restore;
extern func snippet_restore_end;
extern func snippet_restore_mh;
extern func snippet_restore_mh_end;
extern func snippet_save_result;
extern func snippet_save_result_end;

extern func snippet_trace_enter;
extern func snippet_trace_enter_end;
extern func snippet_trace_capture;
extern func snippet_trace_capture_end;
extern func snippet_trace_increment;
extern func snippet_trace_increment_end;
extern func snippet_trace_exit_1;
extern func snippet_trace_exit_1_end;
extern func snippet_trace_exit_2;
extern func snippet_trace_exit_2_end;
	
#define WORD_SIZE sizeof(long)
#define handlerClosureOffset	6 * WORD_SIZE
#define MaxNumOfHandlersForDebug	255
#define MaxNumOfArgumentsForDebug	15
#define REGISTERS_SAVE_FRAME	96
#define GUARD_RESULTS_FRAME	(MaxNumOfHandlersForDebug + 1) * WORD_SIZE
#define ARGUMENTS_FRAME		(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE
#define PESSIMISTIC_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME + \
					 GUARD_RESULTS_FRAME + \
					 ARGUMENTS_FRAME)
#define resultHandlingOffset    7 * WORD_SIZE

/* instruction snippets */
extern func snippet_bis;
extern func snippet_bis_i;
extern func snippet_stq;
extern func snippet_ldq;

/* st_write_zero symbols */
extern func st_write_zero;

/* 
 * function prototypes
 */

static void patch_gpdisp(int *src, int *dst);
static void patch_long(int *dst, long val);
func *clone_stub(func *stub, func *stub_end);
void  patch_ptr(int *dst_h, int *dst_l, int *src, long arg);
void  patch_arg(func *base, func *clone, func *stub, 
		func *arg_h, func *arg_l, long arg);
void  patch_zero(int *dst);
func *get_gp(func *src);
void patch_pessim_callee_saved(func *copy, int n_args, int n_handlers);
void patch_load_callee_saved(func *copy, int n_args, int closure);
int *add_snippet(int *ptr, func *stub, func *stub_end);
int patch_branch(int *dst, int **ptr);
int  patch_delayed();
void patch_adjust_sp(func *clone, int n_args);
int *copy_args(int *ptr, int guard, int n_args, 
	       int **cl_clone,int **call_clone,int save);
int clone_size_1(dispatch_desc_t *dispatch_list, 
		 func *clone, int *imp_guard_cnt, 
		 int n_args, int res, int n_handlers, int save, long gp, 
		 int do_guards);
int clone_clone_1(func *clone, int size, void *desc,
		  dispatch_desc_t *dispatch_list,
		  int n_args, int res, int n_handlers, int save, long gp,
		  int imp_guard_cnt, int do_guards);
int clone_size_2(dispatch_desc_t *dispatch_list, 
		 func *clone, int *imp_guard_cnt, 
		 int n_args, int res, int n_handlers, int save, 
		 int do_inline, long gp, int *fsize, int trace);
int clone_clone_2(func *clone, int size, void *desc, 
		  dispatch_desc_t *dispatch_list,
		  int n_args, int res, int n_handlers, int save, long gp,
		  int do_inline, int imp_guard_cnt, int trace);
int clone_size_from_list(guardDescT *guardDesc, int cnt, func *clone,
			 int n_args, int res, int save, long gp);
int clone_clone_from_list(func *clone, int size, guardDescT *guardDesc, int cnt,
			  int n_args, int res, int save, long gp);
int *copy_args_1(int *ptr, int n_args, int use_closure, 
		 long closure,int save, long base);
int *add_copy_args(int *ptr, int src_reg, int dst_reg, int offset, int nArgs);
void clone_init(int n_args, int n_handlers, int trace);
int clone_size(int n_args, int res, int n_handlers, int save);
int clone_clone(func *clone, int size, void *desc, 
		int n_args, int res, int n_handlers, int save);
int get_snippet_size(func *stub, func *stub_end);
int get_args_size(int n_args, int save);
int get_args_size_1(int n_args, int use_closure, 
		    long closure, int save, long base);

void copy_bis(func *dst, int reg_a, int reg_b, int reg_c);
void copy_bis_i(func *dst, int reg_a, int reg_c, int lit);
void copy_stq(func *dst, int reg_a, int disp, int reg_b);
void copy_ldq(func *dst, int reg_a, int disp, int reg_b);
void copy_inst(func *dst, func *snippet);
int *add_part_snippet(int *ptr, func *stub, int start, int end);
int get_part_snippet_size(func *stub, int start, int end);

void set_reg_a(func *dst, int reg);
int  get_reg_a(func *dst);
void set_reg_b(func *dst, int reg);
int  get_reg_b(func *dst);
void set_reg_c(func *dst, int reg);
int  get_reg_c(func *dst);

void set_disp(func *code, int disp);
int  get_disp(func *code);
void set_br_disp(func *code, int disp);
int  get_br_disp(func *code);
void set_lit(func *code, int lit);
int  set_offset(int *dst, int *target);
void patch_callee_saved(int *ptr, int n_regs);

#define REG_V0      0
#define REG_T0      1
#define REG_T1      2
#define REG_T2      3
#define REG_S0      9
#define REG_S4     13
#define REG_S6     15
#define REG_A0     16
#define REG_A1     17
#define REG_T12    27
#define REG_SP     30
#define REG_ZERO   31

/******************************************************************************
 *
 * global area for clones
 *
 *****************************************************************************/

/*
 * debugging mode 
 */

static int stitch_debug = 0;
static int verbose      = 0;


int frame_size = PESSIMISTIC_STUB_FRAME_SIZE;
int registers_save_frame = REGISTERS_SAVE_FRAME;
int arguments_frame = ARGUMENTS_FRAME;
int guard_results_frame = GUARD_RESULTS_FRAME;

/******************************************************************************
 *
 * stub generation
 *
 *****************************************************************************/

func *CloneTrampolineStub(func *handler, int jtable_offset, int save)
{
  int size, offset;
  func *clone;
  int *ptr;

  /* create a clone */
  clone = clone_stub(trampoline_stub, trampoline_stub_end);

  /* patch restoring of gp */
  offset = (int *)trampoline_gp_patch-(int *)trampoline_stub;
  patch_gpdisp((int *)trampoline_gp_patch, (int *)clone+offset);

#ifdef CALL_GRAPH
  /* If this is a saveregs event then we want to leave in the instruction
     which sets the low-order bit of ra. If it is not, then we turn this
     instruction into a no-op. */
  if (!save) {
      copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  }

  offset = (int *)trampoline_profile_gp_patch-(int *)trampoline_stub;
  patch_gpdisp((int *)trampoline_profile_gp_patch, (int *)clone+offset);
#endif CALL_GRAPH

  /* patch the parameter */
  offset = (int *)trampoline_long_patch-(int *)trampoline_stub;
  patch_long((int *)clone+offset, (long)handler);

  /* patch the procedure offset */
  ptr = (int *)clone + ((int *)trampoline_offset_patch-(int *)trampoline_stub);
  *ptr = (*ptr & 0xffff0000) | jtable_offset; 
  
  /* flush the i-cache */
  imb();

  return clone;
}

func *CloneDefaultStub(void *desc, int n_args, int res, int save)
{
  int offset;
  func *clone;
  int *ptr;

  /*
  printf("CloneDefaultStub: %s 0x%lx %d\n", 
	 Dispatcher__GetName((((void ***)desc)[2])[0]), desc, n_args);
	 */
  if(stitch_debug) {
    printf("CloneDefaultStub: 0x%lx %d\n", desc, n_args);
  }

  frame_size = PESSIMISTIC_STUB_FRAME_SIZE;
  registers_save_frame = REGISTERS_SAVE_FRAME;
  arguments_frame = ARGUMENTS_FRAME;
  guard_results_frame = GUARD_RESULTS_FRAME;

  clone = clone_stub(pessimistic_stub, pessimistic_stub_end);
  offset = (int *)pessimistic_handlers_gl_long-(int *)pessimistic_stub;
  patch_long((int *)clone+offset, (long)desc);
  offset = (int *)pessimistic_handlers_hl_long-(int *)pessimistic_stub;
  patch_long((int *)clone+offset, (long)desc);
  offset = (int *)pessimistic_gp_patch-(int *)pessimistic_stub;
  patch_gpdisp((int *)pessimistic_gp_patch, (int *)clone+offset);

#ifdef CALL_GRAPH
  /* If this is a saveregs event then we want to leave in the instruction
     which sets the low-order bit of ra. If it is not, then we turn this
     instruction into a no-op. */
  if (!save) {
      copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  } 
  offset = (int *)pessimistic_profile_gp_patch-(int *)pessimistic_stub;
  patch_gpdisp((int *)pessimistic_profile_gp_patch, (int *)clone+offset);
#endif CALL_GRAPH

  offset = (int *)pessimistic_guard_gp_patch-(int *)pessimistic_stub;
  patch_gpdisp((int *)pessimistic_guard_gp_patch, (int *)clone+offset);

  offset = (int *)pessimistic_handler_gp_patch-(int *)pessimistic_stub;
  patch_gpdisp((int *)pessimistic_handler_gp_patch, (int *)clone+offset);

  if(!res) {
    offset = (int *)pessimistic_jmp_no_handler_1-(int *)pessimistic_stub;
    set_br_disp((func *)((int *)clone+offset), 
	(int *)pessimistic_return - (int *)pessimistic_jmp_no_handler_1 - 1);
    offset = (int *)pessimistic_jmp_no_handler_2-(int *)pessimistic_stub;
    set_br_disp((func *)((int *)clone+offset), 
	(int *)pessimistic_return - (int *)pessimistic_jmp_no_handler_2 - 1);
  }

  if(save) {
    if(n_args < 5) {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args + 1);
    } else if(n_args < 6) {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 0, REG_SP);
    } else {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 
	       8 * (n_args - 6), REG_SP);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 
	       8 * (n_args - 5), REG_SP);
    }
  }
  
  imb();
  /* printf("CloneDefaultStub returning: 0x%lx\n", clone);*/
  return clone;
}

#define REGISTERS_SAVE_FRAME_1	96
#define GUARD_RESULTS_FRAME_1	0
#define ARGUMENTS_FRAME_1	(MaxNumOfArgumentsForDebug + 1 - 4) * WORD_SIZE
#define PESSIMISTIC_1_STUB_FRAME_SIZE	(REGISTERS_SAVE_FRAME_1 + \
					 GUARD_RESULTS_FRAME_1 + \
					 ARGUMENTS_FRAME_1)
	       
func *CloneDefaultStub_new(void *desc, int n_args, int save)
{
  int offset;
  func *clone;

  if(stitch_debug) {
    printf("CloneDefaultStub: 0x%lx %d\n", desc, n_args);
  }

  frame_size = PESSIMISTIC_1_STUB_FRAME_SIZE;
  registers_save_frame = REGISTERS_SAVE_FRAME_1;
  arguments_frame = ARGUMENTS_FRAME_1;
  guard_results_frame = GUARD_RESULTS_FRAME_1;

  clone = clone_stub(pessimistic_1_stub, pessimistic_1_stub_end);
#ifdef SHORT
  patch_arg(get_gp(pessimistic_1_stub), clone, pessimistic_1_stub,
	    pessimistic_1_handlers_gl_h, pessimistic_1_handlers_gl_l,
	    (long)desc);

  patch_arg(get_gp(pessimistic_1_stub), clone, pessimistic_1_stub,
	    pessimistic_1_handlers_hl_h, pessimistic_1_handlers_hl_l,
	    (long)desc);
#endif SHORT
#ifdef LONG
  offset = (int *)pessimistic_1_handlers_gl_long-(int *)pessimistic_1_stub;
  patch_long((int *)clone+offset, (long)desc);
  offset = (int *)pessimistic_1_handlers_hl_long-(int *)pessimistic_1_stub;
  patch_long((int *)clone+offset, (long)desc);
#endif LONG

  offset = (int *)pessimistic_1_gp_patch-(int *)pessimistic_1_stub;
  patch_gpdisp((int *)pessimistic_1_gp_patch, (int *)clone+offset);

#ifdef CALL_GRAPH
  /* If this is a saveregs event then we want to leave in the instruction
     which sets the low-order bit of ra. If it is not, then we turn this
     instruction into a no-op. */
  if (!save) {
      copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  }
  offset = (int *)pessimistic_1_profile_gp_patch-(int *)pessimistic_1_stub;
  patch_gpdisp((int *)pessimistic_1_profile_gp_patch, (int *)clone+offset);
#endif CALL_GRAPH

  offset = (int *)pessimistic_1_guard_gp_patch-(int *)pessimistic_1_stub;
  patch_gpdisp((int *)pessimistic_1_guard_gp_patch, (int *)clone+offset);

  offset = (int *)pessimistic_1_handler_gp_patch-(int *)pessimistic_1_stub;
  patch_gpdisp((int *)pessimistic_1_handler_gp_patch, (int *)clone+offset);


  if(save) {
    if(n_args < 5) {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args + 1);
    } else if(n_args < 6) {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      set_reg_c((func *)(((int *)clone) + offset + 1),
		REG_A0 + n_args);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 0, REG_SP);
    } else {
      offset = (int *)pessimistic_pass_callee_saved_1-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 
	       8 * (n_args - 6), REG_SP);
      offset = (int *)pessimistic_pass_callee_saved_2-(int *)pessimistic_stub;
      copy_ldq((func *)(((int *)clone) + offset + 1), REG_T2, 
	       8 * (n_args - 5), REG_SP);
    }
  }
  
  imb();
  return clone;
}

/******************************************************************************
 *
 *
 *
 *****************************************************************************/

int *clone_guard_arguments = NULL;
int *clone_guard_true = NULL;
int *clone_gl_loop_head = NULL;
int *clone_gl_loop_tail = NULL;
int *clone_handler_loop = NULL;
int *clone_exec_handlers = NULL;
int *clone_no_handler = NULL;
int *clone_return = NULL;
int *clone_handler_arguments = NULL;
int *clone_call_guard = NULL;
int *clone_call_handler = NULL;
int *clone_hl_loop_head = NULL;
int *clone_guard_closure = NULL;
int *clone_handler_closure = NULL;
int **clone_imposed_skip;

func *CloneStub(void *desc, int n_args, int res, int n_handlers, int save)
{
  func *clone;
  int size;

  if(stitch_debug) {
    printf("CloneStub: 0x%lx %d %d %d\n", desc, n_args, n_handlers, save);
  }

  clone_init(n_args, n_handlers, 0);
  size = clone_size(n_args, res, n_handlers, save);

  if(stitch_debug) {
    printf("CloneStub: size %d\n", size);
  }

  if(( clone = (func *)disp_malloc(size)) == NULL ) {
    printf("stitcher: no memory for a stub\n");
    return NULL;
  }

  if(clone_clone(clone, size, desc, n_args, res, n_handlers, save)) {
    return clone;
  } else {
    return NULL;
  }
}

extern int **GPA_ptr;

void **XXX_proc = NULL;
void **XXX_ptr  = NULL;
int    XXX_cnt  = 0;

void XXX_add(void *proc, void *ptr)
{
  if(XXX_ptr == NULL) {
    XXX_proc = (void **)spin_malloc(100 * sizeof(void *));
    XXX_ptr  = (void **)spin_malloc(100 * sizeof(void *));
  }

  /* printf("XXX_add %d 0x%lx 0x%lx\n", XXX_cnt, proc, ptr);*/

  XXX_proc[XXX_cnt] = proc;
  XXX_ptr[XXX_cnt] = ptr;
  XXX_cnt++;
}

func *CloneUnrolledStub(void *desc, dispatch_desc_t *dispatch_list,
			int n_args, int res, int n_handlers, int save, 
			int do_guards, void **guard_list, int guard_cnt)
{
  func *clone;
  int size, init_size;
  long gp;
  int imp_cnt;
  int i;

  if(stitch_debug) {
    printf("CloneUnrolledStub\n");
  }

  /* FIXME: cannot optimize guards if more than 64 handlers present */
  if(do_guards && n_handlers > 64) {
    printf("ERROR >> cannot optimize guards for more than 64 handlers\n");
    return NULL;
  }

  /* there is nothing to optimize if there is no handlers */
  if(n_handlers == 0) {
    do_guards = FALSE;
  }

  clone_init(n_args, n_handlers, 0);

  /* conservative estimate */
  init_size = clone_size_1(dispatch_list, NULL, &imp_cnt, 
			   n_args, res, n_handlers, save, NULL, do_guards);
  
  if(( clone = (func *)disp_malloc(init_size)) == NULL ) {
    printf("ERROR >> stitcher: no memory for a stub\n");
    return NULL;
  }

  gp = (long)clone;

  /* exact size */
  size = clone_size_1(dispatch_list, clone, &imp_cnt, 
		      n_args, res, n_handlers, save, gp, do_guards);
  if(size > init_size) {
    printf("ERROR >> size of the stub underestimated\n");
    return NULL;
  }

  /* reset the list of procedures */
  XXX_proc = XXX_ptr = NULL;
  XXX_cnt = 0;

  if(clone_clone_1(clone, size, desc, dispatch_list, 
		   n_args, res, n_handlers, save, gp, imp_cnt, do_guards))
  {
    /* AsmPrint(clone, size/sizeof(int), "### UNROLLED ###"); */
    if(do_guards) {
      if(XXX_proc == NULL) {
	printf("ERROR >> optimizer did not find guards when expected\n");
	return NULL;
      }
      if(1 + 3 * guard_cnt != XXX_cnt) {
	printf("ERROR >> mismatched number of guards: %d %d\n",
	       guard_cnt, XXX_cnt);
	return NULL;
      }
      for(i = 0; i < XXX_cnt; i++) {
	guard_list[i] = XXX_proc[i];
      }
    }
    return clone;
  } else {
    return NULL;
  }
}

func *CloneFromList(guardDescT *guardDesc, int cnt, int n_args, int res, int save)
{
  func *clone;
  int size, init_size;
  long gp;
  int imp_cnt;

  if(stitch_debug) {
    printf("CloneFromList\n");
  }

  /* do not do it for more than 64 guards */
  clone_init(n_args, 0, 0);

  /* conservative estimate */
  init_size = clone_size_from_list(guardDesc, cnt, NULL, n_args, res, save, NULL);
  
  if(( clone = (func *)disp_malloc(init_size)) == NULL ) {
    printf("ERROR >> stitcher: no memory for a stub\n");
    return NULL;
  }

  gp = (long)clone;

  /* exact size */
  size = clone_size_from_list(guardDesc, cnt, clone, n_args, res, save, gp);
  if(size > init_size) {
    printf("ERROR >> size of the stub underestimated\n");
    return NULL;
  }

  if(clone_clone_from_list(clone, size, guardDesc, cnt, n_args, res, save, gp))
  {
    /* AsmPrint(clone, size/sizeof(int), "### UNROLLED ###"); */
    /* printf("CloneFromList: 0x%lx\n", clone);*/
    return clone;
  } else {
    /* printf("CloneFromList: NULL\n");*/
    return NULL;
  }
}

void PatchCall(int *ptr, void *proc)
{
 patch_long(ptr, (long )proc);
}

extern void *AsmClone();

func *CloneInlinedStub(void *desc, dispatch_desc_t *dispatch_list,
		       int n_args, int res, int n_handlers, 
		       int save, int do_inline, int trace)
{
  func *clone, * xxxx;
  int size, init_size, fsize;
  long gp;
  int imp_cnt;

  if(stitch_debug) {
    printf("CloneInlinedStub\n");
  }

  clone_init(n_args, n_handlers, trace);
  
  /* conservative estimate */
  fsize = 0;
  init_size = clone_size_2(dispatch_list, NULL, &imp_cnt, n_args, res, 
			   n_handlers, save, do_inline, NULL, &fsize, trace);
    
  if(( clone = (func *)disp_malloc(init_size)) == NULL ) {
    printf("stitcher: no memory for a stub\n");
    return NULL;
  }

  gp = (long)clone;

  /* exact size */
  size = clone_size_2(dispatch_list, clone, &imp_cnt, n_args, res, 
		      n_handlers, save, do_inline, gp, &fsize, trace);
  if(size > init_size) {
    printf("ERROR >> size of the stub underestimated %d %d\n", 
	   init_size, size);
    return NULL;
  }

  XXX_proc = XXX_ptr = NULL;
  XXX_cnt = 0;

  if(clone_clone_2(clone, size, desc, dispatch_list, n_args, 
		   res, n_handlers, save, gp, do_inline, imp_cnt, trace)) {
/* #define DO_CLONE*/
#define DO_INLINE
#ifdef DO_INLINE
    if(do_inline && XXX_cnt != 0) {
      xxxx = (func *)AsmInline(clone, (size - fsize)/sizeof(int), 
			       XXX_proc, XXX_ptr, XXX_cnt);
      if(xxxx != NULL) {
	clone = xxxx; 
      }
    }
    if(verbose) {
      AsmPrint(clone, size/sizeof(int), "### INLINED ###");
    }
#else
#ifdef DO_CLONE
    if(do_inline) {
      xxxx = AsmClone(clone, (size - fsize)/sizeof(int), "dispatch");
      if(xxxx != NULL) {
	clone = xxxx; 
      }
    }
#endif
#endif
    return clone;
  } else {
    return NULL;
  }
}





/*
 * count all the important options used in optimizing
 */
count_them(dispatch_desc_t *dispatch_list,
	   int n_handlers,
	   int *n_cl, int *n_can, int *n_nil, int *n_exec, 
	   int *n_imp, int *n_post)
{
  int i, j, n;
  char *invoke;
  handler_desc_t *handlers;
  imposed_desc_t *guards;

  invoke = (char *)(dispatch_list + 1);
  for(i = 0; i < n_handlers; i++) {
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    if(handlers->useGuardClosure) { 
      (*n_cl)++; 
    };
    if(handlers->useHandlerClosure) { 
      (*n_cl)++; 
    };
    if(handlers->cancel) { (*n_can)++; };
    if(handlers->guard == NULL) { (*n_nil)++; };
    if(handlers->guard == NULL && handlers->nImposed == 0){ (*n_exec)++; };
    if(handlers->nImposed) {
      (*n_imp) += handlers->nImposed;
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);
	if(guards->useGuardClosure) { (*n_cl)++; };
	if(guards->postGuard != NULL) { (*n_post)++; }
      }
    }
  }
}

int min_disp_bytes(long a, long b)
{
  long diff = (a > b) ? a - b : b - a;

  /* printf(">>> min_disp_bytes: 0x%lx 0x%lx 0x%lx\n", a, b, diff);*/

  if((diff & ~0x0ffff) == 0) { return 1; }
  if((diff & ~0x0ffffffff) == 0) { return 2; }
  if((diff & ~0x0ffffffffffff) == 0) { return 3; }
  if((diff & 0x8000000000000000) != 0) { 
    printf("ERROR >> arithmetic error inside RCG: 0x%lx 0x%lx 0x%lx\n",
	   a, b, diff);
    return -1; 
  }
  return 4;
}

void patch_short(int *inst, long val, long base)
{
  int disp, off_1, off_2;

  disp = val - base;
  off_1 = disp >> 16;
  off_2 = (((int)(disp - (off_1 << 16))) << 16) >> 16;
  if(off_2 < 0) {
    off_1 += 1;
  }
  set_disp((func *)inst,     off_1);
  set_disp((func *)(inst+1), off_2);
}

void patch_short_2(int *inst, long val, long base)
{
  int disp, off_1, off_2;

  disp = val - base;
  off_1 = disp >> 16;
  off_2 = (((int)(disp - (off_1 << 16))) << 16) >> 16;
  if(off_2 < 0) {
    off_1 += 1;
  }
  if(off_1 != 0) {
    printf("ERROR >> internal RCG erreo (in patch_short_2)\n");
  }
  set_disp((func *)inst,     off_2);
}

int get_load_size(long val, long base)
{
  int n = min_disp_bytes(val, base);
  if(base != 0 && (n == 2 || n == 1)) {
    return get_snippet_size(snippet_load_short, snippet_load_short_end);
  } else {
    return get_snippet_size(snippet_load_long, snippet_load_long_end);
  }
}

int *add_load(int *ptr, int reg, long val, long base)
{
  int *new_ptr;
  int  n;
  
  n = min_disp_bytes(val, base);

  if(n == 1) {
    /* printf("<ONE>");*/
  }
  if(n == 2 || n == 1) {
    new_ptr = add_snippet(ptr, snippet_load_short, snippet_load_short_end);
    patch_short(ptr, val, base);
    set_reg_a((func *)(ptr+1), reg);
  } else {
    /* printf("<FOUR>");*/
    new_ptr = add_snippet(ptr, snippet_load_long, snippet_load_long_end);
    patch_long(ptr, val);
    set_reg_a((func *)(ptr+4), reg);
  }
  return new_ptr;
}

int get_call_size(int n_args, long proc, int useClosure, long closure, 
		  long gp, int save, int trace)
{
  int size = 0;
  size += get_args_size_1(n_args, useClosure, closure, save, gp);
  size += get_load_size(proc, gp);
  if(trace) {
    size += get_snippet_size(snippet_trace_capture,
			     snippet_trace_capture_end);
    size += get_snippet_size(snippet_trace_increment,
			     snippet_trace_increment_end);
  }
  size += get_snippet_size(snippet_call_dir, snippet_call_dir_end);
  if(trace) {
    size += get_snippet_size(snippet_trace_capture,
			     snippet_trace_capture_end);
  }
  return size;
}

int clone_size_2 (dispatch_desc_t *dispatch_list, 
		  func *clone, int *imp_guard_cnt,
		  int n_args, int res, int n_handlers, int save, 
		  int do_inline, long gp, int *fsize, int trace)
{
  int size = 0;
  int i, j, na, ns, use_cl;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0, n_post = 0;
  char *invoke;
  handler_desc_t *handlers;
  imposed_desc_t *guards;
  int imp_used, imp_cnt;
  imposed_desc_t *post;

  count_them(dispatch_list,n_handlers, &n_cl, &n_can, &n_nil,
	     &n_exec, &n_imp, &n_post);
  use_cl = (n_cl != 0);

  /* number of callee-saved registers used to keep args */
  na = n_args + use_cl;
  if(res) {
    ns = (na >= 5) ? 5 : na;
  } else {
    ns = (na >= 6) ? 6 : na;
  }

  if(save) {
    size += get_snippet_size(snippet_prolog, snippet_prolog_end);
  } else {
    size += get_snippet_size(snippet_prolog_1, snippet_prolog_1_end);
  }

  if(trace) {
    size += get_snippet_size(snippet_trace_enter, snippet_trace_enter_end);
  }

  /*  if(do_inline) {*/
  /*
  } else {
    size += get_snippet_size(snippet_prolog_2, snippet_prolog_2_end);
  }
  */

  if(save) {
    size += get_snippet_size(snippet_save_callee, snippet_save_callee_end);
  } else {
    size += get_part_snippet_size(snippet_save_callee, 0, ns-1);
    if(res) {
      size += get_part_snippet_size(snippet_save_callee, 5, 6);
    }
  }

  /* printf("s> %d ", size);*/
  if(save) {
    size += get_snippet_size(snippet_save_regs, snippet_save_regs_end);
  } else {
    size += get_part_snippet_size(snippet_save_regs, 0, (na >= 6) ? 5 : na-1);
  }

  if(res) {
    size += get_snippet_size(snippet_start_up_unrolled_2, 
			     snippet_start_up_unrolled_2_end);
  }

  /* the guard and handler loop */
  invoke = (char *)(dispatch_list + 1);
  imp_cnt = 0;
  for(i = 0; i < n_handlers; i++) {
    post = NULL;
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    /* first the imposed guards */
    imp_used = 0;
    if(handlers->nImposed) {
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);
	size += get_call_size(n_args, guards->guard, guards->useGuardClosure, 
			      guards->guardClosure, gp, FALSE, trace);
	size += get_snippet_size(snippet_beq_v0, snippet_beq_v0_end);
	imp_used = 1;
	post = guards;
      }
    }

    /* printf("b %d ", size);*/

    /* then the guard proper */
    if(handlers->guard != NULL) {
      size += get_call_size(n_args, handlers->guard, 
			    handlers->useGuardClosure, handlers->guardClosure,
			    gp, FALSE, trace);
      size += get_snippet_size(snippet_beq_v0, snippet_beq_v0_end);
      imp_used = 1;
    } 

    /* printf("c %d ", size);*/

    /* lastly the handler */
    size += get_call_size(n_args, handlers->handler, 
			  handlers->useHandlerClosure, 
			  handlers->handlerClosure, gp, save, trace);
    if(res) {
      size += get_snippet_size(snippet_save_result,
			       snippet_save_result_end);
    }

    /* and the post-guard if necessary */
    if(post && post->postGuard) {
      size += get_call_size(n_args, post->postGuard,
			    post->usePostGuardClosure, 
			    post->postGuardClosure, gp, save, trace);
    }

    if(dispatch_list->resultHandler != NULL) {
      size += get_call_size(0, dispatch_list->resultHandler, 
			    0, 0, gp, 0, trace);
    }

    if(handlers->cancel) {
      size += get_snippet_size(snippet_branch_fwd, snippet_branch_fwd_end);
    }

    if(imp_used) {
      imp_cnt++;
    }
    /* printf("d %d ", size);*/
  }
  /* printf("e %d ", size);*/

  /* check whether any handler was executed */
  if(res && n_exec == 0) {
    size += get_snippet_size(snippet_check_handlers,
			     snippet_check_handlers_end);
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      size += get_snippet_size(snippet_restore, snippet_restore_end);
    } else {
      if(res) {
	size += get_part_snippet_size(snippet_restore, 0, ns);
	size += get_part_snippet_size(snippet_restore, 6, 9);
      } else {
	size += get_part_snippet_size(snippet_restore, 1, ns);
	size += get_part_snippet_size(snippet_restore, 8, 9);
      }
    }
  } else {
    size += get_snippet_size(snippet_restore_mh, snippet_restore_mh_end);
  }

  /* printf("f %d ", size);*/

  if(trace) {
    size += get_snippet_size(snippet_trace_exit_1, snippet_trace_exit_1_end);
    size += get_snippet_size(snippet_trace_capture, snippet_trace_capture_end);
    size += get_snippet_size(snippet_trace_exit_2, snippet_trace_exit_2_end);
  }

  size += get_snippet_size(snippet_return, snippet_return_end);

  /* printf("g %d ", size);*/
  if(res && n_exec == 0) {
    *fsize = get_snippet_size(snippet_no_handler_gp,snippet_no_handler_gp_end);
    size += *fsize;
  }

  /* printf("h %d ", size);*/

  *imp_guard_cnt = imp_cnt;
  return size;
}

int *add_trace_capture(int *ptr, int offset)
{
  int *new_ptr = ptr;
  new_ptr = add_snippet(ptr, snippet_trace_capture,
			snippet_trace_capture_end);
  set_disp((func *)(ptr+2),  frame_size + get_disp((func *)(ptr+2)));
  set_disp((func *)(ptr+4),  frame_size + get_disp((func *)(ptr+4)) + offset);
  set_disp((func *)(ptr+6),  frame_size + get_disp((func *)(ptr+6)));
  set_disp((func *)(ptr+12), frame_size + get_disp((func *)(ptr+12)) + offset);
  return new_ptr;
}

int *add_trace_increment(int *ptr, int offset)
{
  int *new_ptr = ptr;
  new_ptr = add_snippet(ptr, snippet_trace_increment,
			snippet_trace_increment_end);
  set_disp((func *)(ptr), frame_size + get_disp((func *)(ptr))+offset+8);
  set_disp((func *)(ptr+2), frame_size + get_disp((func *)(ptr+2))+offset+8);
  return new_ptr;
}

int n_install = 0;

void trace_counts_add();

#define TOTAL_OFF    (0)
#define IMPOSED_OFF  (-16)
#define GUARD_OFF    (-32)
#define HANDLER_OFF  (-48)

int clone_clone_2 (func *clone, int size, void *desc, 
		   dispatch_desc_t *dispatch_list,
		   int n_args, int res, int n_handlers, int save, long gp,
		   int do_inline, int imp_guard_cnt, int trace)
{
  int i, j, k, na, ns, offset, n, use_cl;
  int *ptr, *new_ptr, *old_ptr;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0, n_post = 0;
  char *invoke;
  handler_desc_t *handlers;
  imposed_desc_t *guards;
  int imp_used, imp_cnt;
  void *new;
  imposed_desc_t *post;

  if(imp_guard_cnt != 0) {
    clone_imposed_skip = (int **)spin_malloc(imp_guard_cnt * sizeof(int *));
  } else {
    clone_imposed_skip = NULL;
  }
  for(i=0; i<imp_guard_cnt; i++) {
    clone_imposed_skip[i] = NULL;
  }

  ptr = (int *)clone;

  n_install++;
  count_them(dispatch_list,n_handlers, &n_cl, &n_can, &n_nil,
	     &n_exec, &n_imp, &n_post);
  use_cl = (n_cl != 0);

  /* number of callee-saved registers used to keep args */
  /* need one to know whether any handler was invoked */
  /*      one for the result */
  na = n_args + use_cl;
  if(res) {
    ns = (na >= 5) ? 5 : na;
  } else {
    ns = (na >= 6) ? 6 : na;
  }
/*
  if(res) {
    ns = (na >= 6) ? 6 : na;
  } else {
    ns = (na >= 7) ? 7 : na;
  }
*/

  /* prolog */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_prolog, snippet_prolog_end);
    patch_short(ptr, (long)gp, (long)clone);

#ifndef CALL_GRAPH
    set_disp((func *)(ptr+3), -frame_size);
    set_disp((func *)(ptr+4), arguments_frame + guard_results_frame + 0);
#else
    patch_long(((int *)clone + 3), (long)_mcount);

    offset = (int *)snippet_profile_gp_patch-(int *)snippet_prolog;
    patch_short(ptr + offset, (long)clone, (long)(ptr + offset));
    set_disp((func *)(ptr+12), -frame_size);
    set_disp((func *)(ptr+13), arguments_frame + guard_results_frame + 0);
#endif CALL_GRAPH
  } else {
    new_ptr = add_snippet(ptr, snippet_prolog_1, snippet_prolog_1_end);
    patch_short(ptr, (long)gp, (long)clone);

#ifndef CALL_GRAPH
    set_disp((func *)(ptr+2), -frame_size);
    set_disp((func *)(ptr+3), arguments_frame + guard_results_frame + 0);
#else
    copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
    patch_long(((int *)clone + 3), (long)_mcount);

    offset = (int *)snippet_profile_gp_patch_1-(int *)snippet_prolog_1;
    patch_short(ptr + offset, (long)clone, (long)(ptr + offset));
    set_disp((func *)(ptr+11), -frame_size);
    set_disp((func *)(ptr+12), arguments_frame + guard_results_frame + 0);
#endif CALL_GRAPH
  }
  ptr = new_ptr;

  if(trace) {
    new_ptr = add_snippet(ptr, snippet_trace_enter, snippet_trace_enter_end);
    set_disp((func *)(ptr),   frame_size + get_disp((func *)(ptr)));
    set_disp((func *)(ptr+1), frame_size + get_disp((func *)(ptr+1)));
    set_disp((func *)(ptr+2), frame_size + get_disp((func *)(ptr+2)));
    set_disp((func *)(ptr+3), frame_size + get_disp((func *)(ptr+3)));
    set_disp((func *)(ptr+4), frame_size + get_disp((func *)(ptr+4)));
    set_disp((func *)(ptr+5), frame_size + get_disp((func *)(ptr+5)));
    set_disp((func *)(ptr+6), frame_size + get_disp((func *)(ptr+6)));
    set_disp((func *)(ptr+9), frame_size + get_disp((func *)(ptr+9)));
    ptr = new_ptr;
  }

  /* save callee-saved registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_callee, snippet_save_callee_end);
    for(i = 0; i <= 6; i++) {
      set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
    }
    set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+8);
  } else {
    new_ptr = add_part_snippet(ptr, snippet_save_callee, 0, ns-1);
    if(res) {
      new_ptr = add_part_snippet(new_ptr, snippet_save_callee, 5, 6);
      for(i = 0; i <= ns+2; i++) {
	set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
      }
    } else {
      for(i = 0; i <= ns; i++) {
	set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
      }
    }
  }
  ptr = new_ptr;
  /* printf("c> %d ", (char *)ptr - (char *)clone);*/
  /* save arguments registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_regs, snippet_save_regs_end); 
  } else {
    new_ptr = add_part_snippet(ptr, snippet_save_regs, 0,(na >= 6) ? 5 : na-1);
  }
  if(save || na > 4) {
    set_disp((func *)(ptr + 4), arguments_frame+guard_results_frame+16);
  }
  if(save || na > 5) {
    set_disp((func *)(ptr + 5), arguments_frame+guard_results_frame+24);
  }
  ptr = new_ptr;

  /* start-up code */
  if(res) {
    ptr = add_snippet(ptr, snippet_start_up_unrolled_2,
		      snippet_start_up_unrolled_2_end);
  }

  invoke = (char *)(dispatch_list + 1);
  imp_cnt = 0;
  for(i = 0; i < n_handlers; i++) {
    /* printf("a %d ", (char *)ptr - (char *)clone);*/
    post = NULL;
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    if(verbose) {
      printf("  handler    : 0x%lx\n", handlers->handler);
      printf("    guard    : 0x%lx\n", handlers->guard);
      printf("    nImposed : %d\n",    handlers->nImposed);
      printf("    cancel   : %d\n",    handlers->cancel);
      printf("    gclosure : %d\n",    handlers->useGuardClosure);
      printf("    hclosure : %d\n",    handlers->useHandlerClosure);
    }

    imp_used = 0;
    if(handlers->nImposed) {
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);
	ptr = copy_args_1(ptr, n_args, guards->useGuardClosure,
			  guards->guardClosure, FALSE, gp);

#ifdef DO_CLONE
	if(do_inline && (new = AsmClone(guards->guard, 0, "imp_guard"))) {
	  ptr = add_load(ptr, REG_T12, (long)new, gp);
	} else {
	  ptr = add_load(ptr, REG_T12, guards->guard, gp);
	}
#else
	ptr = add_load(ptr, REG_T12, guards->guard, gp);
#endif

	if(trace) {
	  ptr = add_trace_capture(ptr, TOTAL_OFF);
	  ptr = add_trace_increment(ptr, IMPOSED_OFF);
	}

	if(verbose) {
	  printf("  imposed: 0x%lx 0x%lx\n", guards->guard, ptr);
	}

	if(do_inline) {
	  XXX_add((void *)guards->guard, ptr);
	}
	
	/* do the jump */
	new_ptr = add_snippet(ptr, snippet_call_dir, snippet_call_dir_end);
	patch_short(ptr+1, (long)gp, (long)(ptr+1));
	ptr = new_ptr;

	/* trace */
	if(trace) {
	  ptr = add_trace_capture(ptr, IMPOSED_OFF);
	}

	/* branch on the result */
	new_ptr = add_snippet(ptr, snippet_beq_v0, snippet_beq_v0_end);
	if(!patch_branch(ptr, &clone_imposed_skip[imp_cnt])) { return 0; }
	ptr = new_ptr;

	imp_used = 1;
	post = guards;
      }
    }
    
    /* printf("b %d ", (char *)ptr - (char *)clone);*/

    if(handlers->guard != NULL) {
      ptr = copy_args_1(ptr, n_args, handlers->useGuardClosure,
			handlers->guardClosure, FALSE, gp);

#ifdef DO_CLONE
      if(do_inline && (new = AsmClone(handlers->guard, 0, "guard"))) {
	ptr = add_load(ptr, REG_T12, (long)new, gp);
      } else {
	ptr = add_load(ptr, REG_T12, handlers->guard, gp);
      }
#else
      ptr = add_load(ptr, REG_T12, handlers->guard, gp);
#endif
      
      if(trace) {
	ptr = add_trace_capture(ptr, TOTAL_OFF);
	ptr = add_trace_increment(ptr, GUARD_OFF);
      }

      if(do_inline) {
	XXX_add((void *)handlers->guard, ptr);
      }

      /* do the jump */
      new_ptr = add_snippet(ptr, snippet_call_dir, snippet_call_dir_end);
      patch_short(ptr+1, (long)gp, (long)(ptr+1));
      ptr = new_ptr;
      
      /* trace */
      if(trace) {
	ptr = add_trace_capture(ptr, GUARD_OFF);
      }

      /* branch on the result */
      new_ptr = add_snippet(ptr, snippet_beq_v0, snippet_beq_v0_end);
      if(!patch_branch(ptr, &clone_imposed_skip[imp_cnt])) { return 0; }
      ptr = new_ptr;

      imp_used = 1;
    } 

    /* printf("c %d ", (char *)ptr - (char *)clone);*/

    ptr = copy_args_1(ptr, n_args, handlers->useHandlerClosure, 
		      handlers->handlerClosure, save, gp);
#ifdef DO_CLONE
    if(do_inline && (new = AsmClone(handlers->handler, 0, "handler"))) {
      ptr = add_load(ptr, REG_T12, (long)new, gp);
    } else {
      ptr = add_load(ptr, REG_T12, handlers->handler, gp);
    }
#else
    ptr = add_load(ptr, REG_T12, handlers->handler, gp);
#endif

    if(trace) {
      ptr = add_trace_capture(ptr, TOTAL_OFF);
      ptr = add_trace_increment(ptr, HANDLER_OFF);
    }

    if(do_inline) {
      XXX_add((void *)handlers->handler, ptr);
    }

    /* do the jump */
    new_ptr = add_snippet(ptr, snippet_call_dir, snippet_call_dir_end);
    patch_short(ptr+1, (long)gp, (long)(ptr+1));
    ptr = new_ptr;
    
    /* trace */
    if(trace) {
      ptr = add_trace_capture(ptr, HANDLER_OFF);
    }
      
    if(res) {
      new_ptr = add_snippet(ptr, snippet_save_result, snippet_save_result_end);
      ptr = new_ptr;
    }

    /* and the post-guard if necessary */
    if(post && post->postGuard) {
      ptr = copy_args_1(ptr, n_args, post->usePostGuardClosure, 
			post->postGuardClosure, save, gp);
      ptr = add_load(ptr, REG_T12, post->postGuard, gp);

      /* do the jump */
      new_ptr = add_snippet(ptr, snippet_call_dir, snippet_call_dir_end);
      patch_short(ptr+1, (long)gp, (long)(ptr+1));
      ptr = new_ptr;
    }

    if(dispatch_list->resultHandler != NULL) {
      ptr = add_load(ptr, REG_T12, dispatch_list->resultHandler, gp);
      if(trace) {
	ptr = add_trace_capture(ptr, TOTAL_OFF);
	ptr = add_trace_increment(ptr, HANDLER_OFF);
      }
      if(do_inline) {
	XXX_add((void *)dispatch_list->resultHandler, ptr);
      }
      new_ptr = add_snippet(ptr, snippet_call_dir, snippet_call_dir_end);
      patch_short(ptr+1, (long)gp, (long)(ptr+1));
      ptr = new_ptr;
      if(trace) {
	ptr = add_trace_capture(ptr, HANDLER_OFF);
      }
    }

    if(handlers->cancel) {
      new_ptr = add_snippet(ptr, snippet_branch_fwd, snippet_branch_fwd_end);
      if(!patch_branch(ptr, &clone_return)) { return 0; }
      ptr = new_ptr;
    }

    if(imp_used) {
      clone_imposed_skip[imp_cnt] = ptr;
      imp_cnt++;
    }
    /* printf("d %d ", (char *)ptr - (char *)clone);*/
  }

  /* printf("e %d ", (char *)ptr - (char *)clone);*/
  /* check whether any handler was executed */
  if(res && n_exec == 0) {
    new_ptr = add_snippet(ptr,snippet_check_handlers,
			  snippet_check_handlers_end);
    if(!patch_branch(ptr+1, &clone_no_handler)) { return 0; }
    ptr = new_ptr;
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      new_ptr = add_snippet(ptr, snippet_restore, snippet_restore_end);
      k = 1;
      n = 7;
    } else {
      if(res) {
	new_ptr = add_part_snippet(ptr, snippet_restore, 0, ns);
	new_ptr = add_part_snippet(new_ptr, snippet_restore, 6, 9);
	k = 1;
	n = ns+2;
      } else {
	new_ptr = add_part_snippet(ptr, snippet_restore, 1, ns);
	new_ptr = add_part_snippet(new_ptr, snippet_restore, 8, 9);
	k = 0;
	n = ns;
      }
    }
  } else {
    new_ptr = add_snippet(ptr, snippet_restore_mh, snippet_restore_mh_end);
    k = 1;
    n = 7;
  }
  for(i = 0; i < n; i++) {
    set_disp((func *)(ptr+k+i), arguments_frame+guard_results_frame+32+i*8);
  }

  set_disp((func *)(ptr + n+1+k-1), arguments_frame+guard_results_frame+0);
  set_disp((func *)(ptr + n+2+k-1), frame_size);
  clone_return = ptr;
  ptr = new_ptr;

  /* printf("f %d ", (char *)ptr - (char *)clone);*/

  if(trace) {
    new_ptr = add_snippet(ptr, snippet_trace_exit_1, snippet_trace_exit_1_end);
    set_disp((func *)(ptr), -frame_size);
    set_disp((func *)(ptr+1),  frame_size + get_disp((func *)(ptr+1)));
    ptr = new_ptr;
    ptr = add_trace_capture(ptr, TOTAL_OFF);
    new_ptr = add_snippet(ptr, snippet_trace_exit_2, snippet_trace_exit_2_end);
    set_disp((func *)(ptr+0), frame_size + get_disp((func *)(ptr+0)));
    set_disp((func *)(ptr+1), frame_size + get_disp((func *)(ptr+1)));
    set_disp((func *)(ptr+2), frame_size + get_disp((func *)(ptr+2)));
    set_disp((func *)(ptr+3), frame_size + get_disp((func *)(ptr+3)));
    set_disp((func *)(ptr+4), frame_size + get_disp((func *)(ptr+4)));
    set_disp((func *)(ptr+5), frame_size + get_disp((func *)(ptr+5)));
    set_disp((func *)(ptr+6), frame_size + get_disp((func *)(ptr+6)));
    set_disp((func *)(ptr+7), frame_size + get_disp((func *)(ptr+7)));
    patch_long(((int *)ptr+11), (long)trace_counts_add);
    set_disp((func *)(ptr+18), frame_size + get_disp((func *)(ptr+18)));
    set_disp((func *)(ptr+19), arguments_frame + guard_results_frame + 0);
    set_disp((func *)(ptr+20), frame_size);
    ptr = new_ptr;
  }

  ptr = add_snippet(ptr, snippet_return, snippet_return_end);

  /* printf("g %d ", (char *)ptr - (char *)clone);*/
  /* no handler invoked */
  if(res && n_exec == 0) {
    new_ptr = add_snippet(ptr, snippet_no_handler_gp, 
			  snippet_no_handler_gp_end);
    patch_gpdisp(((int *)(snippet_no_handler_gp))+1, ptr+1);
    clone_no_handler = ptr;
    ptr = new_ptr;
  }

  /* printf("h %d\n", (char *)ptr - (char *)clone);*/

  /* patch delayed branches */
  if(!patch_delayed()) { return FALSE; }

  if((char *)ptr - (char *)clone != size) {
    printf("ERROR >> clone_clone_2: different size - %d instead of %d\n", 
	   (char *)ptr - (char *)clone, size);
    if(clone_imposed_skip != NULL) { spin_free(clone_imposed_skip); }
    return FALSE;
  }
  
  imb();

  if(clone_imposed_skip != NULL) { spin_free(clone_imposed_skip); }
  return TRUE;
}

int clone_size_1(dispatch_desc_t *dispatch_list, 
		 func *clone, int *imp_guard_cnt, 
		 int n_args, int res, int n_handlers, int save, long gp, 
		 int do_guards)
{
  int size = 0;
  int i, j, na, ns, use_cl;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0, n_post = 0;
  char *invoke;
  handler_desc_t *handlers;
  imposed_desc_t *guards;
  int imp_used, imp_cnt;
  int nil_guards;

  count_them(dispatch_list,n_handlers, &n_cl, &n_can, &n_nil,
	     &n_exec, &n_imp, &n_post);
  use_cl = (n_cl != 0);

  /* number of callee-saved registers used to keep args */
  na = n_args + use_cl;
  ns = (na >= 4) ? 4 : na;

  size += get_snippet_size(snippet_prolog, snippet_prolog_end);

  if(save) {
    size += get_snippet_size(snippet_save_callee, snippet_save_callee_end);
  } else {
    size += get_part_snippet_size(snippet_save_callee, 0, ns-1);
    size += get_part_snippet_size(snippet_save_callee, 5, 6);
  }

  if(save) {
    size += get_snippet_size(snippet_save_regs, snippet_save_regs_end); 
  } else {
    size += get_part_snippet_size(snippet_save_regs, 0, (na >= 6) ? 5 : na-1);
  }
  size += get_snippet_size(snippet_start_up_unrolled, 
			   snippet_start_up_unrolled_end);

  imp_cnt = 0;

  /* the guard loop */
  if(!do_guards) {
    invoke = (char *)(dispatch_list + 1);
    for(i = 0; i < n_handlers; i++) {
      handlers = (handler_desc_t *)invoke;
      invoke += sizeof(handler_desc_t);
      
      imp_used = 0;
      if(handlers->nImposed) {
	for(j = 0; j < handlers->nImposed; j++) {
	  guards = (imposed_desc_t *)invoke;
	  invoke += sizeof(imposed_desc_t);
	  
	  size += get_args_size_1(n_args, guards->useGuardClosure, 
				  guards->guardClosure, FALSE, (long)clone);
	  size += get_load_size(guards->guard, (long)clone);
	  size += get_snippet_size(snippet_call_imposed_dir, 
				   snippet_call_imposed_dir_end);
	  imp_used = 1;
	}
      }

      if(handlers->guard != NULL) {
	size += get_args_size_1(n_args, handlers->useGuardClosure,
				handlers->guardClosure, FALSE, (long)clone);
	size += get_load_size(handlers->guard, (long)clone);
	size += get_snippet_size(snippet_call_guard_dir, 
				 snippet_call_guard_dir_end);
      } else {
	/*size += get_snippet_size(snippet_bis_value, snippet_bis_value_end);*/
	size += get_snippet_size(snippet_shift_value, 
				 snippet_shift_value_end);
      }
      if(handlers->cancel) {
	size += get_snippet_size(snippet_branch_fwd, snippet_branch_fwd_end);
      }
      if(imp_used) {
	imp_cnt++;
      }
    }
  } else {
    /* only one call to the optimized guards */
    /* assume pessimistically that this is the longest load */
    size += get_snippet_size(snippet_load_long, snippet_load_long_end);
    size += get_snippet_size(snippet_call_all_guard, 
			     snippet_call_all_guard_end);

    nil_guards = 0;
    invoke = (char *)(dispatch_list + 1);
    for(i = 0; i < n_handlers; i++) {
      handlers = (handler_desc_t *)invoke;
      invoke += sizeof(handler_desc_t);
      invoke += handlers->nImposed * sizeof(imposed_desc_t);
      if(handlers->guard == NULL) {
	nil_guards++;
      }
    }
    if(nil_guards) {
      size += sizeof(int);
    }
  }

  /* check if any handler called */
  if(res && n_exec == 0) {
    size += get_snippet_size(snippet_handler_loop, 
			     snippet_handler_loop_end);
  }

  /* the handler loop */
  invoke = (char *)(dispatch_list + 1);
  for(i = 0; i < n_handlers; i++) {
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    size += get_snippet_size(snippet_check_call, snippet_check_call_end);
    size += get_args_size_1(n_args, handlers->useHandlerClosure,
			    handlers->handlerClosure, save, (long)clone);
    size += get_load_size(handlers->handler, (long)clone);
    size += get_snippet_size(snippet_call_handler_dir, 
			     snippet_call_handler_dir_end);
    if(handlers->nImposed) {
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);
      }
    }
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      size += get_snippet_size(snippet_epilog, snippet_epilog_end);
    } else {
      size += get_part_snippet_size(snippet_epilog, 0, ns);
      size += get_part_snippet_size(snippet_save_callee, 6, 10);
    }
  } else {
    size += get_snippet_size(snippet_epilog, snippet_epilog_end);
  }

  if(res && n_exec == 0) {
    size += get_snippet_size(snippet_no_handler_gp, snippet_no_handler_gp_end);
  }

  *imp_guard_cnt = imp_cnt;
  return size;
}

int clone_size_from_list(guardDescT *guardDesc, int cnt, func *clone,
			 int n_args, int res, int save, long gp)
{
  int size = 0;
  int n_handlers = 0;
  int i, j, na, ns, use_cl;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0;

  use_cl = 0;

  /* number of callee-saved registers used to keep args */
  na = n_args + use_cl;
  ns = (na >= 4) ? 4 : na;

  size += get_snippet_size(snippet_prolog, snippet_prolog_end);

  if(save) {
    size += get_snippet_size(snippet_save_callee, snippet_save_callee_end);
  } else {
    size += get_part_snippet_size(snippet_save_callee, 0, ns-1);
    size += get_part_snippet_size(snippet_save_callee, 5, 6);
  }

  if(save) {
    size += get_snippet_size(snippet_save_regs, snippet_save_regs_end); 
  } else {
    size += get_part_snippet_size(snippet_save_regs, 0, (na >= 6) ? 5 : na-1);
  }

  size += get_snippet_size(snippet_start_up_unrolled, 
			   snippet_start_up_unrolled_end);

  /* the guard loop */
  for(i = 0; i < cnt; i++) {
    if(guardDesc->guard != NULL) {
      size += get_args_size_1(n_args, guardDesc->useClosure,
			      (long)guardDesc->closure, FALSE, (long)clone);
      size += get_load_size((long)guardDesc->guard, (long)clone);
      size += get_snippet_size(snippet_call_guard_dir, 
			       snippet_call_guard_dir_end);
    } else {
      size += get_snippet_size(snippet_shift_value, 
			       snippet_shift_value_end);    
    }
    guardDesc++;
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      size += get_snippet_size(snippet_epilog_g, snippet_epilog_g_end);
    } else {
      /* FIXME: WHY THE FUCK IS THIS SAVE_CALLEE */
      size += get_part_snippet_size(snippet_epilog_g, 0, ns);
      size += get_part_snippet_size(snippet_save_callee, 6, 10);
    }
  } else {
    /* FIXME */
    size += get_snippet_size(snippet_epilog_mh, snippet_epilog_mh_end);
  }

  return size;
}

int clone_clone_1(func *clone, int size, void *desc, 
		  dispatch_desc_t *dispatch_list,
		  int n_args, int res, int n_handlers, int save, long gp,
		  int imp_guard_cnt, int do_guards)
{
  int i, j, na, ns, offset, n, use_cl;
  int *ptr, *new_ptr, *old_ptr;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0, n_post = 0;
  char *invoke;
  handler_desc_t *handlers;
  imposed_desc_t *guards;
  int imp_used, imp_cnt;
  int nil_guards;

  /* FIXME: cannot optimize imposed guards */
  if(do_guards && imp_guard_cnt != 0) {
    printf("ERROR >> cannot optimize guards if there are imposed guards\n");
    return FALSE;
  }

  if(imp_guard_cnt != 0) {
    clone_imposed_skip = (int **)spin_malloc(imp_guard_cnt * sizeof(int *));
  } else {
    clone_imposed_skip = NULL;
  }
  for(i=0; i<imp_guard_cnt; i++) {
    clone_imposed_skip[i] = NULL;
  }

  ptr = (int *)clone;

  n_install++;
  count_them(dispatch_list,n_handlers, &n_cl, &n_can, &n_nil,
	     &n_exec, &n_imp, &n_post);
  use_cl = (n_cl != 0);
  
  /* number of callee-saved registers used to keep args */
  na = n_args + use_cl;
  ns = (na >= 4) ? 4 : na;

  /* prolog */
  new_ptr = add_snippet(ptr, snippet_prolog, snippet_prolog_end);
  patch_short(ptr, (long)clone, (long)clone);
  /* patch_short(ptr, (long)gp, (long)clone);*/
#ifndef CALL_GRAPH
  set_disp((func *)(ptr+3), -frame_size);
  set_disp((func *)(ptr+4), arguments_frame + guard_results_frame + 0);
#else
  /* Unfortunately, the profiling code must come before the change
     of the stack pointer, which means that the displacements are at
     different offsets in the profiled and unprofiled cases. */
  if (!save) {
    copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  }
  /* Overwrite the constant fields with the address of _mcount, known
     at run time. 2, 3, 5, 6 */
  patch_long(((int *)clone + 3), (long)_mcount);

  offset = (int *)snippet_profile_gp_patch-(int *)snippet_prolog;
  patch_short(ptr + offset, (long)clone, (long)(ptr + offset));
  
  set_disp((func *)(ptr+12), -frame_size);
  set_disp((func *)(ptr+13), arguments_frame + guard_results_frame + 0);
#endif CALL_GRAPH
  ptr = new_ptr;

  /* save callee-saved registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_callee, snippet_save_callee_end);
    for(i = 0; i <= 6; i++) {
      set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
    }
    set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+8);
  } else {
    /* s? for arguments */
    new_ptr = add_part_snippet(ptr, snippet_save_callee, 0, ns-1);

    /* s4, s5, s6 always used */
    new_ptr = add_part_snippet(new_ptr, snippet_save_callee, 5, 6);

    for(i = 0; i <= ns+2; i++) {
      set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
    }
  }
  ptr = new_ptr;

  /* save arguments registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_regs, snippet_save_regs_end); 
  } else {
    new_ptr = add_part_snippet(ptr, snippet_save_regs, 0,(na >= 6) ? 5 : na-1);
  }
  if(save || na > 4) {
    set_disp((func *)(ptr + 4), arguments_frame+guard_results_frame+16);
  }
  if(save || na > 5) {
    set_disp((func *)(ptr + 5), arguments_frame+guard_results_frame+24);
  }
  ptr = new_ptr;

  /* start-up code */
  ptr = add_snippet(ptr, snippet_start_up_unrolled,
		    snippet_start_up_unrolled_end);

  /* generate the code to invoke the collapsed guards */
  /* remember its position */
  if(do_guards) {
    XXX_add(ptr, NULL);
    ptr = add_snippet(ptr, snippet_load_long, snippet_load_long_end);
    new_ptr = add_snippet(ptr, snippet_call_all_guard, 
			  snippet_call_all_guard_end);
    patch_short(ptr+1, (long)clone, (long)(ptr+1));
    ptr = new_ptr;

    nil_guards = 0;
    invoke = (char *)(dispatch_list + 1);
    for(i = 0; i < n_handlers; i++) {
      handlers = (handler_desc_t *)invoke;
      invoke += sizeof(handler_desc_t);
      invoke += handlers->nImposed * sizeof(imposed_desc_t);
      if(handlers->guard == NULL) {
	nil_guards |= 1 << i;
      }
    }
    if(nil_guards) {
      copy_bis_i((func *)ptr, REG_V0, REG_V0, nil_guards);
      ptr++;
    }
  }

  invoke = (char *)(dispatch_list + 1);
  imp_cnt = 0;
  for(i = 0; i < n_handlers; i++) {
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    imp_used = 0;
    if(handlers->nImposed) {
      if(do_guards) {
	printf("ERROR >> no guard optimizations for imposed guards\n");
	return FALSE;
      }
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);

	ptr = copy_args_1(ptr, n_args, guards->useGuardClosure,
			  guards->guardClosure, FALSE, gp);
	ptr = add_load(ptr, REG_T12, guards->guard, (long)clone);
	new_ptr = add_snippet(ptr, snippet_call_imposed_dir,
			      snippet_call_imposed_dir_end);
	/* patch_short(ptr+1, (long)gp, (long)(ptr+1));*/
	patch_short(ptr+1, (long)clone, (long)(ptr+1));
	if(!patch_branch(ptr+3, &clone_imposed_skip[imp_cnt])) { 
	  printf("ERROR >> could not patch a branch\n");
	  return FALSE; 
	}
	imp_used = 1;
	ptr = new_ptr;
      }
    }

    if(!do_guards) {
      if(handlers->guard != NULL) {
	ptr = copy_args_1(ptr, n_args, handlers->useGuardClosure,
			  handlers->guardClosure, FALSE, gp);
	ptr = add_load(ptr, REG_T12, handlers->guard, (long)clone);
	new_ptr = add_snippet(ptr, snippet_call_guard_dir,
			      snippet_call_guard_dir_end);
	/* patch_short(ptr+1, (long)gp, (long)(ptr+1));*/
	patch_short(ptr+1, (long)clone, (long)(ptr+1));
	set_lit((func *)(ptr+5), i);
	/* adjust the branch to skip over one extra instruction */
	if(handlers->cancel) {
	  set_br_disp((func *)(ptr+3), get_br_disp((func *)(ptr+3)) + 1);
	}
	ptr = new_ptr;
      } else {
	/*new_ptr=add_snippet(ptr, snippet_bis_value, snippet_bis_value_end);*/
	new_ptr = add_snippet(ptr, snippet_shift_value, 
			      snippet_shift_value_end);    
	set_lit((func *)(ptr+1), i);
	ptr = new_ptr;
      }
    } else {
      XXX_add((void *)handlers->guard, NULL);
      XXX_add((void *)handlers->guardClosure, NULL);
      XXX_add((void *)handlers->useGuardClosure, NULL);
    }

    if(handlers->cancel) {
      if(do_guards) {
	printf("ERROR >> no guard optimizations for cancelling handlers\n");
	return FALSE;
      }

      /* jump to the handlers if cancels */
      new_ptr = add_snippet(ptr, snippet_branch_fwd, snippet_branch_fwd_end);
      if(!patch_branch(ptr, &clone_exec_handlers)) { 
	printf("ERROR >> could not patch a branch (1)\n");
	return FALSE; 
      }
      ptr = new_ptr;
    }

    if(imp_used) {
      if(imp_cnt == imp_guard_cnt) {
	printf(
	 "ERROR >> sticher underestimated the number of imposed guards\n");
      }
      clone_imposed_skip[imp_cnt] = ptr;
      imp_cnt++;
    }
  }

  if(!do_guards) {
    if(imp_cnt != imp_guard_cnt) {
      printf("ERROR > sticher overestimated the number of imposed guards\n");
      return FALSE;
    }
  }

  /* handler loop start-up */
  if(res && n_exec == 0) {
    new_ptr = add_snippet(ptr,snippet_handler_loop, 
			  snippet_handler_loop_end);
    if(!patch_branch(ptr+1, &clone_no_handler)) { 
      printf("ERROR >> could not patch a branch (2)\n");
      return FALSE; 
    }
    ptr = new_ptr;
  }

  clone_exec_handlers = ptr;

  /* the handler loop */
  invoke = (char *)(dispatch_list + 1);
  for(i = 0; i < n_handlers; i++) {
    handlers = (handler_desc_t *)invoke;
    invoke += sizeof(handler_desc_t);

    new_ptr = add_snippet(ptr, snippet_check_call, snippet_check_call_end);
    set_lit((func *)(ptr+1), i);
    old_ptr = ptr+3;
    ptr = new_ptr;
    ptr = copy_args_1(ptr, n_args, handlers->useHandlerClosure, 
		      handlers->handlerClosure, save, gp);
    ptr = add_load(ptr, REG_T12, handlers->handler, (long)clone);
    new_ptr = add_snippet(ptr, snippet_call_handler_dir, 
			  snippet_call_handler_dir_end);
    /* patch_short(ptr+1, (long)gp, (long)(ptr+1));*/
    patch_short(ptr+1, (long)clone, (long)(ptr+1));
    ptr = new_ptr;
    clone_handler_loop = ptr;
    if(!patch_branch(old_ptr, &clone_handler_loop)) { 
      printf("ERROR >> could not patch a branch (3)\n");
      return FALSE; 
    }

    if(handlers->nImposed) {
      for(j = 0; j < handlers->nImposed; j++) {
	guards = (imposed_desc_t *)invoke;
	invoke += sizeof(imposed_desc_t);
      }
    }
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      new_ptr = add_snippet(ptr, snippet_epilog, snippet_epilog_end);
      n = 7;
    } else {
      /* copy result + s? for arguments */
      new_ptr = add_part_snippet(ptr, snippet_epilog, 0, ns);
      
      /* s4, s5, s6 always used */
      new_ptr = add_part_snippet(new_ptr, snippet_epilog, 6, 10);
      
      n = ns+2;
    }
  } else {
    new_ptr = add_snippet(ptr, snippet_epilog_mh, snippet_epilog_mh_end);
    n = 7;
  }
  for(i = 0; i < n; i++) {
    set_disp((func *)(ptr+1+i), arguments_frame+guard_results_frame+32+i*8);
  }

  set_disp((func *)(ptr + n+1), arguments_frame+guard_results_frame+0);
  set_disp((func *)(ptr + n+2), frame_size);
  clone_return = ptr;
  ptr = new_ptr;

  /* no handler invoked */
  if(res && n_exec == 0) {
    new_ptr = add_snippet(ptr, snippet_no_handler_gp, 
			  snippet_no_handler_gp_end);
    patch_gpdisp(((int *)(snippet_no_handler_gp))+1, ptr+1);
    clone_no_handler = ptr;
    ptr = new_ptr;
  }

  /* patch delayed branches */
  if(!patch_delayed()) { 
    printf("ERROR >> could not patch delayed branches\n");
    return FALSE; 
  }

  if((char *)ptr - (char *)clone != size) {
    printf("ERROR >> clone_clone_1: different size - %d instead of %d\n", 
	   (char *)ptr - (char *)clone, size);
    if(clone_imposed_skip != NULL) { spin_free(clone_imposed_skip); }
    return FALSE;
  }
  
  imb();

  if(clone_imposed_skip != NULL) { spin_free(clone_imposed_skip); }
  return TRUE;
}

int clone_clone_from_list(func *clone, int size, guardDescT *guardDesc, int cnt,
			  int n_args, int res, int save, long gp)
{
  int i, j, na, ns, offset, n, use_cl;
  int *ptr, *new_ptr, *old_ptr;
  int n_cl = 0, n_can = 0, n_nil = 0, n_exec = 0, n_imp = 0;
  int n_handlers = 0;

  clone_imposed_skip = NULL;
  ptr = (int *)clone;
  use_cl = 0;
  
  /* number of callee-saved registers used to keep args */
  na = n_args + use_cl;
  ns = (na >= 4) ? 4 : na;

  /* prolog */
  new_ptr = add_snippet(ptr, snippet_prolog, snippet_prolog_end);
  patch_short(ptr, (long)clone, (long)clone);
  /* patch_short(ptr, (long)gp, (long)clone);*/


#ifndef CALL_GRAPH
  set_disp((func *)(ptr+3), -frame_size);
  set_disp((func *)(ptr+4), arguments_frame + guard_results_frame + 0);
#else
  /* Unfortunately, the profiling code must come before the change
     of the stack pointer, which means that the displacements are at
     different offsets in the profiled and unprofiled cases. */
  if (!save) {
    copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  }
  /* Overwrite the constant fields with the address of _mcount, known
     at run time. 2, 3, 5, 6 */
  patch_long(((int *)clone + 3), (long)_mcount);

  offset = (int *)snippet_profile_gp_patch-(int *)snippet_prolog;
  patch_short(ptr + offset, (long)clone, (long)(ptr + offset));
  
  set_disp((func *)(ptr+12), -frame_size);
  set_disp((func *)(ptr+13), arguments_frame + guard_results_frame + 0);
#endif CALL_GRAPH
  ptr = new_ptr;

  /* save callee-saved registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_callee, snippet_save_callee_end);
    for(i = 0; i <= 6; i++) {
      set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
    }
    set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+8);
  } else {
    /* s? for arguments */
    new_ptr = add_part_snippet(ptr, snippet_save_callee, 0, ns-1);

    /* s4, s5, s6 always used */
    new_ptr = add_part_snippet(new_ptr, snippet_save_callee, 5, 6);

    for(i = 0; i <= ns+2; i++) {
      set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
    }
  }
  ptr = new_ptr;

  /* save arguments registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_save_regs, snippet_save_regs_end); 
  } else {
    new_ptr = add_part_snippet(ptr, snippet_save_regs, 0,(na >= 6) ? 5 : na-1);
  }
  if(save || na > 4) {
    set_disp((func *)(ptr + 4), arguments_frame+guard_results_frame+16);
  }
  if(save || na > 5) {
    set_disp((func *)(ptr + 5), arguments_frame+guard_results_frame+24);
  }
  ptr = new_ptr;

  /* start-up code */
  ptr = add_snippet(ptr, snippet_start_up_unrolled,
		    snippet_start_up_unrolled_end);

  for(i = 0; i < cnt; i++) {
    if(guardDesc->guard != NULL) {
      ptr = copy_args_1(ptr, n_args, guardDesc->useClosure,
			(long)guardDesc->closure, FALSE, gp);
      ptr = add_load(ptr, REG_T12, (long)guardDesc->guard, (long)clone);
      new_ptr = add_snippet(ptr, snippet_call_guard_dir,
			    snippet_call_guard_dir_end);
      patch_short(ptr+1, (long)clone, (long)(ptr+1));
      set_lit((func *)(ptr+5), i);
      ptr = new_ptr;
    } else {
      new_ptr = add_snippet(ptr, snippet_shift_value, 
			    snippet_shift_value_end);    
      set_lit((func *)(ptr+1), i);
      ptr = new_ptr;
    }
    guardDesc++;
  }

  /* epilog */
  if(n_handlers <= 64) {
    if(save) {
      new_ptr = add_snippet(ptr, snippet_epilog_g, snippet_epilog_g_end);
      n = 7;
    } else {
      /* copy result + s? for arguments */
      new_ptr = add_part_snippet(ptr, snippet_epilog_g, 0, ns);
      
      /* s4, s5, s6 always used */
      new_ptr = add_part_snippet(new_ptr, snippet_epilog_g, 6, 10);
      
      n = ns+2;
    }
  } else {
    /* FIXME */
    new_ptr = add_snippet(ptr, snippet_epilog_mh, snippet_epilog_mh_end);
    n = 7;
  }
  for(i = 0; i < n; i++) {
    set_disp((func *)(ptr+1+i), arguments_frame+guard_results_frame+32+i*8);
  }

  set_disp((func *)(ptr + n+1), arguments_frame+guard_results_frame+0);
  set_disp((func *)(ptr + n+2), frame_size);
  clone_return = ptr;
  ptr = new_ptr;

  /* patch delayed branches */
  if(!patch_delayed()) { return FALSE; }
  
  if((char *)ptr - (char *)clone != size) {
    printf("ERROR >> clone_clone_from_list: different size - %d instead of %d\n", 
	   (char *)ptr - (char *)clone, size);
    return FALSE;
  }
  
  imb();
  return TRUE;
}

void clone_init(int n_args, int n_handlers, int trace)
{
  int i;

  clone_guard_arguments = NULL;
  clone_guard_true = NULL;
  clone_gl_loop_head = NULL;
  clone_gl_loop_tail = NULL;
  clone_handler_loop = NULL;
  clone_exec_handlers = NULL;
  clone_no_handler = NULL;
  clone_return = NULL;
  clone_handler_arguments = NULL;
  clone_call_guard = NULL;
  clone_call_handler = NULL;
  clone_hl_loop_head = NULL;
  clone_guard_closure = NULL;
  clone_handler_closure = NULL;

  /* ra, a4, a5, s0-s6, sp */
  registers_save_frame = (3 + 7 + 1) * sizeof(long); 

  /* do we spill on the stack? */
  if(n_args > 4) {
    /* n_args - 4 (kept in s0-s4) + closure */
    arguments_frame = (n_args + 1 - 4) * sizeof(long);
  } else {
    /* n_args + closure */
    arguments_frame = (n_args + 1) * sizeof(long);
  }

  if(n_handlers <= 64) {
    guard_results_frame = 0;
  } else {
    guard_results_frame = GUARD_RESULTS_FRAME;
  }

  /* total frame size, align to 16 */
  frame_size = arguments_frame + guard_results_frame + registers_save_frame;
  if(trace) {
    frame_size += 32;
  }
  frame_size = ((frame_size + 0xf) & (~0xf));
}

int clone_size(int n_args, int res, int n_handlers, int save)
{
  int size = 0;

  size += get_snippet_size(snippet_prolog, snippet_prolog_end);
  size += get_snippet_size(snippet_save_callee, snippet_save_callee_end);
  size += get_snippet_size(snippet_save_regs, snippet_save_regs_end); 
  size += get_snippet_size(snippet_load_desc, snippet_load_desc_end); 
  if(n_handlers <= 64) {
    size += get_snippet_size(snippet_start_up, snippet_start_up_end);
    size += get_snippet_size(snippet_gl_loop_head, 
			     snippet_gl_loop_head_end);
  } else {
    size += get_snippet_size(snippet_start_up_mh, snippet_start_up_mh_end);
    size += get_snippet_size(snippet_gl_loop_head_mh, 
			     snippet_gl_loop_head_mh_end);
  }
  size += get_args_size(n_args, FALSE);
  if(n_handlers <= 64) {
    size += get_snippet_size(snippet_call_guard, snippet_call_guard_end);
    size += get_snippet_size(snippet_handler_loop, 
			     snippet_handler_loop_end);
  } else {
    size += get_snippet_size(snippet_call_guard_mh, snippet_call_guard_mh_end);
    size += get_snippet_size(snippet_handler_loop_mh, 
			     snippet_handler_loop_mh_end);
  }
  size += get_snippet_size(snippet_load_desc, snippet_load_desc_end); 
  if(n_handlers <= 64) {
    size += get_snippet_size(snippet_hl_loop_head,
			     snippet_hl_loop_head_end);
  } else {
    size += get_snippet_size(snippet_hl_loop_head_mh, 
			     snippet_hl_loop_head_mh_end);
  }
  size += get_args_size(n_args, save);
  if(n_handlers <= 64) {
    size += get_snippet_size(snippet_call_handler, 
			     snippet_call_handler_end);
  } else {
    size += get_snippet_size(snippet_call_handler_mh,
			     snippet_call_handler_mh_end);
  }
  if(n_handlers <= 64) {
    size += get_snippet_size(snippet_hl_loop_tail, 
			     snippet_hl_loop_tail_end);
    size += get_snippet_size(snippet_epilog, snippet_epilog_end);
  } else {
    size += get_snippet_size(snippet_hl_loop_tail_mh,
			     snippet_hl_loop_tail_mh_end);
    size += get_snippet_size(snippet_epilog_mh, snippet_epilog_mh_end);
  }
  size += get_snippet_size(snippet_no_handler, snippet_no_handler_end);

  return size;
}

int clone_clone(func *clone, int size, void *desc, 
		int n_args, int res, int n_handlers, int save)
{
  int i;
  int offset;
  int *ptr, *new_ptr;

  ptr = (int *)clone;

  /* prolog */
  new_ptr = add_snippet(ptr, snippet_prolog, snippet_prolog_end);
  patch_gpdisp((int *)snippet_prolog, ptr);
#ifndef CALL_GRAPH
  set_disp((func *)(ptr+3), -frame_size);
  set_disp((func *)(ptr+4), arguments_frame + guard_results_frame + 0);
#else
  /* Unfortunately, the profiling code must come before the change
     of the stack pointer, which means that the displacements are at
     different offsets in the profiled and unprofiled cases. */
  if (!save) {
      copy_bis((func *)((int *)clone + 2), REG_ZERO, REG_ZERO, REG_ZERO);
  }
  /* Overwrite the constant fields with the address of _mcount, known
     at run time. 2, 3, 5, 6 */
  patch_long(((int *)clone + 3), (long)_mcount);

  offset = (int *)snippet_profile_gp_patch-(int *)snippet_prolog;
  patch_gpdisp((int*)snippet_profile_gp_patch, (int *)clone+offset);
 
  set_disp((func *)(ptr+12), -frame_size);
  set_disp((func *)(ptr+13), arguments_frame + guard_results_frame + 0);
#endif CALL_GRAPH
  ptr = new_ptr;

  /* save callee-saved registers and sp */
  new_ptr = add_snippet(ptr, snippet_save_callee, snippet_save_callee_end);
  for(i = 0; i <= 6; i++) {
    set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+32+i*8);
  }
  set_disp((func *)(ptr+i), arguments_frame+guard_results_frame+8);
  ptr = new_ptr;

  /* save registers */
  new_ptr = add_snippet(ptr, snippet_save_regs, snippet_save_regs_end); 
  set_disp((func *)(ptr + 4), arguments_frame+guard_results_frame+16);
  set_disp((func *)(ptr + 5), arguments_frame+guard_results_frame+24);
  ptr = new_ptr;

  /* load desc pointer */
  new_ptr = add_snippet(ptr, snippet_load_desc, snippet_load_desc_end); 
  patch_long(ptr, (long)desc);
  ptr = new_ptr;

  /* start-up code */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_start_up, snippet_start_up_end);
  } else {
    new_ptr = add_snippet(ptr, snippet_start_up_mh, snippet_start_up_mh_end);
    set_disp((func *)(ptr+3), arguments_frame);
  }
  if(res) {
    if(!patch_branch(ptr+2, &clone_no_handler)) { return 0; }
  } else {
    if(!patch_branch(ptr+2, &clone_return)) { return 0; }
  }
  ptr = new_ptr;

  /* guard loop head */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_gl_loop_head, 
			  snippet_gl_loop_head_end);
    if(!patch_branch(ptr+1, &clone_guard_arguments)) { return 0; }
    if(!patch_branch(ptr+2, &clone_guard_true)) { return 0; }
  } else {
    new_ptr = add_snippet(ptr, snippet_gl_loop_head_mh, 
			  snippet_gl_loop_head_mh_end);
    if(!patch_branch(ptr+1, &clone_guard_arguments)) { return 0; }
    if(!patch_branch(ptr+4, &clone_guard_true)) { return 0; }
  }
  clone_gl_loop_head = ptr;
  ptr = new_ptr;

  /* arguments for the guard call */
  clone_guard_arguments = ptr;
  ptr = copy_args(ptr, 1, n_args, 
		  &clone_guard_closure, &clone_call_guard, FALSE);
  if(ptr == NULL) {
    return 0;
  }

  /* call guard */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_call_guard, 
			  snippet_call_guard_end);
    if(!patch_branch(ptr+3, &clone_gl_loop_tail)) { return 0; }
    if(!patch_branch(ptr+6, &clone_handler_loop)) { return 0; }
    if(!patch_branch(ptr+10, &clone_handler_loop)) { return 0; }
    if(!patch_branch(ptr+11, &clone_gl_loop_head)) { return 0; }
    clone_guard_true = ptr+4;
    clone_gl_loop_tail = ptr+7;
  } else {
    new_ptr = add_snippet(ptr, snippet_call_guard_mh,
			  snippet_call_guard_mh_end);
    if(!patch_branch(ptr+4, &clone_gl_loop_tail)) { return 0; }
    if(!patch_branch(ptr+7, &clone_handler_loop)) { return 0; }
    if(!patch_branch(ptr+11, &clone_handler_loop)) { return 0; }
    if(!patch_branch(ptr+12, &clone_gl_loop_head)) { return 0; }
    clone_guard_true = ptr+5;
    clone_gl_loop_tail = ptr+8;
  }
  clone_call_guard = ptr;
  patch_gpdisp((int *)snippet_call_guard+1, ptr+1);
  ptr = new_ptr;

  /* handler loop start-up */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr,snippet_handler_loop, 
			  snippet_handler_loop_end);
  } else {
    new_ptr = add_snippet(ptr,snippet_handler_loop_mh, 
			  snippet_handler_loop_mh_end);
    set_disp((func *)(ptr+2), arguments_frame);
  }
  if(res) {
    if(!patch_branch(ptr+1, &clone_no_handler)) { return 0; }
  } else {
    if(!patch_branch(ptr+1, &clone_return)) { return 0; }
  }
  clone_handler_loop = ptr;
  ptr = new_ptr;

  /* reload desc pointer */
  new_ptr = add_snippet(ptr, snippet_load_desc, snippet_load_desc_end); 
  patch_long(ptr, (long)desc);
  ptr = new_ptr;

  /* handler loop head */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr,snippet_hl_loop_head,
			  snippet_hl_loop_head_end);
  } else {
    new_ptr = add_snippet(ptr,snippet_hl_loop_head_mh, 
			  snippet_hl_loop_head_mh_end);
  }
  if(!patch_branch(ptr+1, &clone_return)) { return 0; }
  if(!patch_branch(ptr+3, &clone_handler_arguments)) { return 0; }
  if(!patch_branch(ptr+6, &clone_hl_loop_head)) { return 0; }
  clone_hl_loop_head = ptr;
  ptr = new_ptr;

  /* arguments for the handler call */
  clone_handler_arguments = ptr;
  ptr = copy_args(ptr, 0, n_args,
		  &clone_handler_closure, &clone_call_handler,save);
  if(ptr == NULL) {
    return 0;
  }


  /* call handler */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_call_handler, 
			  snippet_call_handler_end);
  } else {
    new_ptr = add_snippet(ptr, snippet_call_handler_mh,
			  snippet_call_handler_mh_end);
  }
  patch_gpdisp((int *)snippet_call_handler+1, ptr+1);
  clone_call_handler = ptr;
  ptr = new_ptr;

  /* handler loop tail */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_hl_loop_tail, 
			  snippet_hl_loop_tail_end);
    if(!patch_branch(ptr+4, &clone_hl_loop_head)) { return 0; }
  } else {
    new_ptr = add_snippet(ptr, snippet_hl_loop_tail_mh, 
			  snippet_hl_loop_tail_mh_end);
    if(!patch_branch(ptr+4, &clone_hl_loop_head)) { return 0; }
  }
  if(!patch_branch(ptr+1, &clone_return)) { return 0; }
  ptr = new_ptr;

  /* epilog */
  if(n_handlers <= 64) {
    new_ptr = add_snippet(ptr, snippet_epilog, snippet_epilog_end);
  } else {
    new_ptr = add_snippet(ptr, snippet_epilog_mh, snippet_epilog_mh_end);
  }
  for(i = 0; i <= 6; i++) {
    set_disp((func *)(ptr+1+i), arguments_frame+guard_results_frame+32+i*8);
  }
  set_disp((func *)(ptr + 8), arguments_frame+guard_results_frame+0);
  set_disp((func *)(ptr + 9), frame_size);
  clone_return = ptr;
  ptr = new_ptr;

  /* no handler invoked */
  new_ptr = add_snippet(ptr, snippet_no_handler, snippet_no_handler_end);
  clone_no_handler = ptr;
  ptr = new_ptr;

  /* patch delayed branches */
  if(!patch_delayed()) { return FALSE; }

  if((char *)ptr - (char *)clone != size) {
    printf("ERROR >> clone_clone: different size - %d instead of %d\n", 
	   (char *)ptr - (char *)clone, size);
    return FALSE;
  }
  
  imb();

  return TRUE;
}


int *copy_args(int *ptr, int guard, int n_args, 
	       int **clone_no_closure, int **clone_call, int save)
{
  int *new_ptr;

  /* check for closure */
  new_ptr = add_snippet(ptr, snippet_check_no_closure, 
			snippet_check_no_closure_end);

  /* patch the offset if not guard */
  if(! guard) {
    set_disp((func *)ptr, get_disp((func *)ptr) + sizeof(void *));
  }

  /* jump over the closure arguments */
  if(n_args > 0 || save) {
    if(!patch_branch(ptr+1, clone_no_closure)) { return NULL; }
  } else {
    if(!patch_branch(ptr+1, clone_call)) { return NULL; }
  }
  ptr = new_ptr;

  /* closure arguments */
  copy_ldq((func *)ptr, REG_A0, handlerClosureOffset, REG_S4);
  ptr++;
  ptr = add_copy_args(ptr, REG_S0, REG_A1, 1, n_args);

  /* callee-saved registers */
  if(save) {
    new_ptr = add_snippet(ptr, snippet_pass_callee_saved,
			  snippet_pass_callee_saved_end);
    set_disp((func *)ptr, arguments_frame + guard_results_frame);
    patch_callee_saved(ptr+1, n_args+1);
    ptr = new_ptr;
  }

  if(n_args > 0 || save) {
    /* jump over the regular arguments */
    new_ptr = add_snippet(ptr, snippet_branch_closure, 
			  snippet_branch_closure_end);
    if(!patch_branch(ptr, clone_call)) { return NULL; }
    ptr = new_ptr;

    /* regular arguments */
    new_ptr = add_copy_args(ptr, REG_S0, REG_A0, 0, n_args);
    *clone_no_closure = ptr;
    ptr = new_ptr;

    /* callee-saved registers */
    if(save) {
      new_ptr = add_snippet(ptr, snippet_pass_callee_saved,
			    snippet_pass_callee_saved_end);
      set_disp((func *)ptr, arguments_frame + guard_results_frame);
      patch_callee_saved(ptr+1, n_args);
      ptr = new_ptr;
    }
  }

  return ptr;
}


void patch_callee_saved(int *ptr, int n_regs)
{
  if(n_regs < 6) {
    set_reg_c((func *)ptr, REG_A0 + n_regs);
  } else {
    copy_ldq((func *)ptr, REG_T2, 8 * (n_regs - 6), REG_SP);
  }
}

int get_args_size_1(int n_args, int use_closure, 
		    long closure, int save, long base)
{
  int size = 0;

  if(n_args > 0 || use_closure) {
    if(!use_closure) {
      size += get_copy_args_size(0, n_args);
    } else {
      size += get_load_size(closure, base);
      size += get_copy_args_size(1, n_args);
    }
  }

  if(save) {
    size += get_snippet_size(snippet_pass_callee_saved,
			     snippet_pass_callee_saved_end);
  }

  return size;
}

int *copy_args_1(int *ptr, int n_args, int use_closure, 
		 long closure, int save, long base)
{
  int *new_ptr;

  if(n_args > 0 || use_closure || save) {
    if(!use_closure) {
      ptr = add_copy_args(ptr, REG_S0, REG_A0, 0, n_args);
    } else {
      ptr = add_load(ptr, REG_A0, closure, base);
      ptr = add_copy_args(ptr, REG_S0, REG_A1, 1, n_args);
    }
  }

  if(save) {
    new_ptr = add_snippet(ptr, snippet_pass_callee_saved,
			  snippet_pass_callee_saved_end);
    set_disp((func *)ptr, arguments_frame + guard_results_frame);
    patch_callee_saved(ptr+1, n_args+use_closure);
    ptr = new_ptr;
  }

  return ptr;
}

int get_copy_args_size(int offset, int n_args)
{
  int size = 0;
  int i;

  if(offset == 0) {
    for(i = 1; i <= n_args; i++) {
      if(i < 5) {
	size++;
      } else if(i < 7) {
	size++;
      } else {
	size += 2;
      }
    }
  } else {
    for(i = 1; i <= n_args; i++) {
      if(i < 5) {
	size++;
      } else if(i == 5) {
	size++;
      } else if(i == 6) {
	size += 2;
      } else {
	size += 2;
      }
    }
  }

  return size * sizeof(int);
}

int get_args_size(int n_args, int save)
{
  int size = 0;

  size += get_snippet_size(snippet_check_no_closure,
			   snippet_check_no_closure_end);
  size += sizeof(int);  /* closure arguments */
  size += get_copy_args_size(1, n_args);
  if(save) {
    size += get_snippet_size(snippet_pass_callee_saved,
			     snippet_pass_callee_saved_end);
  }

  if(n_args > 0 || save) {
    size += get_snippet_size(snippet_branch_closure,
			     snippet_branch_closure_end);
    size += get_copy_args_size(0, n_args);
    if(save) {
      size += get_snippet_size(snippet_pass_callee_saved,
			       snippet_pass_callee_saved_end);
    }
  }

  return size;
}

int *add_copy_args(int *ptr, int src_reg, int dst_reg, int offset, int n_args)
{
  int i;
  int s_off, l_off;

  if(offset == 0) {
    for(i = 1; i <= n_args; i++) {
      if(i < 5) {
	copy_bis((func *)ptr, src_reg, src_reg, dst_reg);
	ptr++;
	src_reg++;
	dst_reg++;
      } else if(i < 7) {
	copy_ldq((func *)ptr, 
		 dst_reg, arguments_frame+guard_results_frame+(i-3)*8,REG_SP);
	ptr++;
	dst_reg++;
      } else {
	s_off = l_off = (i-7)*8;
	copy_ldq((func *)ptr, REG_T2, frame_size+l_off, REG_SP);
	ptr++;
	copy_stq((func *)ptr, REG_T2, s_off, REG_SP);
	ptr++;
      }
    }
  } else {
    for(i = 1; i <= n_args; i++) {
      if(i < 5) {
	copy_bis((func *)ptr, src_reg, src_reg, dst_reg);
	ptr++;
	src_reg++;
	dst_reg++;
      } else if(i == 5) {
	copy_ldq((func *)ptr, 
		 dst_reg, arguments_frame+guard_results_frame+(i-3)*8,REG_SP);
	ptr++;
	dst_reg++;
      } else if(i == 6) {
	s_off = (i+1-7)*8;
	copy_ldq((func *)ptr, 
		 REG_T2, arguments_frame+guard_results_frame+(i-3)*8,REG_SP);
	ptr++;
	copy_stq((func *)ptr, REG_T2, s_off, REG_SP);
	ptr++;
	dst_reg++;
      } else {
	l_off = (i-7)*8;
	s_off = (i+1-7)*8;
	copy_ldq((func *)ptr, REG_T2, frame_size+l_off, REG_SP);
	ptr++;
	copy_stq((func *)ptr, REG_T2, s_off, REG_SP);
	ptr++;
      }
    }
  }
  return ptr;
}

/******************************************************************************
 *
 * code patching
 *
 *****************************************************************************/

int *add_snippet(int *ptr, func *stub, func *stub_end)
{
  int size = (char *)stub_end - (char *)stub;
  bcopy(stub, ptr, size);
  return (int *)((char *)ptr + size);
}

int get_snippet_size(func *stub, func *stub_end)
{
  return (char *)stub_end - (char *)stub;
}

int *add_part_snippet (int *ptr, func *stub, int start, int end)
{
  int size = (end - start + 1) * sizeof(int);
  bcopy(((int *)stub)+start, ptr, size);
  return (int *)((char *)ptr + size);
}

int get_part_snippet_size(func *stub, int start, int end)
{
  return (end - start + 1) * sizeof(int);
}

/*
 *
 */

struct b_list {
  int *dst;
  int **ptr;
  struct b_list *next;
} * branch_list;

int patch_branch(int *dst, int **ptr)
{
  struct b_list *new;

  if(ptr == NULL) {
    printf("ERROR >> stitcher: NULL pointer in patch_branch\n");
    return 0;
  }

  if(*ptr == NULL) {
    new = (struct b_list *)spin_malloc(sizeof(struct b_list));
    new->dst = dst;
    new->ptr = ptr;
    new->next = branch_list;
    branch_list = new;
    return 1;
  } else {
    if(!set_offset(dst, *ptr)) {
      printf("PB\n");
      return FALSE;
    }
    return TRUE;
  }
}

int patch_delayed()
{
  struct b_list *ptr, *next;

  for(ptr = branch_list; ptr != NULL; ptr = next) {
    if(*(ptr->ptr) == NULL) {
      printf("ERROR >> stitcher: NULL pointer in patch_delayed\n");
      break;
    }

    if(!set_offset(ptr->dst, *(ptr->ptr))) {
      printf("PD\n");
      return 0;
    }
    next = ptr->next;
    spin_free(ptr);
  }
  branch_list = NULL;
  return 1;
}

int set_offset(int *dst, int *target)
{
  int  disp = (((char *)target-(char *)dst)-sizeof(int))>>2; 

  if((((unsigned)disp & (unsigned)(~0x1fffff)) != 0) && 
     (((unsigned)disp | (unsigned)0x1fffff) != ((unsigned)~0)))
  {
    printf("ERROR>> set_offset: target and source too far: 0x%x 0x%lx 0x%lx\n",
	   disp, dst, target);
    return 0;
  }

  *dst = (*dst & 0xffe00000) | (0x001fffff & disp); 
  return 1;
}

/*
 * make a clone of a template
 */

func *clone_stub(func *stub, func *stub_end)
{
  int size;
  func *clone;
  
  size = (char *)stub_end - (char *)stub;
  if(stitch_debug) {
    printf("clone_stub (1): 0x%lx 0x%lx %d\n", stub, stub_end, size);
  }

  if(( clone = (func *)disp_malloc(size)) == NULL ) {
    printf("stitcher: no memory for a stub\n");
    return NULL;
  }

  bcopy(stub, clone, size);

  if(stitch_debug) {
    printf ("clone_stub (2): 0x%lx 0x%lx\n", clone, ((char *)clone)+size);
  }
  return clone;
}

void patch_adjust_sp(func *clone, int n_args)
{
  int n_regs;                  /* how many in registers */
  int n_stack;                 /* how many on the stack */
  int stack_inc;               /* how much to increment the stack */

  /* split the saved information into registers and stack */
  n_regs  = 6 - n_args;
  n_stack = 9 - n_regs;
  stack_inc = n_stack + 1; /* add one for closure */
  stack_inc = (stack_inc + (stack_inc % 2)) * 8;

  set_disp(clone, stack_inc);
}

void patch_load_callee_saved(func *clone, int n_args, int closure)
{
  int n_regs;                  /* how many in registers */
  int n_stack;                 /* how many on the stack */
  int stack_inc;               /* how much to increment the stack */
  int reg, disp, i;
  func *ptr;
  int offset;

  /* split the saved information into registers and stack */
  n_regs  = 6 - (n_args + closure);
  n_stack = 9 - n_regs;
  stack_inc = n_stack + 1 - closure;  /* adjust for closure */
  stack_inc = (stack_inc + (stack_inc % 2)) * 8;

  /* patch the stack decrement */
  set_disp(clone, -stack_inc);

  /* patch loading of saved information from stack into a temporary register */
  ptr = (func *)((int *)clone+1);
  for(i = 0; i < 9; i++) {
    if(i < 8) {
      offset = 24 + i * 8;
    } else {
      offset = 0;
    }
    set_disp(ptr, arguments_frame + guard_results_frame + offset + stack_inc);
    ((int *)ptr) += 2;
  }

  /* patch loading of the temporary register into argument registers */
  ptr = (func *)((int *)clone+2);
  reg  = REG_A0 + n_args + closure;
  for(i = 0; i < n_regs; i++) {
    copy_bis(ptr, REG_T1, REG_T1, reg);
    ((int *)ptr) += 2;
    reg++;
  }

  /* patch loading of the temporary register into argument area of stack */
  disp = 0;
  for(i = 0; i < n_stack; i++) {
    copy_stq(ptr, REG_T1, disp, REG_SP);
    ((int *)ptr) += 2;
    disp += 8;
  }
}


/*
 * set the offset for loading a pointer relative off the value of a register
 * 
 * base  - the base pointer 
 * clone - destination snippet 
 * stub  - original snippet
 * arg_h - original instruction loading high 16 bits
 * arg_l - original instruction loading low 16 bits
 * arg   - new argument
 *
 */

void patch_arg(func *base, func *clone, func *stub, 
	       func *arg_h, func *arg_l, long arg)
{
  int *ptr_h = (int *)clone + ((int *)arg_h - (int *)stub);
  int *ptr_l = (int *)clone + ((int *)arg_l - (int *)stub);
  
  if(stitch_debug) {
    printf("arg (1): 0x%lx 0x%lx 0x%x 0x%x\n", ptr_h, ptr_l, *ptr_h, *ptr_l);
  }

  if(arg == 0) {
    patch_zero(ptr_h);
    patch_zero(ptr_l);
  } else {
    patch_ptr(ptr_h, ptr_l, (int *)base, arg);
  }

  if(stitch_debug) {
    printf ("arg (2): 0x%lx 0x%lx 0x%x 0x%x\n", ptr_h, ptr_l, *ptr_h, *ptr_l);
  }
}

/*
 * overwrite the original instruction with loading zero to t12
 */

void patch_zero(int *dst)
{
  if(stitch_debug) {
    printf ("patch_zero: 0x%lx 0x%x ", dst, *dst);
  }

  *dst = *((int *)st_write_zero);

  if(stitch_debug) {
    printf ("0x%x\n", *dst);
  }
}

void patch_ptr(int *dst_h, int *dst_l, int *base, long arg)
{
  long disp;
  int x, y;

  disp = (long)arg - (long)base;

  if(stitch_debug) {
    printf ("disp (1): 0x%lx 0x%lx 0x%x 0x%x : 0x%lx 0x%lx 0x%lx\n", 
	    dst_h, dst_l, *dst_h, *dst_l, base, arg, disp);
  }

  if((disp > 0 && disp > 0x0ffffffff) || 
     (disp < 0 && disp < - 0x0100000000))
  {
    printf("ERROR >> offset too big in patch_ptr\n");
  }

  x = (disp >> 16) & 0xffff;
  y = disp & 0xffff;
  if (y & 0x8000)
    x = (x + 1) & 0xffff;

  *dst_h = (*dst_h & 0xffff0000) | x;
  *dst_l = (*dst_l & 0xffff0000) | y;

  if(stitch_debug) {
    printf ("disp (2): 0x%x 0x%x 0x%lx 0x%lx 0x%x 0x%x\n", 
	    x, y, dst_h, dst_l, *dst_h, *dst_l);
  }
}

static void patch_gpdisp(int *src, int *dst)
{
  long gp, olddisp, newdisp, oldx, oldy, newx, newy, sign;
  
  oldx = *src & 0xffff;
  oldy = *(src + 1) & 0xffff;
  sign = *(src + 1) & 0x8000;
  oldy += (sign) ? 0xffffffffffff0000 : 0;
  olddisp = ( oldx << 16) + oldy;
  
  gp = (long)src + olddisp;
  newdisp = gp - (long)dst ;
  newx = (newdisp >> 16) & 0xffff;
  newy = newdisp & 0xffff;
  
  if (newy & 0x8000)
    newx = (newx + 1) & 0xffff;
  *dst =   (*dst & 0xffff0000) | newx;
  *(dst + 1) = (*(dst + 1) & 0xffff0000) | newy;
  
  if (stitch_debug) {
    printf("patch_gpdisp: src: 0x%lx; dst 0x%lx; ", src, dst);
    printf("gp: 0x%x -> 0x%lx; disp 0x%lx -> 0x%lx\n",src,gp,olddisp,newdisp);
  }
}

static void patch_long(int *dst, long val)
{
  int a1, a2, a3, a4;

  a1 = val & 0x0ffff;
  a2 = (val >> 16) & 0x0ffff;
  a3 = (val >> 32) & 0x0ffff;
  a4 = (val >> 48) & 0x0ffff;

  if(a1 & 0x18000) { a2++; }
  if(a2 & 0x18000) { a3++; }
  if(a3 & 0x18000) { a4++; }

  *dst     = (*dst     & 0xffff0000) | (a4 & 0xffff);
  *(dst+1) = (*(dst+1) & 0xffff0000) | (a3 & 0xffff);
  *(dst+3) = (*(dst+3) & 0xffff0000) | (a2 & 0xffff);
  *(dst+4) = (*(dst+4) & 0xffff0000) | (a1 & 0xffff);

  if(stitch_debug) {
    printf( "patch_long: 0x%lx -> 0x%x 0x%x 0x%x 0x%x\n", val, a1, a2, a3, a4);
  }
}

/******************************************************************************
 *
 * initialization
 *
 *****************************************************************************/


func *get_gp(func *src_func)
{
  long gp, olddisp, oldx, oldy, sign;
  int *src = (int *)src_func;

  oldx = *src & 0xffff;
  oldy = *(src + 1) & 0xffff;
  sign = *(src + 1) & 0x8000;
  oldy += (sign) ? 0xffffffffffff0000 : 0;
  olddisp = (oldx << 16) + oldy;
  
  gp = (long)src + olddisp;

  if(stitch_debug) {
    printf("get_gp: gp for 0x%lx is 0x%lx\n", src, gp);
  }
  return (func *)gp;
}

/******************************************************************************
 *
 * assembly of single instructions
 *
 *****************************************************************************/

void copy_bis(func *dst, int reg_a, int reg_b, int reg_c)
{
  copy_inst(dst, snippet_bis);
  set_reg_a(dst, reg_a);
  set_reg_b(dst, reg_b);
  set_reg_c(dst, reg_c);
}

void copy_bis_i(func *dst, int reg_a, int reg_c, int lit)
{
  copy_inst(dst, snippet_bis_i);
  set_reg_a(dst, reg_a);
  set_reg_c(dst, reg_c);
  set_lit(dst, lit);
}

void copy_stq(func *dst, int reg_a, int disp, int reg_b)
{
  copy_inst(dst, snippet_stq);
  set_reg_a(dst, reg_a);
  set_reg_b(dst, reg_b);
  set_disp(dst, disp);
}

void copy_ldq(func *dst, int reg_a, int disp, int reg_b)
{
  copy_inst(dst, snippet_ldq);
  set_reg_a(dst, reg_a);
  set_reg_b(dst, reg_b);
  set_disp(dst, disp);
}

void copy_inst(func *dst, func *snippet)
{
  *(int *)dst = *(int *)snippet;
}

/******************************************************************************
 *
 * operations on instruction fields
 *
 *****************************************************************************/

#define REG_A_MASK     0x03e00000
#define REG_B_MASK     0x001f0000
#define REG_C_MASK     0x0000001f
#define OPCODE_MASK    0xfc000000
#define FUNCTION_MASK  0x00000fe0
#define DISP_MASK      0x0000ffff
#define BR_DISP_MASK   0x001fffff
#define LIT_MASK       0x001fe000

#define REG_A_SHIFT      21
#define REG_B_SHIFT      16
#define REG_C_SHIFT       0
#define OPCODE_SHIFT     26
#define FUNCTION_SHIFT    5
#define DISP_SHIFT        0
#define BR_DISP_SHIFT     0
#define LIT_SHIFT        13 

#define SET_INST_FIELD(dst, val, mask, shift) \
  *(int *)(dst) = (((*(int *)(dst)) & (~mask)) | (((val) << (shift)) & (mask)))

#define GET_INST_FIELD(dst, mask, shift) \
  (((*(int *)(dst)) & (mask)) >> (shift))

void set_reg_a(func *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_A_MASK, REG_A_SHIFT);
}

int get_reg_a(func *dst)
{
  return GET_INST_FIELD(dst, REG_A_MASK, REG_A_SHIFT);
}

void set_reg_b(func *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_B_MASK, REG_B_SHIFT);
}

int get_reg_b(func *dst)
{
  return GET_INST_FIELD(dst, REG_B_MASK, REG_B_SHIFT);
}

void set_reg_c(func *dst, int reg)
{
  SET_INST_FIELD(dst, reg, REG_C_MASK, REG_C_SHIFT);
}

int get_reg_c(func *dst)
{
  return GET_INST_FIELD(dst, REG_C_MASK, REG_C_SHIFT);
}

void set_disp(func *dst, int disp)
{
  SET_INST_FIELD(dst, disp, DISP_MASK, DISP_SHIFT);
}

int get_disp(func *dst)
{
  return GET_INST_FIELD(dst, DISP_MASK, DISP_SHIFT);
}

void set_br_disp(func *dst, int disp)
{
  SET_INST_FIELD(dst, disp, BR_DISP_MASK, BR_DISP_SHIFT);
}

int get_br_disp(func *dst)
{
  return GET_INST_FIELD(dst, BR_DISP_MASK, BR_DISP_SHIFT);
}

void set_lit(func *dst, int lit)
{
  SET_INST_FIELD(dst, lit, LIT_MASK, LIT_SHIFT);
}

int get_lit(func *dst)
{
  return GET_INST_FIELD(dst, LIT_MASK, LIT_SHIFT);
}

extern func bypass_stub;
extern func bypass_stub_end;
extern func jumpin_stub;
extern func jumpin_stub_end;

/*
   Each time caller is called we want the callee to be called with
   one additional argument which is the PC of the caller's caller.
   In order to do this, we overwrite the first instructions of
   the caller with code from bypass_stub.
 */
func *AddPCBypass(func *caller, func *callee, long n_args)
{
  int size, offset;
  func *jumpin_clone;
  int *ptr;
  int i;

  if(n_args < 0 && n_args > 5) {
    printf("ERROR >> unsupported number of arguments in add_bypass\n");
    return NULL;
  }
   
  /* create and the jumpin stub */
  jumpin_clone = clone_stub(jumpin_stub, jumpin_stub_end);

  /* patch the original procedure */
  for(i = 0; i < 12; i++) {
    ((int *)jumpin_clone)[i+4+1+5] = ((int *)caller)[i+2];
    ((int *)caller)[i+2] = ((int *)&bypass_stub)[i];
  }

  set_reg_c((func *)((int *)caller+4), REG_A0 + n_args);
  patch_long((int *)caller+5, (long)callee);
  patch_gpdisp((int *)caller, (int *)jumpin_clone);
  patch_long((int *)jumpin_clone+4+1, (long)((int *)caller + 2 + 12));

  imb();
  
  return jumpin_clone;
}

#ifdef QPAPA

func *AddTraceBypass(func *caller, func *callee, long n_args)
{
  int size, offset;
  func *jumpin_clone;
  int *ptr;
  int i;

  if(n_args < 0 && n_args > 5) {
    printf("ERROR >> unsupported number of arguments in add_bypass\n");
    return NULL;
  }
   
  /* create and the jumpin stub */
  jumpin_clone = clone_stub(jumpin_stub, jumpin_stub_end);

  /* patch the original procedure */
  for(i = 0; i < 12; i++) {
    ((int *)jumpin_clone)[i+4+1+5] = ((int *)caller)[i+2];
    ((int *)caller)[i+2] = ((int *)&bypass_stub)[i];
  }

  set_reg_c((func *)((int *)caller+4), REG_A0 + n_args);
  patch_long((int *)caller+5, (long)callee);
  patch_gpdisp((int *)caller, (int *)jumpin_clone);
  patch_long((int *)jumpin_clone+4+1, (long)((int *)caller + 2 + 12));

  imb();
  
  return jumpin_clone;
}

#endif

/******************************************************************************
 *
 * initialization
 *
 *****************************************************************************/

char *disp_malloc(int size)
{
  return (char *)spin_malloc(size);
}

void StitcherInitialize()
{
  init_gpa();
}

#define DIFF 10000

unsigned long total_time = 0;
unsigned long max_time = 0;
unsigned long min_time = 0xffffffffffffffff;
unsigned long total_cnt  = 0;

typedef struct tdesc {
  void          *stub;
  unsigned long  total_time;
  unsigned long  max_time;
  unsigned long  min_time;
  unsigned long  total_cnt;
} tdesc;

tdesc events[1000];

void init_events () 
{
  int i;
  for(i = 0; i < 1000; i++) {
    events[i].stub = NULL;
    events[i].total_time = 0;
    events[i].max_time = 0;
    events[i].min_time = 0xffffffffffffffff;
    events[i].total_cnt = 0;
  }
}

unsigned long ttou(unsigned long ticks)
{
  unsigned long rate = get_rpb_counter() / 1000000;
  if(rate != 0) {
    return ticks / rate;
  } else {
    printf("ERROR >> add_xxx_counts: rate = 0\n");
    return 0;
  }
}

void trace_counts_add(void *stub, 
		      unsigned long total_time,
		      unsigned long imposed_cnt,
		      unsigned long imposed_time,
		      unsigned long guard_cnt,
		      unsigned long guard_time,
		      unsigned long handler_cnt,
		      unsigned long handler_time)
{
  unsigned long time, rate;
  int spl;
  int i;

  /* no interrupts */
  spl = SpinSwapIpl(7);

  if(total_time < 0 || imposed_cnt < 0 || imposed_time < 0 ||
     guard_cnt < 0 || guard_time < 0 || handler_cnt < 0 || handler_time < 0)
  {
    printf("ERROR >> trace_counts_add: some arguments were < 0\n");
    SpinSwapIpl(spl);
    return;
  }
  
  time = total_time;
  total_time += time;
  total_cnt++;
  if(time > max_time) { max_time = time; }
  if(time < min_time) { min_time = time; }

  for(i = 0; i < 1000; i++) {
    if(events[i].stub == stub || events[i].stub == NULL) {
      break;
    }
  }

  if(i != 1000) {
    if(events[i].stub == NULL) {
      events[i].stub = stub;
    }
    events[i].total_time += time;
    events[i].total_cnt++;
    if(time > events[i].max_time) { events[i].max_time = time; }
    if(time < events[i].min_time) { events[i].min_time = time; }
  }

  SpinSwapIpl(spl);
}

void trace_counts_reset()
{
  int spl = SpinSwapIpl(7);
  int i;

  for(i = 0; i < 1000; i++) {
    events[i].stub = NULL;
  }
  SpinSwapIpl(spl);
}

void trace_counts_dump()
{
  unsigned long time, rate;
  int spl = SpinSwapIpl(7);
  int i;

  printf("\ntotal > %lu: ticks: %lu %lu %lu %lu; usec: %lu %lu\n", 
	 total_cnt, total_time, total_time/total_cnt, min_time, max_time,
	 ttou(total_time), ttou(total_time/total_cnt), 
	 ttou(min_time), ttou(max_time));
  for(i = 0; i < 1000; i++) {
    if(events[i].stub != NULL) {
      printf("%s > %lu: ticks: %lu %lu %lu %lu; usec: %lu %lu\n", 
	     Dispatcher__GetName(events[i].stub),
	     events[i].total_cnt, events[i].total_time, 
	     events[i].total_time/events[i].total_cnt, 
	     events[i].min_time, events[i].max_time,
	     ttou(events[i].total_time), 
	     ttou(events[i].total_time/events[i].total_cnt), 
	     ttou(events[i].min_time), ttou(events[i].max_time));
    }
  }
  SpinSwapIpl(spl);
}

mutateit(void *addr, void *pc)
{
  RTHeapRep__MutatorFault(addr, pc);
}

fuckingshit(void **lloc, void *lptr, void *rptr, void *pc)
{
  RTHeapRep__Assignement(lloc, lptr, rptr, pc);
}


shouldnotrefcount(void *pc)
{
  printf("\nERROR >> reference counting code called @ 0x%lx\n", pc);
  Debugger();
}


/******************************************************************************
 *	
 *				LINEARIZED ARGUMENTS
 *
 *****************************************************************************/

static int argument_stub_size(void *proc, int n_args,
			      int useClosure, long closure, int long gp);

static int clone_argument_stub(func *clone, void *proc, int n_args,
			       int useClosure, long closure, long gp);

extern func argument_prolog;
extern func argument_prolog_end;
extern func argument_prolog_sp;
extern func argument_array_size;
extern func argument_epilog;
extern func argument_epilog_end;
extern func argument_epilog_sp;
extern func argument_gp_patch;
#ifdef CALL_GRAPH
extern func argument_profile_gp_patch;
#endif

/*
 * A bypass procedure that callects all the arguments, puts them
 * in an open array allocated on stack and passes it as an argument
 * to the original procedure.
 */

func *CloneArgumentStub(void *proc, long n_args, long useClosure, long closure)
{
  func *clone;
  int   init_size, clone_size, size;
  long  gp;

  if(stitch_debug) {
    printf("CloneArgumentStub: 0x%lx %d %d 0x%lx\n",
	   proc, n_args, useClosure, closure);
  }

  init_size = argument_stub_size(proc, n_args, useClosure, closure, 0);
  if((clone = (func *)disp_malloc(init_size)) == NULL ) {
    printf("ERROR >> stitcher: no memory for a stub\n");
    return NULL;
  }
  gp = (long)clone;
  size = argument_stub_size(proc, n_args, useClosure, closure, gp);
  if(size > init_size) {
    printf("ERROR >> stitcher: size of the stub underestimated\n");
    return NULL;
  }
  
  clone_size = clone_argument_stub(clone, proc, n_args,
				   useClosure, closure, gp);

  if(stitch_debug) {
    printf("CloneArgumentStub done: 0x%lx %d\n", clone, size);
  }

  if(clone_size != size) {
    printf("ERROR >> CloneArgumentStub: different size - %d instead of %d\n",
	   clone_size, size);
    return NULL;
  }

  imb();

  if(stitch_debug) {
    printf("CloneArgumentStub: 0x%lx\n", clone);
  }

  return clone;
}


static int argument_stub_size(void *proc, int n_args,
			      int useClosure, long closure, int long gp)
{
  int size;

  /* the beginning of the procedure */
  size = ((char *)argument_prolog_end - (char *)argument_prolog);

  /* collecting arguments */
  if(n_args <= 6) {
    size += n_args * sizeof(int);
  } else {
    size += (6 + (n_args - 6) * 2) * sizeof(int);
  }

  /* creating the closure */
  if(useClosure) {
    size += get_load_size(closure, gp);
  }

  /* creating the jump target */
  size += get_load_size((long)proc, gp);

  /* copy T0 to either A0 or A1 */
  size += sizeof(int);

  /* the end of the procedure */
  size += ((char *)argument_epilog_end - (char *)argument_epilog);

  return size;
} 

static int clone_argument_stub(func *clone, void *proc, int n_args,
			       int useClosure, long closure, long gp)
{
  int *ptr = (int *)clone;
  int *new_ptr;
  int  offset, offset1, offset2, i;
  int  stack_size = (2 + n_args + 2)*sizeof(long);

  /* prolog */
  new_ptr = add_snippet(ptr, argument_prolog, argument_prolog_end);
  patch_short(ptr, (long)clone, (long)clone);
  offset = (int *)argument_prolog_sp - (int *)argument_prolog;
  set_disp((func *)(ptr + offset), -stack_size);
  offset = (int *)argument_array_size - (int *)argument_prolog;
  set_disp((func *)(ptr + offset), n_args);
  ptr = new_ptr;

  /* arguments */
  offset1 = 16;
  offset2 = stack_size;
  for(i = 0; i < n_args; i++) {
    if(i < 6) {
      /* it's in a register */
      copy_stq((func *)ptr, REG_A0+i, offset1, REG_T0);
      ptr++;
    } else {
      /* it's on stack */
      copy_ldq((func *)ptr, REG_T1, offset2, REG_SP);
      ptr++;
      copy_stq((func *)ptr, REG_T1, offset1, REG_T0);
      ptr++;
      offset2 += 8;
    }
    offset1 += 8;
  }

  /* closure */
  if(useClosure) {
    ptr = add_load(ptr, REG_A0, closure, gp);
  }
  
  /* target procedure */
  ptr = add_load(ptr, REG_T12, (long)proc, gp);
  
  /* the argument */
  copy_bis((func *)ptr, REG_T0, REG_T0, (useClosure) ? REG_A1 : REG_A0);
  ptr++;

  /* epilog */
  new_ptr = add_snippet(ptr, argument_epilog, argument_epilog_end);
  patch_short(ptr+1, (long)clone, (long)(ptr+1));
  offset = (int *)argument_epilog_sp - (int *)argument_epilog;
  set_disp((func *)(ptr + offset), stack_size);
  ptr = new_ptr;

  return (char *)ptr - (char *)clone;
}

  

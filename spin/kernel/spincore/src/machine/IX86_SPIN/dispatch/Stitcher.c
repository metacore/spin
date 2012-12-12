/*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Run-time code generation for the dispatcher
 *
 * HISTORY
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *	IX86 version.
 *
 */

#define NULL ((void *) 0)

typedef void (func)();
typedef unsigned long vm_offset_t;

/* 
 * pointers to templates
 */

/* trampoline_stub symbols */
extern func trampoline_stub;
extern func trampoline_stub_end;
#ifdef CALL_GRAPH
extern func trampoline_profile;
#endif
extern func trampoline_long_patch;
extern func trampoline_offset_patch;

#ifdef CALL_GRAPH

/*
 * the procedure to be called to do profiling
 */
extern func _mcount;
#endif CALL_GRAPH

/* 
 * function prototypes
 */

static char* disp_malloc(int);
func *clone_stub(func *stub, func *stub_end);

/*
 * debugging mode 
 */

static int stitch_debug = 0;

/******************************************************************************
 *
 * stub generation
 *
 *****************************************************************************/

func *CloneTrampolineStub(func *handler, int jtable_offset, int save)
{
  func *clone;
  vm_offset_t *ptr;

  /* create a clone */
  clone = (func *)clone_stub(trampoline_stub, trampoline_stub_end);

  /* patch the parameter */
  ptr = (vm_offset_t *) ((vm_offset_t) clone + 
			 (vm_offset_t) trampoline_long_patch - 
			 (vm_offset_t) trampoline_stub + 1);
  *ptr = (vm_offset_t) handler;

   /* patch the procedure offset */
  ptr = (vm_offset_t *) ((vm_offset_t) clone + 
			 (vm_offset_t) trampoline_offset_patch - 
			 (vm_offset_t) trampoline_stub + 1);
  *ptr = (vm_offset_t) jtable_offset;
  
  /* flush the i-cache */
  FlushInstructionCache();

  return clone;
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

/******************************************************************************
 *
 * initialization
 *
 *****************************************************************************/

#ifdef CALL_GRAPH
#define DispMemorySize 200 * 1024
#else
#define DispMemorySize 0
#endif

/*
 * HACK HACK HACK
 *
 * allocate memory from a contiguous range so that profiling doesn't get 
 * confused
 */

char *DispMemory;
char *DispMemoryEnd;
char *disp_mem_ptr;

char *disp_malloc(int size)
{
  char *ptr = disp_mem_ptr;
  int aligned_size = ((size + 7) & ~(0x7));

  /*
     printf("stitcher allocate: %d(%d) - start: 0x%lx cur: 0x%lx end: 0x%lx\n",
     size, aligned_size, DispMemory, disp_mem_ptr, DispMemoryEnd);
     */
  if(disp_mem_ptr + aligned_size <= DispMemoryEnd) {
    disp_mem_ptr += aligned_size;
    return ptr;
  } else {
#ifdef CALL_GRAPH
    printf("WARNING >>> stitcher ran out of its stash of memory\n");
#endif CALL_GRAPH
    return (char *)spin_malloc(size);
  }
}

void StitcherInitialize()
{
  DispMemory = (char *)spin_malloc(DispMemorySize);
  DispMemoryEnd = DispMemory + DispMemorySize;
  disp_mem_ptr = DispMemory;
}

void trace_counts_reset() 
{
  printf("cannot use event traces on PC\n");
}

void trace_counts_dump()
{
  printf("cannot dump event traces on PC\n");
}

/******************************************************************************
 *	
 *				LINEARIZED ARGUMENTS
 *
 *****************************************************************************/

extern func argument_stub;
extern func argument_stub_end;
extern func argument_long_patch;

extern func argument_cl_stub;
extern func argument_cl_stub_end;
extern func argument_cl_long_patch;
extern func argument_cl_cl_patch;

/*
 * A bypass procedure that callects all the arguments, puts them
 * in an open array allocated on stack and passes it as an argument
 * to the original procedure.
 */

func *CloneArgumentStub_old(void *proc, long n_args, long useClosure, long closure)
{
  func *clone;
  vm_offset_t *ptr;
  
  if(stitch_debug) {
    if(closure != 0) {
    printf("CloneArgumentStub: 0x%lx %d %d 0x%lx - 0x%lx 0x%lx 0x%lx\n",
	   proc, n_args, useClosure, closure,
	   ((long *)closure)[0], ((long *)closure)[1], ((long *)closure)[2]);
  }
  }

  if(!useClosure) {
    /* create a clone */
    clone = (func *)clone_stub(argument_stub, argument_stub_end);
    
    
    /* patch the procedure address */
    ptr = (vm_offset_t *) ((vm_offset_t) clone + 
			   (vm_offset_t) argument_long_patch - 
			   (vm_offset_t) argument_stub + 1);
    *ptr = (vm_offset_t) (proc) - (vm_offset_t)ptr - 4;    
  } else {
    /* create a clone */
    clone = (func *)clone_stub(argument_cl_stub, argument_cl_stub_end);
    
    /* patch the procedure address */
    ptr = (vm_offset_t *) ((vm_offset_t) clone + 
			   (vm_offset_t) argument_cl_long_patch - 
			   (vm_offset_t) argument_cl_stub + 1);
    *ptr = (vm_offset_t) (proc) - (vm_offset_t)ptr - 4;    

    /* patch the closure */
    ptr = (vm_offset_t *) ((vm_offset_t) clone + 
			   (vm_offset_t) argument_cl_cl_patch - 
			   (vm_offset_t) argument_cl_stub + 1);
    *ptr = (vm_offset_t) (closure);
  }

  /* flush the i-cache */
  FlushInstructionCache();

  if(stitch_debug) {
    printf("CloneArgumentStub done: 0x%lx\n", clone);
  }

  return clone;
}


extern func argument_prolog;
extern func argument_prolog_end;
extern func argument_push_arg;
extern func argument_push_arg_end;
extern func argument_push_ptr;
extern func argument_push_ptr_end;
extern func argument_push_array;
extern func argument_push_array_end;
extern func argument_push_closure;
extern func argument_push_closure_end;
extern func argument_call;
extern func argument_call_end;
extern func argument_epilog;
extern func argument_epilog_end;

static int argument_stub_size(void *proc, int n_args,
			      int useClosure, long closure);
static int clone_argument_stub(func *clone, void *proc, int n_args,
			       int useClosure, long closure);
static char *add_snippet(char *ptr, func *stub, func *stub_end);
static void set_long(char *ptr, long val);
static void set_offset(char *ptr, int offset);

func *CloneArgumentStub(void *proc, long n_args, long useClosure, long closure)
{
  func *clone;
  int   size, clone_size;
  vm_offset_t *ptr;
  
  if(stitch_debug) {
    printf("CloneArgumentStub: 0x%lx %d %d 0x%lx\n",
	   proc, n_args, useClosure, closure);
  }

  size = argument_stub_size(proc, n_args, useClosure, closure);

  if((clone = (func *)disp_malloc(size)) == NULL ) {
    printf("ERROR >> stitcher: no memory for a stub\n");
    return NULL;
  }
  
  clone_size = clone_argument_stub(clone, proc, n_args, useClosure, closure);

  if(clone_size != size) {
    printf("ERROR >> CloneArgumentStub: different size - %d instead of %d\n",
	   clone_size, size);
    return NULL;
  }

  FlushInstructionCache();

  if(stitch_debug) {
    printf("CloneArgumentStub done: 0x%lx\n", clone);
  }

  return clone;
}

static int argument_stub_size(void *proc, int n_args,
			      int useClosure, long closure)
{
  int size;

  size = ((char *)argument_prolog_end) - ((char *)argument_prolog);
  size += n_args * 
    (((char *)argument_push_arg_end) - ((char *)argument_push_arg));
  size += ((char *)argument_push_ptr_end) - ((char *)argument_push_ptr);
  size += ((char *)argument_push_array_end) - ((char *)argument_push_array);
  if(useClosure) {
    size += 
      ((char *)argument_push_closure_end) - ((char *)argument_push_closure);
  }
  size += ((char *)argument_call_end) - ((char *)argument_call);
  size += ((char *)argument_epilog_end) - ((char *)argument_epilog);

  return size;
}


static int clone_argument_stub(func *clone, void *proc, int n_args,
			       int useClosure, long closure)
{
  char *ptr;
  char *new_ptr;
  int i;

  ptr = (char *)clone;

  ptr = add_snippet(ptr, argument_prolog, argument_prolog_end);

  for(i = n_args-1; i >= 0; i--) {
    new_ptr =
      add_snippet(ptr, argument_push_arg, argument_push_arg_end);
    set_offset(ptr, 8 + i * 4);
    ptr = new_ptr;
  }

  ptr = add_snippet(ptr, argument_push_ptr, argument_push_ptr_end);

  new_ptr = add_snippet(ptr, argument_push_array, argument_push_array_end);
  set_long(ptr, n_args);
  ptr = new_ptr;

  if(useClosure) {
    new_ptr =
      add_snippet(ptr, argument_push_closure, argument_push_closure_end);
    set_long(ptr, closure);
    ptr = new_ptr;
  }

  new_ptr = add_snippet(ptr, argument_call, argument_call_end);
  set_long(ptr, ((char *)proc - (char *)ptr) - 5);
  ptr = new_ptr;
  
  ptr = add_snippet(ptr, argument_epilog, argument_epilog_end);

  return (char *)ptr - (char *)clone;
}

static char *add_snippet(char *ptr, func *stub, func *stub_end)
{
  int size = (char *)stub_end - (char *)stub;
  bcopy(stub, ptr, size);
  return (char *)((char *)ptr + size);
}

static void set_long(char *ptr, long val)
{
  *((long *)(ptr + 1)) = val;
}

static void set_offset(char *ptr, int offset)
{
  *(ptr + 2) = offset;
}

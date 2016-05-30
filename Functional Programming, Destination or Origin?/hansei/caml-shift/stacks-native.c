/*
 *------------------------------------------------------------------------
	   Native-code implementation of the scAPI:
        noting stack pointers and copying C stacks fragments

  This code deals with exception frames and stack fragments
  between two exception frames in NATIVE-code OCaml.

  The code is written for the i386 architecture (although it may work for
  ia64 and amd64). 

  This file implements two abstract data types: |ek| for exception frame
  pointers, and |ekfragment| for the stack fragment between two exception
  frames. These data types are used in delimcc.ml

  In Native OCaml, stacks grow downwards or upwards, depending on
  the architecture. At present, we do not support systems where the
  stack grows upwards (to the larger addresses): such systems seem
  too rare to bother with.
  Trap (exception) frames are linked, the global caml_exception_pointer 
  (if not NULL) points to the top trap frame.

 Justification for stack copying:

 The examination of asmcomp/i386/emit.mlp shows that the absolute stack 
 address is only manipulated in the pieces of the code related to
 exception handling: in Lpushtrap, Lpoptrap, and Lraise Cmm instructions.
 The absolute stack address is also used in the standard C frame linking,
 the sequence "pushl %ebp; movl %esp,%ebp". That sequence appears
 in OCaml-generated code only when the profiler is invoked; %ebp is 
 restored afterwards. We do not capture continuations from
 within the profiler.
 OCaml does not use C-style stack frames and C-style linking. With
 two exceptions, OCaml's native program stack does not contain absolute 
 stack pointers. Therefore, the stack or its parts may be copied.
 The two exceptions are:
  -- When OCaml calls C and the C code calls OCaml, the stack
     contains C code frames, which follow C-style linking.
     As usual, we should not capture continuations from a call-back
     that extends past the point the call-back has been invoked.
     Capturing C-code frames in continuations is always the invitation
     for trouble. Scheme systems have exactly the same limitation;
     call-backs from C code may not use call/cc, ever.
     Delimited continuations have an edge here: we can safely use
     continuations in a call-back provided that their scope is within
     the call-back.
     The file asmrun/fail.c shows another reason why capturing
     C stack frames is dangerous: we may overlook caml-local-roots,
     which are global roots to OCaml values that are registered
     by the C code, by FFI CAMLlocal macros.

  -- trap frames links. We have to adjust these links when copying
     the stack. We had to do the adjustment in the byte code too.

Exceptions are really stable points: proc.ml (on i386, amd64 and
ia64 architectures) says that all phys registers are destroyed by
'raise.' The exception handler would not assume that the register contain
anything useful: the register caches are `flushed', so to speak.

When a C function (such as the C functions in this file) is entered,
globals caml_last_return_address and caml_bottom_of_stack (lowest
stack address) are set.
The global caml_last_return_address is the address to return to the
ML code that invoked the C function.
The global caml_bottom_of_stack points to the last word of the last
ML stack frame. Typically -- certainly on i386 and amd64 --
caml_bottom_of_stack points to the first word of the stack before 
the return pointer to the ML code. That is, typically 
  caml_last_return_address == *((word*)caml_bottom_of_stack - 1)

On some architectures (e.g., amd64) the value of the
caml_exception_pointer is kept in a dedicated register while ML code
is running. The register is stored in the global
caml_exception_pointer only when a C function is called.  When the C
function returns, the register is typically NOT reloaded from the
global caml_exception_pointer. Therefore, changing the global within a
C function would have no effect on the operation of the ML code on
some architectures. The sole exception is raising an exception in the
C code. In that case, the current value of caml_exception_pointer is
loaded into its dedicated register and the exception handling is
initiated. Therefore, to stay portable, any C function that wishes to
modify caml_exception_pointer must exit abnormally, by
raising an exception.


The biggest challenge of capturing a portion of the native code stack
is that the native stack -- unlike the stack of the byte-code
interpreter -- contains unboxed values. We can't make the captured
portion of the stack an OCaml heap value.  We can't let GC scan the
captured stack as a regular heap value.  We have to know which words
on the stack are definitely the ML heap pointers. The compiler
provides this information for us in the form of frame tables.

See asmrun/stack.h for the format of the frame table and
asmrun/roots.c for the algorithm to scan the stack.

Other useful files
	otherlibs/systhreads/posix.c
	asmrun/i386.S

XXX refuse to operate when local_roots are non-zero?

Attempting to quantify costs:
`W5Direct.test5 ()' in bench_nondet runs at 27.82 sec.
Disabling stat_free (that is, letting the garbage accumulate), the
running time of the benchmark becomes 27.5 sec
Using the dummy allocator (and 200 MB of initial memory),
the running time becomes  27.4 sec.
  Conclusion: malloc/free are not the bottleneck.
If I remove 
  const frame_descr * fd1 = find_frame((char *)tp1);
  (which is needed only for checking)
the time is 27.6 sec.
Conclusion: find_frame is not the bottleneck.

Compiling -O2 and simplifying pop_stack_fragment:
the running time is 25.1 sec.

Simplifying API, transitioning to copy_stack_fragment:
the running time is 22.4 sec.

NB!! Although this code can be compiled with -O2 by GCC 4.2.1, 4.5 and
4.6, GCC 4.7 over-optimizes the code. To use GCC 4.7, one should either
disable optimizations, or do
	-O2 -fno-ipa-sra
Thanks to Anthony Tavener for investigation.

*/


#include "misc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stack.h"
#include "roots.h"
#include <string.h>
#include <custom.h>

#define DEBUG 0
/* For debugging */
#include <stdio.h>

extern void caml_raise_exception (const value bucket) Noreturn;


/* Assuming p is the pointer to the trap stack frame, return
   the pointer to the previous trap frame.
*/
#define Trap_link(p) (*(char **)(p))

#ifdef Mask_already_scanned
#define RetAddr_from_frame(sp) \
  (Mask_already_scanned(Saved_return_address(sp)))
#else
#define RetAddr_from_frame(sp) (Saved_return_address(sp))
#endif

/* The byte-code interpreter provides the global stack_high: the
   top of the stack, the largest address within the stack.
   That made it convenient to represent stack addresses as offsets
   from that high address -- when communicating stack addresses
   to OCaml. Alas, the native-code OCaml does not provides a similar
   global -- although something like that does exist in
   asmrun/signals_asm.c, but it is not exported.
   The following is a good approximation, as the base for
   computing offsets. It makes offsets generally small.
*/
#define Highest_stack_addr ((value*)(-1024))

/* Under no circumstances EVER EVER EVER should assert be disabled!!! */

#define myassert(x) ((x) ? 1 : \
  (fprintf(stderr, "\nFailed assert %s in %s at %d\n", #x,__FILE__, __LINE__),\
  exit(8)))


/* ---------------------------------------------------------------------- */
/* Printing out information about the ML stack and traversing it 
 */


/* Print the information about the Native Caml program stack */
static void print_gl_stack(const char * title)
{
  int local = 0;		/* for sure allocated on stack */
  fprintf(stderr, "\n%s\n",title);
  fprintf(stderr, "current esp               %p\n", &local);
  fprintf(stderr, "caml_bottom_of_stack      %p\n", 
	  caml_bottom_of_stack);
  fprintf(stderr, "caml_last_return_address  %lx\n", 
	  caml_last_return_address);
  fprintf(stderr, "caml_gc_regs              %p\n", caml_gc_regs);
  fprintf(stderr, "local_roots               %p\n", caml_local_roots);

  fprintf(stderr, "caml_exception_pointer    %p\n", caml_exception_pointer);
}

/* Walk the trap frame link */
static void print_exc_trace(const char * title)
{
  const char * p;
  fprintf(stderr, "\nexc_trace: %s\n",title);
  for(p = caml_exception_pointer; p; p = Trap_link(p))
    fprintf(stderr, "  %p\n",p);
}

/* Walk the stack and print its trace */
static void print_stack_trace(const char * title)
{
  const char * sp = caml_bottom_of_stack;
  uintnat retaddr = caml_last_return_address;

  const frame_descr * d;
#ifdef Stack_grows_upwards
  const short * p;  /* PR#4339: stack offsets are negative in this case */
#else
  const unsigned short * p;
#endif

  fprintf(stderr, "\nstack_trace: %s\n",title);
  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (sp != NULL) {
    fprintf(stderr, "sp       %p\n",sp);
    fprintf(stderr, "retaddr  %lx\n",retaddr);
    /* Find the descriptor corresponding to the return address */
    uintnat h = Hash_retaddr(retaddr);
    while(1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) 
	fprintf(stderr, 
		"Failed to find the descriptor for retaddr %lx; sp is %p\n",
		retaddr,sp), exit(8);
      if (d->retaddr == retaddr) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    if (d->frame_size != 0xFFFF) {
      int reg_count, n;
      for (p = d->live_ofs, n = d->num_live, reg_count = 0; n > 0; n--, p++) {
          const int ofs = *p;
          if (ofs & 1)
	    reg_count++;
      }
      fprintf(stderr, "ML frame: size %d,  live ptrs %d (in registers %d)\n",
	      d->frame_size, d->num_live, reg_count);
      /* Move to next frame */
#ifndef Stack_grows_upwards
      sp += (d->frame_size & 0xFFFC);
#else
      sp -= (d->frame_size & 0xFFFC);
#endif
      retaddr = RetAddr_from_frame(sp);
    } else {
      /* This marks the top of a stack chunk for an ML callback */
      struct caml_context * next_context = Callback_link(sp);
      fprintf(stderr, "C frame\n");
      sp = next_context->bottom_of_stack;
      retaddr = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
    }
  }
  fprintf(stderr, "=== end of stack_trace\n");
}


/* Given a pointer p, check to make sure p points to an
   ML stack frame.
   This is the pre-condition to cutting the stack at point p.
   We return the pointer to the frame table describing the found
   frame.
   Check that no registers contain live pointers?
*/
static const frame_descr * find_frame(const char * fp)
{
  const char * sp = caml_bottom_of_stack;
  uintnat retaddr = caml_last_return_address;

  const frame_descr * d;

  if (caml_frame_descriptors == NULL) caml_init_frame_descriptors();

  while (sp != NULL) {
    /* Find the descriptor corresponding to the return address */
    uintnat h = Hash_retaddr(retaddr);
    while(1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) 
	fprintf(stderr, 
		"Failed to find the descriptor for retaddr %lx; sp is %p\n",
		retaddr,sp), exit(8);
      if (d->retaddr == retaddr) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    if (d->frame_size != 0xFFFF) {
      if (sp == fp) 
	return d;
      /* Move to next frame */
#ifndef Stack_grows_upwards
      sp += (d->frame_size & 0xFFFC);
#else
      sp -= (d->frame_size & 0xFFFC);
#endif
      retaddr = RetAddr_from_frame(sp);
    } else {
      /* This marks the top of a stack chunk for an ML callback */
      struct caml_context * next_context = Callback_link(sp);
      sp = next_context->bottom_of_stack;
      retaddr = next_context->last_retaddr;
      /* A null sp means no more ML stack chunks; stop here. */
    }
  }
  fprintf(stderr, "Failed to find the ML frame for %p\n", fp);
  print_stack_trace("Stack at the point of failure");
  exit(8);
}

/* ---------------------------------------------------------------------- */
/* Implementing the abstract data type ekfragment
 *
 * The data type encapsulates a fragment of ML stack captured in a
 * continuation.
 * The fragment itself is contained in a structure ekfragment_struct,
 * which is malloc-ated. It is _not_ on the ML heap.
 * We register a custom GC scanning procedure to scan stack fragments.
 * All ekfragment_structs are linked in a double-linked list.
 * The pointer to ekfragment_struct is exported to ML as a value of an abstract
 * data type, with a finalization function.
 * When GC collects the ML value representing an ekfragment_struct,
 * we unlink and dispose of the structure.
 */

/* The first frame is the exception frame.
   We do not save GC regs or local roots: we should not have any.
   No live ML heap pointers should be in the registers.
*/
struct ekfragment_struct {
  struct ekfragment_struct * next;  /* Double linking of captured fragments */
  struct ekfragment_struct * prev;
  char * top_of_stack;              /* Pointer to a byte one past the saved
				       stack fragment */
  uintnat last_retaddr;             /* Saved caml_last_return_address */
  value stack_fragment[0];	    /* The stack fragment follows here */
};

typedef struct ekfragment_struct * ekfragment_t;

/* The double-linked list of live, captured ekfragments */
static ekfragment_t ekfragments = NULL;

/* Previous GC hook */
static void (*prev_scan_roots_hook) (scanning_action);
/* Check if we have hooked into GC already */
static int already_initialized = 0;

static void delimcc_scan_roots_ekfragments(scanning_action action);


#define EKFragment_val(v) \
  (* ((struct ekfragment_struct **) Data_custom_val(v)))

/* Custom finalization procedure: unlink the ekfragment_struct 
   and dispose of its memory
*/
static void delimcc_ekfragment_finalize(value wrapper)
{
  ekfragment_t ekp = EKFragment_val(wrapper);

  myassert( ekfragments != NULL );

#if defined(DEBUG) && DEBUG
  fprintf(stderr, 
	  "\ndelimcc_ekfragment_finalize for retaddr %lx\n",
	  ekp->last_retaddr);
#endif

  if( ekp->prev == NULL )
    ekfragments = ekp->next;
  else
    (ekp->prev)->next = ekp->next;
  if( ekp->next )
    (ekp->next)->prev = ekp->prev;

  /* We are about to deallocate dependent memory: see the comments
     before the call to caml_alloc_dependent_memory.
     We have to tell GC of the size of the deallocated memory, in bytes.
  */
  caml_free_dependent_memory((ekp->top_of_stack - (char*) ekp->stack_fragment) 
			     + sizeof(struct ekfragment_struct));
  caml_stat_free(ekp);
}

static struct custom_operations delimcc_ekfragment_ops = {
  "delimcc_ekfragment",
  delimcc_ekfragment_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Naive allocation, for testing. This is the fastest allocator
   one can imagine. There is no `free' operation: let the garbage
   accumulate. This is OK for testing, to get the baseline.
*/
#if 0
static void * naive_alloc(const size_t size)
{
  static char * area = NULL;
  static char * free_ptr = NULL;
  const int init_alloc = 200<<20; /* That's how much we need for
				     W5Direct.test5 ();; in bench_nondet!
				  */
  
  if( area == NULL ) {
    myassert( area = malloc(init_alloc) );
    free_ptr = area;
  }
  myassert( free_ptr + size + 16 < area + init_alloc );
  {
    char * p = free_ptr;
    free_ptr += (size + 15) & (-16);
    return p;
  }
}
#endif

/* Allocate a new ekfragment struct and enqueue it.
   The pointers tp1 and tp2 specify the fragment of the stack
   to copy.
   We assume that stack grows to the lower addresses, hence
   tp1 > tp2.
*/
static ekfragment_t new_ekfragment_t(const char * tp1,
				     const char * tp2)
{
  myassert(tp2 < tp1);		/* stack grows downwards */

  if (!already_initialized) {
    already_initialized = 1;
    prev_scan_roots_hook = caml_scan_roots_hook;
    caml_scan_roots_hook = delimcc_scan_roots_ekfragments;
  }

  ekfragment_t ekp = 
    caml_stat_alloc(sizeof(struct ekfragment_struct) + (tp1 - tp2));

  ekp->prev = NULL;
  ekp->next = ekfragments;
  if( ekfragments != NULL )
    ekfragments->prev = ekp;
  ekfragments = ekp;

  /* Although unneeded, it is nice to initialize everything */
  ekp->top_of_stack = NULL;
  ekp->last_retaddr = 0;

  /* Here we copy the part of the stack */
  memcpy( &(ekp->stack_fragment), tp2, (tp1-tp2));

  return ekp;
}


/*
   Custom root-scanning function, to scan the captured delimited
   continuation, the body of the custom data type ekfragment.
   The function delimcc_scan_roots_ekfragments should be installed 
   as a GC's root-scanning hook. The function is a simple
   iterator that invokes delimcc_scan_roots_ekfragment
   on each ekfragment_struct in the list.

   The function delimcc_scan_roots_ekfragment is a minor adjustment
   of the function `caml_do_local_roots' in asmrun/roots.c.
   The adjustment is needed to install a scanning termination
   criterion. The function caml_do_local_roots assumes that
   there is a C-stack frame with the NULL Callback_link underneath
   the very first ML stack frame. Encountering that C-stack frame
   terminates the scan. Since we have captured only the part of
   ML stack, we do not have the dedicated C-stack frame to terminate
   the stack. We need a different criterion to end the scanning
   action.
 */

static void delimcc_scan_roots_ekfragment(scanning_action action,
					  const ekfragment_t ekp)
{
  char * sp       = (char*)(ekp->stack_fragment);
  uintnat retaddr = ekp->last_retaddr;

  const frame_descr * d;
  uintnat h;
#ifdef Stack_grows_upwards
  const short * p;  /* PR#4339: stack offsets are negative in this case */
#else
  const unsigned short * p;
#endif

#if defined(DEBUG) && DEBUG
  fprintf(stderr, 
	  "\ndelimcc_scan_roots_ekfragment for retaddr %lx\n",
	  retaddr);
#endif

  while (sp != NULL) {
    #if defined(DEBUG) && DEBUG
    fprintf(stderr, "sp       %p\n",sp);
    fprintf(stderr, "retaddr  %lx\n",retaddr);
    #endif
    /* Find the descriptor corresponding to the return address */
    h = Hash_retaddr(retaddr);
    while(1) {
      d = caml_frame_descriptors[h];
      if (d == NULL) 
	fprintf(stderr, 
		"Failed to find the descriptor for retaddr %lx; sp is %p\n",
		retaddr,sp), exit(8);
      if (d->retaddr == retaddr) break;
      h = (h+1) & caml_frame_descriptors_mask;
    }
    if (d->frame_size != 0xFFFF) {
      /* Scan the roots in this frame */
      int n;
      for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
	const int ofs = *p;
	value * root;
	if (ofs & 1) {
	  fprintf(stderr, 
		  "ekfragment scan: found live register %d, retaddr %lx\n",
		  ofs >> 1,retaddr), exit(8);
	  /*             root = regs + (ofs >> 1); */
	} else {
	  root = (value *)(sp + ofs);
	}
	action (*root, root);
      }
      /* Move to next frame */
#ifndef Stack_grows_upwards
      sp += (d->frame_size & 0xFFFC);
#else
      sp -= (d->frame_size & 0xFFFC);
#endif

      /* Custom termination: scanned the whole stack fragment */
      if (sp == ekp->top_of_stack)
	break;

      retaddr = RetAddr_from_frame(sp);
    } else {
      /* This marks the top of a stack chunk for an ML callback.
	 Skip C portion of stack and continue with next ML stack chunk.
	 We must report an error. Otherwise, we should have adjusted the
	 Callback_link pointers when we captured the fragment!
      */
      fprintf(stderr, "Found a C stack frame in the captured fragment!"
	      "retaddr %lx, sp (copied) %p\n",retaddr,sp);
      /*
        struct caml_context * next_context = Callback_link(sp);
        sp = next_context->bottom_of_stack;
        retaddr = next_context->last_retaddr;
        regs = next_context->gc_regs;
      */
      /* A null sp means no more ML stack chunks; stop here. */
    }
  }
}

static void delimcc_scan_roots_ekfragments(scanning_action action)
{
  ekfragment_t ekp = NULL;

  for(ekp = ekfragments; ekp != NULL; ekp = ekp->next)
    delimcc_scan_roots_ekfragment(action,ekp);

  /* Run the previous hook if any */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}


/* ---------------------------------------------------------------------- */
/* Exported functions, implementing scAPI
 */

/* Get the current value of trapsp, or caml_exception_pointer,
   which must have been set.
   At the very least, it is word-aligned (on MacOS, I think, it is
   even 16-byte aligned). We are justified then in scaling it by 4.
   So, the return value is the offset to the caml_exception_pointer 
   in the units of value. 
   The result fits within OCaml's int (that is, Val_long).
   The return result is consistent (small and positive) with the result
   of the corresponding function for the byte-code interpreter.

   The function does NO ML heap allocations.
*/
value get_trapsp(const value unit)
{
  myassert(caml_exception_pointer > caml_bottom_of_stack);
  return Val_long( Highest_stack_addr - (value *)caml_exception_pointer );
}

/* Print out the value of trapsp. NO ML heap allocations.
 */
value dbg_print_trapsp_simple(const value title, const value k)
{
  const ptrdiff_t t = Long_val(k);
  value * const captured_k = Highest_stack_addr - t;
  fprintf(stderr, "%s; k %p (%x)\n",String_val(title),captured_k,t);
  return Val_unit;
}

value dbg_print_trapsp(const value k)
{
  const ptrdiff_t t = Long_val(k);
  value * const captured_k = Highest_stack_addr - t;
  print_gl_stack("dbg_print_trapsp");
  print_exc_trace("dbg_print_trapsp");
  print_stack_trace("dbg_print_trapsp");
  fprintf(stderr, "captured k %p (%x)\n",captured_k,t);
  return Val_unit;
}


/*
  external reset_trapsp : ek -> exn -> unit
  Set caml_trapsp to an earlier value that corresponds to ek
  and raise the exception exn. Since this code modified
  caml_trapsp, it does not return normally.
  This code may be regarded as a generalized `raise', which
  explicitly identifies the trap frame.
  No heap allocations.
*/

value reset_trapsp(const value val_ek, const value delimcc_exc)
  Noreturn;

value reset_trapsp(const value val_ek, const value delimcc_exc)
{
  char * const tp = (char *)(Highest_stack_addr - (ptrdiff_t)Long_val(val_ek));;
  const char *p;

#if defined(DEBUG) && DEBUG
  print_gl_stack("reset_trapsp");
  fprintf(stderr, "to %p\n",tp);
  print_exc_trace("reset_trapsp: before");
#endif

  /* We check the invariants */
  myassert(caml_exception_pointer > caml_bottom_of_stack);
  myassert(tp >= caml_exception_pointer);

  /* Check the invariant that tp must occur somewhere in the Trap_link
     chain
  */
  for(p=caml_exception_pointer; p == tp; p = Trap_link(p))
    if( p == NULL )
    { print_gl_stack("ERROR: tp is not found in the Trap_link chain!!!");
      print_exc_trace("ERROR: tp is not found...");
      myassert(0);
    }
      
  caml_exception_pointer = tp;		/* Reset the chain */
  caml_raise_exception(delimcc_exc);    /* does not return */
}


/*
  external copy_stack_fragment : ek1 -> ekfragment
  Copy the stack between ek1 and the current trapsp: from trapsp 
  upwards but not including ek1. Return the fragment of stack.
  The API design principle explained in the title comments demands
  this function not affect the state of the Caml machine.
  In particular, caml_trapsp must not be modified.

  The argument ek1 is NOT a heap value. This code does no ML
  heap allocation, we don't need to set local roots.

  Recall, OCaml calls the most recent ML stack frame, at the
  smallest memory address, as caml_bottom_of_stack.
*/

value copy_stack_fragment(const value vek1)
{
  char * const tp1 = (char*)(Highest_stack_addr - (ptrdiff_t)Long_val(vek1));
  char * const tp2 = caml_exception_pointer;
#if defined(DEBUG) && DEBUG
  const frame_descr * fd1 = find_frame((char *)tp1);
#endif
  const frame_descr * fd2 = find_frame((char *)tp2);

  myassert(tp2 < tp1);				/* stack grows downwards */
  const mlsize_t size = tp1 - tp2;		/* tp2 is more recent ptr */

#if defined(DEBUG) && DEBUG
  print_gl_stack(">>>>copy_stack_fragment");
  fprintf(stderr, "between %p and %p (size %ld)\n",tp2,tp1,size);
  print_exc_trace("copy_stack_fragment: before");
  print_stack_trace("copy_stack_fragment: before");
#endif

  /* Check the invariants  */
  myassert(tp2 > caml_bottom_of_stack);

  ekfragment_t ekp = new_ekfragment_t(tp1, tp2);
  char * const bottom_captured_stack =
    (char*)(ekp->stack_fragment); /* corresponds to tp2 */
  ekp->last_retaddr = fd2->retaddr;
  ekp->top_of_stack = bottom_captured_stack + size;

  /* Adjust the trap frame links in the copied code: make them relative to
     tp2, which is the address of the most recent trap frame, located
     at the smallest memory address.
     In other words, make trap frame links relative to 
     bottom_captured_stack.
  */
  char *p = tp2;
  while (1) {
    if( p == NULL )
      { print_gl_stack("ERROR: tp1 is not found in the Trap_link chain!!!");
        print_exc_trace("ERROR: tp1 is not found...");
        myassert(0);
      }
    char * q = Trap_link(p);
    if (q == tp1)
    {
      /* end of the chain */
      *(char **)(bottom_captured_stack + ((char*)&(Trap_link(p)) - tp2)) = NULL;
      break;
    }
    *(char **)(bottom_captured_stack + ((char*)&(Trap_link(p)) - tp2)) =
      (q - tp2) + NULL;
    p = q;
  }

  /* Since the allocated memory, the ekfragment itself plus wrapper,
     are allocated outside of the Caml heap and reply on the
     finalizer for deallocation, it is classsified as `dependent memory'.
     We tell GC its size, in bytes, so GC will adjust its speed.
  */
  caml_alloc_dependent_memory(size+sizeof(struct ekfragment_struct));

  /* Allocate the OCaml proxy for the ekfragment_struct, holding the
     pointer to it. This is the last operation.
     It may invoke GC!
     Check that we don't have any live ML heap pointers before that
     point.
  */
  value ekfragment_wrapper = caml_alloc_custom(&delimcc_ekfragment_ops,
					       sizeof(ekfragment_t),
					       0, 0);
  EKFragment_val(ekfragment_wrapper) = ekp;
  return ekfragment_wrapper;
}

/*
  Push the captured stack fragment back onto the stack

  Since OCaml calls the most recent ML stack frame as caml_bottom_of_stack,
  the captured stack fragment is pushed to the `bottom of the stack' in
  OCaml terminology. Contrary to its common meaning, push and pop in
  OCaml affect the `bottom' of the stack rather than its `top'.

  This is quite tricky as we are going to manipulate the stack
  we ourselves are using. Also, we should keep GC happy, that is,
  maintain the linkage of ML stack frames as described by the frametables.

  On entrance to this function, the stack has the following layout:

            |.................... |
	    |       C frame       |
	    -----------------------
            | last_return_address |  =  caml_last_return_address
	    -----------------------
	    |        ML frame     |  <- caml_bottom_of_stack
	    |.....................|
	    |      trap frame     |  <- caml_exception_pointer


  The captured fragment has the following layout
            |  trap frame bottom  |
	    | ................... |
	    -----------------------
	    | captured_last_addr  |

  First we reserve enough stack space, via alloca. Second, we invoke
  an auxiliary function push_stack_fragment_really. So, the stack has
  this layout

	    | push_really C frame |
	    -----------------------
            |       reserved      |
            |.................... |
	    |  push_stack C frame |
	    -----------------------
            | last_return_address |
	    -----------------------
	    |        ML frame     |  <- caml_bottom_of_stack
	    |.....................|
	    |      trap frame     |  <- caml_exception_pointer

  The function push_stack_fragment_really now copies the captured
  fragment (except the last word of it, captured_last_addr) onto
  the reserved space, *overwriting* the push_stack C frame. So,
  the stack will look as follows


	    | push_really C frame |
	    -----------------------
            |  optional filler    |
            |  ................   |
            |  trap frame bottom  |  <- caml_exception_pointer, bottom_of_stack
	    | ................... |
	    -----------------------
            | new_return_address  |
	    -----------------------
	    |      trap frame     |

  captured_last_addr is of no use: first of all, it pertains to the frame
  from which the captured fragment was split off. (In OCaml, the return
  address is the first word of the next frame rather than the last
  word of the current one.) Second, the captured stack fragment contains
  raise at the end, so it never normally returns. So, we overwrite
  captured_return_address with new_return_address, which is determined
  from the frame table associated with 'trap frame'. 

  The final arrangement makes the captured fragment to appear to
  immediately follow the ML trap frame, as desired -- reversing
  the situation that existed at capture. Now, the function
  push_stack_fragment_really, having overwritten the stack frame of
  its parent, can't return normally. So, it raises the delimcc_exc
  exception. The execution will continue from trap frame pointed to
  the caml_exception_pointer, which is the first frame of the pushed
  stack fragment.

  Alas, in native code, we can't check for stack overflow and
  resize the stack.

  The push_stack_fragment functions do NO ML heap allocations!

*/

#ifdef Stack_grows_upwards
#error "This code was written assuming that stack grows downwards"
#endif

/* As was explained above, the whole point of this function
   is to have a separate frame from push_stack_fragment.
   So, push_stack_fragment_really must not be inlined!
*/
static void push_stack_fragment_really(ekfragment_t ekp, 
				       char * reserved_area,
				       const frame_descr * fdtop,
				       const value delimcc_exc)
  __attribute__((noinline));


value push_stack_fragment(const value ekfragment, const value delimcc_exc)
{
  ekfragment_t ekp;
  const char * frag_bottom;
  const frame_descr * fdtop = find_frame(caml_exception_pointer);
  char *p;

  /*   myassert(Is_block(ekfragment) && Tag_val(ekfragment) == Custom_tag); */

  ekp = EKFragment_val(ekfragment);
  frag_bottom = (const char*)(ekp->stack_fragment);
  const mlsize_t size = ekp->top_of_stack - frag_bottom;

#if defined(DEBUG) && DEBUG
  print_gl_stack("<<<<push_stack_fragment");
  fprintf(stderr, "size of the fragment: %ld bytes\n",size);
  print_exc_trace("push_stack_fragment: before");
  print_stack_trace("push_stack_fragment: before");
#endif

  /* Can't check for stack overflow.... */
  p = alloca(size);
  push_stack_fragment_really(ekp, p, fdtop, delimcc_exc);
  /* should not be tail recursive, to keep the alloca-ted space */

  return Val_unit;
}

static void push_stack_fragment_really(ekfragment_t ekp, 
				       char * reserved_area,
				       const frame_descr * fdtop,
				       const value delimcc_exc)
{
  const char * frag_bottom = (const char*)(ekp->stack_fragment);
  const mlsize_t size = ekp->top_of_stack - frag_bottom;

  /* We are certain that caml_exception_pointer points to the ML frame,
     because we just found its frametable, fdtop.
  */
  char * const new_trapsp = caml_exception_pointer - size;

  memcpy(new_trapsp,frag_bottom,size);

  Saved_return_address(caml_exception_pointer) = fdtop->retaddr;

  /* adjust the links of trap frames in the fragment: convert 
     offsets to abs addresses, 
     connecting the copied frames to the existing frames
  */
  char *p;
  for (p = new_trapsp; Trap_link(p) != NULL; ) {
    char * pnew = new_trapsp + (Trap_link(p) - (char *)NULL);
    Trap_link(p) = pnew;
    p = pnew;
  }
  Trap_link(p) = caml_exception_pointer;
  caml_exception_pointer    = new_trapsp;

  /* Not strictly needed, but helps with traces */
  caml_bottom_of_stack = new_trapsp;
  caml_last_return_address = ekp->last_retaddr;

#if defined(DEBUG) && DEBUG
  print_exc_trace("push_stack_fragment: after");
  print_stack_trace("push_stack_fragment: after");
#endif

  caml_raise_exception(delimcc_exc); /* does not return */
}


/* Return the size of the captured stack in the units of value.
   No heap allocations.
 */
value size_stack_fragment(const value ekfragment)
{
  myassert(Is_block(ekfragment) && Tag_val(ekfragment) == Custom_tag);
  ekfragment_t ekp = EKFragment_val(ekfragment);
  return Val_long((value*)(ekp->top_of_stack) -
		  (value*)(ekp->stack_fragment));
}


/*
 * Simple routines to write a string to stderr or to report an error
   and quit.
   We use these routines rather than OCaml's own print_endline
   to avoid dealing with OCaml's channels, etc. and triggering other errors
   along the way.
   The functions do NO ML heap allocations.
 */

value dbg_fatal_error(const value message)
{
  myassert(Is_block(message) && Tag_val(message) == String_tag);
  caml_fatal_error(String_val(message));
  return Val_unit;		/* Doesn't return, actually */
}

value dbg_note(const value message)
{
  myassert(Is_block(message) && Tag_val(message) == String_tag);
  fprintf(stderr,"%s\n",String_val(message));
  return Val_unit;
}


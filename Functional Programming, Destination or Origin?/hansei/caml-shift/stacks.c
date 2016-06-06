/*
 *------------------------------------------------------------------------
	   Byte-code implementation of the scAPI:
        noting stack pointers and copying C stacks fragments

  This code deals exclusively with exception frames, and stack fragments
  between two exception frames in byte-code OCaml.

  In byte-code OCaml, stacks grow downwards, from caml_stack_high to 
  caml_stack_low (actually, until the threshold, after which the stack
  is reallocated). The bottom (most recent) exception frame is
  pointed to by caml_trapsp.
  The other exception frames are reachable via Trap_link(ef) field.

  See byterun/stacks.c, byterun/stacks.h and especially 
  byterun/interp.c

  The present code is a slight modification of 
  caml_realloc_stack() in byterun/stacks.c
  Indeed, Caml already copies stacks and adjusts trapsp. We also use
  a great deal of code from callcc.c by Xavier Leroy.

  This file implements two abstract data types: |ek| for exception frame
  pointers, and |ekfragment| for the stack fragment between two exception
  frames. These data types are used in delimcc.ml

  The type ek is actually int, or to be more precise, Value_int()
  from the OCaml point of view and ptrdiff_t from the C point of view.
  The pointer difference is in the units of `value'. Because `value'
  has a sufficient size to hold a pointer, ptrdiff_t in units of value
  will definitely fit within value even taking the tag bit into account.
  
  The `mark' ek is the relative exception frame pointer, the base being either
  caml_stack_high or the top (least recent) of the copied fragment.
  The pointer must be relative because the stack can be
  reallocated and because we copy the stack ourselves.
  The reallocation cannot occur while the control is in any of the functions
  of this file (re-allocation may only occur only on APPLY and similar
  bytecode instruction).

  ekfragment is the copy of a part of the stack, from
  some trapsp up to (but not including) some other exception frame
  down the Trap_link.

  The design of the API is governed by the principle that a C function
  invoked by the Caml machine and returning normally must not modify the 
  state of the Caml machine (that is, must not change caml_stack_high or
  caml_stack_low or caml_trapsp).
  If a C function does need to change caml_trapsp (as some of our functions
  do), the function must exit abnormally, by raising an exception.
  This principle explains the interface of the functions reset_trapsp 
  and push_stack_fragment.
  Strictly speaking, this principle is not required for the OCaml byte-code.
  It is necessary for compatibility with the version of this file for
  native-code OCaml, see stacks-native.c. See the latter file for
  justification.

  $Id: stacks.c,v 1.5 2006/02/02 01:29:28 oleg Exp $

 *------------------------------------------------------------------------
 */

#include <string.h>
#include "misc.h"
#include <alloc.h>
#include "memory.h"
#include <fail.h>
#include <mlvalues.h>
#include <stacks.h>

#define DEBUG 0
/* For debugging */
#include <stdio.h>

/* This function is defined in stacks.h and implemented in the ocamlrun.
   We re-define it here with the weak attribute: although 
   the function is implemented by ocamlrun and so will be available
   at run-time, it is not exported by ocamlc. Unfortunately, ocamlc, 
   when linking the bytecode executable and checking dlls requires 
   that all symbols imported by dlls be satisfied at link time.
*/
void caml_realloc_stack (asize_t required_size)
  __attribute__ ((weak));


/* Under no circumstances EVER EVER EVER should assert be disabled!!! */

#define myassert(x) ((x) ? 1 : \
  (fprintf(stderr, "\nFailed assert %s in %s at %d\n", #x,__FILE__, __LINE__),\
  exit(8)))


/* Print the information about Caml interpreter stacks */
static void print_gl_stack(char * title)
{
  fprintf(stderr, "\n%s\n",title);
  fprintf(stderr, "caml_stack_low       %p\n", caml_stack_low);
  fprintf(stderr, "caml_stack_high      %p\n", caml_stack_high);
  fprintf(stderr, "caml_stack_threshold %p\n", caml_stack_threshold);
  fprintf(stderr, "caml_extern_sp       %p\n", caml_extern_sp);
  fprintf(stderr, "caml_trapsp          %p\n", caml_trapsp);
}

/* Walk the trapsp link */
static void print_exc_trace(char * title)
{
  value * p;
  fprintf(stderr, "\nexc_trace: %s\n",title);
  fprintf(stderr, "caml_trapsp          %p\n", caml_trapsp);
  for(p = caml_trapsp; p < caml_stack_high; p = Trap_link(p))
    fprintf(stderr, "  %p\n",p);
}

/* ---------------------------------------------------------------------- */
/* Exported functions, implementing scAPI
 */

/* Get the current value of trapsp (relative to caml_stack_high)
   NO ML heap allocations.
*/
value get_trapsp(const value unit)
{
  myassert(caml_trapsp    <= caml_stack_high);
  return Val_long(caml_stack_high - caml_trapsp);
}

/* Print out the value of trapsp. NO ML heap allocations.
*/
value dbg_print_trapsp(const value k)
{
  const ptrdiff_t t = Long_val(k);
  value * const captured_k = caml_stack_high - t;
  print_gl_stack("dbg_print_trapsp");
  fprintf(stderr, "captured k %p (%x)\n",captured_k,t);
  return Val_unit;
}

/* Print out the value of trapsp. NO ML heap allocations.
 */
value dbg_print_trapsp_simple(const value title, const value k)
{
  const ptrdiff_t t = Long_val(k);
  value * const captured_k = caml_stack_high - t;
  fprintf(stderr, "%s; k %p (%x)\n",String_val(title),captured_k,t);
  return Val_unit;
}

/* For historical interest only */
#if 0
/* add_trapsp (ek1,ek2) --> ek1 + ek2
*/
value add_trapsp(value block_twok)
{
  const value ek1v = Field(block_twok, 0);
  const value ek2v = Field(block_twok, 1);

  return Val_long(Long_val(ek1v) + Long_val(ek2v));
}

/* sub_trapsp (ek1,ek2) --> ek1 - ek2
*/
value sub_trapsp(value block_twok)
{
  const ptrdiff_t ek1 = Long_val(Field(block_twok, 0));
  const ptrdiff_t ek2 = Long_val(Field(block_twok, 1));

  myassert( ek1 >= ek2 );

  return Val_long(ek1 - ek2);
}
#endif


/*
  external copy_stack_fragment : ek1 -> ekfragment
  Copy the stack between ek1 and the current trapsp: from trapsp 
  upwards but not including ek1. Return the fragment of stack.
  The API design principle explained in the title comments demands
  this function not affect the state of the Caml machine.
  In particular, caml_trapsp must not be modified.

  The argument ek1 is NOT a heap value. Although this code does ML
  heap allocation (only once),  we don't need to set local roots.

  This code is largely based on callcc.c by Xavier Leroy.
*/

value copy_stack_fragment(const value vek1)
{
  value * const tp1 = caml_stack_high - (ptrdiff_t)Long_val(vek1);
  value * const tp2 = caml_trapsp;
  value block;

  myassert(tp2 < tp1);				/* stack grows downwards */
  const mlsize_t size = tp1 - tp2;		/* tp2 is more recent ptr */

#if defined(DEBUG) && DEBUG
  print_gl_stack("copy_stack_fragment");
  fprintf(stderr, "between %p and %p (size %ld)\n",tp2,tp1,size);
  print_exc_trace("copy_stack_fragment: before");
#endif

  if (size < Max_young_wosize) {
    block = alloc(size, 0);
    memcpy(&Field(block, 0), tp2, size * sizeof(value));
  } else {
    block = alloc_shr(size, 0);
    mlsize_t i;
    for (i = 0; i < size; i++)
      initialize(&Field(block, i), tp2[i]);
  }

  /* We check the invariants after the allocation of block, which may
     cause a GC run. Stack should not be moved though. */
  myassert(tp1            < caml_stack_high &&
	   caml_extern_sp <  tp2);

 
  /* Adjust the links in the copied code: make them relative to
     tp2: the bottom of the copied stack
  */
  value *p = tp2;
  while (1) {
    if( !(p < caml_stack_high) )
      { print_gl_stack("ERROR: tp1 is not found in the Trap_link chain!!!");
        print_exc_trace("ERROR: tp1 is not found...");
        myassert(0);
      }
    value *q = Trap_link(p);
    if (q == tp1)
    {
      /* end of the chain */
      Field(block, (value*)(&(Trap_link(p))) - tp2) = Val_long(0);
      break;
    }
    Field(block, (value*)(&(Trap_link(p))) - tp2) = Val_long(q - tp2);
    p = q;
  }
  return block;
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
  value * const tp = caml_stack_high - Long_val(val_ek);

#if defined(DEBUG) && DEBUG
  print_gl_stack("reset_trapsp");
  fprintf(stderr, "to %p\n",tp);
  print_exc_trace("reset_trapsp: before");
#endif

  /* We check the invariants */
  myassert(caml_extern_sp >= caml_stack_low &&
	   caml_extern_sp <= caml_stack_high);
  myassert(caml_extern_sp <  tp && tp >= caml_trapsp && 
	   caml_trapsp    <  caml_stack_high);

  /* Check the invariant that tp must occur somewhere in the Trap_link
     chain
  */
  value *p;
  for(p=caml_trapsp; p == tp; p = Trap_link(p))
    if( !(p < caml_stack_high) )
    { print_gl_stack("ERROR: tp is not found in the Trap_link chain!!!");
      print_exc_trace("ERROR: tp is not found...");
      myassert(0);
    }
      
  caml_trapsp = tp;		/* Reset the chain */
  caml_raise(delimcc_exc);	/* No return */
}


/*
  push the stack fragment between the caml_extern_sp and caml_trapsp
  so that the current exception frame links to the top of the
  pushed fragment.
  Set caml_trapsp to the bottom of the pushed fragment and raise
  the exception delimcc_exc. The function does not return.
  The stack fragment is a part of the stack between two exception
  frames (exclusive of the top, inclusive of the bottom frames).

  This code is largely based on callcc.c by Xavier Leroy.
*/

value push_stack_fragment(const value ekfragment, const value delimcc_exc)
  Noreturn;

value push_stack_fragment(const value ekfragment, const value delimcc_exc)
{
  const mlsize_t size = Wosize_val(ekfragment);

 redo:
#if defined(DEBUG) && DEBUG
  print_gl_stack("push_stack_fragment");
  print_exc_trace("push_stack_fragment: before");
#endif

  myassert(caml_extern_sp >= caml_stack_low &&
	   caml_extern_sp <= caml_stack_high);
  myassert(caml_trapsp    < caml_stack_high &&
	   caml_trapsp    > caml_extern_sp);

  /* The following is essentially the functionality of the
     primitive `ensure_stack_space' that appeared in 3.11
  */
  if( caml_extern_sp - size < caml_stack_low )
  {
    print_gl_stack("Reallocating OCaml stack!");
    caml_realloc_stack(size);
    goto redo;
  }

  value * const new_trapsp = caml_trapsp - size;
  value * const new_sp = new_trapsp;
/*   new_sp = caml_extern_sp - size; */
/*   memmove(new_sp, caml_extern_sp,  */
/* 	          (caml_trapsp - caml_extern_sp) * sizeof(value)); */
  memcpy(new_trapsp, &Field(ekfragment,0), size * sizeof(value));

  /* adjust the links of exc frames (convert to abs addresses)
     and connect the copied frames to the existing frames
  */
  value *p;
  for (p = new_trapsp; (value)Trap_link(p) != Val_long(0); p = Trap_link(p)) {
    myassert( p < caml_stack_high );
    Trap_link(p) = new_trapsp + Long_val((value) Trap_link(p));
  }
  Trap_link(p) = caml_trapsp;
  caml_extern_sp = new_sp;
  caml_trapsp    = new_trapsp;

#if defined(DEBUG) && DEBUG
  print_exc_trace("push_stack_fragment: after");
#endif

  caml_raise(delimcc_exc);	/* No return */
}

/* Return the size of the captured stack in the units of value.
   No heap allocations.
 */
value size_stack_fragment(const value ekfragment)
{
  return Val_long(Wosize_val(ekfragment));
}

/*
 * Simple routines to write a string to stderr or to report an error
   and quit.
   We use these routines rather than OCaml's own print_endline
   to avoid dealing with OCaml's channels, etc. and triggering other errors
   along the way.
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


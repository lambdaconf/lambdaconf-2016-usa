  A direct implementation of persistent delimited continuations
	       for byte- and native-code OCaml
		    The delimcc library

OVERVIEW:

  The library delimcc implements multi-prompt delimited control operators
for byte- and native-code OCaml. The library implements the superset 
of the interface proposed by Dybvig, Sabry, and Peyton-Jones:

   A Monadic Framework for Delimited Continuations
   R. Kent Dybvig, Simon Peyton Jones, and Amr Sabry
   JFP, v17, N6, pp. 687--730, 2007.
   http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR615

The framework supports `static' (shift/reset) and `dynamic'
(prompt/control, shift0, control0) delimited continuation operators
with multiple, arbitrarily typed prompts.  Please see testd0.ml and
the benchmarks for the examples of using shift/reset in OCaml
programs. Please see lwc.ml and sieve.ml for a lightweight concurrency
library implemented with delimcc.

The delimcc implementation is direct: it copies only the relevant
fragment of the OCaml stack; The stack or its fragments are never
inspected however. The implementation is fully integrated with OCaml
exceptions: exception handlers may be captured in delimited
continuations (and re-instated when the captured continuation is
installed); exceptions remove the prompts along the way. The
implementation has no typing problems, no bizarre 'a cont types, and
no use for magic. The implementation does no patching to the OCaml
system and is a regular (static or dynamically-loaded) library.  The
library is therefore perfectly compatible with any existing OCaml
code, source or binary.

The native- and byte-code versions of the library implement the
identical interface, described in delimcc.mli. Furthermore,
the OCaml code for the two versions is identical. Only the C code
files, implementing scAPI, differ for native-code.


The byte-code portion of the library has been tested for OCaml 3.08,
3.09, 3.10.2, 3.11, 3.12.0, 4.00.1 on i386 and amd64 Linux and FreeBSD
platforms.  The native-code portion of the library has been tested
with OCaml 3.11, 3.12.0 and 4.00.1 on i386 and amd64 Linux and FreeBSD
platforms.

Caution! When using GCC 4.7 to compile stacks-native.c (the part of
the library for the native OCaml), either disable optimizations, or use
the flags -O2 -fno-ipa-sra
Thanks to Anthony Tavener for investigation.


The implementation is described in the TCS paper (the extended
version of FLOPS 2010),
http://okmij.org/ftp/continuations/


The library is distributed under the MIT license.


INSTALLATION FOR UNIX (including Linux, and Cygwin):

Unless you are on i386 or amd64 platform, you need a copy of the OCaml source
distribution to compile this library. This is because we need some
files that are not normally installed by an OCaml distribution. For
the common case of i386 and amd64 platform, the present distribution includes
suitable files, which may be sufficient. There are three sets of suitable
files (for different versions of OCaml), arranged in the
directories ocaml-byterun-3.09/, ocaml-byterun-3.10/, and
ocaml-byterun-3.11/ (the latter supports OCaml 3.12.0 too).

Please examine the Makefile and adjust OCAMLINCLUDES and OCAMLBIN 
variables if necessary. Please see the Makefile for available targets.

INSTALLATION FOR MacOS X

Paul Snively reports (caml-list, Apr 2013) of compiling delimcc on 
Mac OS X 10.6.8:

When linking byte-code executables containing delimcc, one should use
the byte-code ocamlc (the byte-code compiler that is running in
byte-code) rather than ocamlc.opt (the byte-code compiler that is
running natively).  The source files may be compiled with ocamlc.opt;
ocamlc is needed only at the final stage of linking the executable.

For the native delimcc, one has to use GCC 4.6.2 to compile
stacks-native.c.  Apple's GCC 4.2.1 for Mac OS X 10.6.8 or clang 3.2
lead to problems.


INSTALLATION FOR MS WINDOWS:
  Not tested. 


USAGE:

Byte-code:

  ocamlc -o myprogram delimcc.cma <the .cmo files for myprogram>

If you compile the top-level (see `make top'), you can use delimited
continuation operators in interactive OCaml sessions.

For MacOS, be sure to use ocamlc rather than ocamlc.opt for linking.

Native code:

  ocamlopt -o myprogram -cclib -L. delimcc.cmxa <the .cmx files for myprogram>

assuming that libdelimccopt.a and dlldelimccopt.so are in the current
directory. Otherwise, "-cclib -L." has to be modified to include the
path to the libraries.

For reference, see the targets testd0 and testd0opt in the Makefile.


USAGE NOTES:

One should keep in mind the fundamental limitation of native-code
delimited continuations -- the limitation that also holds for Scheme
and other systems with delimited control. The limitation concerns
capturing continuations in the OCaml code invoked as a call-back from
C code, which was, in turn, called from OCaml. It is safe to capture a
delimited continuation in the callback _provided_ the extent of the
captured continuation is limited to the call-back. The continuation
captured in the callback may be resumed within the call-back or after
the call-back has exited.  One must not capture the continuation
through the C code that invoked the call-back. In short, no C frames
may be captured in delimited continuations. The library detects C
frames in captured continuations, and throws a run-time error. Such a
behavior of delimcc is consistent with the behavior of native-code
Scheme systems.



VERSION: August 2013


MANIFEST


Makefile			How to build it all
META				Findlib description


        Implementation
delimcc.mli			Library interface
delimcc.ml			Library implementation

stacks.c			Implementation of scAPI for byte-code
stacks-native.c			Implementation of scAPI for native code

delim_serialize.c		Serialization support for byte-code
				Serialization for native code is not yet
				supported.

		A collection of C include files from the OCaml distribution 
		needed for the compilation of stacks.c. The collections
		below are for the i386 and amd64 platform and distributed for
		convenience.
		For other platforms, use OCaml distribution.
ocaml-byterun-3.11/		for OCaml 3.11.1 and higher (i386 and amd64)
ocaml-byterun-3.10/		for OCaml 3.10.2
ocaml-byterun-3.09/		for OCaml 3.10.1 and earlier


        Tests
testd0.ml			Delimited control regression test suite
tests0.ml			Continuation serialization tests


	Memory leak tests
memory_leak_plugged.ml		No memory leak on successive serializations
memory_leak1.ml			No memory leak in resuming the
				ever-running thread

	Sample applications
paper_example.ml		Example from the delimcc paper, Sec 2

lwc.mli                         Light-weight concurrency library with MVAR
lwc.ml                          as the synchronization primitive
sieve.ml                        Concurrent Eratosthenes sieve


	Benchmarks
bench_exc.ml			Micro-benchmark comparing delimcc's abort
				with native OCaml exceptions

bench_nondet.ml			Micro-benchmark of direct, monadic and CPS
				implementations of non-determinism

bench_coroutine.ml		The co-routine benchmark by Xavier Leroy
				from his ocaml-callcc library: comparing
				undelimited and delimited continuations in
				performance and expressivity


CHANGES since the August 2010 version of the library:
  -- incorporating Christophe Deleuze's patches and suggestions
     about using caml_alloc_dependent_memory in native code
     delimcc. GC is able to better adjust its speed of running
     finalizers, resulting in fewer GC cycles and less, sometimes
     significantly less, running time.
  -- A new benchmark: lightweight concurrency library and
     the concurrent Eratosthenes sieve. It is based on the code
     kindly sent by Christophe Deleuze.
  -- Small adjustments to the code, making it more understandable
  -- Added notes on compiling and using delimcc on MacOS X and with
     later versions of GCC.

CHANGES since the April 2010 version of the library:
  -- the first release of the native code implementation, on i386 and amd64
     platforms

  -- even more simplified implementation


CHANGES since the July 2008 version of the library:
  -- greatly modified and simplified implementation. It is about 10% faster, 
     taking less memory. According to memory_leaked_plugged test,
     the size of the stored continuation decreased from 437 bytes 
     to 387 bytes in this version.

  -- new exported function shift0, capturing a frequent pattern

  -- added micro-benchmarks


CHANGES since the April 2008 version of the library:
  -- new primitive, push_prompt_subcont, to push a captured continuation
     along with the control delimiter. The primitive is used to implement
     shift without leaking memory. See the test memory_leak.ml.

  -- many optimizations, which reduce the size of captured continuations


CHANGES since the 2006 version of the library:
  -- persistent, twice delimited continuations

  -- addition of shift, control, abort, is_prompt_set
     and various debugging facilities (of which the most notable
     is show_val, to describe the structure of an OCaml value).
     The operations shift and control are implemented solely in terms
     of the basic take_subcont and push_subcont and included in the
     library merely for convenience. On the other hand, abort -- although
     too expressible in terms of take_subcont -- is implemented natively
     for efficiency. Now abort is literally raise.


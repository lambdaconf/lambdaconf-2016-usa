# Building the library of delimited continuations and the corresponding ocaml
# top level
#
# 	make all
#	     to build byte-code libraries
# 	make opt
#	     to build native-code libraries
#	make top
#	     to build the top level
#	make install
#	     to install the libraries
#	make testd0
#	make testd0opt
#	     to build and run the tests
#	make tests0
#	     to build and run the continuation serialization test
#	make memory_leak1
#	make memory_leak1opt
#	     to build and run the memory leak test. It should run forever
#	     in constant memory
#	make memory_leak_plugged
#	     to build and run another memory leak test. See the comments
#	     in the source code.
#       make bench_exc
#       make bench_excopt
#	     to benchmark delimcc's abort, comparing with native exceptions
#       make sieve
#       make sieveopt
#	     to run the Eratosthenes sieve concurrency benchmark
#
# This Makefile is based on the one included in the callcc distribution
# by Xavier Leroy
#
# $Id: Makefile,v 1.6 2006/02/07 00:33:52 oleg Exp $

# To compile the library, we need a few files that are not normally
# installed by the OCaml distribution.
# We only need .h files from the directory $OCAMLSOURCES/byterun/
# If you don't have the OCaml distribution handy, the present distribution
# contains the copy, in the directory ocaml-byterun
# That copy corresponds to the ia32 (x86) platform. For other platforms,
# you really need a configured OCaml distribution.

#OCAMLINCLUDES=../../byterun
#OCAMLINCLUDES=./ocaml-byterun-3.09
#OCAMLINCLUDES=./ocaml-byterun-3.10
OCAMLINCLUDES=./ocaml-byterun-3.11
# Directory of your OCAML executables...
#OCAMLBIN=../../bin
OCAMLBIN := $(shell dirname `which ocaml`)
OCAMLC=$(OCAMLBIN)/ocamlc
LIBDIR := $(shell ocamlc -where)

# The following Makefile.config will set ARCH, MODEL, SYSTEM
include $(LIBDIR)/Makefile.config

# CFLAGS=$(FLAGS) -O $(NATIVECCCOMPOPTS)
# DFLAGS=$(FLAGS) -g -DDEBUG $(NATIVECCCOMPOPTS)

OCAMLOPT=$(OCAMLBIN)/ocamlopt
OCAMLTOP=$(OCAMLBIN)/ocaml
OCAMLMKLIB=$(OCAMLBIN)/ocamlmklib
OCAMLMKTOP=$(OCAMLBIN)/ocamlmktop
OCAMLFIND=$(OCAMLBIN)/ocamlfind
OCAMLFIND_INSTFLAGS=
DESTDIR=
STDINCLUDES=$(LIBDIR)/caml
STUBLIBDIR=$(LIBDIR)/stublibs
# CC=$(NATIVECC)
CC=gcc
CFLAGS=-fPIC -Wall -I$(OCAMLINCLUDES) -I$(STDINCLUDES) -O2
NATIVEFLAGS=-DCAML_NAME_SPACE -DNATIVE_CODE \
       -DTARGET_$(ARCH) -DSYS_$(SYSTEM)
RANLIB=ranlib


.SUFFIXES: .ml .mli .cmo .cmi .cmx .tex .pdf

all: libdelimcc.a delimcc.cma
opt: libdelimccopt.a delimcc.cmxa

libdelimcc.a: stacks.o delim_serialize.o
	$(OCAMLMKLIB) -oc delimcc -dllpath . stacks.o delim_serialize.o

delimcc.cma: delimcc.cmo
	$(OCAMLMKLIB) -o delimcc -oc delimcc -dllpath . delimcc.cmo

libdelimccopt.a: stacks-native.o delim_serialize.o
	$(OCAMLMKLIB) -oc delimccopt -dllpath . \
	stacks-native.o delim_serialize.o

delimcc.cmxa: delimcc.cmx
	$(OCAMLMKLIB) -o delimcc -oc delimccopt -dllpath .  delimcc.cmx

install:
	if test -f dlldelimcc.so; then cp dlldelimcc.so $(STUBLIBDIR); fi
	cp libdelimcc.a $(LIBDIR) && \
	  cd $(LIBDIR) && $(RANLIB) libdelimcc.a
	cp delimcc.cma delimcc.cmi $(LIBDIR)
	if test -f dlldelimccopt.so; then \
	  cp dlldelimccopt.so $(STUBLIBDIR); fi
	if test -f libdelimccopt.a;  then \
	  cp libdelimccopt.a $(LIBDIR); \
	  cd $(LIBDIR) && $(RANLIB) libdelimccopt.a; fi
	if test -f delimcc.cmxa; then cp delimcc.cmxa $(LIBDIR); fi

findlib-install: META dlldelimcc.so libdelimcc.a delimcc.cma delimcc.cmi \
	dlldelimccopt.so libdelimccopt.a delimcc.cmxa
	$(OCAMLFIND) install $(OCAMLFIND_INSTFLAGS) delimcc $^


.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

delimcc.cmx: delimcc.cmi
delimcc.cmo: delimcc.cmi

# When using GCC 4.7, add the flag -fno-ipa-sra
stacks-native.o: stacks-native.c
	$(CC) -c $(NATIVEFLAGS) -O2 -fPIC -Wall \
	-I$(OCAMLINCLUDES) -I$(STDINCLUDES) stacks-native.c

top:	libdelimcc.a delimcc.cma
	$(OCAMLMKTOP) -o ocamltopcc delimcc.cma

.PRECIOUS: testd0
testd0: libdelimcc.a testd0.ml delimcc.cmi
	$(OCAMLC) -o $@ -dllpath . delimcc.cma $@.ml
	./testd0

testd0opt: libdelimccopt.a testd0.ml delimcc.cmi
	$(OCAMLOPT) -o $@ -cclib -L. delimcc.cmxa  testd0.ml
	./testd0opt

# serialization test
.PRECIOUS: tests0
tests0: libdelimcc.a tests0.ml delimcc.cmi
	$(OCAMLC) -o $@ -dllpath . delimcc.cma $@.ml
	./$@
	./$@ /tmp/k1

clean::
	rm -f testd0 tests0 testd0opt


clean::
	rm -f *.cm[ixo] *.[oa] *~

clean::
	rm -f delimcc.a delimcc.cma libdelimcc.a dlldelimcc.so \
	delimcc.cmxa libdelimccopt.a dlldelimccopt.so


memory%: libdelimcc.a memory%.ml  delimcc.cmi
	$(OCAMLC) -o $@ -dllpath . delimcc.cma $@.ml
	./$@ > /dev/null

memory%opt: libdelimccopt.a memory%.ml delimcc.cmi
	$(OCAMLOPT) -o $@ -cclib -L. delimcc.cmxa memory$*.ml
	./$@ > /dev/null

%: libdelimcc.a %.ml delimcc.cmi
	$(OCAMLC) -o $@ -dllpath . delimcc.cma $@.ml
	./$@

%opt: libdelimccopt.a %.ml delimcc.cmi
	$(OCAMLOPT) -o $@ -cclib -L. delimcc.cmxa $*.ml
	./$@

sieve: libdelimcc.a lwc.cmi lwc.ml sieve.ml
	$(OCAMLC) -o $@ -dllpath . unix.cma delimcc.cma lwc.ml $@.ml
	./$@

sieveopt: libdelimccopt.a lwc.cmi lwc.ml sieve.ml
	$(OCAMLOPT) -o $@ -cclib -L. unix.cmxa delimcc.cmxa lwc.ml sieve.ml
	./$@

clean::
	rm -f sieve sieveopt

clean::
	rm -f memory_leak1 memory_leak1opt memory_leak_plugged


clean::
	rm -f bench_exc bench_excopt


export BIBTEX := bibtex -min-crossrefs=9999

.tex.pdf:
	texi2dvi -b --pdf $<


# Used during the development
.PRECIOUS: try-native
try-native: stacks-native.o delim_serialize.o delimccopt.ml
	$(OCAMLOPT) -o $@ \
	stacks-native.o delim_serialize.o delimccopt.ml 
	./$@


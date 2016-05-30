# HANSEI: OCaml library for direct probabilitic programming
#
# The library requires the caml-shift library, delimcc.cma and the
# correspondent .mli files. See caml-shift.tar.gz.
# Please adjust DELIMCC= below to point to the library.
#
# The typical command to use the OCaml top-level:
# $ ocaml -I ../../Continuations/caml-shift/ \
#	../../Continuations/caml-shift/delimcc.cma
# and then do #load "prob.cma";;

# $Id$

DELIMCC=./caml-shift
OCAMLC=ocamlc
OCAMLOPT=ocamlopt

DELIMCCRUN=-I $(DELIMCC) -dllpath $(DELIMCC) $(DELIMCC)/delimcc.cma
DELIMCCINT=$(DELIMCC)/delimcc.cmi
DELIMCCOPT=-I $(DELIMCC)  $(DELIMCC)/delimcc.cmxa

LIB=prob.cma
LIBOPT=prob.cmxa

.SUFFIXES: .ml .mli .cmi .cmo .cmx .cmxa .dvi .pdf .ps .tex .eps .jpg .png .bbl

%.cmi: %.mli
	$(OCAMLC) -c -I $(DELIMCC) $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c -I $(DELIMCC) $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c -I $(DELIMCC) $<

.PHONY: lib libopt
lib: prob.cma
libopt: prob.cmxa

prob.cma: pMap.cmo pMap.cmi ptypes.cmi inference.cmi inference.cmo \
	probM.cmo probM.cmi
	$(OCAMLC) -a -o $@ $(filter-out %.cmi,$^)

prob.cmxa: pMap.cmx pMap.cmi ptypes.cmi inference.cmi inference.cmx \
	probM.cmx probM.cmi
	$(OCAMLOPT) -a -o $@ $(filter-out %.cmi,$^)

probM.cmo, probM.cmi : inference.cmi
probM.cmx, probM.cmi : inference.cmi

lazy_permutations.cmo: probM.cmi


TESTS = test_exactInfM test_samplingM test_slazy \
	test_music1 test_music1a test_nested \
	paper_examples daily_schedule logic_vars

LONG_TESTS = zebra sorted

test: $(TESTS)

clean::
	rm -f *.cm[iox] *.[oa] *~ *.aux *.vrb


paper_examples sorted daily_schedule logic_vars : % : $(LIB) %.ml
	$(OCAMLC) -o $@ $(DELIMCCRUN) $(filter-out %.cmi,$^)
	./$@

test_%: $(LIB) %.ml
	$(OCAMLC) -o $@ $(DELIMCCRUN) $(filter-out %.cmi,$^)
	./$@

test_music2: $(LIB) music2.cmi music2.ml
	$(OCAMLC) -o $@ $(DELIMCCRUN) $(filter-out %.cmi,$^)
	/usr/bin/time -l ./$@

test_music2opt: $(LIBOPT) music2.cmi music2.ml
	$(OCAMLOPT) -o $@ $(DELIMCCOPT) $(filter-out %.cmi,$^)
	/usr/bin/time -l ./$@

music2_app: $(LIB) music2.cmo music2.cmi music2_app.ml
	$(OCAMLC) -o $@ $(DELIMCCRUN) $(filter-out %.cmi,$^)

zebra : % : $(LIB) lazy_permutations.cmi lazy_permutations.cmo %.ml
	$(OCAMLC) -o $@ $(DELIMCCRUN) $(filter-out %.cmi,$^)
	/usr/bin/time -l ./$@


clean::
	rm -f $(TESTS)

Music: Music.hs
	ghc -o $@ -static -optl-static -optl-pthread -O2 -funbox-strict-fields $^

export BIBTEX := bibtex -min-crossrefs=9999

.tex.dvi:
# rm -f $(patsubst %.dvi,%.aux,$@)
	texi2dvi -b $< # && killall -USR1 xdvi.bin

.tex.pdf:
	texi2dvi -b --pdf $<

.pdf.ps:
	pdftops $< $@

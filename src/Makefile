GENERATED = parsing/parser.ml parsing/lexer.ml

MODULES =					\
  common/positions				\
  common/errors					\
  common/unionFind				\
  common/infiniteArray				\
  common/basicSetEquations			\
  common/misc					\
  common/mark					\
  name types AST IAST XAST			\
  parsing/tokens				\
  parsing/parser				\
  inference/intRank				\
  inference/inferenceTypes			\
  inference/multiEquation			\
  inference/typeAlgebra		 	        \
  parsing/prettyPrint				\
  parsing/lexer					\
  parsing/ASTio					\
  inference/env					\
  inference/inferenceExceptions			\
  inference/kindInferencer			\
  inference/constraint				\
  inference/typingEnvironment			\
  inference/alphaRename				\
  inference/internalizeTypes			\
  inference/unifier				\
  inference/constraintSimplifier		\
  inference/externalizeTypes			\
  inference/constraintSolver			\
  inference/constraintGeneration		\
  inference/inferenceErrors			\
  inference/inferTypes				\
  elaboration/elaborationExceptions		\
  elaboration/elaborationErrors 		\
  elaboration/elaborationEnvironment 		\
  elaboration/elaborateDictionaries		\
  options					\
  front

EXECUTABLE = joujou

PPRINT = parsing/pprint-20130324/src

SRC_INCLUDES = -I parsing -I inference -I elaboration -I common
INCLUDES = -I $(PPRINT)/_build $(SRC_INCLUDES)
BFLAGS = -dtypes -g $(INCLUDES)
OFLAGS = -g $(INCLUDES)
BLIBS  = $(PPRINT)/_build/PPrintLib.cma
OLIBS  = $(PPRINT)/_build/PPrintLib.cmxa

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
MENHIR = menhir

.PHONY: all clean depend pprint

all: pprint $(EXECUTABLE)

pprint:
	$(MAKE) -C $(PPRINT)

$(EXECUTABLE): $(MODULES:%=%.cmo)
	$(OCAMLC) $(BFLAGS) -o $(EXECUTABLE) $(BLIBS) $(MODULES:%=%.cmo)

$(EXECUTABLE).opt: $(MODULES:%=%.cmx)
	$(OCAMLOPT) $(OFLAGS) -o $(EXECUTABLE) $(OLIBS) $(MODULES:%=%.cmx)

%.cmo: %.ml
	$(OCAMLC) $(BFLAGS) -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(BFLAGS) -c $*.mli
%.cmx: %.ml
	$(OCAMLOPT) $(OFLAGS) -c $*.ml

%.ml %.mli: %.mly
	$(MENHIR) --explain --external-tokens Tokens $*.mly
	sed -e 's/4611686018427387903/max_int/' $*.ml > $*.tmp
	mv $*.tmp $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

clean:
	rm -f $(EXECUTABLE) $(EXECUTABLE).opt parsing/parser.ml parsing/lexer.ml *.cm[iox] **/*.cm[iox] **/*.o *.annot **/*.annot

.depend: $(GENERATED) Makefile
	$(OCAMLDEP) $(SRC_INCLUDES) $(patsubst %, %.ml, $(MODULES)) \
		$(patsubst %, %.mli, $(MODULES)) $(GENERATED) > .depend

depend:
	rm -f .depend
	$(MAKE) .depend

include .depend

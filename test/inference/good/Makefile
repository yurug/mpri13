JOUJOU = ../../../src/joujou
OPTIONS =
OCAMLC = ocamlc -w -P-Y-Z

TESTS = $(shell ls *.mlt 2> /dev/null)
RUNS = $(shell ls *_run.ml 2> /dev/null)

TARGETS = $(patsubst %.mlt, %.ml, $(TESTS)) \
	  $(patsubst %.mlt, %.cmo, $(TESTS)) \
	  $(patsubst %_run.ml, %.out, $(RUNS))

all: $(TARGETS)

$(JOUJOU):
	@echo "*** Please build $(JOUJOU) first"
	@exit 1

%.ml: %.mlt $(JOUJOU)
	@if OCAMLRUNPARAM=b $(JOUJOU) $(OPTIONS) $<; then \
	  echo [OK] Elaboration of $<;			\
	else						\
	  echo [KO] Elaboration of $<;			\
	fi

%.cmo: %.ml
	@ if $(OCAMLC) -c $<; then 		\
	  echo [OK] Compilation of $<;		\
	else 					\
	  echo [KO] Compilation of $<;		\
	fi

%_run.cmo: %.cmo

%.run: %.cmo %_run.cmo
	@ $(OCAMLC) -o $@ $^ # This is our source file. It must compile.

%.out: %.run
	@ if ./$<; then 			\
	  echo [OK] Evaluation of $<;		\
	else					\
	  echo [KO] Evaluation of $<;		\
	fi
	@ touch $@

clean:
	rm -f $(TARGETS) *.cmo *.cmi *.mls *.mlse *.mle *.mlr *.out

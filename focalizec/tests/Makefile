OK_TEST_FILES = \
	ok__baby_toy.foc \
	ok__in_example.foc \
	ok__in_example2.foc \
	ok__list.foc \
	ok__need_re_ordering.foc \
	ok__odd_even.foc \
	ok__phd_def_deps.foc \
	ok__phd_meths_gen.foc \
	ok__phd_sample.foc
	#ok__coll_outside.foc

KO_TEST_FILES = \
	ko__bad_self_use.foc \
	ko__param_toy.foc \
	ko__test_error.foc \
	ko__test_rec.foc

all: ok_all ko_all

ok_all: $(OK_TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(OK_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  ../toplevel/check_file -c $$i; \
	done; \
	# Compile the generated OCaml code.
	for i in $(OK_TEST_FILES:.foc=.ml); do \
	  echo "OCamllizing $$i"; \
	  ocamlc -w a -I ../lib/ -I . ../lib/focvasives.cmo ../lib/basics.cmo $$i -o $$i.out; \
	done

ko_all: $(KO_TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(KO_TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  ../toplevel/check_file -c $$i; \
	done


clean:
	rm -f *.fo *.mangled *~ .*~ *.cm* *.o *.ml *.v *.out

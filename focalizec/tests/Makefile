TEST_FILES = basics.foc \
	phd_sample.foc \
	odd_even.foc \
	baby_toy.foc \
	in_example.foc \
	in_example2.foc


all: $(TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  ../toplevel/check_file -I . -c $$i; \
	done; \
	# Compile the generated OCaml code.
	for i in $(TEST_FILES:.foc=.ml); do \
	  echo "OCamllizing $$i"; \
	  ocamlc -I ../lib/ -I . ../lib/focvasives.cmo basics.cmo $$i -o $$i.out; \
	done

clean:
	rm -f *.fo *.mangled *~ .*~ *.cm* *.o *.ml *.out

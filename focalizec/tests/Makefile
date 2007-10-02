TEST_FILES = basics.foc \
	phd_sample.foc \
	baby_toy.foc


all: $(TEST_FILES)
	# Compile from FoCaL to OCaml.
	for i in $(TEST_FILES); do \
	  echo "Focalizing $$i"; \
	  ../toplevel/check_file -I . -c $$i; \
	done; \
	# Compile the generated OCaml code.
	for i in $(TEST_FILES:.foc=.ml); do \
	  echo "OCamllizing $$i"; \
	  ocamlc -c -I ../lib/ -I . -c $$i; \
	done

clean:
	rm -f *.fo *.mangled *~ .*~ *.cm* *.o *.ml

TEST_FILES = sets_orders.foc

tests:: all

all:: ../check_file
	for i in $(TEST_FILES); do \
	   ../check_file -c $$i; \
	done

../check_file:
	cd ..; $(MAKE) all

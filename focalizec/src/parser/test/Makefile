TEST_FILES = sets_orders.foc

ROOT_DIR = ../..

CHECK_FILE = $(ROOT_DIR)/toplevel/check_file.byt

tests:: all

all:: $(CHECK_FILE)
	for i in $(TEST_FILES); do \
	   $(CHECK_FILE) -c $$i; \
	done

$(CHECK_FILE):
	cd $(ROOT_DIR); $(MAKE) all

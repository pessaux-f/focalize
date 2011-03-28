MAKEFILE=$(shell ls -d */ | sed -e 's,/,,' -e 's,CVS,,')

define one_make

$(1).all:
	make -C $(1) all

$(1).clean:
	make -C $(1) clean

$(1).cleanall:
	make -C $(1) cleanall

$(1).cleanxml:
	make -C $(1) cleanxml

endef

all: $(MAKEFILE:%=%.all)

clean: $(MAKEFILE:%=%.clean)

cleanall: $(MAKEFILE:%=%.cleanall)

cleanxml: $(MAKEFILE:%=%.cleanxml)

$(foreach example,$(MAKEFILE),$(eval $(call one_make,$(example))))

.DEFAULT_GOAL := all
.PHONY : all
.PHONY : $(MAKEFILE)


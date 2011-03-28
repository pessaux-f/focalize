all:
	@make -f Makefile.spec
	@make -f Makefile.parameters


clean:
	@make -f Makefile.spec clean
	@make -f Makefile.parameters clean

cleanall:
	@make -f Makefile.spec cleanall
	@make -f Makefile.parameters cleanall

cleanxml:
	@make -f Makefile.spec cleanxml
	@make -f Makefile.parameters cleanxml

.DEFAULT_GOAL:=all
.PHONY: all clean cleanall cleanxml

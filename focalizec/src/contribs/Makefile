# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                               LIP6  --  INRIA Rocquencourt                 #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR=../../..

include $(ROOT_DIR)/Makefile.common

# Fill here to add a new directory for a new contribution
SUB_DIRS=\
 utils
# voter
# automata

lib: all

byt bin clean doc depend install:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." &&\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

# TODO
uninstall:
	echo "Uninstall extlib TODO"

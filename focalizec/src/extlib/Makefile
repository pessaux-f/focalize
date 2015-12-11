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

# Fill here to add a new directory for a new library
SUB_DIRS=algebra polynomials
# polynomials (PW: Commented out since does not compile yet (as of 0.6.0 version).)

include $(ROOT_DIR)/Makefile.common

lib: all

byt bin:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ..." &&\
	  ($(CD) $$i && $(MAKE) all);\
	  err=$$?;\
	  echo "<-- $$i [$$err]" &&\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

clean doc depend install:
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

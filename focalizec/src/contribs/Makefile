#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.19 2012-10-15 14:47:40 pessaux Exp $

ROOT_DIR=../../..

include $(ROOT_DIR)/Makefile.common

# Fill here to add a new directory for a new contribution
SUB_DIRS=\
 utils\
 voter
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

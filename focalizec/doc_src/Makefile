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

# $Id: Makefile,v 1.7 2012-10-15 15:00:22 pessaux Exp $

ROOT_DIR=../..

SUB_DIRS=man tex

include $(ROOT_DIR)/Makefile.common

all clean doc docdir depend install:
	@for i in $(SUB_DIRS); do\
	  echo "--> $$i ...";\
	  ($(CD) $$i && $(MAKE) $@);\
	  err=$$?;\
	  echo "<-- $$i [$$err]";\
	  case $$err in 0);; *) exit $$err;; esac;\
	done

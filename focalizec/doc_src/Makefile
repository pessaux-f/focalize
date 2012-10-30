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
#  Copyright 2007 - 2012 LIP6 and INRIA                                      #
#            2012 ENSTA ParisTech                                            #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

# $Id: Makefile,v 1.8 2012-10-30 13:56:05 pessaux Exp $

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

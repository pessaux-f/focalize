#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.21 2012-02-10 16:24:40 pessaux Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

BYT_OBJS=\
 build_stamp.cmo installation.cmo configuration.cmo handy.cmo\
 location.cmo types.cmo\
 files.cmo anti_keyword_conflict.cmo parsetree_utils.cmo miscHelpers.cmo

include $(ROOT_DIR)/Makefile.common

depend-local:
	$(CAML_DEP) $(BYT_OBJS:.cmo=.mli) $(BYT_OBJS:.cmo=.ml) > .depend

installation.ml:
	@if test ! -L $@ ; then\
	   echo "Please, run configure first.";\
	fi

build_stamp.ml: .FORCE

.PHONY: .FORCE

.FORCE:
	echo "let build_stamp = \"" `date` "\" ;;" > build_stamp.ml

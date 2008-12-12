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

# $Id: Makefile,v 1.15 2008-12-12 09:52:56 weis Exp $

# Makefile for access control models

ROOT_DIR = ../../..

include $(ROOT_DIR)/Makefile.config

LIB_INSTALL_DIR = $(LIB_DIR)/focalize/extlib/access_control

ZENON_DIR = $(ROOT_DIR)/../zenon/

STDLIB_DIR = $(ROOT_DIR)/src/stdlib

FOCALIZE_COMPILER = $(ROOT_DIR)/src/toplevel/focalizec

FOCALIZE_COMP_FLAGS = -no-stdlib-path -I $(STDLIB_DIR)

#From Makefile.config FOCALIZEC = $(FOCALIZE_COMPILER) $(FOCALIZE_COMP_FLAGS)

CAML_COMP_FLAGS = -w A -warn-error Ae -dtypes -I $(STDLIB_DIR)

COQ_INCLUDES = -I $(ZENON_DIR) -I $(STDLIB_DIR)

include $(ROOT_DIR)/Makefile.common

lib: all

all:: ensembles_finis.fo access_control.fo hru.fo rbac.fo ticket.fo unix.fo \
ensembles_finis.v access_control.v hru.v rbac.v ticket.v unix.v

install::
	$(MKDIR) $(LIB_INSTALL_DIR)
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) $(LIB_INSTALL_DIR)/ ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(LIB_INSTALL_DIR)/ ; \
	fi

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc

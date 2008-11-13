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

# $Id: Makefile,v 1.8 2008-11-13 11:05:11 habib Exp $

# Makefile for access control models

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config
include $(ROOT_DIR)/Makefile.common
include $(ROOT_DIR)/Makefile.install


lib: all

all:: ensembles_finis.fo access_control.fo hru.fo rbac.fo ticket.fo unix.fo \
ensembles_finis.v access_control.v hru.v rbac.v ticket.v unix.v

install:: all
	$(MKDIR) $(FOC_INSTALL_LIB)/extlib/access_control
	@if [ "$(FOCALIZE_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(FOCALIZE_BYT_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BYT_OBJS_UNSAFE) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(FOCALIZE_BIN_OBJS_UNSAFE)" != "" ] ; then \
	  $(CP) $(FOCALIZE_BIN_OBJS_UNSAFE) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(FOCALIZE_VOS)" != "" ] ; then \
	  $(CP) $(FOCALIZE_VOS) $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(EXTERNAL_BYT_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BYT_OBJS) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(EXTERNAL_BIN_OBJS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_BIN_OBJS) \
	  $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi
	@if [ "$(EXTERNAL_VOS)" != "" ] ; then \
	  $(CP) $(EXTERNAL_VOS) $(FOC_INSTALL_LIB)/extlib/access_control ; \
	fi

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc

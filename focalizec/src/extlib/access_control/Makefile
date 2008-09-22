# $Id: Makefile,v 1.2 2008-09-22 14:58:19 weis Exp $

# Makefile for access control models

ROOT_DIR = ../..

include $(ROOT_DIR)/Makefile.config

CHECK_FILE = $(ROOT_DIR)/toplevel/check_file.byt
FOCC = $(CHECK_FILE)

include $(ROOT_DIR)/Makefile.common

all:: access_control.fo hru.fo rbac.fo ticket.fo unix.fo

clean::
	$(RM) *.ml *.mli *.zv *.v *.pfc

.PRECIOUS: *.mli *.fo

$(CHECK_FILE):
	cd $(ROOT_DIR); $(MAKE) all

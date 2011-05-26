#*********************************************************************#
#                                                                     #
#                                Htmlc                                #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 1997 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#*********************************************************************#

# $Id: Makefile,v 1.13 2011-05-26 10:21:32 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

DOCDIR_DIR=$(DOC_ROOT_DIR)/man

# Makefile for man page generation.

# From a list of source files (.man files) generates the
# corresponding objects (.1 files) using Htmlc.

# Usage: fill in the HTMLC and FILES variables below, then
# make (or make all) will rebuild the whole thing
# make clean will remove all the generated files

# The Htmlc compiler with its include path options
# For instance htmlc -I Includes -env env
HTMLC=htmlc -env focalizec.env

# Prefix before regular names
PACKAGE=focalizec
PACKAGE_DEP=focalizedep

# The list of source files
MAN_FILES=$(PACKAGE).man $(PACKAGE_DEP).man focalizetest.man

######## This part should automatically handle the generation of
######## object files

# The list of object files
MAN_OBJS:=$(MAN_FILES:.man=.$(PROJECT_MAN_EXT))

.PHONY: all doc visu clean docdir install

all doc: $(MAN_OBJS)

docdir: $(MAN_OBJS)
	$(MKDIR) $(DOCDIR_DIR) &&\
	$(CP) $(MAN_OBJS) $(DOCDIR_DIR)/

visu: $(MAN_OBJS)
	nroff -man $(PACKAGE).$(PROJECT_MAN_EXT) | more
	nroff -man $(PACKAGE_DEP).$(PROJECT_MAN_EXT) | more

clean-local:
	$(RM) $(MAN_OBJS) *~
#	cd Includes; $(RM) *~

distclean-local:
	$(RM) $(DOCDIR_DIR)

install-local:
	$(MKDIR) $(MAN_DIR) &&\
	$(CP) $(MAN_OBJS) $(MAN_DIR)/

uninstall-local:
	@for i in $(MAN_OBJS); do\
	  $(RM) $(MAN_DIR)/$$i;\
	done

depend:

.SUFFIXES:
.SUFFIXES: .man .1

.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t $@

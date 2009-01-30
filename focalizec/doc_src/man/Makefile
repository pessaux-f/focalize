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

# $Id: Makefile,v 1.5 2009-01-30 10:52:09 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

DOCDIR_DIR=$(DOCDIR_DIR)/man

# Makefile for man page generation.

# From a list of source files (.man files) generates the
# corresponding objects (.1 files) using Htmlc.

# Usage: fill in the HTMLC and FILES variables below, then
# make (or make all) will rebuild the whole thing
# make clean will remove all the generated files

RM=rm -f

# The Htmlc compiler with its include path options
# For instance htmlc -I Includes -env env
HTMLC=htmlc -env focalizec.env

# Prefix before regular names
PACKAGE=focalizec

# The list of source files
MAN_FILES=$(PACKAGE).man

######## This part should automatically handle the generation of
######## object files

# The list of object files
MAN_OBJS=$(MAN_FILES:.man=.1)

.PHONY: all doc visu clean docdir install

all doc: $(MAN_OBJS)

docdir: $(MAN_OBJS)
	$(MKDIR) $(DOCDIR_DIR) && \
	$(CP) $(MAN_OBJS) $(DOCDIR_DIR)/

visu: $(MAN_OBJS)
	nroff -man $(PACKAGE).1 | more

clean::
	$(RM) $(MAN_OBJS) *~
#	cd Includes; $(RM) *~

distclean:: clean
	$(RM) $(DOCDIR_DIR)

depend::

.SUFFIXES:
.SUFFIXES: .man .1

.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t $@

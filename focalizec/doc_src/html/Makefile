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

# $Id: Makefile,v 1.1 2009-01-30 09:05:37 weis Exp $

ROOT_DIR=../..

include $(ROOT_DIR)/Makefile.config

DOC_DIR=$(ROOT_DIR)/doc

# Makefile for man page generation.

# From a list of source files (.man files) generates the
# corresponding objects (.1 files) using Htmlc.

# Usage: fill in the HTMLC and FILES variables below, then
# make (or make all) will rebuild the whole thing
# make clean will remove all the generated files

# The Htmlc compiler with its include path options
# For instance htmlc -I Includes -env env
HTMLC=htmlc -I Includes -env focalizec.env

# Prefix before regular names
PACKAGE=focalizec

# The list of source files
HTML_FILES=
#$(PACKAGE).htm

######## This part should automatically handle the generation of
######## object files

# The list of object files
HTML_OBJS=$(HTML_FILES:.html=.htm)

.PHONY: all doc visu clean docdir install

all doc: $(HTML_OBJS)

visu: $(HTML_OBJS)
	dillo $(PACKAGE).htm

clean::
	$(RM) $(HTML_OBJS) *~
	cd Includes; $(RM) *~

distclean:: clean
	$(RM) $(DOC_DIR)

docdir: $(HTML_OBJS)
	$(MKDIR) $(DOC_DIR) && \
	if ! test -z $(HTML_OBJS); then \
	  $(CP) $(HTML_OBJS) $(DOC_DIR); \
	fi

.SUFFIXES:
.SUFFIXES: .html .htm

.html.htm:
	$(RM) $@
	$(HTMLC) -f $< -t $@

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

# $Id: Makefile,v 1.2 2009-01-16 16:34:29 weis Exp $

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
MANFILES=$(PACKAGE).man

######## This part should automatically handle the generation of
######## object files

# The list of object files
MANOBJS=$(MANFILES:.man=.1)

all: $(MANOBJS)

visu: $(MANOBJS)
	nroff -man $(PACKAGE).1 | more

clean::
	$(RM) $(MANOBJS) *~
#	cd Includes; $(RM) *~

.SUFFIXES:
.SUFFIXES: .man .1

.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t $@

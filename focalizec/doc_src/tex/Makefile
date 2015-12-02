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

ROOT_DIR=../../..

SUB_DIRS=refman tutorial tuto_proofs

include $(ROOT_DIR)/Makefile.common

# Only enter these directories if doc was requested and if LaTeX was found.
all clean depend install doc:
	@ if [ "$(MAKE_DOC)" == "true" ] && [ "$(LATEX)" != "" ] ; \
		then \
			for i in $(SUB_DIRS); do \
				echo "--> $$i ..." >&2 && \
				($(CD) $$i && $(MAKE) $@); \
				err=$$?; \
				echo "<-- $$i [$$err]" >&2 && \
				case $$err in 0);; *) exit $$err;; esac; \
			done ;\
	else \
		echo "PDF documentation skipped on user demand or because of missing tools." ;\
	fi

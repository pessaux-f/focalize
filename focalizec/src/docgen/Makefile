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
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #


ROOT_DIR = ../../..

# Must be defined before including Makefile.common since is used by it.
BYT_OBJS = doc_lexer.cmo utils_docgen.cmo env_docgen.cmo main_docgen.cmo
XSL_OBJS = focdoc2html.xsl mmlctop2_0.xsl proposition2tex.xsl focdoc2tex.xsl \
	proposition.xsl focdoc.dtd focdoc.css

include $(ROOT_DIR)/Makefile.common

CAML_INCLUDES = -I ../basement -I ../parser -I ../typing
CAML_FLAGS = -w A-4-9 -warn-error $(WARN_ERROR) -safe-string


byt: $(BYT_OBJS) fcd2html.sh

bin: $(BIN_OBJS) fcd2html.sh

fcd2html.sh: $(ROOT_DIR)/.config_var
	@echo -e '#!/bin/sh\nxsltproc '$(INSTALL_LIB_DIR)'/focdoc2html.xsl $$1 | xsltproc '$(INSTALL_LIB_DIR)'/mmlctop2_0.xsl - > $$1.xml' > fcd2html.sh


depend:
	$(CAMLDEP) $(CAML_INCLUDES) *.mli *.ml > ./.depend

install: fcd2html.sh $(XSL_OBJS)
	@$(MKDIR) $(INSTALL_LIB_DIR) &&\
	if [ "$(XSL_OBJS)" != "" ]; then\
	  echo "Installing XSL files." &&\
	  $(CP) $(XSL_OBJS) $(INSTALL_LIB_DIR)/;\
	fi &&\
	$(MKDIR) $(INSTALL_BIN_DIR) &&\
	echo "Installing doc generation scripts." &&\
	$(CP) fcd2html.sh $(INSTALL_BIN_DIR)/ &&\
	$(CHMOD) o+x $(INSTALL_BIN_DIR)/fcd2html.sh

uninstall:
	$(RM) $(INSTALL_BIN_DIR)/fcd2html.sh
	echo "Uninstall docgen TODO"

clean:
	$(RM) *.cm* *.o *.a *.annot *.out *.output *.bin *.byt fcd2html.sh

include .depend

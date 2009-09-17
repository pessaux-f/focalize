#**********************************************************************#
#                                                                      #
#                        FoCaLize compiler                             #
#                                                                      #
#            David Delahaye                                            #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                                                                      #
#                        CNAM - LIP6  -  INRIA Rocquencourt            #
#                                                                      #
#  Copyright 2009 CNAM, LIP6 and INRIA                                 #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.1 2009-09-17 16:31:50 delahaye Exp $

OCAMLLEX=ocamllex
OCAMLC=ocamlc

all:
	$(OCAMLLEX) lexer.mll
	$(OCAMLC) -c xml.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c printer.ml
	$(OCAMLC) -c bench.ml
	$(OCAMLC) -o bench lexer.cmo printer.cmo bench.cmo

bench: all
	@echo -e "**** Benches ****"; \
        for i in `seq 0 3`; do echo -e "test$$i.xml: \c"; \
        bench "test$$i.xml" > /dev/null 2>&1; \
        if [ "$$?" = 0 ]; then echo "ok"; else echo "fail"; fi; done

clean:
	rm -f *~ *.cm* lexer.mli lexer.ml bench

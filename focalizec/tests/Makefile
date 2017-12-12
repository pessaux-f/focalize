# ************************************************************************** #
#                                                                            #
#                        FoCaLiZe compiler                                   #
#                                                                            #
#            François Pessaux                                                #
#            Pierre Weis                                                     #
#            Damien Doligez                                                  #
#                                                                            #
#                 LIP6  --  INRIA Rocquencourt - ENSTA ParisTech             #
#                                                                            #
#  Copyright 2007 - ... LIP6 and INRIA                                       #
#            2012 - ... ENSTA ParisTech                                      #
#  Distributed only by permission.                                           #
#                                                                            #
# ************************************************************************** #

ROOT_DIR=../..

CHECKED_FOCALIZEC=$(ROOT_DIR)/focalizec/src/toplevel/focalizec
CHECKED_STDLIB=$(ROOT_DIR)/focalizec/src/stdlib


# These must pass but with generating / compiling to OCaml and Coq.
MUST_BE_OK_SRCS_COMPLETE=\
  ok__caveat.fcl ok__coll_outside.fcl \
	ok__definition_72_rule_PRM.fcl ok__in_example.fcl ok__in_example2.fcl \
  ok__multiple_inherit.fcl ok__need_inspect_self.fcl \
	ok__need_re_ordering.fcl ok__phd_def_deps.fcl \
	ok__phd_meths_gen.fcl ok__phd_sample.fcl ok__pmatch_checks.fcl \
	ok__scoping_tricky.fcl ok__term_measure.fcl ok__toplevel_odd_even.fcl \
	ok__torture_params.fcl ok__torture_params2.fcl ok__torture_params3.fcl \
  ok__coll_gen_llifts_on_non_remapped.fcl \
  ok__coll_gen_llifts_on_non_remapped2.fcl ok__need_remap_deps.fcl \
  ok__definition_72_rule_PRM_miss_subst.fcl ok__bug18.fcl ok__record1.fcl \
	ok__record2.fcl ok__final_def1.fcl ok__final_def2.fcl ok__final_def3.fcl \
  ok__llift_rec_meth_paramed.fcl ok__proof_by_def_poly.fcl \
  ok__was_bug_catchall_capture.fcl


# These must pass but without generating / compiling to Coq.
MUST_BE_OK_SRCS_NO_COQ = \
	ok__baby_toy.fcl ok__list.fcl ok__odd_even.fcl


# These must fail and be rejected by FoCaLiZe.
MUST_BE_KO_SRCS = \
  ko__bad_self_use.fcl ko__param_toy.fcl ko__test_error.fcl ko__test_rec.fcl \
  ko__final_def1.fcl ko__final_def2.fcl ko__final_def3.fcl


include $(ROOT_DIR)/Makefile.common

all: $(CHECKED_FOCALIZEC) externals must_ok_complete must_ok_no_coq must_ko

externals: ok__baby_toy_coq_externals.v ok__baby_toy_externals.ml
	$(COQC) ok__baby_toy_coq_externals.v
	$(CAMLBYT) ok__baby_toy_externals.ml


must_ok_complete: $(MUST_BE_OK_SRCS_COMPLETE)
	@$(RM) must_ok_complete.log
	@for i in $(MUST_BE_OK_SRCS_COMPLETE); do \
		echo "PROCESSING: $$i" ; \
	  $(CHECKED_FOCALIZEC) -no-dedukti-code -I $(CHECKED_STDLIB) $$i; \
	  err=$$?; \
	  case $$err in \
	 		0);; \
	 	  *) echo "***********************************************************" ; \
	       echo "*** !!! BROKEN: $$i" ; \
	       echo "***********************************************************" ; \
				 echo "*** !!! BROKEN: $$i" >> must_ok_complete.log ;; \
	  esac; \
	done


must_ok_no_coq: $(MUST_BE_OK_SRCS_NO_COQ)
	@$(RM) must_ok_no_coq.log
	@for i in $(MUST_BE_OK_SRCS_NO_COQ); do \
		echo "PROCESSING: $$i" ; \
	  $(CHECKED_FOCALIZEC) -no-dedukti-code -I $(CHECKED_STDLIB) -no-coq-code $$i; \
		err=$$?;\
	  case $$err in \
		  0);; \
	    *) echo "***********************************************************" ; \
	       echo "*** !!! BROKEN: $$i" ; \
	       echo "***********************************************************" ; \
				 echo "*** !!! BROKEN: $$i" >> must_ok_no_coq.log ;; \
	  esac; \
	done


must_ko: $(MUST_BE_KO_SRCS)
	@$(RM) must_ko.log
	@for i in $(MUST_BE_KO_SRCS); do \
		echo "PROCESSING: $$i" ; \
	  $(CHECKED_FOCALIZEC) -no-dedukti-code -I $(CHECKED_STDLIB) $$i; \
		err=$$?;\
	  case $$err in \
	    0) echo "***********************************************************" ; \
	       echo "*** !!! BROKEN: $$i" ; \
	       echo "***********************************************************" ; \
				 echo "*** !!! BROKEN: $$i" >> must_ko.log ;; \
	    *) ;; \
	  esac; \
	done


clean:
	$(RM) $(MUST_BE_OK_SRCS_COMPLETE:.fcl=.v) $(MUST_BE_OK_SRCS_COMPLETE:.fcl=.ml)
	$(RM) $(MUST_BE_OK_SRCS_NO_COQ:.fcl=.ml)
	$(RM) *.annot *.glob *.pfc *.zv *~ *.vo *.cm* *.fo *.mangled

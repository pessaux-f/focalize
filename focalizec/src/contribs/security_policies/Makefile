FOCC = focalizec
FOCFLAGS = 

OCAMLC = ocamlc
INCLUDES = /usr/local/lib/focalizec

BASICS_OBJS = ml_builtins.cmo basics.cmo
SETS_OBJS = sets_externals.cmo sets.cmo

FINITE_SETS_OBJS = $(BASICS_OBJS) $(SETS_OBJS) utils/finite_sets.cmo
LTS_OBJS = $(BASICS_OBJS) $(SETS_OBJS) products.cmo utils/lts.cmo
LTS_MUTABLE_OBJS = $(BASICS_OBJS) $(SETS_OBJS) utils/lts_mutable.cmo
RELATIONS_OBJS = $(FINITE_SETS_OBJS) utils/relations.cmo
RELATIONS_MUTABLE_OBJS = $(FINITE_SETS_OBJS) products.cmo\
                         utils/relations_mutable.cmo
P_POLICIES_OBJS = $(RELATIONS_OBJS) policies/p_policies.cmo
SECURE_LTS_OBJS = $(P_POLICIES_OBJS) products.cmo utils/lts.cmo\
                  policies/secure_lts.cmo
ACCESSES_OBJS = $(FINITE_SETS_OBJS) exemples/accesses.cmo
ACCESS_SYSTEM_OBJS = $(ACCESSES_OBJS) products.cmo utils/lts.cmo\
                     exemples/access_system.cmo
ACCESS_P_POLICIES_OBJS = $(P_POLICIES_OBJS) exemples/accesses.cmo\
                         exemples/access_p_policies.cmo
HRU_OBJS = $(P_POLICIES_OBJS) exemples/accesses.cmo\
           exemples/access_p_policies.cmo exemples/hru.cmo


all: access_system lts_mutable relations_mutable hru secure_lts

finite_sets: utils/finite_sets.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(FINITE_SETS_OBJS)

lts: utils/lts.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(LTS_OBJS)

lts_mutable: utils/lts_mutable.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(LTS_OBJS)

relations: utils/finite_sets.cmo utils/relations.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(RELATIONS_OBJS)

relations_mutable: utils/finite_sets.cmo utils/relations_mutable.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(RELATIONS_MUTABLE_OBJS)

p_policies: utils/finite_sets.cmo policies/p_policies.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(P_POLICIES_OBJS)

secure_lts: utils/finite_sets.cmo utils/lts.cmo policies/p_policies.cmo\
            policies/secure_lts.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(SECURE_LTS_OBJS)

accesses: utils/finite_sets.cmo exemples/accesses.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(ACCESSES_OBJS)

access_system: utils/finite_sets.cmo utils/lts.cmo exemples/accesses.cmo\
               exemples/access_system.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(ACCESS_SYSTEM_OBJS)

access_p_policies: utils/finite_sets.cmo utils/relations.cmo\
                   policies/p_policies.cmo exemples/accesses.cmo\
                   exemples/access_p_policies.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(ACCESS_P_POLICIES_OBJS)

hru: utils/finite_sets.cmo utils/relations.cmo policies/p_policies.cmo\
     exemples/accesses.cmo exemples/access_p_policies.cmo exemples/hru.cmo
	$(OCAMLC) -I $(INCLUDES) -o test $(HRU_OBJS)

.fcl.cmo:
	$(FOCC) -I utils -I policies -I exemples $<


clean:
	rm -rf utils/*.cm* utils/*.fo utils/*.vo utils/*.ml* utils/*.pfc\
               utils/*.v utils/*.vo utils/*.zv*
	rm -rf policies/*.cm* policies/*.fo policies/*.vo policies/*.ml*\
               policies/*.pfc policies/*.v policies/*.vo policies/*.zv*
	rm -rf exemples/*.cm* exemples/*.fo exemples/*.vo exemples/*.ml*\
               exemples/*.pfc exemples/*.v exemples/*.vo exemples/*.zv*
	rm -rf test

.PHONY: all clean
.SUFFIXES:
.SUFFIXES: .fcl .cmo
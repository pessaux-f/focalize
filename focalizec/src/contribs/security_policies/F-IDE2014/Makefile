# makefile tres approximatif et temporaire

C=focalizec
FLAGS=-stop-before-coq -zvtovopt -script
SRC=listes.fcl finite_sets_f.fcl code.fcl
V=$(SRC:.fcl=.v)
ML=$(SRC:.fcl=.ml)

# voir pour makefile conditionnel avec listes des fichiers
# pour lesquels y a le bug zenon

all : $(V) # j'utilise .v plutot que .ml car je veux des preuves...

%.ml : 	%.fcl
	$(C) $(FLAGS) $<

%.v : 	%.fcl
	$(C) $(FLAGS) $<

# indique les dépendances (pas moyen d'automatiser ?)
code.ml : finite_sets_f.ml listes.ml
code.v : finite_sets_f.v listes.v
finite_sets_f.ml : listes.ml
finite_sets_f.v : listes.v

# nettoyage
CMO=$(SRC:.fcl=.cmo)
CMI=$(SRC:.fcl=.cmi)
FO=$(SRC:.fcl=.fo)
PFC=$(SRC:.fcl=.pfc)
VO=$(SRC:.fcl=.vo)
ZV=$(SRC:.fcl=.zv)
GLOB=$(SRC:.fcl=.glob)

clean :
	rm -f $(CMI) $(CMO) $(FO) $(ML) $(PFC) $(V) $(VO) $(ZV) $(GLOB)

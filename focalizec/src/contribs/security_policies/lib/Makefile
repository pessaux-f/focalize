# makefile tres approximatif et temporaire

C=focalizec
FLAGS=-stop-before-coq -zvtovopt -script
SRC=listes.fcl finite_sets_f.fcl ternary_product.fcl
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
finite_sets_f.ml : listes.ml

# nettoyage
clean :
	rm -f *.cmi *.cmo *.fo *.ml *.pfc *.v *.vo *.zv *.glob

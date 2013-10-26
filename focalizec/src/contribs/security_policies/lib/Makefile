# makefile tres approximatif et temporaire

C=focalizec
FLAGS=-stop-before-coq -zvtovopt -script
EXEC=listes.v finite_sets_f.v
# j'utilise .v plutot que .ml car je veux des preuves...

# voir pour makefile conditionnel avec listes des fichiers
# pour lesquels y a le bug zenon

all : $(EXEC)

%.ml : 	%.fcl
	$(C) $(FLAGS) $<

%.v : 	%.fcl
	$(C) $(FLAGS) $<

# indique les dépendances (pas moyen d'automatiser ?)
finite_sets_f.ml : listes.ml

# nettoyage
clean :
	rm -f *.cmi *.cmo *.fo *.ml *.pfc *.v *.vo *.zv *.glob
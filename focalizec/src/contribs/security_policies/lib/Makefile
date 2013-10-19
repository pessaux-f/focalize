# makefile tres approximatif et temporaire

C=focalizec
FLAGS=-stop-before-coq
EXEC=listes.v finite_sets_f.v
# j'utilise .v plutot que .ml car je veux des preuves...

# voir pour makefile conditionnel avec listes des fichiers
# pour lesquels y a le bug zenon

all : $(EXEC)

%.ml : 	%.fcl # devrait pas aussi generer .v ?? bon ...
	$(C) $(FLAGS) $<

%.v : 	%.fcl # devrait pas aussi generer .v ?? bon ...
	$(C) $(FLAGS) $<

# indique les dépendances (pas moyen d'automatiser ?)
finite_sets_f.ml : listes.ml

clean :
	rm -f *.cmi *.cmo *.fo *.ml *.pfc *.v *.zv *.glob
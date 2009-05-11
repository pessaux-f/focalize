#***********************************************************************#
#*                                                                     *#
#*                        FoCaLize compiler                            *#
#*                                                                     *#
#*            Pierre Weis                                              *#
#*            Damien Doligez                                           *#
#*            François Pessaux                                         *#
#*                                                                     *#
#*                               LIP6  --  INRIA Rocquencourt          *#
#*                                                                     *#
#*  Copyright 2009 LIP6 and INRIA                                      *#
#*  Distributed only by permission.                                    *#
#*                                                                     *#
#***********************************************************************#

Some thoughts and specs for the automatic documentation generator.

# $Id: docgen.spec,v 1.1 2009-05-11 19:21:13 weis Exp $

1) Le commentaire parsé de départ (celui avec author) est posé sur le noeud
file de l'arbre de parsing.

2) On génère directement du HTML avec feuilles de style intégrées au document
   (pour qu'il soit auto-contenu). Sont-elles expansées à la génération ?

3) Les symboles mathématiques sont produits sont ceux de HTML: &exist;
   &forall; etc. pour la sortie HTML ou TeX pour la sortie LaTeX (produite
   peut-être par un autre processeur).

4) Dans les commentaires parsés on utilise le texte brut avec des balises à la
   ocamldoc: on a droit à

 4.1 Fontes:

 On a droit

   - au verbatim (\tt) {tt ...} avec toute indentation respectée.
   utilise-t-on aussi la convention "blabla" comme ocamldoc ?

   - à l'italique (\em) {em ...}
   
   - au gras (\bf) {bf ...}

 4.2 Titres automatiques: une espèce une collection, un type produise un titre
   et une balise dans le document.

   Titre manuels: {3 ...} à la ocamldoc. {n } est un titre de niveau n
   (1 <= n <= 6). {3a ...} avec a = l r c as Left, Right, Center.

 4.3 Enumération {li } {ol }
   1 ligne blanche suivie de ' '*-
   Puis, chaque
   \n' '*-
   donne lieu à un item.
   fin d'én umération: une ligne blanche.

 4.4 Paragraphe: { ...} (note the space)
  ou {a ...} avec a = l r c as Left, Right, Center
  Break: {\n}.

 4.5 Tableaux ?

5) Balises:
   \label \ref

6) Escapes:

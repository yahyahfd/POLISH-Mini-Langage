## Identifiants

##### **Nom:** Hafid
##### **Prénom:** Yahya
##### **Numéro étudiant:** 71800578
##### **Pseudo Gitlab:** hafid

## Fonctionnalités
J'ai fais ce qui a été demandé pour le premier rendu, c'est-à-dire:
- gérer la traduction de fichiers polish en syntaxe abstraite (polish -> ocaml)
- le ré-affichage en syntaxe concrète polish de ce code (ocaml -> polish)
- l'évaluation de ces syntaxes abstraites (ocaml -> résultat de calculs affichés sur le terminal selon le code)

J'ai tout de même fait en sorte d'utiliser de nombreuses méthodes auxiliaires afin de tester si le résultat est le bon dans certains cas, comme par exemple: la méthode `block_to_instr_list` qui garde le numéro de ligne jusqu'au bout au lieu de le retirer afin de tester la position de elses par rapports à leurs blocks parents ifs etc...

Lors de l'évaluation, je me suis permis de définir une variable de type (string,int) liste ref  `env` correspondante à la liste d'association d'environnement. J'ai choisi une référence plutôt qu'autre chose car il faut qu'on stock dedans les nouvelles variables déclarée, ainsi que récupérer la liste des variables déjà présentes lors de l'évaluation d'instructions.

Concernant le résultat de l'évaluation de polish, ça ne marche bien évidemment pas pour des nombres enormes, car je n'ai pas utilisé la bibliothèque ZArith.

## Compilation et exécution
Pour compiler le projet, il suffit d'utiliser la commande `make` à la racine du dépot, puis d'utiliser les commandes suivantes afin de l'executer:
- ./run -eval <Chemin du fichier polish: dans le dossier "exemples" dans notre cas>
- ./run -reprint <Chemin du fichier polish>
Je me suis servi de deux bibliothèques externes: String afin d'utiliser la méthode `split_on_char` et List afin de manipuler les listes.

##### List: https://ocaml.org/api/List.html
##### String: https://ocaml.org/api/String.html

## Découpage modulaire
J'ai découpé le projet en différent module (.ml) accompagnés de leurs interfaces (.mli):
- read: Nous permet de lire un fichier polish et de le retranscrire en code ocaml. J'ai caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `read_polish` du main.
- print: Nous permet d'imprimer du polish obtenu à partir de code ocaml. J'ai également caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `print_polish` du main.
- evaluation: Comme son nom l'indique, ce module permet d'évaluer un type program et d'afficher le résultat qu'on celà est demandé. J'ai également caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `eval_polish` du main.
- types: J'ai déplacé tout les types spécifiés au départ dans `polish.ml` dans une nouveau module à part afin qu'il servent aussi aux nouveaux modules.
- polish: J'ai rajouté un polish.mli qui n'affiche que le main.

## Organisation du travail
Je suis l'unique membre de mon groupe et donc j'ai fais le projet en entier tout seul. Le projet fut annoncé lors de la deuxième semaine du mois de novembre. J'ai donc lu le sujet pour la première fois deux semaines après afin de définir un ensemble de tâches au brouillon, bien avant de les rajouter sur le git. Je penses qu'il n'y aurait eu aucun intérêt à rajouter les tâches directement vu que je n'ai pas de binôme avec qui en discuter. J'avais tout le projet en tête vers fin novembre, mais je n'ai pu commencer à réellement le programmer qu'une semaine avant la date limite du premier rendu à cause d'autres projets que je devait rendre avant (langage C).

## Misc
Aucune remarque par rapport au sujet en lui-même mais j'ai tout de même une remarque par rapport à une des restrictions données:
- Il est écrit au 6ème point des critères d'évaluations données dans le fichier `CONSIGNES.md` qu'il vaudrait mieux éviter l'utilisation d'impératif, mais je ne vois pas du tout comment on pourrait faire autrement pour l'environnement lors de l'évaluation sans un ref.

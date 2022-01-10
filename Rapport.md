## Identifiants

##### **Nom:** Hafid

##### **Prénom:** Yahya

##### **Numéro étudiant:** 71800578

##### **Pseudo Gitlab:** hafid

## Fonctionnalités

J'ai fais ce qui a été demandé pour le premier rendu, c'est-à-dire faire des méthodes qui permettent de:

- gérer la traduction de fichiers polish en syntaxe abstraite ocaml (polish -> ocaml)
- faire le ré-affichage en syntaxe concrète polish de ce code via l'option -reprint (ocaml -> polish)
- faire l'évaluation de ces syntaxes abstraites via l'option -eval (ocaml -> résultat de calculs affichés sur le terminal selon le code)

ainsi que ce qui a été demandé pour le second rendu, c'est-à-dire faire des méthodes qui permettent de:

- gérer la simplification de fichiers polish avec suppression des blocs morts via l'option -simpl (polish -> polish)
- faire un calcul statique des variables d'environnement risquant d'être accédées avant initialisation via l'option -vars
- faire l'analyse statique des signes possibles des variables lors du déroulement du programme avec une gestion de division par zéro via l'option -sign

J'ai tout de même fait en sorte d'utiliser de nombreuses méthodes auxiliaires afin de tester si le résultat est le bon dans certains cas, comme par exemple: la méthode `block_to_instr_list` qui garde le numéro de ligne jusqu'au bout au lieu de le retirer afin de tester la position de elses par rapports à leurs blocks parents ifs etc...

Lors de l'évaluation, j'avais utilisé une variable de type (string,int) liste ref  `env` correspondante à la liste d'association d'environnement pour le premier rendu, mais j'ai pensé que ce n'était pas une solution optimale, et donc j'ai remplacé `env` par un module interne `NameTable` qui gére l'environnement de calcul du programme.

Concernant le résultat de l'évaluation de polish, ça marche également pour des nombres enormes, car j'ai utilisé la bibliothèque ZArith lors de ce second rendu.

J'ai modifié le type `Num of int` en le rendant `Num of Z.t` afin de manipuler des entiers de précision arbitraire (très grands) plutôt que les entiers de base de type int lors des simplifications (mais aussi pour le reste).

## Compilation et exécution

Pour compiler le projet, il suffit d'utiliser la commande `make` à la racine du dépot, puis d'utiliser les commandes suivantes afin de l'executer:

- ./run -eval <Chemin du fichier polish: dans le dossier "exemples" dans notre cas>
- ./run -reprint <Chemin du fichier polish>
Je me suis servi de trois bibliothèques: String afin d'utiliser la méthode `split_on_char` et List afin de manipuler les listes, ainsi que de la librairie ZArith qui permet de manipuler des entiers de précision arbitraire (très grands) plutôt que les entiers de base de type int.

##### List: <https://ocaml.org/api/List.html>

##### String: <https://ocaml.org/api/String.html>

##### ZArith: <https://antoinemine.github.io/Zarith/doc/latest/Z.html>

## Découpage modulaire

J'ai créer deux nouveaux dossiers lib et bin qui séparent les librairies et méthodes communes, et les modules principaux qui correspondent à chaque option. Chaque module (.ml) dans bin est associé à son interface (.mli) dans ces dossiers.

Dans lib, il y a :
- types: J'ai déplacé tous les types spécifiés au départ dans `polish.ml` dans un nouveau module à part afin qu'il servent aussi aux nouveaux modules.
- common: Ce module contient une unique méthode `cat_list` qui permet de concatèner deux listes en tailrec. Cette méthode est utilisée dans la plupart des modules principaux.

Dans bin, il y a:

- read: Nous permet de lire un fichier polish et de le retranscrire en code ocaml. J'ai caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `read_polish` du main.
- print: Nous permet d'imprimer du polish obtenu à partir de code ocaml. J'ai également caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `print_polish` du main.
- evaluation: Comme son nom l'indique, ce module permet d'évaluer un type program et d'afficher le résultat quand cela est demandé (via un PRINT). J'ai également caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `eval_polish` du main.
- simplification: Nous permet de simplifier un fichier polish et de réafficher cette version simplifiée. J'ai caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `simpl_polish` du main.
- variable: Affiche sur une première ligne toutes les variables présentes dans un fichier polish, puis dans une seconde ligne toutes les variables accédées avant une première initialisation. J'ai caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `vars_polish` du main.
- signs: Affiche toutes les variables du polish selon le format suivant: une variable par ligne, suivie d'un espace et des signes possibles de cette variable en fin d'execution du programme. Une dernière ligne affiche safe si aucune division par zéro n'a lieu, ou sinon un message personnalisé avec le numéro de la ligne de la première division par zéro rencontrée. J'ai caché toutes les méthodes intermédiaires via l'interface correspondante. Ce module est utilisé dans la méthode `sign_polish` du main.
- polish: Ce module correspond au main et gére l'ensemble du projet. J'ai rajouté un polish.mli qui n'affiche que le main.

## Organisation du travail

Je suis l'unique membre de mon groupe et donc j'ai fais le projet en entier tout seul. Le projet fut annoncé lors de la deuxième semaine du mois de novembre. J'ai donc lu le sujet pour la première fois deux semaines après afin de définir un ensemble de tâches au brouillon, bien avant de les rajouter sur le git. Je penses qu'il n'y aurait eu aucun intérêt à rajouter les tâches directement vu que je n'ai pas de binôme avec qui en discuter. J'avais tout le projet en tête vers fin novembre, mais je n'ai pu commencer à réellement le programmer qu'une semaine avant la date limite du premier rendu à cause d'autres projets que je devait rendre avant (langage C). J'ai suivi à peu près le même schéma concernant le deuxième rendu.

## Misc

Aucune remarque par rapport au sujet en lui-même mais j'ai tout de même une remarque par rapport à une des restrictions données:

- Remarque premier rendu: Il est écrit au 6ème point des critères d'évaluations données dans le fichier `CONSIGNES.md` qu'il vaudrait mieux éviter l'utilisation d'impératif, mais je ne vois pas du tout comment on pourrait faire autrement pour l'environnement lors de l'évaluation sans un ref, quand on utilise une liste d'association en tout cas: Peut-être avec un record ou le module NameTable donné en exemple, mais je n'ai pas eu le temps ou le courage d'utiliser l'une de ces approches vu que je suis plus à l'aise avec les listes.
- Remarque second rendu: J'ai finalement eu le courage de modifier la ref d'environnement en utilisant le module proposé dans le sujet et ça m'a facilité la tâche pour la suite.
- Remarque à part: Je me suis permi de remplacer le type bool de base par un new_bool qui propose trois possibilitées: true, false, ou Impossible pour la gestion des boucles if/else et while, et il se peut qu'il y ait plus simple afin de gérer celà, vu que cette approche n'a pas été proposée dans le sujet.
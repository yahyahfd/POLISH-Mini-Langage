Installation d'OCaml sur vos machines
=====================================

**Attention, le projet 2022 nécessite finalement dune >= 2.7 qui n'est disponible que pour Ubuntu >= 21, mais pas Ubuntu 20.04.
Solution pour Ubuntu 20.04 : installer manuellement le [paquet ocaml-dune 2.9.1 fourni](outils), ou bien utiliser OPAM pour installer un dune récent (voir plus bas)**


A partir du TP7 (exercices sur le morpion), et pour le projet de
ce cours, il vous faudra disposer du compilateur OCaml sur une machine
locale. Le TP7 nécessitera également la bibliothèque `graphics`, voir [plus bas](#graphics).

  - Si vous utilisez une machine Linux des salles 2031/2032/2003, pas de souci, tout y est.

  - Sur vos propres machines, tout dépend du système (lancer `lsb_release -a` en cas de doute sur la version):
    * Sur une Ubuntu 20.04+ ou Debian 11 : `sudo apt install ocaml ocaml-findlib ocaml-dune` suffit pour commencer.
      Ajouter aussi `sudo apt install libgraphics-ocaml-dev` sur Ubuntu 21+ / Debian 11.
    * Par contre Ubuntu 18.04 est **déconseillé** pour ce cours, [tout y est obsolète](#ubuntu-18).
    * Les autres Linux fournissent aussi OCaml directement, voir leur documentations respectives.
    * Si vous voulez avoir la dernière version d'OCaml, lisez la section ci-dessous sur [OPAM](#opam).
    * Sur Mac et Windows, privilégier une véritable machine virtuelle Linux. Sinon avec une installation récente de Windows, vous pouvez essayer via [WSL](https://docs.microsoft.com/fr-fr/windows/wsl/install#install).
      A défaut, suivre les instructions de https://ocaml.org/docs/install.html .

  - Depuis chez vous, une autre possibilité est de se connecter par ssh
    au serveur de l'UFR nommé `lulu`, [détails ici](http://www.informatique.univ-paris-diderot.fr/wiki/howto_connect).
    Ce serveur propose les mêmes programmes que les PC/Linux des salles
    2031/2032, et donc en particulier tout ce qu'il faut pour programmer
    en OCaml. En s'y connectant via `ssh -Y` vous pourrez même lancer
    des programmes graphiques, tant qu'ils ne sont pas trop gourmands
    en bande passante, et que vous avez un [serveur X](https://fr.wikipedia.org/wiki/X_Window_System)
    de votre côté.

## OPAM

OPAM est un outil qui vous permet de gérer facilement plusieurs installations
d'OCaml, et installer plus de bibliothèques que celles fournies par votre système.
En particulier, OPAM vous permet d'utiliser une version d'OCaml plus récente que
celle disponible dans votre distribution. Par exemple, OCaml 4.08, installé par défaut
dans Ubuntu 20.04, est sortie il y a plusieurs années.

Avec Ubuntu 20.04+ et Debian 11, vous pouvez installer OPAM directement depuis votre dépôt, c’est-à-dire
`sudo apt install opam`. Ne pas utiliser le OPAM 1.2 fourni par Ubuntu 18.04, [détails ici](#ubuntu-18).
Voir ces [instructions](https://ocaml.org/docs/install.html) pour les autres systèmes.
On y trouve aussi les premiers détails d'utilisation d'OPAM. Par exemple, pour installer OCaml 4.14.0 via OPAM :
```
  opam init
  eval $(opam env)
  opam switch create 4.14.0
  eval $(opam env)
```
Exécutez ensuite `ocaml -version` pour vérifier si vous avez bien installé la bonne
version d'OCaml.

**Attention** : en cas d'utilisation d'OPAM, bien penser ensuite à exécuter
dans son terminal `eval $(opam env)` à chaque session. En particulier, cela ajoute
à son `$PATH` les chemins non-standards utilisés par OPAM lors de ses installations.
On peut éventuellement ajouter cela une fois pour toute à sa configuration shell
(p.ex. dans son `~/.bashrc`).

## Dune

L'outil `dune` est très commode pour superviser la compilation d'un programme OCaml.
Sur un système Debian ou Ubuntu récent, faire `sudo apt install ocaml-dune`.
Note : ce paquet s'appellait `dune` à ses débuts (par exemple sur Debian 10).
Et là encore [attention avec Ubuntu 18.04](#ubuntu-18).

**Attention, le projet 2022 nécessite finalement dune >= 2.7 qui n'est disponible que pour Ubuntu >= 21, mais pas Ubuntu 20.04.
Solution pour Ubuntu 20.04 : installer manuellement le [paquet ocaml-dune 2.9.1 fourni](outils), ou bien utiliser OPAM pour installer un dune récent**

Après configuration de [opam](#opam) (voir plus haut), on peut installer `dune` via `opam install dune ocamlfind`.
Comme expliqué ci-dessus, bien penser alors à utiliser `eval $(opam env)` à chaque session pour ajuster son `$PATH`.

## Graphics

Sur MacOS, pour une installation native, commencer par installer le
serveur X nommé [XQuartz](https://www.xquartz.org/).

Si votre version d'OCaml est 4.08 ou moins, la bibliothèque `graphics` 
est fournie avec l'installation standard d'OCaml, a priori rien à faire de plus. 
C'est par exemple le cas d'Ubuntu 20.04. Attention alors à bien installer
le paquet `ocaml` et pas seulement `ocaml-nox` vu que ce dernier est
précisément OCaml *sans* graphics (nox c'est pour "No-X11").

Par contre si vous avez OCaml 4.09 ou plus récent, il faudra installer `graphics` 
en plus. Par exemple sur Ubuntu 21+ ou Debian 11+ : `sudo apt install libgraphics-ocaml-dev`.
Ou pour une installation via [opam](#opam) (voir plus haut), faire :
`opam install graphics ocamlfind`.

## Editeur de code

Mon favori est `emacs` équipé du plugin `tuareg` (paquet nommé `tuareg-mode` 
sous Ubuntu et Debian, ou plus récemment `elpa-tuareg`), et éventuellement du plugin `merlin`.
Sinon il y a beaucoup d'autres éditeurs de code proposant un support pour OCaml, à vous de voir.
Par exemple
[Visual Studio Code](https://code.visualstudio.com/) avec l'extension
[OCaml Platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform).
Pour l'utiliser via une connexion SSH (y compris localement avec WSL), installez l'extension
[Remote Development](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack).

## Pour aller plus loin

Revoir le cours 3 sur les [Outils](slides/cours-03-outils.md) disponibles dans 
l'écosystème OCaml.

## Ubuntu 18

La version 18.04 d'Ubuntu est fortement déconseillé pour ce cours, tout y date trop.
Il est temps de mettre à jour vers une Ubuntu plus récente (p.ex. 20.04 ou 22.04).
A défaut, voir sinon ces [conseils](https://opam.ocaml.org/doc/Install.html#Ubuntu) pour
installer `opam` version 2.x depuis un "PPA" plutôt que la version d'origine 1.x du système.
Ensuite il faudra utiliser cet [opam](#opam) pour installer `dune` sur ce système, via `opam install dune ocamlfind`.
En effet, Ubuntu 18.04 ne propose pas de paquet officiel pour l'outil `dune`.
Pire, Ubuntu 18.04 suggère alors un paquet nommé `whitedune` qui n'a **rien à voir**
avec OCaml. Mieux vaut le **désinstaller** si vous l'avez installé par erreur,
pour éviter toute confusion.

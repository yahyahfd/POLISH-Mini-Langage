open Lib.Types

(** Cette méthode permet de formater l'environnement final afin d'afficher
    une variable par ligne, suivie d'un espace et enfin de tous ses signes
    possibles. La dernière ligne affiche la première ligne où s'est passée
    une division par 0, sinon affiche "safe" *)
val print_signs : block -> unit

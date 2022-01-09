open Lib.Types

(** Cette méthode finale est celle utilisée afin d'afficher toutes 
    les variables du programme sur une ligne, puis les variables 
    non initialisés sur la suivante *)
val print_vars : block -> unit
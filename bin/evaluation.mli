open Lib.Types

(** NameTable représente l'environnement de calcul du programme *)
module NameTable :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
  end

(** Cette méthode finale fait tourner un program *)
val evaluation : block -> Z.t NameTable.t
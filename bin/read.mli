open Lib.Types

(** Cette méthode permet d'ouvrir un fichier et nous renvoie une liste de
    (position,String) correspondant à une ligne de chaque ligne du fichier *)
val file_to_pos_string_list : string -> (int * string) list

(** Cette méthode renvoie une liste de la forme (postion,indent,liste_de_mots)
    afin de faciliter le traitement *)
val indent_final_list : (int * string) list -> (int * int * string list) list

(** Cette méthode transforme une (indent,position,string list) list en block*)
val mk_instr : ('a * position * name list) list -> block

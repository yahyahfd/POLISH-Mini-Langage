open Lib.Types

(** Cette méthode transforme un block en (indent,position,string list) list*)
val block_to_instr_list : block -> (int * position * name list) list

(** Cette méthode permet de passer d'une (indent, position, string list) list
    à une string list *)
val ipsl_list_to_string : (int * 'a * string list) list -> string list

(** Cette méthode permet de passer d'une liste de string indentés à un unique
    string séparé par des \n pour signifier la fin d'une ligne *)
val string_list_to_string : string list -> string

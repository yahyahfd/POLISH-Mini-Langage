(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
open Types

(***********************************************************************)
let read_polish (filename:string) : program =
  let file = Read.file_to_pos_string_list filename in
  let indented_version = Read.indent_final_list file in
  Read.mk_instr indented_version;;

let print_polish (p:program) : unit =
  let ipsl_list = Print.block_to_instr_list p
  in let s_list = Print.ipsl_list_to_string ipsl_list
  in let final_string = Print.string_list_to_string s_list
  in print_string final_string;;

let eval_polish (p:program) : unit = Evaluation.evaluate p;;

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()

open Lib.Types

(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Lit un fichier polish et renvoie un objet de type program *)
let read_polish (filename:string) : program =
  let file = Read.file_to_pos_string_list filename in
  let indented_version = Read.indent_final_list file in
  Read.mk_instr indented_version;;

(** Prend un program en paramètre et le print *)
let print_polish (p:program) : unit =
  let ipsl_list = Print.block_to_instr_list p
  in let s_list = Print.ipsl_list_to_string ipsl_list
  in let final_string = Print.string_list_to_string s_list
  in print_string final_string;;

(** Prend un program en paramètre et l'évalue en le faisant tourner *)
let eval_polish (p:program) : unit = 
  let a = Evaluation.evaluation p in ();;

(** Prend un program en paramètre, le simplifie puis l'affiche *)
let simpl_polish (p:program) : unit = Simplification.simplify p;;

let vars_polish (p:program) : unit = Variable.print_vars p;;

let sign_polish (p:program) : unit = failwith "todo";;

(** Description du programme Polish *)
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string ("usage:\n./run -eval <filepath> to evaluate a polish file\n./r"^
               "un -reprint <filepath> to print a program with a polish syntax"^
               "\n")

(** Main *)
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> simpl_polish (read_polish file)
  | [|_;"-vars";file|] -> vars_polish(read_polish file)
  | [|_;"-sign";file|] -> failwith "todo"
  | _ -> usage ()

(* Lancement de ce main *)
let () = main ()
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)

(** Cette méthode permet d'ouvrir un fichier et nous renvoie une liste de (position,String) correspondant à une ligne de chaque ligne du fichier *)
let file_to_pos_string_list fname =
  let file = open_in fname in
  let read_file () = try Some (input_line file) with End_of_file -> None in
  let rec line_by_line acc count = match read_file () with
    | Some x -> line_by_line ((count,x)::acc) (count+1)
    | None -> close_in file; List.rev acc
  in line_by_line [] 1;;

(** Cette méthode sert à compter le nombre d'indentation d'un ligne: On compte le nombre de cases vides qui précèdent le premier mot *)
let rec indent_string_list count = function
  | [] -> count
  | x::xs -> if x = " " then indent_string_list (count+1) xs else count;;

(** Cette méthode est utilisé afin de retirer toute case vide de notre liste formée à partir d'un String de ligne *)
let rec remove_spaces_string_list acc = function
  | [] -> List.rev acc
  | x::xs ->
      if x = " " then remove_spaces_string_list acc xs
      else remove_spaces_string_list (x::acc) xs;;

(** Cette méthode renvoie une liste de la forme (postion,indentation,liste_de_mots) afin de faciliter le traitement *)
let rec pos_ind_string_list_list acc = function
  | [] -> acc
  | (c,x)::xs ->
      let string_list = (String.split_on_char ' ' x) in
      let indent = indent_string_list 0 string_list in
      let no_space_list = remove_spaces_string_list [] string_list in
      pos_ind_string_list_list ((c,indent,no_space_list)::acc) xs;;

let string_to_op x = match x with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Not an op";;
(** Not working in all cases: need to add aux function that finishes everything by "combining" accs *)
let string_to_expr x = try Num (int_of_string x) with int_of_string -> Var x;;

(** Cette méthode prend en argument deux accumulateurs faisant office de pile, une liste l d'instructions de type expr et en renvoie une liste d'expr *)
let rec list_to_expression acc1 acc2 l = match l with
  | [] ->
      begin match acc1,acc2 with
        | [], _ -> acc2
        | [x],y::y'::ys -> list_to_expression [] (ys@[Op (string_to_op x,y,y')]) l
        | x::x'::xs,y::ys -> list_to_expression xs (ys@[Op (string_to_op x,string_to_expr x',y)]) l

        | _, _ -> failwith "Wrong Syntax"
      end
  | x::xs ->
      if List.length acc1 > 1 then
        match acc1 with
        | y::y'::ys ->
            if (y'="+" || y'="-" || y'="*" || y'="/" || y'="%")
            && (y<>"+" && y<>"-" && y<>"*" && y<>"/" && y<>"%") then
              if (x<>"+" && x<>"-" && x<>"*" && x<>"/" && x<>"%") then
                list_to_expression ys (acc2@[Op (string_to_op y',string_to_expr y,string_to_expr x)]) xs
              else list_to_expression (x::acc1) acc2 xs
            else list_to_expression (x::acc1) acc2 xs
        | _ -> list_to_expression (x::acc1) acc2 xs
      else list_to_expression (x::acc1) acc2 xs;;

let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

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

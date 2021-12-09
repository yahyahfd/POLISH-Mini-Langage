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

(** Cette méthode permet de transformer un opérateur en son type op correspondant sinon renvoie une exception *)
let string_to_op x = match x with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Not an op";;

(** Cette méthode transforme un String en Num ou Var en fonction de s'il est reconnu ou pas en tant qu'int *)
let string_to_expr x = try Num (int_of_string x) with int_of_string -> Var x;;

(** Cette méthode retire le premier élément d'une liste si c'est possible, sinon renvoie une exception*)
let drop_first l = match l with
  | [] -> failwith "Empty List: Cannot Drop first element"
  | x::xs -> xs;;

(** Cette méthode rajoute l'élément var à la fin de la liste l (fonction récursive terminale) *)
let rec add_last var l acc = match l with
  | [] -> List.rev (var::acc)
  | x::xs -> add_last var xs (x::acc);;

(** Cette méthode prend en argument deux accumulateurs faisant office de pile, une liste l d'instructions de type expr et en renvoie une liste d'expr *)
let rec list_to_expression_list acc1 acc2 l = match l with
  | [] ->
      begin match acc1, acc2 with
        | [], _ -> acc2
        | x::xs, y::ys ->
            begin
              try list_to_expression_list xs (add_last (Op (string_to_op x,y,(List.nth ys 0))) (drop_first ys) []) l with Failure _ ->
                if List.length xs < 1 then failwith "Wrong Syntax"
                else match xs with
                  | z::zs -> list_to_expression_list zs (ys@[Op (string_to_op z,string_to_expr x,y)]) l
                  | _ -> failwith "Wrong Syntax"
            end
        | _, _ -> failwith "Wrong Syntax"
      end
  | x::xs ->
      if List.length acc1 > 1 then
        match acc1 with
        | y::y'::ys ->
            if (y'="+" || y'="-" || y'="*" || y'="/" || y'="%")
            && (y<>"+" && y<>"-" && y<>"*" && y<>"/" && y<>"%") then
              if (x<>"+" && x<>"-" && x<>"*" && x<>"/" && x<>"%") then
                list_to_expression_list ys (add_last (Op (string_to_op y',string_to_expr y,string_to_expr x)) acc2 []) xs
              else list_to_expression_list (x::acc1) acc2 xs
            else list_to_expression_list (x::acc1) acc2 xs
        | _ -> list_to_expression_list (x::acc1) acc2 xs
      else list_to_expression_list (x::acc1) acc2 xs;;

(** Cette méthode permet de transformer le résultat obtenu à travers la précédente en expression au lieu de liste d'expression d'une case *)
let expr_from_expr_list l = match l with
  | [x] -> x
  | _ -> failwith "Wrong Syntax";;

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

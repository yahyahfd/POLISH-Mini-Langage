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

(** Cette méthode sert à compter le nombre d'indentation d'un ligne: On compte le nombre d'espaces qui précèdent le premier mot *)
let indent_string_l l =
  let rec indent_string_list count = function
    | [] -> count
    | x::xs -> if x = "" then indent_string_list (count+1) xs else count
  in indent_string_list 0 l;;

(** Cette méthode est utilisé afin de retirer toute case vide de notre liste *)
let no_space_l l =
  let rec remove_spaces_string_list acc = function
    | [] -> List.rev acc
    | x::xs ->
        if x = "" then remove_spaces_string_list acc xs
        else remove_spaces_string_list (x::acc) xs
  in remove_spaces_string_list [] l;;

(** Cette méthode renvoie une liste de la forme (postion,indentation,liste_de_mots) afin de faciliter le traitement *)
let indent_final_list l =
  let rec pos_ind_string_list_list acc = function
    | [] -> List.rev acc
    | (c,x)::xs ->
        let string_list = (String.split_on_char ' ' x) in
        let indent = indent_string_l string_list in
        let no_space_list = no_space_l string_list in
        pos_ind_string_list_list ((c,indent,no_space_list)::acc) xs
  in pos_ind_string_list_list [] l;;

(** Cette méthode permet de transformer un opérateur en son type op correspondant sinon renvoie une exception *)
let string_to_op x = match x with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Not an op";;

(** Cette méthode transforme un String en Num ou Var en fonction de s'il est reconnu ou pas en tant qu'int. Renvoie une exception si x est un opérateur *)
let string_to_expr x = match x with
  | "+" | "-" | "*" | "/" | "%" -> failwith "This is an op"
  | _ -> try Num (int_of_string x) with int_of_string -> if x="" then failwith "Empty Var" else Var x;;

(** Cette méthode retire le premier élément d'une liste si c'est possible, sinon renvoie une exception*)
let drop_first l = match l with
  | [] -> failwith "Empty List: Cannot Drop first element"
  | x::xs -> xs;;

(** Cette méthode rajoute l'élément x à la fin de la liste l *)
let add_last x l =
  let rec add_last_list var l acc = match l with
    | [] -> List.rev (var::acc)
    | x::xs -> add_last_list var xs (x::acc)
  in add_last_list x l [];;

(** Cette méthode prend en argument deux accumulateurs faisant office de pile, une liste l d'instructions de type expr et en renvoie une liste d'expr, puis une expression finale *)
let expr_from_list l =
  let expr_from_expr_list = function
    | [x] -> x
    | _ -> failwith "Wrong Syntax: Expected a single element inside this list" in
  let rec list_to_expression_list acc1 acc2 l = match (no_space_l l) with
    | [] -> (match acc1, acc2 with
        | [x], [] -> [string_to_expr x]
        | [],[x]  -> acc2
        | x::x'::xs, y::ys -> (try list_to_expression_list xs (add_last (Op (string_to_op x',string_to_expr x,y)) ys) l
                               with _ -> failwith "Wrong Syntax: The formula is wrong")
        | _, [] -> failwith "Wrong Syntax: There are no operations left (acc2 and l empty) and acc1 contains more than 1 element"
        | [],_ -> failwith "Wrong Syntax: There are more than 1 operation left (acc2) but no more operators to calculate (acc1 and l empty)"
        | _, _ -> failwith "Wrong Syntax: Since both accumulators aren't matching proper results, the whole operation had a wrong syntax")
    | x::xs ->
        if List.length acc1 > 1 then
          match acc1 with
          | y::ys ->
              if (y="+" || y="-" || y="*" || y="/" || y="%")
              then
                if List.length acc2 = 1
                then
                  (try list_to_expression_list ys (add_last (Op (string_to_op y,(List.nth acc2 0),string_to_expr x)) (drop_first acc2)) xs
                   with | _ -> list_to_expression_list (x::acc1) acc2 xs)
                else
                  (if List.length acc2 > 1
                   then list_to_expression_list ys (add_last (Op (string_to_op y,(List.nth acc2 0),(List.nth acc2 1))) (drop_first (drop_first acc2))) (x::xs)
                   else list_to_expression_list (x::acc1) acc2 xs)
              else
                (if List.length ys > 0
                 then
                   (try
                      if List.length acc2 = 0 then
                        list_to_expression_list (drop_first ys) (add_last (Op (string_to_op (List.nth ys 0),string_to_expr y,string_to_expr x)) acc2) xs
                      else
                        list_to_expression_list (drop_first ys) (add_last (Op (string_to_op (List.nth ys 0),string_to_expr y,(List.nth acc2 0))) (drop_first acc2)) (x::xs)
                    with _ -> list_to_expression_list (x::acc1) acc2 xs)
                 else
                   list_to_expression_list (x::acc1) acc2 xs)
          | _ -> list_to_expression_list (x::acc1) acc2 xs
        else (if (List.length acc1 = 1 && List.length l = 1 && List.length acc2 = 1)
              then
                try [Op (string_to_op (List.nth acc1 0),(List.nth acc2 0),string_to_expr (List.nth l 0))]
                with _ -> failwith "Wrong Syntax: The formula is wrong"
              else list_to_expression_list (x::acc1) acc2 xs)
  in expr_from_expr_list (list_to_expression_list [] [] l);;

let string_to_comp x = match x with
  | "=" -> Eq
  | "<>" -> Ne
  | "<" -> Lt
  | "<=" -> Le
  | ">" -> Gt
  | ">=" -> Ge
  | _ -> failwith "Not a comp";;

let cond_list l : cond =
  let cond_from_list = function
    | [x] -> x
    | _ -> failwith "Wrong Syntax: Expected a single element inside a list" in
  let rec list_to_condition_list l exp1 res = match (no_space_l l) with
    | [] -> res
    | x::xs ->
        try list_to_condition_list [] [] [(expr_from_list (List.rev exp1),(string_to_comp x),expr_from_list xs)]
        with Failure _ ->
          list_to_condition_list xs (x::exp1) res
  in cond_from_list (list_to_condition_list l [] []);;

let line_to_instr l = match l with
  | [] -> failwith "Wrong Syntax"
  | x::xs -> match x with
    | "READ" -> if (List.length xs = 1) then Read (List.nth xs 0) else failwith "Wrong Syntax"
    | "PRINT" -> Print (expr_from_list xs)
    | "WHILE" -> failwith "todo:while" (*on appelle la fnct suivante*)
    | "IF" -> failwith "todo:if"
    | _ -> failwith "nah"
  (*  | _ -> if (List.length xs > 1) && ((List.nth xs 0) = ":=" ) then (*Set*) failwith "todo:set" else failwith "Wrong Syntax"
and instr_to_block l acc i : block = match l with
  | [] -> List.rev acc
  | x::xs -> instr_to_block xs ((i,line_to_instr xs)::acc) (i+1);;
Il faut correctement détecter un si c'est un block ou pas, via l'indentation, puis d'autres règles*)

let block_list_from_ind_final_list l =
  let rec group_indent acc = function
      | [] -> List.rev acc
      | (x,y,z)::xs -> if (x mod 2) <> 0 then failwith "Wrong Syntax" else
          match acc with
          | [] -> group_indent [[(x,y,z)]] xs
          | ((q,s,d)::ys)::zs ->
              if x = q then group_indent (((x,y,z)::(q,s,d)::ys)::zs) xs
              else group_indent ([(x,y,z)]::acc) xs
          | _ -> failwith "Impossible"
  in let rec remove_indent acc = function
      | [] -> List.rev acc
      | ((x,y,z)::zs)::xs -> remove_indent ((y,z)::acc) xs
      | _ -> failwith "todo"
  in remove_indent [] (group_indent [] l)

let test_ind_pos_stringl_list : (int*position*string list) list =
  [(0,1,["READ";"N"]);(0,2,["IF";"N";"=";"0"]);(2,3,["N";":=";"1"]);(0,4,["ELSE"]);(2,5,["N";":=";"2"])];;

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

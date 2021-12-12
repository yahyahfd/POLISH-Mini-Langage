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

(** Cette méthode permet de retirer les commentaires, et ajuste la position des lignes en fonction des changementes *)
let remove_comments l =
  let rec rm_com acc i = function
    | [] -> List.rev acc
    | (x,y)::xs -> match y with
      | [] -> failwith "Empty Line"
      | z::zs ->
          if z="COMMENT"
          then rm_com acc (i+1) xs
          else rm_com (((x-i),y)::acc) i xs
  in rm_com [] 0 l;;

(** Cette méthode transforme une liste de (postion,string) list en (position,string list) list*)
let int_string_list_list l =
  let rec isl_list acc = function
    | [] -> List.rev acc
    | (x,y)::xs -> isl_list ((x,(String.split_on_char ' ' y))::acc) xs
  in isl_list [] l;;

(** Cette méthode renvoie une liste de la forme (postion,indentation,liste_de_mots) afin de faciliter le traitement *)
let indent_final_list l =
  let rec pos_ind_string_list_list acc = function
    | [] -> List.rev acc
    | (c,x)::xs ->
        let indent = indent_string_l x in
        let no_space_list = no_space_l x in
        if (indent mod 2 = 0) then
          (if c=1 then
             if indent = 0 then pos_ind_string_list_list ((indent,c,no_space_list)::acc) xs
             else failwith "The first line of the program has to be of indent 0"
           else pos_ind_string_list_list ((indent,c,no_space_list)::acc) xs)
        else failwith ("Wrong Indentation at Line "^(string_of_int c)^": Indent has to be an even number")
  in pos_ind_string_list_list [] (remove_comments (int_string_list_list l));;

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

(** Cette méthode transforme un comparator en sa valeur correspondante *)
let string_to_comp x = match x with
  | "=" -> Eq
  | "<>" -> Ne
  | "<" -> Lt
  | "<=" -> Le
  | ">" -> Gt
  | ">=" -> Ge
  | _ -> failwith "Not a comp";;

(** Cette méthode transforme une string list en condition *)
let cond_list l : cond =
  let cond_from_list = function
    | [x] -> x
    | _ -> failwith "Wrong Syntax: Expected a single element inside this list" in
  let rec list_to_condition_list l exp1 res = match (no_space_l l) with
    | [] -> res
    | x::xs ->
        try list_to_condition_list [] [] [(expr_from_list (List.rev exp1),(string_to_comp x),expr_from_list xs)]
        with Failure _ ->
          list_to_condition_list xs (x::exp1) res
  in cond_from_list (list_to_condition_list l [] []);;

(*let block_list_from_ind_final_list l =
   let rec group_indent acc = function
| [] -> List.rev acc
| (x,y,z)::xs -> if (x mod 2) <> 0 then failwith ("Wrong Indentation at Line "^(string_of_int y)^": Indent has to be an even number") else
    match acc with
    | [] -> group_indent [[(x,y,z)]] xs
    | []::s -> group_indent s xs
    | ((q,s,d)::ys)::zs ->
        if x = q then group_indent ((add_last (x,y,z) ((q,s,d)::ys))::zs) xs
        else group_indent ([(x,y,z)]::acc) xs
in let rec remove_indent acc = function
    | [] -> List.rev acc
    | ((x,y,z)::xs)-> remove_indent ((y,z)::acc) xs
in let rec no_ind_list acc = function
    | [] -> List.rev acc
    | x::xs -> no_ind_list ((remove_indent [] x)::acc) xs
in no_ind_list [] (group_indent [] l);; *)

let mk_instr l =
  let rec instr_u acc = function
    | (x,y,z)::xs -> (match z with
        | r::rs -> (match r with
            | "READ" ->
                if (List.length rs = 1)
                then
                  match xs with
                  | [] -> instr_u (add_last (y,(Read (List.nth rs 0))) acc) xs
                  | (x',y',z')::xs' ->
                      if x<>x' then failwith ("Wrong Syntax at line "^(string_of_int y')^": Read can only be followed by an instruction with same indent (unless inside an if, else or while loop)")
                      else instr_u (add_last (y,(Read (List.nth rs 0))) acc) xs
                else failwith "Wrong Syntax: Read expects a single argument"
            | "PRINT" ->
                (match xs with
                 | [] -> instr_u (add_last (y,(Print (expr_from_list rs))) acc) xs
                 | (x',y',z')::xs' ->
                     if x<>x' then failwith ("Wrong Syntax at line "^(string_of_int y')^": Print can only be followed by an instruction with same indent (unless inside an if, else or while loop)")
                     else instr_u (add_last (y,(Print (expr_from_list rs))) acc) xs)
            | "WHILE" ->
                if List.length rs > 0 then
                  let rec find_block n acc' = function
                    | [] -> instr_u (add_last (y,(While(cond_list rs,(instr_u [] acc')))) acc) []
                    | (x',y',z')::xs' ->
                        if (x'> n) then find_block n (add_last (x',y',z') acc') xs'
                        else instr_u (add_last (y,(While(cond_list rs,(instr_u [] acc')))) acc) ((x',y',z')::xs')
                  in find_block x [] xs
                else failwith "Wrong Syntax: While expects arguments"
            | "IF" ->
                if List.length rs > 0 then
                  let rec find_if_block n acc' = function
                    | [] -> instr_u (add_last (y,(If(cond_list rs,(instr_u [] acc'),[]))) acc) []
                    | (x',y',z')::xs' ->
                        if (x'> n) then find_if_block n (add_last (x',y',z') acc') xs'
                        else
                          (if x'=n then
                             (if List.nth z' 0 = "ELSE" then
                                let rec find_else_block n' acc'' = function
                                  | [] -> instr_u (add_last (y,If(cond_list rs,(instr_u [] acc'),(instr_u [] acc''))) acc) []
                                  | (x'',y'',z'')::xs'' ->
                                      if(x''> n') then find_else_block n' (add_last (x'',y'',z'') acc'') xs''
                                      else instr_u (add_last (y,If(cond_list rs,(instr_u [] acc'),(instr_u [] acc''))) acc) ((x'',y'',z'')::xs'')
                                in find_else_block x' [] xs'
                              else instr_u (add_last (y,If(cond_list rs,(instr_u [] acc'),[])) acc) ((x',y',z')::xs'))
                           else instr_u (add_last (y,If(cond_list rs,(instr_u [] acc'),[])) acc) ((x',y',z')::xs'))
                  in find_if_block x [] xs
                else failwith "Wrong Syntax: If expects arguments"
            | _ -> (if (List.length rs > 1  && (List.nth rs 0) = ":=")
                    then
                      match string_to_expr r with
                      | Var _ ->
                          (match xs with
                           | [] -> instr_u (add_last (y,(Set (r,(expr_from_list (drop_first rs))))) acc) xs
                           | (x',y',z')::xs' ->
                               if x<>x' then failwith ("Wrong Syntax at line "^(string_of_int y')^": Set can only be followed by an instruction with same indent (unless inside an if, else or while loop)")
                               else instr_u (add_last (y,(Set (r,(expr_from_list (drop_first rs))))) acc) xs)
                      | _ -> failwith "You can only set a variable"
                    else failwith "Wrong Syntax: Pattern not matched"))
        | [] -> instr_u acc xs)
    | [] -> acc
  in instr_u [] l;;

let test_a =
  [(0,1,["READ";"N"]);(0,2,["IF";"N";"=";"0"]);(2,3,["N";":=";"1"]);(0,4,["ELSE"]);(2,5,["N";":=";"2"])];;
let test_b =
  [(0,1,["READ";"N"]);(0,2,["N";":=";"0"]);(0,3,["PRINT";"N"])];;
let test_c =
  [(0,1,["READ";"N"]);(0,2,["N";":=";"0"]);(0,3,["WHILE";"N";"<";"3"]);(2,4,["N";":=";"+";"N";"1"]);(0,5,["PRINT";"N"])];;
let test_d = (*Will never happen*)
  [(0,1,["READ";"N"]);(2,2,["N";":=";"0"])]
let test_e = (*Will never happen*)
  [(0,1,["READ";"N"]);(1,2,["N";":=";"0"])]
let test_f = (*Will never happen*)
  [(2,1,["READ";"N"]);(0,2,["N";":=";"0"])]
let test_g =
  [(1,"COMMENT valeur absolue");(2,"READ n");(3,"IF n < 0");(4,"  res := - 0 n");(5,"ELSE");(6,"  res := n");(7,"PRINT res")]
let test_h =
  [(0,1,["READ";"N"]);(0,2,["IF";"N";"=";"0"]);(2,3,["N";":=";"1"])];;
let test_i = (*Will never happen*)
  [(0,1,["READ";"N"]);(0,3,["IF";"N";"=";"0"]);(2,4,["N";":=";"1"])];;

let read_polish (filename:string) : program =
  let file = file_to_pos_string_list filename in
  let indented_version = indent_final_list file in
  mk_instr indented_version;;

(** Cette méthode permet de passer d'une expression à un string list correspondant *)
let expr_string_list p =
  let op_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
  in let rec expr_to_string acc = function
      | Op (a,b,c) -> (expr_to_string (expr_to_string ((op_to_string a)::acc) b) c)
      | Var a -> (a::acc)
      | Num a -> ((string_of_int a)::acc)
  in List.rev (expr_to_string [] p);;

(** Cette méthode concatène en deux liste *)
let cat_list l1 l2 =
  let rec concat_list acc l = function
    | [] -> (match l with
        | [] -> List.rev acc
        | _ -> concat_list acc [] l
      )
    | x::xs -> concat_list (x::acc) l xs
  in concat_list [] l2 l1;;

(** Cette méthode transforme une condition p en un string list correspondant*)
let cond_string_list p =
  let comp_to_string = function
    | Eq -> "="
    | Ne -> "<>"
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
  in let cond_to_string = function
      | (a,b,c) -> cat_list (add_last (comp_to_string b) (expr_string_list a)) (expr_string_list c)
  in cond_to_string p;;

(** Cette méthode transforme un block en (indentation,position,string list) list*)
let block_to_instr_list l =
  let rec count_ind acc i = function
    | [] -> acc
    | (x,y)::xs -> match y with
      | Read r -> count_ind ((i,x,["READ";r])::acc) i xs
      | Print r -> count_ind ((i,x,("PRINT"::(expr_string_list r)))::acc) i xs
      | If (a,b,c) -> (match c with
          | [] -> count_ind (cat_list (count_ind [(i,x,("IF"::cond_string_list a))] (i+2) b) acc) i xs
          | (line,ins)::_ -> count_ind (cat_list (count_ind [(i,(line-1),["ELSE"])] (i+2) c) (cat_list (count_ind [(i,x,("IF"::cond_string_list a))] (i+2) b) acc)) i xs)
      | While (a,b) -> count_ind (cat_list (count_ind [(i,x,("WHILE"::cond_string_list a))] (i+2) b) acc) i xs
      | Set (a,b) -> count_ind ((i,x,(a::":="::(expr_string_list b)))::acc) i xs
  in List.rev (count_ind [] 0 l);;

(** Cette méthode permet de passer d'une (indent,position,stringlist) list à une string list*)
let ipsl_list_to_string l =
  let rec remove_pos acc = function
    | [] -> List.rev acc
    | (a,b,c)::ac -> remove_pos ((a,c)::acc) ac
  in let rec add_indent s = function
      | 0 -> s
      | i -> add_indent (" "^s) (i-1)
  in let rec res_list acc = function
      | [] -> List.rev acc
      | (x,y)::xs -> res_list ((add_indent (String.concat " " y) x)::acc) xs
  in res_list [] (remove_pos [] l);;

let string_list_to_string l =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (acc^x^"\n") xs
  in aux "" l;;

let print_polish (p:program) : unit =
  let ipsl_list = block_to_instr_list p
  in let s_list = ipsl_list_to_string ipsl_list
  in let final_string = string_list_to_string s_list
  in print_string final_string;;

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

open Types

(** Cette méthode permet de transformer un op en son string correspondant*)
let op_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%";;

(** Cette méthode permet de passer d'une expression à un string list *)
let expr_string_list p =
  let rec aux acc = function
    | Op (a,b,c) -> (aux (aux ((op_to_string a)::acc) b) c)
    | Var a -> (a::acc)
    | Num a -> ((string_of_int a)::acc)
  in List.rev (aux [] p);;

(** Cette méthode concatène en deux liste *)
let cat_list l1 l2 =
  let rec aux acc l = function
    | [] -> (match l with
        | [] -> List.rev acc
        | _ -> aux acc [] l
      )
    | x::xs -> aux (x::acc) l xs
  in aux [] l2 l1;;

(** Cette méthode permet de transformer un comp en string *)
let comp_to_string = function
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">=";;

(** Cette méthode transforme une condition p en un string list correspondant*)
let cond_string_list = function
  | (a,b,c) -> cat_list (expr_string_list a)
                 ((comp_to_string b)::(expr_string_list c));;

(** Cette méthode transforme un block en (indent,position,string list) list*)
let block_to_instr_list l =
 let rec count_ind acc i = function
   | [] -> acc
   | (x,y)::xs -> match y with
     | Read r -> count_ind ((i,x,["READ";r])::acc) i xs
     | Print r -> count_ind ((i,x,("PRINT"::(expr_string_list r)))::acc) i xs
     | If (a,b,c) -> (match c with
         | [] -> count_ind (cat_list
                              (count_ind [(i,x,("IF"::cond_string_list a))]
                                 (i+2) b) acc) i xs
         | (line,ins)::_ ->
             let else_block = (count_ind [(i,(line-1),["ELSE"])] (i+2) c) in
             let if_block = (count_ind [(i,x,("IF"::cond_string_list a))]
                               (i+2) b) in
             count_ind (cat_list else_block (cat_list if_block acc)) i xs)
     | While (a,b) -> count_ind (cat_list
                                   (count_ind
                                      [(i,x,("WHILE"::cond_string_list a))]
                                      (i+2) b) acc) i xs
     | Set (a,b) -> count_ind ((i,x,(a::":="::(expr_string_list b)))::acc) i xs
 in List.rev (count_ind [] 0 l);;

 (** Cette méthode permet de passer d'une (indent, position, string list) list
     à une string list *)
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

(** Cette méthode permet de passer d'une liste de string indentés à un unique
    string séparé par des \n pour signifier la fin d'une ligne *)
let string_list_to_string l =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (acc^x^"\n") xs
  in aux "" l;;

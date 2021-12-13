open Types

(** Cette méthode rajoute l'élément x à la fin de la liste l *)
let add_last x l =
  let rec add_last_list var l acc = match l with
    | [] -> List.rev (var::acc)
    | x::xs -> add_last_list var xs (x::acc)
  in add_last_list x l [];;

(** Cette méthode permet d'ouvrir un fichier et nous renvoie une liste de
    (position,String) correspondant à une ligne de chaque ligne du fichier *)
let file_to_pos_string_list fname =
  let file = open_in fname in
  let read_file () = try Some (input_line file) with End_of_file -> None in
  let rec line_by_line acc count = match read_file () with
    | Some x -> line_by_line ((count,x)::acc) (count+1)
    | None -> close_in file; List.rev acc
  in line_by_line [] 1;;

(** Cette méthode sert à compter le nombre d'indentation d'un ligne: On compte
    le nombre d'espaces qui précèdent le premier mot *)
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

(** Cette méthode permet de retirer les commentaires, et ajuste la position des
    lignes en fonction des changements *)
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

(** Cette méthode transforme une liste de (postion,string) list en
    (position,string list) list*)
let int_string_list_list l =
  let rec isl_list acc = function
    | [] -> List.rev acc
    | (x,y)::xs -> isl_list ((x,(String.split_on_char ' ' y))::acc) xs
  in isl_list [] l;;

(** Cette méthode renvoie une liste de la forme (position,indent,liste_de_mots)
    afin de faciliter le traitement *)
let indent_final_list l =
  let rec pos_ind_string_list_list acc = function
    | [] -> List.rev acc
    | (c,x)::xs ->
        let indent = indent_string_l x in
        let no_space_list = no_space_l x in
        if (indent mod 2 = 0) then
          (if c=1 then
             if indent = 0 then pos_ind_string_list_list
                 ((indent,c,no_space_list)::acc) xs
             else failwith "The first line of the program has to be of indent 0"
           else pos_ind_string_list_list ((indent,c,no_space_list)::acc) xs)
        else failwith ("Wrong Indentation at Line "^(string_of_int c)^
                       ": Indent has to be an even number")
  in pos_ind_string_list_list [] (remove_comments (int_string_list_list l));;

(** Cette méthode permet de transformer un opérateur en son type op
    correspondant sinon renvoie une exception *)
let string_to_op x = match x with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith "Not an op";;

(** Cette méthode transforme un String en Num ou Var en fonction de s'il est
    reconnu ou pas en tant qu'int. Renvoie une exception si x est un op *)
let string_to_expr x = match x with
  | "+" | "-" | "*" | "/" | "%" -> failwith "This is an op"
  | _ ->
      try Num (int_of_string x)
      with int_of_string ->
        if x="" then failwith "Empty Var" else Var x;;

(** Cette méthode retire le premier élément d'une liste si c'est possible,
    sinon renvoie une exception*)
let drop_first l = match l with
  | [] -> failwith "Empty List: Cannot Drop first element"
  | x::xs -> xs;;

(** Cette méthode prend en argument deux accumulateurs faisant office de pile,
    une liste l d'instructions de type expr et en renvoie une liste d'expr,
    puis une expression finale *)
let expr_from_list l =
  let expr_from_expr_list = function
    | [x] -> x
    | _ -> failwith "Wrong Syntax: Expected a single element in this list" in
  let rec list_to_expression_list acc1 acc2 l = match (no_space_l l) with
    | [] -> (match acc1, acc2 with
        | [x], [] -> [string_to_expr x]
        | [],[x]  -> acc2
        | x::x'::xs, y::ys ->
            (try list_to_expression_list xs
                   (add_last (Op (string_to_op x',string_to_expr x,y)) ys) l
             with _ -> failwith "Wrong Syntax: The formula is wrong")
        | _, [] -> failwith
                     ("Wrong Syntax: There are no operations left (acc2 and "^
                      "l empty) and acc1 contains more than 1 element")
        | [],_ -> failwith
                    ("Wrong Syntax: There is more than 1 operation left (acc2)"^
                     " but no more operators to calculate (acc1 and l empty)")
        | _, _ -> failwith
                    ("Wrong Syntax: Since both accumulators aren't matching"^
                     "proper results, the whole operation had a wrong syntax"))
    | x::xs ->
        if List.length acc1 > 1 then
          match acc1 with
          | y::ys ->
              if (y="+" || y="-" || y="*" || y="/" || y="%")
              then
                if List.length acc2 = 1
                then
                  (try list_to_expression_list ys
                         (add_last (Op (string_to_op y,(List.nth acc2 0),
                                        string_to_expr x)) (drop_first acc2)) xs
                   with | _ -> list_to_expression_list (x::acc1) acc2 xs)
                else
                  (if List.length acc2 > 1
                   then list_to_expression_list ys
                       (add_last (Op (string_to_op y,(List.nth acc2 0),
                                      (List.nth acc2 1)))
                          (drop_first (drop_first acc2))) (x::xs)
                   else list_to_expression_list (x::acc1) acc2 xs)
              else
                (if List.length ys > 0
                 then
                   (try
                      if List.length acc2 = 0 then
                        list_to_expression_list (drop_first ys)
                          (add_last (Op (string_to_op (List.nth ys 0),
                                         string_to_expr y,string_to_expr x))
                             acc2) xs
                      else
                        list_to_expression_list (drop_first ys)
                          (add_last (Op (string_to_op (List.nth ys 0),
                                         string_to_expr y,
                                         (List.nth acc2 0))) (drop_first acc2))
                          (x::xs)
                    with _ -> list_to_expression_list (x::acc1) acc2 xs)
                 else
                   list_to_expression_list (x::acc1) acc2 xs)
          | _ -> list_to_expression_list (x::acc1) acc2 xs
        else (if (List.length acc1 = 1 && List.length l = 1
                  && List.length acc2 = 1)
              then
                try [Op (string_to_op (List.nth acc1 0),(List.nth acc2 0),
                         string_to_expr (List.nth l 0))]
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
    | _ -> failwith "Wrong Syntax: Expected a single element in this list" in
  let rec list_to_condition_list l exp1 res = match (no_space_l l) with
    | [] -> res
    | x::xs ->
        try list_to_condition_list [] []
              [(expr_from_list (List.rev exp1),
                (string_to_comp x),
                expr_from_list xs)]
        with Failure _ ->
          list_to_condition_list xs (x::exp1) res
  in cond_from_list (list_to_condition_list l [] []);;

(** Cette méthode transforme une (indent,position,stringlist) list en block*)
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
                      if x<>x'
                      then
                        failwith ("Wrong Syntax at line "^(string_of_int y')^
                                  ": Read can only be followed by an "^
                                  "instruction with same indent (unless inside"^
                                  " an if, else or while loop)")
                      else instr_u (add_last (y,(Read (List.nth rs 0))) acc) xs
                else failwith "Wrong Syntax: Read expects a single argument"
            | "PRINT" ->
                (match xs with
                 | [] -> instr_u (add_last (y,(Print (expr_from_list rs)))
                                    acc) xs
                 | (x',y',z')::xs' ->
                     if x<>x'
                     then
                       failwith ("Wrong Syntax at line "^(string_of_int y')^
                                 ": Print can only be followed by an "^
                                 "instruction with same indent (unless inside "^
                                 "an if, else or while loop)")
                     else instr_u (add_last (y,(Print (expr_from_list rs)))
                                     acc) xs)
            | "WHILE" ->
                if List.length rs > 0 then
                  let rec find_block n acc' = function
                    | [] -> instr_u (add_last (y,(While(cond_list rs,
                                                        (instr_u [] acc'))))
                                       acc) []
                    | (x',y',z')::xs' ->
                        if (x'> n) then find_block n (add_last (x',y',z')
                                                        acc') xs'
                        else instr_u (add_last (y,(While(cond_list rs,
                                                         (instr_u [] acc'))))
                                        acc) ((x',y',z')::xs')
                  in find_block x [] xs
                else failwith "Wrong Syntax: While expects arguments"
            | "IF" ->
                if List.length rs > 0 then
                  let rec find_if_block n acc' = function
                    | [] -> instr_u (add_last (y,(If(cond_list rs,
                                                     (instr_u [] acc'),[])))
                                       acc) []
                    | (x',y',z')::xs' ->
                        if (x'> n) then find_if_block n (add_last (x',y',z')
                                                           acc') xs'
                        else
                          (if x'=n then
                             (if List.nth z' 0 = "ELSE" then
                                let rec find_else_block n' acc'' = function
                                  | [] -> instr_u (add_last
                                                     (y,If(cond_list rs,
                                                           (instr_u [] acc'),
                                                           (instr_u [] acc'')))
                                                     acc) []
                                  | (x'',y'',z'')::xs'' ->
                                      if(x''> n') then find_else_block n'
                                          (add_last (x'',y'',z'') acc'') xs''
                                      else instr_u (add_last
                                                      (y,If(cond_list rs,
                                                            (instr_u [] acc'),
                                                            (instr_u [] acc'')))
                                                      acc) ((x'',y'',z'')::xs'')
                                in find_else_block x' [] xs'
                              else instr_u (add_last (y,
                                                      If(cond_list rs,
                                                         (instr_u [] acc'),[]))
                                              acc) ((x',y',z')::xs'))
                           else instr_u (add_last (y,If(cond_list rs,
                                                        (instr_u [] acc'),[]))
                                           acc) ((x',y',z')::xs'))
                  in find_if_block x [] xs
                else failwith "Wrong Syntax: If expects arguments"
            | _ -> (if (List.length rs > 1  && (List.nth rs 0) = ":=")
                    then
                      match string_to_expr r with
                      | Var _ ->
                          (match xs with
                           | [] -> instr_u (add_last (y,
                                                      (Set
                                                         (r,
                                                          (expr_from_list
                                                             (drop_first rs)))))
                                              acc) xs
                           | (x',y',z')::xs' ->
                               if x<>x'
                               then
                                 failwith ("Wrong Syntax at line "^
                                           (string_of_int y')^": Set can only "^
                                           "be followed by an instruction with"^
                                           " same indent (unless inside an if,"^
                                           " else or while loop)")
                               else instr_u (add_last
                                               (y,(Set (r,
                                                        (expr_from_list
                                                           (drop_first rs)))))
                                               acc) xs)
                      | _ -> failwith "You can only set a variable"
                    else failwith "Wrong Syntax: Pattern not matched"))
        | [] -> instr_u acc xs)
    | [] -> acc
  in instr_u [] l;;

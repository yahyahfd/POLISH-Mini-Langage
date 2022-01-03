open Types

(** Cette méthode permet de simplifier une expression (quand c'est possible) *)
let expr_simpl expr =
  let rec expr_simpl_aux = function
    | Op (o,Num e1, Num e2) ->
        (match o with
         | Add ->  Num (Z.add e1 e2)
         | Sub ->  Num (Z.sub e1 e2)
         | Mul ->  Num (Z.mul e1 e2)
         | Div ->  Num (Z.div e1 e2)
         | Mod ->  Num (Z.rem e1 e2))
    | Op (o, Var v1, Var v2) as r -> r
    | Op (o, Var v1, x) -> Op (o, Var v1, expr_simpl_aux x)
    | Op (o, x, Var v2) -> Op (o, expr_simpl_aux x, Var v2)
    | Op (o,Num e1, x) ->Op (o, Num e1, expr_simpl_aux x)
    | Op (o,x, Num e2) -> Op (o, expr_simpl_aux x, Num e2)
    | Op (o,e1,e2) -> expr_simpl_aux (Op (o,expr_simpl_aux e1, expr_simpl_aux e2))
    | x -> x
  in expr_simpl_aux expr;;

(** Cette méthode permet de simplifier une condition en sa valeur booléenne *)
let condition_simpl = function
  | (e1,c,e2) -> expr_simpl e1,c,expr_simpl e2;;

let calculate_condition = function
  | (Num e1,Eq,Num e2) -> (Z.equal e1 e2)
  | (Num e1,Ne,Num e2) -> not (Z.equal e1 e2)
  | (Num e1,Lt,Num e2) -> (Z.lt e1 e2)
  | (Num e1,Le,Num e2) -> (Z.leq e1 e2)
  | (Num e1,Gt,Num e2) -> (Z.gt e1 e2)
  | (Num e1,Ge,Num e2) -> (Z.geq e1 e2)
  | _ -> raise (Problem "You can only calculate a condtion that uses Nums");;

let cat_list l1 l2 =
  let rec aux acc l = function
    | [] -> (match l with
        | [] -> List.rev acc
        | _ -> aux acc [] l
      )
    | x::xs -> aux (x::acc) l xs
  in aux [] l2 l1;;

(*simplification UNIQUEMENT sur deux entier, si variable dont on attend un read ou autre, ne PAS simplifier*)
let simplify l =
  let rec simplify_aux i acc = function
    | [] -> List.rev acc
    | (x,y)::xs ->
        let xs_simpl =
          (match xs with
           | [] -> simplify_aux i acc xs
           | (line,_)::_ -> simplify_aux (line-x+i) acc xs) in
        (match y with
         | Read r -> simplify_aux i (((x-i),y)::acc) xs
         | Set (n,e) -> simplify_aux i (((x-i),Set(n,expr_simpl e))::acc) xs
         | If (a,b,c) ->
             let sub_block = function
               | [] -> xs_simpl
               | subs -> simplify_aux (i+1) acc (cat_list subs xs)
             in let sim_cond = condition_simpl a in
             (match sim_cond with
              | (Num e1,_,Num e2) ->
                  if calculate_condition sim_cond
                  then sub_block b
                  else sub_block c
              | _ -> simplify_aux i (((x-i),If (sim_cond,
                                            (simplify_aux i [] b),
                                            (simplify_aux i [] c)))::acc) xs)
         | While (a,b) ->
             let sim_cond = condition_simpl a in
             (match sim_cond with
              | (Num e1, cond1, Num e2) ->
                  if calculate_condition sim_cond
                  then simplify_aux i (((x-i), While (sim_cond,
                                                  (simplify_aux i [] b)))::acc) xs
                  else xs_simpl
              | _ -> simplify_aux i (((x-i),While (sim_cond,
                                               (simplify_aux i [] b)))::acc) xs)
         | Print e -> simplify_aux i ((x,Print (expr_simpl e))::acc) xs)
  in simplify_aux 0 [] l;;

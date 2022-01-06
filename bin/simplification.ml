open Lib.Types
open Lib.Common

(** Cette méthode permet de simplifier une expression (quand c'est possible) *)
let expr_simpl expr =
  let rec expr_simpl_aux = function
    | Op (o,Num e1, Num e2) ->
        (match o with
         | Add ->  Num (Z.add e1 e2)
         | Sub ->  Num (Z.sub e2 e1)
         | Mul ->  Num (Z.mul e1 e2)
         | Div ->  Num (Z.div e1 e2)
         | Mod ->  Num (Z.rem e1 e2))
    | Op (o,Num e1, x) ->
        let res = expr_simpl_aux x in
        (match (e1, o) with
         | tmp, Add when (Z.equal tmp Z.zero) -> expr_simpl_aux x
         | tmp, Sub when (Z.equal tmp Z.zero) ->
             (match res with
              | Num n -> Num (Z.neg n)
              | _ -> Op (o, Num e1, res))
         | tmp, Mul when (Z.equal tmp Z.zero) -> Num Z.zero
         | tmp, Div when (Z.equal tmp Z.zero) -> Num Z.zero
         | tmp, Mod when (Z.equal tmp Z.zero) -> Num Z.zero
         | tmp, Mul when (Z.equal tmp Z.one) -> expr_simpl_aux x
         | _ , _ ->
             (match res with
              | Num n -> expr_simpl_aux (Op (o, Num e1, res))
              | _ -> Op (o, Num e1, res)))
    | Op (o,x, Num e2) ->
        let res = expr_simpl_aux x in
        (match e2, o with
         | tmp, Add when (Z.equal tmp Z.zero) -> expr_simpl_aux x
         | tmp, Sub when (Z.equal tmp Z.zero) -> expr_simpl_aux x
         | tmp, Mul when (Z.equal tmp Z.zero) -> Num Z.zero
         | tmp, Mul when (Z.equal tmp Z.one) -> expr_simpl_aux x
         | tmp, Div when (Z.equal tmp Z.one) -> expr_simpl_aux x
         | tmp, Mod when (Z.equal tmp Z.one) -> Num Z.zero
         | _, _ ->
             (match res with
              | Num n -> expr_simpl_aux (Op (o, res, Num e2))
              | _ -> Op (o, res, Num e2)))
    | Op (o, Var v1, Var v2) as r -> r
    | Op (o, Var v1, x) -> Op (o, Var v1, expr_simpl_aux x)
    | Op (o, x, Var v2) -> Op (o, expr_simpl_aux x, Var v2)
    | Op (o,e1,e2) -> expr_simpl_aux (Op (o,expr_simpl_aux e1, expr_simpl_aux e2))
    | x -> x
  in expr_simpl_aux expr;;

(** Cette méthode permet de simplifier une condition en sa valeur booléenne *)
let condition_simpl = function
  | (e1,c,e2) -> expr_simpl e1,c,expr_simpl e2;;

(** Cette méthode calcule une condition quand il n'y a pas d'inconnu *)
let calculate_condition = function
  | (Num e1,Eq,Num e2) -> (Z.equal e1 e2)
  | (Num e1,Ne,Num e2) -> not (Z.equal e1 e2)
  | (Num e1,Lt,Num e2) -> (Z.lt e1 e2)
  | (Num e1,Le,Num e2) -> (Z.leq e1 e2)
  | (Num e1,Gt,Num e2) -> (Z.gt e1 e2)
  | (Num e1,Ge,Num e2) -> (Z.geq e1 e2)
  | _ -> raise (Problem "You can only calculate a condtion that uses Nums");;

(** Cette méthode finale simplifie un program et le print *)
let simplify l =
  let rec fix_line_count acc i = function
    | [] -> List.rev acc
    | (ind,pos,l')::xs -> fix_line_count ((ind,i,l')::acc) (i+1) xs
  in let rec simplify_aux i acc = function
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
  in let ipsl_list = fix_line_count [] 1 (Print.block_to_instr_list (simplify_aux 0 [] l))
  in let s_list = Print.ipsl_list_to_string ipsl_list
  in let final_string = Print.string_list_to_string s_list
  in print_string final_string;;

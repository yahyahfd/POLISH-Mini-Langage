open Lib.Types

(*deux listes: 
on parcourt le program et
L1: si on croise un set ou read, on rajoute la variable ici
L2: si on croise une variable, on vérifie si elle est dans l1, si elle ne l'est pas, on rajoute ici
cas particuliers:
-while : une var est initialisé avant le while
-if : soit var initialisé avant le if, et donc c'est bon, sinon dans le if ET le else

module Names = Set.make(String)
modules*)
 (** d'abord on ajoute toutes les variables du programme *)

module Names = Set.Make(String)

(** Liste avec répétition de toutes les variables d'une expression 
    env contient les variables initialisées, et env2 les non*)
let rec expr_to_var_list (env,env2) = function
  | Num e -> (env,env2)
  | Var e -> 
      if (Names.mem e env) 
      then (env,env2)
      else (env,(Names.add e env2))
  | Op (op, e1, e2) -> expr_to_var_list (expr_to_var_list (env,env2) e1) e2
      
let env_complet (a,b) = Names.union a b
    
let condition_to_var_list (env,env2) = function
  | e1,c,e2 -> expr_to_var_list (expr_to_var_list (env,env2) e1) e2
  

let all_diffs a b a' b' = 
  let t1 = Names.union a' b'
  in let t2 = (Names.union (Names.diff a b) (Names.diff b a))
  in Names.union t1 t2
  
let all_vars p = 
  let rec get_vars (env,env2) = function
    | [] -> (env,env2)
    | (x,y)::xs -> match y with
      | Read r -> get_vars ((Names.add r env),env2) xs
      | Set (n,e) -> get_vars ((Names.add n env),env2) xs
      | If (a,b,c) -> 
          let a' = (condition_to_var_list (env,env2) a) in
          let (b',b'') = (get_vars a' b) in
          let (c',c'') = (get_vars a' c) in
          get_vars ((Names.inter b' c'),all_diffs b' c' b'' c'') xs
      | While (a,b) ->
          let a' = (condition_to_var_list (env,env2) a) in
          let (b',b'') = (get_vars a' b) in
          get_vars (env,(Names.union b'' (Names.diff b' env))) xs
      | Print e -> get_vars (expr_to_var_list (env,env2) e) xs
  in get_vars (Names.empty,Names.empty) p;;

let only_uninitialized_vars (a,b) = b

let all_vars_union (a,b) = Names.union a b

let print_user p = 
  print_string (p ^ " ");;

let print_vars p =
  let res = all_vars p in
  let u = only_uninitialized_vars (res) in
  let a = all_vars_union (res) in
  (Names.iter print_user a);print_string("\n");
  (Names.iter print_user u);print_string("\n");

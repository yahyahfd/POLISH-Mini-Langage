open Types

(** module Names utilisé afin de gérer les variables du programme*)
module Names = Set.Make(String)

(** Liste avec répétition de toutes les variables d'une expression.
"env" contient les variables initialisées, et "env2" les non*)
let rec expr_to_var_list (env,env2) = function
  | Num e -> (env,env2)
  | Var e ->
      if (Names.mem e env)
      then (env,env2)
      else (env,(Names.add e env2))
  | Op (op, e1, e2) -> expr_to_var_list (expr_to_var_list (env,env2) e1) e2

(** Cette méthode est utilisée afin de convertir une condition en une
    liste de variables *)
let condition_to_var_list (env,env2) = function
  | e1,c,e2 -> expr_to_var_list (expr_to_var_list (env,env2) e1) e2

(** Cette méthode est utilisée afin de faire l'union des environnements
    contenant les éléments non initialisés dans une boucle if/else *)
let all_diffs a b a' b' =
  let t1 = Names.union a' b'
  in let t2 = (Names.union (Names.diff a b) (Names.diff b a))
  in Names.union t1 t2

(** Cette méthode renvoie deux environnement env et env2 qui
    contiennent respectivement les éléments initialisés,
    et ceux non initialisés *)
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

(** Cette méthode est utilisée afin de ne garder que les variables
    non initialisés*)
let only_uninitialized_vars (a,b) = b

(** Cette méthode est utilisée afin de faire l'union des environnements
    contenant les éléments initialisés, et ceux non initialisés *)
let all_vars_union (a,b) = Names.union a b

(** Cette méthode est utilisée afin de formater l'environnement
    lors de l'impression*)
let print_fun p =
  print_string (p ^ " ");;

(** Cette méthode finale est celle utilisée afin d'afficher toutes
    les variables du programme sur une ligne, puis les variables
    non initialisés sur la suivante *)
let print_vars p =
  let res = all_vars p in
  let u = only_uninitialized_vars (res) in
  let a = all_vars_union (res) in
  (Names.iter print_fun a);print_string("\n");
  (Names.iter print_fun u);print_string("\n");

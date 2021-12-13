open Types

(** L'environnement env de calcul du programme est une référence *)
let env = ref []

(** Cette méthode permet de calculer une expression (resultat entier) *)
let rec expr_to_value = function
  | Op (o,e1,e2) ->
      (match o with
       | Add -> ((expr_to_value e1) + (expr_to_value e2))
       | Sub -> ((expr_to_value e1) - (expr_to_value e2))
       | Mul -> ((expr_to_value e1) * (expr_to_value e2))
       | Div -> ((expr_to_value e1) / (expr_to_value e2))
       | Mod -> ((expr_to_value e1) mod (expr_to_value e2)))
  | Num x -> x
  | Var x -> (try (List.assoc x !env)
              with Not_found -> failwith (x^" is undefined."));;

(** Cette méthode permet d'ajouter x' de valeur i' dans la liste
    d'association l en modifiant la valeur de x' s'il existe dans l
    sinon, rajoute un nouveau tuple (x',i') à l *)
let add_to_env x i =
  let rec add_set x i acc = function
    | [] -> List.rev ((x,i)::acc)
    | (y,y')::ys ->
        if y = x then add_set x i ((y,i)::acc) ys
        else add_set x i ((y,y')::acc) ys
  in env := (add_set x i [] !env);;

(** Cette méthode transforme une condition en sa valeur booléenne *)
let condition_to_bool (e1,c,e2) = match c with
  | Eq -> ((expr_to_value e1) = (expr_to_value e2))
  | Ne -> ((expr_to_value e1) <> (expr_to_value e2))
  | Lt -> ((expr_to_value e1) < (expr_to_value e2))
  | Le -> ((expr_to_value e1) <= (expr_to_value e2))
  | Gt -> ((expr_to_value e1) > (expr_to_value e2))
  | Ge -> ((expr_to_value e1) >= (expr_to_value e2));;

let rec evaluate = function
  | [] -> ()
  | ((x,y)::xs) as l -> (match y with
      | Read r -> print_string (r^"?");
          (try
             let tmp = (List.assoc r !env) in
             failwith (r^" already defined and its value is "^
                       (string_of_int tmp))
           with Not_found -> (add_to_env r (read_int()));evaluate xs)
      | Set (n,e) -> (add_to_env n (expr_to_value e));evaluate xs
      | If (a,b,c) ->
          if (condition_to_bool a) then ((evaluate b);evaluate xs)
          else ((evaluate c);evaluate xs)
      | While (a,b) ->
          if (condition_to_bool a) then ((evaluate b);evaluate l)
          else evaluate xs
      | Print e -> print_int (expr_to_value e);print_string "\n";
          evaluate xs);;

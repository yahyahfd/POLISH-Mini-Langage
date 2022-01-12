open Types

(** NameTable représente l'environnement de calcul du programme *)
module NameTable = Map.Make(String)

(** Cette méthode permet de calculer une expression (résultat Z.t) *)
let rec expr_to_value env = function
  | Op (o,e1,e2) ->
      let e'1, e'2 = (expr_to_value env e1), (expr_to_value env e2) in
      (match o with
       | Add -> (Z.add e'1 e'2)
       | Sub -> (Z.sub e'1 e'2)
       | Mul -> (Z.mul e'1 e'2)
       | Div -> (Z.div e'1 e'2)
       | Mod -> (Z.rem e'1 e'2))
  | Num x -> x
  | Var x -> (try (NameTable.find x env)
              with Not_found -> failwith (x^" is undefined."));;

(** Cette méthode transforme une condition en sa valeur booléenne *)
let condition_to_bool (e1,c,e2) env =
  let e'1, e'2 = (expr_to_value env e1), (expr_to_value env e2) in

  match c with
  | Eq -> (Z.equal e'1 e'2)
  | Ne -> not (Z.equal e'1 e'2)
  | Lt -> (Z.lt e'1 e'2)
  | Le -> (Z.leq e'1 e'2)
  | Gt -> (Z.gt e'1 e'2)
  | Ge -> (Z.geq e'1 e'2);;

(** Cette méthode finale fait tourner un program et renvoie
    l'environnement (modifié) à chaque tour de boucle*)
let evaluation p =
  let rec evaluate env = function
    | [] -> env
    | ((x,y)::xs) as l -> (match y with
        | Read r -> print_string (r^"?");
            (try
               let tmp = (NameTable.find r env) in
               failwith (r^" already defined and its value is "^
                         (Z.to_string tmp))
             with Not_found ->
               evaluate (NameTable.add r ((Z.of_int (read_int()))) env) xs)
        | Set (n,e) -> evaluate (NameTable.add n (expr_to_value env e) env) xs
        | If (a,b,c) ->
            if (condition_to_bool a env) then (evaluate (evaluate env b) xs)
            else (evaluate (evaluate env c) xs)
        | While (a,b) ->
            if (condition_to_bool a env) then (evaluate (evaluate env b) l)
            else evaluate env xs
        | Print e -> Z.print (expr_to_value env e);print_string "\n";
            evaluate env xs)
  in evaluate NameTable.empty p;;

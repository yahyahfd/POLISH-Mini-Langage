open Lib.Types

(** Type sign qui associé à chaque variable du programme *)
type sign = Neg | Zero | Pos | Error

(** Type new_bool qui remplace les bools usuels afin de simplifier
    le passage dans les boucles if/else et while *)
type new_bool = true | false | Impossible

(** Module SignTable utilisé afin d'affecter à chaque variable
    une liste de signes *)
module SignTable = Map.Make(String)

(** Résultats d'une addition selon les signes des opérandes *)
let addition a b = match (a,b) with
  | Error,_ | _, Error -> [Error]
  | Neg, Neg | Neg, Zero | Zero, Neg -> [Neg]
  | Pos, Pos | Zero, Pos | Pos, Zero -> [Pos]
  | Zero, Zero -> [Zero]
  | Neg, Pos | Pos, Neg -> [Pos;Neg;Zero]

(** Résultats d'une soustraction selon les signes des opérandes *)
let substraction a b = match (a,b) with
  | Error,_ | _, Error -> [Error]
  | Neg, Neg | Pos, Pos -> [Pos;Neg;Zero]
  | Neg, Zero | Neg, Pos | Zero, Pos -> [Neg]
  | Pos, Zero | Pos, Neg | Zero, Neg -> [Pos]
  | Zero, Zero -> [Zero]

(** Résultats d'une multiplication selon les signes des opérandes *)
let multiplication a b = match (a,b) with
  | Error,_ | _, Error -> [Error]
  | Zero, _ | _, Zero -> [Zero]
  | Neg, Pos | Pos, Neg -> [Neg]
  | _, _ -> [Pos]

(** Résultats d'une division selon les signes des opérandes *)
let division a b = match (a,b) with
  | Error,_ | _, Error | _, Zero -> [Error]
  | Pos, Neg | Neg, Pos -> [Neg]
  | Zero, _ -> [Zero]
  | _, _ -> [Pos]

(** Résultats d'un modulo selon les signes des opérandes *)
let modulo a b = match (a,b) with
  | Error,_ | _, Error | _, Zero -> [Error]
  | _, _ -> [Pos; Zero]

(** Union entre deux listes en tailrec *)
let union l1 l2 =
  let rec aux_union acc lis1 lis2 = match lis1, lis2 with
    | [], [] -> List.rev acc
    | x::xs, [] -> aux_union (x::acc) xs lis2
    | [], y::ys -> aux_union (y::acc) lis1 ys
    | x::xs,y::ys -> if x=y then aux_union (x::acc) xs ys
        else aux_union (y::x::acc) xs ys
  in aux_union [] l1 l2;;

(** Union de toutes les listes d'une liste *)
let rec union_all acc = function
  | [] -> List.rev acc
  | x::xs -> union_all (union x acc) xs

(** Cette méthode est utilisée afin d'obtenir tous les resultats possibles
    d'une opération entre "o" et les éléments de la liste "l" *)
let operation y o l = match o with
  | Add -> List.map (fun x -> addition x y) l
  | Sub -> List.map (fun x -> substraction y x) l
  | Mul -> List.map (fun x -> multiplication x y) l
  | Div -> List.map (fun x -> division y x) l
  | Mod -> List.map (fun x -> modulo y x) l

(** Cette méthode est utilisée afin de fournir tous les résultats
    possibles de toutes les opérations possibles entre chaque deux éléments
      des deux listes "l1" et "l2" *)
let all_combinaison l1 l2 o =
  let rec aux_combi acc l2' o = function
    | [] -> List.rev acc
    | x::xs -> aux_combi (Lib.Common.cat_list (operation x o l2') acc) l2' o xs
  in aux_combi [] l2 o l1

(** Cette méthode est utilisée afin de transformer une expression
    en sa liste de signes correspondante *)
let rec expr_to_sign_list env = function
  | Op (o,e1,e2) -> union_all  []
                      (all_combinaison (expr_to_sign_list env e1)
                         (expr_to_sign_list env e2) o)
  | Num x ->
      if (Z.sign x = 0) then [Zero] else
      if (Z.sign x = 1) then [Pos] else [Neg]
  | Var x -> (try (SignTable.find x env)
              with Not_found -> raise
                                  (Problem ("Variable "^x^" not initialized")))

(** Retourne si une égalité entre a et b est possible et les résultats
    possible de ce test d'égalité *)
let is_equal a b = match (a,b) with
  | Error, _ | _, Error -> [Impossible]
  | Zero, Zero -> [true]
  | Pos, Pos -> [false;true]
  | Neg, Neg -> [false;true]
  | _, _ -> [false]

(** Retourne si une inégalité entre a et b est possible et les résultats
    possible de ce test d'inégalité *)
let not_equal a b = match (a,b) with
  | Error, _ | _, Error -> [Impossible]
  | Zero, Zero -> [false]
  | Pos, Pos -> [false;true]
  | Neg, Neg -> [false;true]
  | _, _ -> [true]

(** Retourne si "a<b" est possible et les résultats
    possible de ce test *)
let lower_than a b = match (a,b) with
  | Error, _ | _, Error -> [Impossible]
  | Zero, Zero | Zero, Neg | Pos, Zero | Pos, Neg -> [false]
  | Zero, Pos | Neg, Pos | Neg, Zero -> [true]
  | Pos, Pos -> [false;true]
  | Neg, Neg -> [false;true]

(** Retourne si "a>b" est possible et les résultats
    possible de ce test *)
let higher_than a b = match (a,b) with
  | Error, _ | _, Error -> [Impossible]
  | Zero, Zero | Zero, Pos | Neg, Pos | Neg, Zero -> [false]
  | Zero, Neg | Pos, Neg | Pos, Zero  -> [true]
  | Pos, Pos -> [false;true]
  | Neg, Neg -> [false;true]

(** Retourne si "a>=b" est possible et les résultats
    possible de ce test *)
let higher_eq a b =
  List.map2 (fun x y ->
      (match (x,y) with
       | Impossible, _ -> Impossible
       | _, Impossible -> Impossible
       | false, false -> false
       | false, true -> true
       | true, true -> true
       | true, false -> true)) (higher_than a b) (is_equal a b)

(** Retourne si "a<=b" est possible et les résultats
    possible de ce test *)
let lower_eq a b =
  List.map2 (fun x y ->
      (match (x,y) with
       | Impossible, _ -> Impossible
       | _, Impossible -> Impossible
       | false, false -> false
       | false, true -> true
       | true, true -> true
       | true, false -> true)) (lower_than a b) (is_equal a b)

(** Cette méthode est utilisée afin de n'afficher qu'une seule fois
    les résultats des comparaisons précédentes dans une liste de new_bool *)
let final_bool_list l =
  let rec simpl_bool_list f t i acc = function
    | [] -> List.rev acc
    | x::xs ->
        if f=1 && t=1 && i =1 then List.rev acc
        else
          match x with
          | Impossible ->
              if i=1 then simpl_bool_list f t i acc xs
              else simpl_bool_list f t 1 (x::acc) xs
          | false ->
              if f=1 then simpl_bool_list f t i acc xs
              else simpl_bool_list 1 t i (x::acc) xs
          | true ->
              if t=1 then simpl_bool_list f t i acc xs
              else simpl_bool_list f 1 i (x::acc) xs
  in simpl_bool_list 0 0 0 [] l;;

(** Cette méthode est utilisée afin d'obtenir tous les resultats possibles
    d'une comparaison entre "o" et les éléments de la liste "l" *)
let comparaison y o l = match o with
  | Eq -> List.map (fun x -> is_equal x y) l
  | Ne -> List.map (fun x -> not_equal x y) l
  | Lt -> List.map (fun x -> lower_than x y) l
  | Le -> List.map (fun x -> higher_than x y) l
  | Gt -> List.map (fun x -> higher_eq x y) l
  | Ge -> List.map (fun x -> lower_eq x y) l

(** Cette méthode définie la manière de tri des signes *)
let default_sort = (fun x y ->
    match x,y with
    | Error, _ -> (-1)
    | _, Error -> 1
    | Neg,Neg | Zero,Zero | Pos,Pos -> 0
    | Pos,_ | Zero,Neg -> 1
    | Zero,Pos | Neg,_ -> (-1))

(** Cette méthode trie une liste de signes *)
let sort l = List.sort default_sort l

(** Cette méthode permet d'obtenir une liste triée sans doublons *)
let unique l =
  let rec sort_and_unique acc = function
    | [] -> List.rev acc
    | x::xs ->
        if (List.length acc = 0)
        then sort_and_unique (x::acc) xs
        else (if (List.nth acc 0 = x) then sort_and_unique acc xs
              else sort_and_unique (x::acc) xs)
  in sort_and_unique [] (sort l)

(** Cette méthode est utilisée afin de fournir tous les résultats
    possibles de toutes les comparaisons possibles entre chaque
    deux éléments des deux listes "l1" et "l2" *)
let all_comb_bool l1 l2 o =
  let rec aux_comb acc l2' o = function
    | [] -> List.rev acc
    | x::xs -> aux_comb (Lib.Common.cat_list (comparaison x o l2') acc) l2' o xs
  in aux_comb [] l2 o l1

(** Cette méthode transforme une condition en une new_bool list *)
let condition (e1,c,e2) env =
  let e'1, e'2 = (expr_to_sign_list env e1), (expr_to_sign_list env e2) in
  final_bool_list (union_all [] (all_comb_bool e'1 e'2 c))

(** Cette méthode permet d'obtenir l'intersection entre deux listes
    sans répétition et triée *)
let inter l1 l2 =
  let rec aux_inter acc lis1 lis2 = match lis1, lis2 with
    | x::xs,y::ys -> let test = default_sort x y in
        if test = 0 then aux_inter (x::acc) xs ys
        else if (default_sort x y) = (-1) then aux_inter acc xs lis2
        else aux_inter acc lis1 ys
    | _,_ -> unique acc
  in aux_inter [] (unique l1) (unique l2);;

(** Cette méthode permet d'obtenir la différence entre deux listes *)
let diff l1 l2 =
  let rec aux_diff acc lis1 lis2 = match lis1, lis2 with
    | [], [] -> sort (List.rev acc)
    | x::xs, [] -> aux_diff (x::acc) xs lis2
    | [], y::ys -> aux_diff (y::acc) lis1 ys
    | x::xs,y::ys -> if x=y then aux_diff acc xs ys
        else aux_diff (y::x::acc) xs ys
  in aux_diff [] (unique l1) (unique l2);;

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors d'une comparaison avec le signe "<"
    en sachant le signe d'une des variables afin d'obtenir un true *)
let poss_sign_lower = function
  | Zero | Neg -> [Neg]
  | Pos -> [Neg;Zero;Pos]
  | Error -> [Error]

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors d'une comparaison avec le signe ">"
    en sachant le signe d'une des variables afin d'obtenir un true*)
let poss_sign_higher = function
  | Zero | Pos -> [Pos]
  | Neg -> [Neg;Zero;Pos]
  | Error -> [Error]

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors d'une comparaison avec le signe ">="
    en sachant le signe d'une des variables afin d'obtenir un true *)
let poss_sign_hq = function
  | Zero -> [Zero;Pos]
  | Pos -> [Pos]
  | Neg -> [Neg;Zero;Pos]
  | Error -> [Error]

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors d'une comparaison avec le signe "<="
    en sachant le signe d'une des variables afin d'obtenir un true*)
let poss_sign_lq = function
  | Zero -> [Neg;Zero]
  | Neg -> [Neg]
  | Pos -> [Neg;Zero;Pos]
  | Error -> [Error]

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors de toutes les comparaisons avec le signe ">"
    de toutes les variables de la liste l avec un inconnu afin
    d'obtenir un true. Si ni est différent de 0, on utilise le signe ">="
    plutôt *)
let all_high l ni =
  let rec all_higher_poss acc n = function
    | [] -> List.rev acc
    | x::xs ->
        if n = 0
        then all_higher_poss ((poss_sign_higher x)::acc) n xs
        else all_higher_poss ((poss_sign_hq x)::acc) n xs
  in union_all [] (all_higher_poss [] ni l)

(** Cette méthode permet d'obtenir la liste des signes auxquels
    il faut s'attendre lors de toutes les comparaisons avec le signe "<"
    de toutes les variables de la liste l avec un inconnu afin
    d'obtenir un true. Si ni est différent de 0, on utilise le signe "<="
    plutôt *)
let all_low l ni =
  let rec all_lower_poss acc n = function
    | [] -> List.rev acc
    | x::xs -> if n = 0
        then all_lower_poss ((poss_sign_lower x)::acc) n xs
        else all_lower_poss ((poss_sign_lq x)::acc) n xs
  in union_all [] (all_lower_poss [] ni l)

(** Cette méthode est utilisée afin de renvoyer la liste des résultats
    possibles pour une expression de gauche lors d'une comparaison *)
let compare_var1 l1 l2 = function
  | Eq -> inter l1 l2
  | Ne -> diff l1 l2
  | Lt -> inter l1 (all_low l2 0)
  | Le -> inter l1 (all_low l2 1)
  | Gt -> inter l1 (all_high l2 0)
  | Ge -> inter l1 (all_high l2 1)

(** Cette méthode est utilisée afin de renvoyer la liste des résultats
    possibles pour une expression de droite lors d'une comparaison *)
let compare_var2 l1 l2 o = match o with
  | Eq -> compare_var1 l2 l1 o
  | Ne -> compare_var1 l2 l1 o
  | Lt -> compare_var1 l2 l1 Gt
  | Le -> compare_var1 l2 l1 Ge
  | Gt -> compare_var1 l2 l1 Lt
  | Ge -> compare_var1 l2 l1 Le

(** Cette méthode renvoie un couple contenant les résultats possibles
    des deux expressions d'une comparaison *)
let compare_final l1 l2 o  =
  (compare_var1 l1 l2 o, compare_var2 l1 l2 o)

(** Cette méthode renvoie l'inverse d'un opérateur, et est utilisée
    afin de déterminer l'inverse d'une condition *)
let reverse_op = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt

(** Cette méthode renvoie true s'il est possible de rentrer dans le block if *)
let rec is_true = function
  | [] -> false
  | x::xs ->
      if x = true then true else is_true xs

(** Cette méthode renvoie true s'il est possible de rentrer dans le block
    else *)
let rec is_false = function
  | [] -> false
  | x::xs ->
      if x = false then true else is_false xs

(** Cette méthode renvoie l'union de deux environnements de liste de signes *)
let rec union_table env1 env2 = match SignTable.is_empty env2 with
  | true -> env1
  | false ->
      let (a,b) = SignTable.choose env2 in
      let c = unique
          (try SignTable.find a env1 with Not_found -> []) in
      let add_c = (SignTable.add a (unique (union b c))
                     (SignTable.remove a env1)) in
      let remove_c = SignTable.remove a env2 in
      (union_table add_c remove_c)

(** Cette méthode renvoie true si une liste de signe contient Error *)
let rec contains_error = function
  | [] -> false
  | x::xs -> if x = Error then true else contains_error xs

(** Cette méthode renvoie un couple contenant l'environnement de
    liste de signes et la première ligne où se trouve une Error liée à
    une division par zéro, sinon vaut -1 si aucune erreur. *)
let all_signs p =
  let rec get_signs (env,line) = function
    | [] -> (env,line)
    | ((x,y)::xs) as prog -> (match y with
        | Read r -> get_signs ((SignTable.add r [Neg;Zero;Pos] env),line) xs
        | Set (n,e) ->
            let res = get_signs
                ((SignTable.add n (expr_to_sign_list env e) env),line) xs
            in (if (contains_error (expr_to_sign_list env e) = true)
                then
                  if line = -1 then
                    get_signs
                      ((SignTable.add n (expr_to_sign_list env e) env),x) xs
                  else res
                else res)
        | If (((e1,c',e2) as a),b,c) ->
            (let e1' = expr_to_sign_list env e1 in
             let e2' = expr_to_sign_list env e2 in
             let (cmp1,cmp1') = compare_final e1' e2' c' in
             let (cmp2,cmp2') = compare_final e1' e2' (reverse_op c') in
             let res_a = condition a env in
             let (res_b,li) =
               let l_i =
                 (if (contains_error e1' = true || contains_error e2' = true)
                  then
                    if line = -1 then
                      x
                    else line
                  else line) in
               (match e1,e2 with
                | Var x_i, Var y_i ->
                    get_signs
                      ((SignTable.add y_i cmp2'
                          (SignTable.add x_i cmp2 env)),l_i) c
                | Var x_i, _ -> get_signs ((SignTable.add x_i cmp2 env),l_i) c
                | _, Var y_i -> get_signs ((SignTable.add y_i cmp2' env),l_i) c
                | _, _ -> get_signs (env,l_i) c) in
             (let (res_c,li) =
                let l_i =
                  (if (contains_error e1' = true || contains_error e2' = true)
                   then
                     if line = -1 then
                       x
                     else line
                   else line) in
                (match e1,e2 with
                 | Var x_i, Var y_i ->
                     get_signs
                       ((SignTable.add y_i cmp1'
                           (SignTable.add x_i cmp1 env)),l_i) b
                 | Var x_i, _ -> get_signs ((SignTable.add x_i cmp1 env),l_i) b
                 | _, Var y_i -> get_signs ((SignTable.add y_i cmp1' env),l_i) b
                 | _, _ -> get_signs (env,l_i) b) in
              if (is_true res_a = true)
              then
                if (is_false res_a = true)
                then
                  get_signs ((union_table res_b res_c),li) xs
                else
                  get_signs (res_b,li) xs
              else
                (if (is_false res_a = true)
                 then get_signs (res_c,li) xs
                 else (env,li))))
        | While ((e1,c',e2),b) ->
            (let e1' = expr_to_sign_list env e1 in
             let e2' = expr_to_sign_list env e2 in
             let (cmp1,cmp1') = compare_final e1' e2' c' in
             let (cmp2,cmp2') = compare_final e1' e2' (reverse_op c') in
             let (res_b,li) =
               let l_i =
                 (if (contains_error e1' = true || contains_error e2' = true)
                  then
                    if line = -1 then
                      x
                    else line
                  else line) in
               (match e1,e2 with
                | Var x_i, Var y_i ->
                    get_signs
                      ((SignTable.add y_i cmp1'
                          (SignTable.add x_i cmp1 env)),l_i) b
                | Var x_i, _ -> get_signs ((SignTable.add x_i cmp1 env),l_i) b
                | _, Var y_i -> get_signs ((SignTable.add y_i cmp1' env),l_i) b
                | _, _ -> get_signs (env,l_i) b) in
             let res_c =
               (match e1,e2 with
                | Var x_i, Var y_i -> (SignTable.add y_i cmp2'
                                         (SignTable.add x_i cmp2 env))
                | Var x_i, _ -> (SignTable.add x_i cmp2 env)
                | _, Var y_i -> (SignTable.add y_i cmp2' env)
                | _, _ -> env) in
             let new_env = union_table res_b env in
             let l_i =
               (if (contains_error e1' = true || contains_error e2' = true)
                then
                  if line = -1 then
                    x
                  else line
                else line) in
             if (SignTable.equal
                   (fun x y -> (unique x) = (unique y))
                   new_env env)
             then get_signs ((union_table res_c env),l_i) xs
             else get_signs (new_env,l_i) prog)
        | Print e -> (env,line))
  in get_signs (SignTable.empty,(-1)) p;;

(** Cette méthode transforme une liste de signes en un string *)
let rec list_to_string acc = function
  | [] -> acc
  | x::xs -> match x with
    | Neg -> list_to_string ("-"^acc) xs
    | Pos -> list_to_string ("+"^acc) xs
    | Zero -> list_to_string ("0"^acc) xs
    | Error -> list_to_string ("!"^acc) xs

(** Cette méthode permet de formater l'environnement final afin d'afficher
    une variable par ligne, suivie d'un espace et enfin de tous ses signes
    possibles *)
let print_fun p l =
  print_string (p ^ " "^(list_to_string "" l));print_string("\n");;

(** Cette méthode permet de formater l'environnement final afin d'afficher
    une variable par ligne, suivie d'un espace et enfin de tous ses signes
    possibles. La dernière ligne affiche la première ligne où s'est passée
    une division par 0, sinon affiche "safe" *)
let print_signs p =
  let (res,line) = all_signs p in
  let s =
    if line <> (-1) then
      ("divbyzero "^(string_of_int line))
    else "safe"
  in
  (SignTable.iter print_fun res);print_string(s);print_string("\n");;

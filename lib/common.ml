(** Cette méthode concatène deux liste en tailrec *)
let cat_list l1 l2 =
  let rec aux acc l = function
    | [] -> (match l with
        | [] -> List.rev acc
        | _ -> aux acc [] l
      )
    | x::xs -> aux (x::acc) l xs
  in aux [] l2 l1;;
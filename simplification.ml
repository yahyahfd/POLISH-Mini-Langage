open Types

(*simplification UNIQUEMENT sur deux entier, si variable dont on attend un read ou autre, ne PAS simplifier*)
let rec simplify acc = function
  | [] -> List.rev acc
  | ((x,y)::xs) as l -> (match y with
      | Read r -> simplify ((x,y)::acc) xs
      | Set (n,e) -> simplify (((x,Set(n,Evaluation.expr_to_value e)))::acc) xs)
      (*| If (a,b,c) -> match a with
          on simplifie la premiere partie et la deuxieme, si condition satisfaite, on remplace tout par le if, sinon par le else, puis si ce qu'il y a est vide, on supprime tout
          if (condition_to_bool a) then ((evaluate b);evaluate xs)
          else ((evaluate c);evaluate xs)
      | While (a,b) ->
          (*on simplifie a de la meme maniere que pour le if, si condition satisfaite, on garde le while, sinon on supprime*)
          if (condition_to_bool a) then ((evaluate b);evaluate l)
          else evaluate xs
      | Print e -> simplify (((x,Print (Evaluation.expr_to_value e)))::acc) xs);;*)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec eval: formula -> bool =
  let rec calc = function
    | NUM i -> i
    | PLUS (l, r) -> calc l + calc r
    | MINUS (l, r) -> calc l - calc r in
  function
  | TRUE -> true
  | FALSE -> false
  | NOT f -> not (eval f)
  | ANDALSO (l, r) -> eval l && eval r
  | ORELSE (l, r) -> eval l || eval r
  | IMPLY (l, r) -> (not (eval l)) || (eval l && eval r)
  | LESS (l, r) -> calc l < calc r

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

type var = FREE | BOUND of float

let calculate: exp -> float =
  let rec range a b =
    if a > b then [] else a :: range (a + 1) b in
  let step = 0.1 in
  let rangef a b =
    let epsilon = epsilon_float *. 10.0 in
    let rec sub i =
      let x = a +. step *. float i in
      if x > b +. epsilon -. step then []
      else x :: sub (i+1) in
    sub 0 in
  let sum r f = List.fold_left (fun a x -> a +. f x) 0.0 r in
  let rec eval exp var =
    match exp with
    | X -> (match var with
        | FREE -> raise FreeVariable
        | BOUND x -> x)
    | INT i -> float_of_int i
    | REAL r -> r
    | ADD (l, r) -> (eval l var) +. (eval r var)
    | SUB (l, r) -> (eval l var) -. (eval r var)
    | MUL (l, r) -> (eval l var) *. (eval r var)
    | DIV (l, r) -> (eval l var) /. (eval r var)
    | SIGMA (a, b, f) ->
      let a_ = int_of_float(eval a var) in
      let b_ = int_of_float(eval b var) in
      sum (range a_ b_) (fun x -> eval f (BOUND (float x)))
    | INTEGRAL (a, b, f) ->
      let (sign, r) = if a <= b
        then (1., rangef (eval a var) (eval b var))
        else (-1., rangef (eval b var) (eval a var)) in
      sign *. (sum r (fun x -> step *. eval f (BOUND x))) in
  fun exp -> eval exp FREE

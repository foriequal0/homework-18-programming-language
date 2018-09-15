type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff: ae * string -> ae =
  let singleton = function
    | TIMES (x::[]) -> x
    | SUM (x::[]) -> x
    | x -> x in
  let rec flatten_times = function
    | TIMES(x) :: xs -> List.append x (flatten_times xs)
    | x :: xs -> x :: flatten_times xs
    | x -> x in
  let rec flatten_sum = function
    | SUM(x) :: xs -> List.append x (flatten_sum xs)
    | x :: xs -> x :: flatten_sum xs
    | x -> x in
  let rec normalize = function
    | POWER (_, 0) -> CONST 1
    | POWER (x, 1) -> VAR x
    | TIMES xs ->
      let norm_xs = List.map normalize xs in
      if List.exists (fun x -> x = CONST 0) norm_xs
      then CONST 0
      else singleton (TIMES (flatten_times (List.filter (fun x -> x <> CONST 1) norm_xs)))
    | SUM xs ->
      let norm_xs = List.map normalize xs in
      singleton (SUM (flatten_sum (List.filter (fun x -> x <> CONST 0) norm_xs)))
    | x -> x in
  fun (exp, name) ->
    match exp with
    | CONST _ -> CONST 0
    | VAR x -> if x <> name then CONST 0 else CONST 1
    | POWER (x, a) ->
      if x <> name then CONST 0
      else normalize(TIMES([CONST a; POWER (x, a-1)]))
    | TIMES [] -> raise InvalidArgument
    | TIMES (x::[]) -> diff (x, name)
    | TIMES (x::xs) ->
      normalize( SUM [TIMES[diff(x,name); TIMES xs]; TIMES[x; diff(TIMES xs, name)] ])
    | SUM [] -> raise InvalidArgument
    | SUM xs ->
      normalize( SUM (List.map (fun x -> diff(x, name)) xs))

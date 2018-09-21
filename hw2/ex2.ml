type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

exception InvalidArgument

let rec diff: ae * string -> ae =
  let singleton = function
    | TIMES (x::[]) -> x
    | SUM [] -> CONST 0
    | SUM (x::[]) -> x
    | x -> x in
  let sort xs =
    let rec cmp a b =
      match (a, b) with
      | (CONST(_), _) -> -1
      | (VAR l, VAR r) -> compare l r
      | (VAR l, POWER (r, _)) -> compare l r
      | (VAR _, TIMES _) -> -1
      | (VAR _, SUM _) -> -1
      | (POWER (l, _), POWER (r, _)) -> compare l r
      | (POWER _, TIMES _) -> -1
      | (POWER _, SUM _) -> -1
      | (TIMES _, TIMES _) -> 0
      | (TIMES _, SUM _) -> -1
      | (SUM _, SUM _) -> 0
      | (_, _) -> -(cmp b a) in
    List.sort cmp xs in
  let fold foldable xs=
    let rec merge xs x = match xs with
      | [] -> [x]
      | h :: tl -> (match foldable(h, x) with
          | [a] -> a :: tl
          | _ -> h :: (merge tl x)) in
    List.rev(List.fold_left merge [] (List.rev(sort xs))) in
  let rec fold_sum = function
    | (CONST a, CONST b) -> [CONST (a+b)]
    | (VAR a, VAR b) -> fold_sum (TIMES[CONST 1; VAR a], TIMES[CONST 1; VAR b])
    | (TIMES[CONST x; VAR a], VAR b) -> fold_sum (TIMES[CONST x; VAR a], TIMES[CONST 1; VAR b])
    | (TIMES[CONST x; VAR a], TIMES[CONST y; VAR b]) ->
      if a <> b then []
      else if x + y = 0 then [CONST 0]
      else [TIMES[CONST (x+y); VAR a]]
    | _ -> [] in
  let fold_times = function
    | (CONST a, CONST b) -> [CONST (a*b)]
    | (VAR a, CONST b) -> [TIMES[CONST b; VAR a]]
    | (TIMES[CONST x; VAR a], CONST b) -> [TIMES[CONST(x*b); VAR a]]
    | (TIMES[CONST x; VAR a], VAR b) -> if a <> b then [] else [TIMES[CONST x; POWER(a, 2)]]
    | (TIMES[CONST x; POWER(a,p)], CONST b) -> [TIMES[CONST(x*b); POWER(a,p)]]
    | (VAR a, VAR b) -> if a <> b then [] else [POWER(a, 2)]
    | (POWER (a, x), VAR b) ->
      if a <> b then []
      else if x = (-1) then [CONST 1]
      else [POWER(a, x+1)]
    | _ -> [] in
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
      else singleton (TIMES (sort(flatten_times (fold fold_times (List.filter (fun x -> x <> CONST 1) norm_xs)))))
    | SUM xs ->
      let norm_xs = List.map normalize xs in
      singleton (SUM (sort (flatten_sum (fold fold_sum (List.filter (fun x -> x <> CONST 0) norm_xs)))))
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

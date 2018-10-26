type treasure = StarBox | NameBox of string

type key = Bar | Node of key * key

type map = End of treasure
         | Branch of map * map
         | Guide of string * map

exception IMPOSSIBLE
type ty = TyVar of int
        | TyPrim
        | TyLam of ty * ty

module TyMap = Map.Make(struct type t = ty let compare = compare end)
module EnvMap = Map.Make(String)

let tyId = ref 0
let genTy ()=
  let id = !tyId in
  tyId := id + 1;
  TyVar id

let rec st s t =
  match t with
  | TyVar x -> if TyMap.mem t s then TyMap.find t s else t
  | TyPrim -> TyPrim
  | TyLam (x, y) -> TyLam (st s x, st s y)
let sr s r = EnvMap.mapi (fun k v -> st s v) r
let ss s' s = begin
  let x = ref (TyMap.mapi (fun k v -> st s' v) s) in
  TyMap.iter (fun k v -> if not (TyMap.mem k !x) then x := TyMap.add k v !x) s';
  !x
end
let rec unify x y =
  if x = y then TyMap.empty
  else
    let rec notSub a b =
      if a = b then true
      else
        match b with
        | TyLam (x, y) -> notSub a y
        | _ -> true in
    let u a b = if notSub a b then TyMap.singleton a b else raise IMPOSSIBLE in
    match (x, y) with
    | (TyVar a, t) -> u x y
    | (t, TyVar a) -> u y x
    | (TyLam (t1,t2), TyLam(t1', t2')) ->
      let s = unify t1 t1' in
      let s' = unify (st s t2) (st s t2') in
      ss s s'
    | _ -> raise IMPOSSIBLE
let rec w r exp tr =
  match exp with
  | End (StarBox) ->
    let tr' = if List.mem_assoc "" tr then tr else ("*", TyPrim) :: tr in
    (TyPrim, TyMap.empty, tr')
  | End (NameBox x) ->
    if EnvMap.mem x r
    then (EnvMap.find x r, TyMap.empty, tr)
    else
      let a = genTy () in
      (a, TyMap.empty, (x, a)::tr)
  | Guide (x, e) ->
    let a = genTy () in
    let env' = EnvMap.add x a r in
    let (t, s, tr') = w env' e tr in
    (st s (TyLam (a,t)), s, (x, a)::tr')
  | Branch (e, e') ->
    let (t, s, tr') = w r e tr in
    let (t', s', tr'') = w (sr s r) e' tr' in
    let a = genTy () in
    let s'' = unify (TyLam ((t', a))) (st s' t) in
    (st s'' a, ss s'' (ss s' s), tr'')

let w' exp =
  let (typ, subst, tr) = w EnvMap.empty exp [] in
  (typ, subst, tr)

let ex1 = End (NameBox "x")
let ex2 = Guide ("x", ex1)
let ex3 = Branch (ex2, End StarBox)
let ex4 = Branch (Guide ("x", Branch (ex1, ex1)), End StarBox)
let ex5 = Branch (ex2, Branch (Guide ("y", End (NameBox "y")), End StarBox))
let ex6 = Branch (ex2, Guide ("y", End (NameBox "y")))
let ex7 = Branch (End (NameBox "x"), End StarBox)

let getReady = fun m ->
  let (_, subst, tr) = w' m in
  let keys = List.map (fun (_, t) -> st subst t) tr in
  let rec to_key = function
    | TyLam (x, y) -> Node (to_key x, to_key y)
    | _ -> Bar in
  List.sort_uniq compare (List.map to_key keys)

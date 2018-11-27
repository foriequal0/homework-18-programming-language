(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TWritable of typ
  | TComparable of typ
  (* Modify, or add more if needed *)

let rec string_of_typ = function
  | TInt ->  "Z"
  | TBool ->  "B"
  | TString ->  "S"
  | TVar x ->  x
  | TPair (a, b) ->  "(" ^ string_of_typ a ^ "," ^ string_of_typ b ^ ")"
  | TFun (a, b) -> string_of_typ a ^ "->" ^ string_of_typ b
  | TLoc a -> "Loc " ^ string_of_typ a
  | TWritable a -> "Wri " ^ string_of_typ a 
  | TComparable a -> "Com " ^ string_of_typ a

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  TVar ("x_" ^ (string_of_int !count))

type tyeqn =
  | Eq of typ * typ
  | And of tyeqn * tyeqn

let rec string_of_tyeqn = function
  | Eq (a, b) -> string_of_typ a ^ " == " ^ string_of_typ b
  | And (a, b) -> string_of_tyeqn a ^ " && " ^ string_of_tyeqn b

let rec subst s = function
  | TVar v -> if List.mem_assoc v s
    then List.assoc v s
    else TVar v
  | TFun (a, b) -> TFun (subst s a, subst s b)
  | TLoc a -> TLoc (subst s a)
  | TPair (a, b) -> TPair (subst s a, subst s b)
  | TWritable t -> let st = subst s t in
    (match st with
     | TWritable t -> t
     | TVar x -> TWritable (TVar x)
     | TComparable t -> TComparable t
     | TInt | TBool | TString -> st
     | _ -> raise (M.TypeError ("WRITE operand is not int/bool/string")))
  | TComparable t -> let st = subst s t in
    (match st with
     | TComparable t -> t
     | TVar x -> TComparable (TVar x)
     | TWritable t -> TWritable t
     | TInt | TBool | TString | TLoc _ -> st
     | _ -> raise (M.TypeError ("WRITE operand is not int/bool/string")))
  | otherwise -> otherwise

let string_of_subst s = String.concat "; " (List.map (fun (x, t) -> x ^ ":" ^ string_of_typ t) s)

let rec subst_eqn s = function
  | Eq (lhs, rhs) -> Eq (subst s lhs, subst s rhs)
  | And (lhs, rhs) -> And (subst_eqn s lhs, subst_eqn s rhs)

let subst_map s r =
  List.sort (fun (x, _) (y, _) -> compare x y) (
  List.append (List.map (fun (x, t) -> (x, subst s t)) r)
    (List.filter (fun (x, t) -> not (List.mem_assoc x r)) s))

let empty_subst = []

let emptyTEnv = []
let appendTEnv tenv x = x :: tenv
let lookupTEnv tenv x = if List.mem_assoc x tenv
  then List.assoc x tenv
  else raise (M.TypeError ("unbound variable " ^ x))

let string_of_tenv s = String.concat "; " (List.map (fun (x, t) -> x ^ ":" ^ string_of_typ t) s)

let rec v tenv exp typ =
  let res = match exp with
  | M.CONST c ->( match c with
      | (M.S _) -> Eq (typ, TString)
      | (M.N _) -> Eq (typ, TInt)
      | (M.B _) -> Eq (typ, TBool))
  | M.VAR x -> Eq (typ, lookupTEnv tenv x)
  | M.FN (x, e) ->
    let a1 = new_var() in
    let a2 = new_var() in
    And (
      Eq (typ, TFun (a1, a2)),
      v (appendTEnv tenv (x, a1)) e a2
    )
  | M.APP (e1, e2) ->
    let a = new_var() in
    And (v tenv e1 (TFun (a, typ)), v tenv e2 a)
  | M.LET (M.VAL (x, e1), e2) ->
    let a1 = new_var () in
    let a2 = new_var () in
    And (Eq (typ, a2),
         And (v tenv e1 a1,
              v (appendTEnv tenv (x, a1)) e2 a2))
  | M.LET (M.REC (f, x, e1), e2) ->
    let a1 = new_var () in
    let a2 = new_var () in
    let a3 = new_var () in
    let tfun = TFun (a1, a2) in
    And (Eq (typ, a3),
         And (
           v (appendTEnv tenv (f, tfun)) (M.FN (x, e1)) tfun,
           v (appendTEnv tenv (f, tfun)) e2 a3
         )
        )
  | M.IF (e1, e2, e3) ->
    And (v tenv e1 TBool,
         And (v tenv e2 typ,
              v tenv e3 typ))
  | M.BOP (bop, e1, e2) ->
    let op targ tres =
      And (
        Eq (typ, tres),
        And (v tenv e1 targ, v tenv e2 targ)
      )
    in
    (match bop with
     | M.ADD | M.SUB -> op TInt TInt
     | M.AND | M.OR -> op TBool TBool
     | M.EQ ->
       let a = TComparable (new_var()) in op a TBool)
  | M.READ -> Eq (typ, TInt)
  | M.WRITE e -> v tenv e (TWritable typ)
  | M.MALLOC e ->
    let a = new_var() in
    And (
      Eq (typ, TLoc a),
      v tenv e a
    )
  | M.ASSIGN (lhs, rhs) ->
    let a = new_var() in
    And (
      Eq (typ, a),
      And (
        v tenv lhs (TLoc a),
        v tenv rhs a
      )
    )
  | M.BANG e ->
    let a = new_var() in
    And (
      Eq (typ, a),
      v tenv e (TLoc a)
    )
  | M.SEQ (e1, e2) ->
    let a = new_var() in
    And (
      v tenv e1 a,
      v tenv e2 typ
    )
  | M.PAIR (e1, e2) ->
    let a1 = new_var() in
    let a2 = new_var() in
    And (
      Eq (typ, TPair (a1, a2)),
      And (v tenv e1 a1,
           v tenv e2 a2)
    )
  | M.FST e ->
    let a1 = new_var() in
    let a2 = new_var() in
    And (
      Eq (a1, typ),
      v tenv e (TPair (a1, a2))
    )
  | M.SND e ->
    let a1 = new_var() in
    let a2 = new_var() in
    And (
      Eq (a2, typ),
      v tenv e (TPair (a1, a2))
    ) in
  res

let rec contains_var tvar = function
  | TVar x -> tvar = x
  | TPair (a, b) | TFun (a, b) -> contains_var tvar a || contains_var tvar b
  | TLoc a | TWritable a | TComparable a -> contains_var tvar a
  | _ -> false

let rec unify = fun (lhs, rhs) ->
  match (lhs, rhs) with
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TVar x, t) | (t, TVar x) -> if contains_var x t then raise (M.TypeError "error") else [(x, t)]
  | (TLoc a, TLoc b) -> unify (a, b)
  | (TPair (a, b), TPair (c, d)) | (TFun (a, b), TFun (c, d)) ->
    let s = unify(a, c) in
    let s' = unify(subst s b, subst s d) in
    subst_map s' s
  | (TComparable x, t) | (t, TComparable x) | (TWritable x, t) | (t, TWritable x)->
    let s = unify (x, t) in
    s
  | _ -> raise (M.TypeError "failed to unify")

let rec is_comparable = function
  | TInt | TBool | TString | TLoc _ -> true
  | TVar x -> true
  | TComparable x -> is_comparable x
  | TWritable x -> is_writable x && is_comparable x
  | _ -> false
and is_writable = function
  | TInt | TBool | TString -> true
  | TVar x -> true
  | TComparable x -> is_writable x && is_comparable x
  | TWritable x -> is_writable x
  | _ -> false

let rec unify_all teq s =
  match teq with
  | Eq (lhs, rhs) ->
    let u = unify(lhs, rhs) in
    let ss = subst_map u s in
    let _ = match (subst ss lhs, subst ss rhs) with
      | (TComparable (TVar x), t) | (t, TComparable (TVar x)) ->
        is_comparable t
      | (TWritable (TVar x), t) | (t, TWritable (TVar x)) ->
        is_writable t
      | _ -> true
    in
    ss
  | And (lhs, rhs) -> let t = unify_all lhs s in unify_all (subst_eqn t rhs) t

let check : M.exp -> M.types = fun exp -> let a = new_var() in
  let s = unify_all (v emptyTEnv exp a) empty_subst in
  let rec f = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (a, b) -> M.TyPair (f a, f b)
  | TLoc a -> M.TyLoc (f a)
  | TFun (a, b) -> M.TyArrow (f a, f b)
  | _ -> raise (M.TypeError "type didn't resolved")
  in
  f (subst s a)

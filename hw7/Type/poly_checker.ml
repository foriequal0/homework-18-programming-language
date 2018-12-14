(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
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
  | TComparable of var
  | TWritable of var
  (* Modify, or add more if needed *)

let rec string_of_typ = function
  | TInt ->  "Z"
  | TBool ->  "B"
  | TString ->  "S"
  | TVar x ->  x
  | TPair (a, b) ->  "(" ^ string_of_typ a ^ "," ^ string_of_typ b ^ ")"
  | TFun (a, b) -> string_of_typ a ^ "->" ^ string_of_typ b
  | TLoc a -> "Loc " ^ string_of_typ a
  | TWritable a -> "Wri " ^ a 
  | TComparable a -> "Com " ^ a

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

let rec string_of_scheme = function
  | SimpleTyp typ -> string_of_typ typ
  | GenTyp (vars, typ) -> "forall. " ^ String.concat "," vars ^ ": " ^ string_of_typ typ

type typ_env = (M.id * typ_scheme) list

let rec string_of_env xs =
  String.concat ", " (List.map (fun (name, sc) -> "(" ^ name ^ " = " ^ (string_of_scheme sc) ^")") xs)

let count = ref 0

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v | TComparable v | TWritable v-> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
    | TComparable x' ->
      if (x <> x') then t'
      else (match t with
          | TVar x -> TComparable x
          | _ -> t)
    | TWritable x' ->
      if (x <> x') then t'
      else (match t with
          | TVar x -> TWritable x
          | TComparable x -> TWritable x
          | _ -> t)
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* TODO : Implement this function *)
let rec expansive : M.exp -> bool = function
  | M.CONST _ -> false
  | M.VAR _ -> false
  | M.FN _ -> false
  | M.MALLOC _ -> true
  | M.BANG e -> expansive e
  | M.APP _ -> true (* conservatively assume *)
  | M.ASSIGN (e1, e2) -> expansive e1 || expansive e2
  | M.LET (M.VAL (_, e1), e2) -> expansive e1 || expansive e2
  | M.LET ((M.REC _), e) -> expansive e
  | M.BOP (_, e1, e2) -> expansive e1 || expansive e2
  | M.IF (e1, e2, e3) -> expansive e1 || expansive e2 || expansive e3
  | M.READ -> false
  | M.WRITE e -> expansive e
  | M.SEQ (e1, e2) -> expansive e1 || expansive e2
  | M.PAIR (e1, e2) -> expansive e1 || expansive e2
  | M.FST e -> expansive e
  | M.SND e -> expansive e

let generalize_if_not_expansive e tyenv t =
  if not (expansive e)
  then (generalize tyenv t)
  else (SimpleTyp t)

let rec contains_var tvar = function
  | TVar x -> tvar = x
  | TPair (a, b) | TFun (a, b) -> contains_var tvar a || contains_var tvar b
  | TLoc a -> contains_var tvar a
  | _ -> false

let is_comparable = function
  | TInt | TBool | TString | TLoc _ | TComparable _ | TWritable _-> true
  | _ -> false
let is_writable = function
  | TInt | TBool | TString | TComparable _ -> true
  | _ -> false

let rec unify = fun lhs rhs ->
  if lhs = rhs then empty_subst
  else
  match (lhs, rhs) with
  | (TInt, TInt) | (TBool, TBool) | (TString, TString) -> empty_subst
  | (TVar x, t) | (t, TVar x) -> if contains_var x t
    then raise (M.TypeError "err")
    else make_subst x t
  | (TLoc a, TLoc b) -> unify a b
  | (TPair (a, b), TPair (c, d)) | (TFun (a, b), TFun (c, d)) ->
    let s = unify a c in
    let s' = unify (s b) (s d) in
    s' @@ s
  | (TComparable x, t) | (t, TComparable x) ->
    if contains_var x t then raise (M.TypeError "err")
    else if is_comparable t then make_subst x t
    else raise (M.TypeError ("incomparable"))
  | (TWritable x, t) | (t, TWritable x) ->
    if contains_var x t then raise (M.TypeError "err")
    else if is_writable t then make_subst x t
    else raise (M.TypeError ("non-printable"))
  | _ -> raise (M.TypeError "failed to unify")

let rec w : typ_env -> M.exp -> (subst * typ) = fun tenv exp ->
  let res = match exp with
  | M.CONST (M.S _) -> (empty_subst, TString)
  | M.CONST (M.N _) -> (empty_subst, TInt)
  | M.CONST (M.B _) -> (empty_subst, TBool)
  | M.VAR x ->
    if not (List.mem_assoc x tenv) then raise (M.TypeError ("not found " ^ x ))
    else let ty = List.assoc x tenv in
    (match ty with
    | SimpleTyp t -> (empty_subst, t)
    | GenTyp (alphas, t) ->
      let betas = List.map (fun _ -> new_var()) alphas in
      let s' =
        List.fold_left2
          (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
          empty_subst alphas betas
      in
      (empty_subst, s' t))
  | M.FN (x, e) ->
    let b = TVar (new_var()) in
    let (s1, t1) = w ((x, SimpleTyp b)::tenv) e in
    (s1, TFun (s1 b, t1))
  | M.APP (e1, e2) ->
    let b = TVar (new_var()) in
    let (s1, t1) = w tenv e1 in
    let (s2, t2) = w (subst_env s1 tenv) e2 in
    let s3 = unify (s2 t1) (TFun (t2, b)) in
    (s3 @@ s2 @@ s1, s3 b)
  | M.LET (M.VAL(x, e1), e2) ->
    let (s1, t1) = w tenv e1 in
    let sr = subst_env s1 tenv in
    let t1' = generalize_if_not_expansive e1 sr t1 in
    let (s2, t2) = w ((x, t1') :: sr) e2 in
    (s2 @@ s1, t2)
  | M.LET (M.REC(f, x, e1), e2) ->
    let b = TVar (new_var()) in
    let (s1, t1) = w ((f, SimpleTyp b) :: tenv) (M.FN (x, e1)) in
    let s2 = unify (s1 b) t1 in
    let (s3, t3) = (s2 @@ s1, s2 t1) in
    let sr = subst_env s3 tenv in
    let t3' = generalize sr t3 in
    let (s4, t4) = w ((f, t3') :: sr) e2 in
    (s4 @@ s3, t4)

  | M.READ -> (empty_subst, TInt)
  | M.IF (e1, e2, e3) ->
    let (s1, t1) = w tenv e1 in
    let s1 = unify (s1 t1) TBool @@ s1 in
    let b = TVar (new_var()) in
    let (s2, t2) = w (subst_env s1 tenv) e2 in
    let s2 = unify (s2 t2) b @@ s2 in
    let (s3, t3) = w (subst_env s2 tenv) e3 in
    let s3 = unify (s3 t3) b @@ s3 in
    (s3 @@ s2 @@ s1, b)
  | M.BOP (bop, e1, e2) ->
    let op targ tres =
      let (s1, t1) = w tenv e1 in
      let s1 = unify t1 targ @@ s1 in
      let (s2, t2) = w (subst_env s1 tenv) e2 in
      let s2 = unify (s2 t2) targ @@ s2 in
      (s2 @@ s1, tres)
    in
    (match bop with
     | M.ADD | M.SUB -> op TInt TInt
     | M.AND | M.OR -> op TBool TBool
     | M.EQ -> op (TComparable (new_var())) TBool)
  | M.WRITE e ->
    let (s1, t1) = w tenv e in
    let s1 = unify (s1 t1) (TWritable (new_var())) @@ s1 in
    (s1, t1)
  | M.MALLOC e ->let (s1, t1) = w tenv e in (s1, TLoc t1)
  | M.ASSIGN (e1, e2) ->
    let t = TVar (new_var()) in
    let (s1, t1) = w tenv e1 in
    let s1 =  unify (s1 t1) (TLoc t) @@ s1 in
    let (s2, t2) = w (subst_env s1 tenv) e2 in
    let s2 =  unify (s2 t2) t @@ s2 in
    (s2 @@ s1, t)
  | M.BANG e ->
    let t = TVar (new_var()) in
    let (s1, t1) = w tenv e in
    let s1 =  unify (s1 t1) (TLoc t) @@ s1 in
    (s1, t)
  | M.SEQ (e1, e2) ->
    let (s1, t1) = w tenv e1 in
    let (s2, t2) = w (subst_env s1 tenv) e2 in
    (s2 @@ s1, t2)
  | M.PAIR (e1, e2) ->
    let (s1, t1) = w tenv e1 in
    let (s2, t2) = w (subst_env s1 tenv) e2 in
    (s2 @@ s1, TPair (t1, t2))
  | M.FST e ->
    let (a, b) = (TVar (new_var()), TVar (new_var())) in
    let (s1, t1) = w tenv e in
    let s1 = unify (s1 t1) (TPair (a, b)) @@ s1 in
    (s1, a)
  | M.SND e ->
    let (a, b) = (TVar (new_var()), TVar (new_var())) in
    let (s1, t1) = w tenv e in
    let s1 = unify (s1 t1) (TPair (a, b)) @@ s1 in
    (s1, b)
  in
  let (a, b) = res in (a, a b)

let check : M.exp -> M.typ = fun exp ->
  let (s, t) = w [] exp in
  let rec f = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (a, b) -> M.TyPair (f a, f b)
  | TLoc a -> M.TyLoc (f a)
  | _ -> raise (M.TypeError "type didn't resolved")
  in
  f (s t)

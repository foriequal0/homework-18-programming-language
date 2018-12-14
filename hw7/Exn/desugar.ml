(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | If (e1, e2, e3) -> If (alpha_conv e1 subst,
                           alpha_conv e2 subst,
                           alpha_conv e3 subst)
  | Equal (e1, e2) -> Equal (alpha_conv e1 subst, alpha_conv e2 subst)
  | Raise (e) -> Raise (alpha_conv e subst)
  | Handle (e1, n, e2) -> Handle (alpha_conv e1 subst, n, alpha_conv e2 subst)

(* TODO : Implement this function *)

let cps (f: xexp -> xexp): xexp =
  let k = new_name() in
  Fn(k, f (Var k))

let cps2 (f: xexp -> xexp -> xexp): xexp =
  let k = new_name() in
  let h = new_name() in
  Fn(k, Fn(h, f (Var k) (Var h)))

let app2 (e: xexp) ((k, h): xexp * xexp): xexp =
  App(App(e, k), h)

let rec t e =
  match e with
  | Num n -> cps2 (fun k h -> App (k, Num n))
  | Var x -> cps2 (fun k h -> App (k, Var x))
  | Fn (x, e) -> cps2 (fun k h -> App (k, Fn (x, t e)))
  | App (e1, e2) -> cps2 (fun k h ->
      app2 (t e1)
        (cps (fun f ->
             app2 (t e2)
               (cps (fun v -> app2 (App (f, v)) (k, h))
               , h))
         , h))
  | If (e1, e2, e3) -> cps2 (fun k h ->
      app2 (t e1)
        (cps (fun v1 ->
             If (v1,
                 app2 (t e2) (cps (fun v2 -> App(k, v2)), h),
                 app2 (t e3) (cps (fun v3 -> App(k, v3)), h)))
        , h))
  | Equal (e1, e2) -> cps2 (fun k h ->
      app2 (t e1)
        (cps (fun v1 ->
             app2 (t e2)
               (cps (fun v2 ->
                    App(k, Equal (v1, v2)))
               , h))
        , h))
  | Raise e -> cps2 (fun k h -> app2 (t e) (h, h))
  | Handle (e1, n, e2) -> cps2 (fun k h ->
      app2 (t e1)
        (k
        , cps (fun v ->
          If (Equal (v, Num n)
              , app2 (App (cps (fun x-> t e2), v)) (k, h)
              , App (h, v))
            )))

let removeExn : xexp -> xexp = fun e ->
  let converted = t (alpha_conv e []) in
  let x = new_name() in
  app2 converted
    (Fn (x, Var x),
     Fn (x, Num 201812))

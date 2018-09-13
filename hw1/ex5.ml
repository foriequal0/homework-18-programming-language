type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun (l, r) ->
  match l with
  | ZERO -> r
  | SUCC(x) -> natadd(x, SUCC(r))

let rec natmul: (nat * nat) -> nat = fun (l, r) ->
  match l with
  | ZERO -> ZERO
  | SUCC(ZERO) -> r
  | SUCC(x) -> natadd(r, natmul(x, r))

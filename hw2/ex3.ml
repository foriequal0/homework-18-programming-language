type heap = EMPTY | NODE of (rank * value * heap * heap)
and rank = int
and value = int

exception EmptyHeap

let rank h = match h with
  | EMPTY -> -1
  | NODE(r,_,_,_) -> r

let shake (x,lh,rh) =
  if (rank lh) >= (rank rh)
  then NODE(rank rh+1, x, lh, rh)
  else NODE(rank lh+1, x, rh, lh)

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,x,_,_) -> x

let rec merge: heap * heap -> heap =
  fun (x, y) -> match (x, y) with
    | (EMPTY, _) -> y
    | (_, EMPTY) -> x
    | (NODE(_, xv, xlh, xrh), NODE(_, yv, ylh, yrh)) ->
      if xv <= yv
      then shake (xv, xlh, merge (xrh, y))
      else shake (yv, ylh, merge (x, yrh))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_,_,lh,rh) -> merge (lh,rh)

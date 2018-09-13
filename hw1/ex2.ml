let sumprod: (int * int -> float) * int * int -> float =
  let rec range a b = if a > b then [] else a :: range (a+1) b in
  let prod a b f = List.fold_left (fun p j -> p *. f j) 1.0 (range a b) in
  let sum a b f  = List.fold_left (fun s i -> s +. f i) 0.0 (range a b) in
  fun (m, n, k) -> sum 1 n (fun i-> prod 1 k (fun j -> m(i,j)))

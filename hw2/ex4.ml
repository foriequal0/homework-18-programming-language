module type Queue =
sig
  type element
  type queue
  exception EMPTYQ
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ =
struct
  type element = int list
  type queue = element list * element list
  exception EMPTYQ
  let emptyQ = ([], [])
  let enQ ((l, r), e) = (e :: l, r)
  let rec deQ = function
    | ([], []) -> raise EMPTYQ
    | (l, []) -> deQ ([], List.rev l)
    | (l, x :: xs) -> (x, (l, xs))
end

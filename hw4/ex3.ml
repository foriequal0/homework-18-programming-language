type require = id * (cond list)
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list
and gift = int
and id = A | B | C | D | E

module Id = struct
  type t = id
  let compare = compare
end
module IdSet = Set.Make(Id)
module IdMap = Map.Make(Id)
module GiftSet = Set.Make(struct type t = gift let compare = compare end)

let collect_deps =
  let rec collect = function
    | Items _ -> []
    | Same x -> [x]
    | Common (a, b) -> List.append (collect a) (collect b)
    | Except (c, _) -> collect c in
  let collect_deps_for cs  =
    List.sort_uniq compare (List.flatten (List.map collect cs)) in
  List.map (fun (id, reqs) -> (id, (collect_deps_for reqs)))

let fill_empty ids reqs =
  let not_specified x = not (List.exists (fun (id, _) -> x = id) reqs) in
  let not_specified_ids = List.filter not_specified ids in
  reqs @ List.map (fun id -> (id, [])) not_specified_ids

let merges reqs =
  let merge building (id, req) =
    if List.exists (fun (i, _) -> i = id) building
    then
      let existing = List.assoc id building in
      let removed = List.remove_assoc id building in
      let merged = List.append existing req in
      (id, merged) :: removed
    else (id, req) :: building in
  List.sort (fun (a, _) (b,_) -> compare a b) (List.fold_left merge [] reqs)

let update env existing reqs =
  let rec collect_items = function
    | Items x -> x
    | Same id -> IdMap.find id env
    | Common (a, b) ->
      let a = collect_items a in
      let b = collect_items b in
      GiftSet.elements (GiftSet.inter (GiftSet.of_list a) (GiftSet.of_list b))
    | Except (c, a) ->
      let c = collect_items c in
      List.filter (fun g -> not( List.mem g a)) c in
  let collect_all_items reqs = List.map collect_items reqs in
  List.sort_uniq compare (List.flatten (existing :: collect_all_items reqs))

let shoppingList: require list -> (id * gift list) list = fun reqs -> begin
  let ids = [A; B; C; D; E] in
  let reqs = fill_empty ids (merges reqs) in
  let deps = collect_deps reqs in

  let queue = Queue.create() in
  let set = ref IdSet.empty in
  let env = ref IdMap.empty in
  List.iter (fun id -> begin
        Queue.push id queue;
        set := IdSet.add id !set;
        env := IdMap.add id [] !env
    end) ids;
  while not (Queue.is_empty queue) do
    let id = Queue.pop queue in
    set := IdSet.remove id !set;
    let gifts = IdMap.find id !env in
    let gifts' = update !env gifts (List.assoc id reqs) in
    if gifts <> gifts'
    then begin
      env := IdMap.add id gifts' !env;
      let depends_on_me = List.map (fun (id, _) -> id) (List.filter (fun (_, d) -> List.mem id d) deps) in
      let not_enqueued = List.filter (fun id -> not (IdSet.mem id !set)) depends_on_me in
      List.iter (fun id -> begin
            Queue.push id queue;
            set := IdSet.add id !set;
        end) not_enqueued;
    end
  done;
  List.map (fun id -> (id, IdMap.find id !env)) ids
end

let jealous = [(A, [Same B]); (B, [Same A]); (C, [Same B])]
let picky = [(A, [Except (Same B, [0])]);
             (B, [Except (Same A, [1])]);
             (C, [Except (Same B, [2])])]
let greedy = [(A, [Same B; Same C]);
              (B, [Same A; Same C]);
              (C, [Same A; Same B])]
let humble = [(A, [Items [0]]);
              (B, [Items [1]]);
              (C, [Items [2]])]
let example = [(A, [Items [1;2]; Common (Same B, Same C)]);
               (B, [Common (Same(C), Items [2;3])]);
               (C, [Items [1]; Except (Same A, [3])])]

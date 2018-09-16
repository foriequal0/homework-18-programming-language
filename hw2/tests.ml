include Ex1
include Ex2
include Ex3
include Ex4

let assert_equal name expect actual = begin
  print_endline ("Test case: " ^ name);
  try begin
    assert(expect = actual);
  end
  with e -> begin
    print_endline "Incorrect";
    raise e;
  end
end
exception DoesNotThrow
let should_throw name f =
  try begin
    print_endline ("Test case: " ^ name);
    let _ = f () in
    print_endline "Incorrect";
    raise DoesNotThrow;
  end
  with _ -> ()

module ValidIntListQ = (IntListQ : Queue)

let main () = begin
  should_throw "ex1-free-var" (fun () -> calculate(SIGMA(INT 1, ADD(X,X), X)));
  assert_equal "ex1" 12.3 (calculate(INTEGRAL(REAL 1.0, REAL (-5.0), X)));

  assert_equal "ex2" (TIMES([CONST 5; SUM([VAR "x"; VAR "x"])]))
    (let exp = SUM ([TIMES [CONST 5; TIMES([VAR "x"; VAR "x"])]; CONST 1]) in
     diff(exp, "x"));

  assert_equal "ex3" 1
    (let heap1 = NODE (1, 1, NODE (0, 5, EMPTY, EMPTY), NODE (0, 3, EMPTY, EMPTY)) in
     let heap2 = NODE (0, 2, NODE (0, 4, EMPTY, EMPTY), EMPTY) in
     findMin(merge (heap1, heap2)));

  assert_equal "ex4" ([1;2;3], ([], [[4; 5]]))
    (IntListQ.deQ (IntListQ.enQ(IntListQ.enQ(IntListQ.emptyQ, [1;2;3]), [4;5])));
end

let _ = main ()

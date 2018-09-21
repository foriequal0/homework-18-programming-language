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

let assert_approx name epsilon expected actual = begin
  print_endline ("Test case: " ^ name);
  try begin
    assert(abs_float(expected -. actual) < epsilon)
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
  should_throw "ex1-raise-1" (fun () -> calculate(SIGMA(INT 1, ADD(X,X), X)));
  should_throw "ex1-raise-2" (fun () -> calculate(ADD(X, X)));
  should_throw "ex1-raise-3" (fun () -> calculate(INTEGRAL(SUB(INT 1, X), REAL 0.4, X)));
  assert_approx "ex1" 0.01 12.3 (calculate(INTEGRAL(REAL 1.0, REAL (-5.0), X)));
  assert_approx "ex1-real" 0.01 0.3 (calculate(REAL 0.3));
  assert_approx "ex1-int" 0.01 342.0 (calculate(INT 342));
  assert_approx "ex1-add" 0.01 342.3 (calculate(ADD(INT 342, REAL 0.3)));
  assert_approx "ex1-sub" 0.01 341.7 (calculate(SUB(INT 342, REAL 0.3)));
  assert_approx "ex1-mul" 0.01 102.6 (calculate(MUL(INT 342, REAL 0.3)));
  assert_approx "ex1-div" 0.01 1140. (calculate(DIV(INT 342, REAL 0.3)));
  assert_approx "ex1-sigma-1" 0.01 0. (calculate(SIGMA(INT 5, INT 2, X)));
  assert_approx "ex1-sigma-2" 0.01 0. (calculate(SIGMA(INT 5, INT 2, ADD(X, X))));
  should_throw "ex1-sigma-3" (fun () -> calculate(ADD(X,SIGMA(INT 5, INT 2, ADD(X,X)))));
  assert_approx "ex1-sigma-4" 0.01 4. (calculate(SIGMA(REAL 2.4, REAL 2.9, ADD(X,X))));
  assert_approx "ex1-sigma-5" 0.01 4. (calculate(SIGMA(REAL 2.0, REAL 2.0, ADD(X,X))));
  assert_approx "ex1-integ-1" 0.01 0. (calculate(INTEGRAL(REAL 2.1, REAL 2.12, ADD(X,X))));
  assert_approx "ex1-integ-2" 0.01 0. (calculate(INTEGRAL(REAL 2.1, REAL 2.12, ADD(X,X))));
  assert_approx "ex1-integ-3" 0.01 0.4 (calculate(INTEGRAL(REAL 2.0, REAL 2.12, ADD(X,X))));
  assert_approx "ex1-integ-4" 0.01 (-0.4) (calculate(INTEGRAL(REAL 2.12, REAL 2.0, ADD(X,X))));
  assert_approx "ex1-integ-5" 0.01 9148726.5 (calculate(INTEGRAL(INT 40, INT 3025, ADD(X,X))));
  assert_approx "ex1-integ-6" 0.01 (-9148352.3)
    (calculate(INTEGRAL(SIGMA(INT 1, INT 10, MUL(MUL(X,X),X)), INTEGRAL(INT 1, INT 5, MUL(X,X)), ADD(X,X))));

  should_throw "ex2-raise-1" (fun () -> diff(TIMES[], "x"));
  should_throw "ex2-raise-2" (fun () -> diff(SUM[], "x"));
  assert_equal "ex2"
    (TIMES[CONST 10; VAR "x"])
    (diff(SUM ([TIMES [CONST 5; TIMES([VAR "x"; VAR "x"])]; CONST 1]), "x"));
  assert_equal "ex2-1"
    (TIMES [CONST 5; VAR "Y"; VAR "Z"])
    (diff(TIMES[VAR "X"; VAR "Y"; VAR "Z"; CONST 5],"X"));
  assert_equal "ex2-2" (CONST 0) (diff(POWER("XY", 5), "X"));
  assert_equal "ex2-3" (TIMES[CONST 5; POWER("XY", 4)]) (diff(POWER("XY", 5), "XY"));
  assert_equal "ex2-4" (CONST 120) (diff(diff(diff(diff(diff(POWER("X",5),"X"),"X"),"X"),"X"),"X"));
  assert_equal "ex2-5" (TIMES[CONST 120; VAR "X"]) (diff(diff(diff(diff(POWER("X",5),"X"),"X"),"X"),"X"));
  assert_equal "ex2-6" (TIMES[CONST 60; POWER("X", 2)]) (diff(diff(diff(POWER("X",5),"X"),"X"),"X"));
  assert_equal "ex2-7" (TIMES[CONST 20; POWER("X", 3)]) (diff(diff(POWER("X",5),"X"),"X"));
  assert_equal "ex2-8" (TIMES[CONST 5; POWER("X", 4)]) (diff(POWER("X",5),"X"));
  assert_equal "ex2-9" (CONST 0) (diff(CONST 1000, "X"));
  assert_equal "ex2-10" (CONST 1) (diff(VAR "ASFSD", "ASFSD"));
  assert_equal "ex2-11"
    (SUM [TIMES [CONST 10; VAR "X"; VAR "Y"; POWER ("Z", 4)];
          TIMES [CONST 10; VAR "Y"; POWER ("Z", 4); SUM [VAR "X"; VAR "Y"]]])
    (diff(TIMES[SUM[VAR "X";VAR"Y"];VAR "X";VAR"Y";CONST 10;POWER("Z",4)],"X"));
  assert_equal "ex2-12"
    (SUM [CONST 1; TIMES [CONST 8; POWER ("X", 3)]])
    (diff(SUM[VAR "X";VAR "Y"; TIMES[CONST 2; POWER("X",4)];CONST 2],"X"));

  should_throw "ex3-raise-1" (fun () -> findMin(EMPTY));
  should_throw "ex3-raise-2" (fun () -> deleteMin(EMPTY));
  assert_equal "ex3-1" 1
    (let heap1 = NODE (1, 1, NODE (0, 5, EMPTY, EMPTY), NODE (0, 3, EMPTY, EMPTY)) in
     let heap2 = NODE (0, 2, NODE (0, 4, EMPTY, EMPTY), EMPTY) in
     findMin(merge (heap1, heap2)));
  assert_equal "ex3-2" 2
    (let heap1 = NODE(0, 2, EMPTY, EMPTY) in
     let heap2 = NODE(0, 3, EMPTY, EMPTY) in
     findMin(merge (heap1, heap2)));

  assert_equal "ex4" ([1;2;3], ([], [[4; 5]]))
    (IntListQ.deQ (IntListQ.enQ(IntListQ.enQ(IntListQ.emptyQ, [1;2;3]), [4;5])));
end

let _ = main ()

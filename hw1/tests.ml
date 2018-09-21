include Ex1
include Ex2
include Ex3
include Ex4
include Ex5

let assert_equal name expect actual = begin
  print_endline ("Test case: " ^ name);
  try
    assert(expect = actual)
  with e -> begin
    print_endline "Incorrect";
    raise e;
  end
end

let main () = begin
  assert_equal "ex1" 55 (sigma (1, 10, fun x -> x));
  (* Test cases from https://github.com/carlsagan21/program-language/tree/hw1-test/src/pl_1_2_test.ml *)
  assert_equal "ex1-sigma1" 110 (sigma (1, 10, fun x -> 2 * x));
  assert_equal "ex1-sigma2" 385 (sigma (1, 10, fun x -> x * x));
  assert_equal "ex1-sigma3" 0 (sigma (3, 1, fun x -> x * x));
  assert_equal "ex1-sigma4" 27 (sigma (3, 3, fun x -> x * x * x));
  assert_equal "ex1-sigma5" 385 (sigma (-10, -1, fun x -> x * x));
  assert_equal "ex1-sigma6" 0 (sigma (11, 10, fun x -> 2 * x));
  assert_equal "ex1-sigma7" 5 (sigma (1, 2, fun x -> x * x));
  assert_equal "ex1-sigma8" 5050 (sigma (1, 100, fun x -> x));
  assert_equal "ex1-sigma9" 1 (sigma (1, 1, fun x -> x));
  assert_equal "ex1-sigma10" 1 (sigma (1, 1, fun x -> x));
  assert_equal "ex1-sigma11" 0 (sigma (1, 0, fun x -> x));
  assert_equal "ex1-sigma12" (-15050) (sigma (101, 200, fun x -> -x));

  let matrix (i, j) = ((float_of_int i) *. 10.) +. (float_of_int j) in begin
    assert_equal "ex2" 4573689120.  (sumprod (matrix, 2, 7));
    assert_equal "ex2-non-positive-n" 0. (sumprod (matrix, 0, 7));
    assert_equal "ex2-non-positive-k" 1. (sumprod (matrix, 1, 0));
  end;

  assert_equal "ex3"
    "(Norway ((Cameroon Poland) Sweden))"
    (parenize (NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))));
  assert_equal "ex3-singleton" "Korea" (parenize (LEAF Korea));

  assert_equal "ex4" false (eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8))));

  let rec nat_of_int = function
    | 0 -> ZERO
    | i -> SUCC (nat_of_int (i - 1))
  in begin
    assert_equal "ex5-natadd" (SUCC (SUCC ZERO)) (natadd (ZERO, (SUCC (SUCC ZERO))));
    assert_equal "ex5-natmul"
      (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))))))))
      (natmul (SUCC (SUCC (SUCC (SUCC ZERO))), SUCC (SUCC (SUCC ZERO))));
    (* Test cases from https://github.com/carlsagan21/program-language/blob/hw1-test/src/pl1_4_test.ml *)
    assert_equal "ex5-nat1" (nat_of_int 7) (natadd (nat_of_int 3, nat_of_int 4));
    assert_equal "ex5-nat2" (nat_of_int 0) (natadd (nat_of_int 0, nat_of_int 0));
    assert_equal "ex5-nat3" (nat_of_int 3) (natadd (nat_of_int 0, nat_of_int 3));
    assert_equal "ex5-nat4" (nat_of_int 4) (natadd (nat_of_int 4, nat_of_int 0));
    assert_equal "ex5-nat5" (nat_of_int 4) (natadd (nat_of_int 1, nat_of_int 3));
    assert_equal "ex5-nat6" (nat_of_int 1) (natadd (nat_of_int 0, nat_of_int 1));
    assert_equal "ex5-nat7" (nat_of_int 10) (natadd (nat_of_int 10, nat_of_int 0));
    assert_equal "ex5-nat8" (nat_of_int 17) (natadd (nat_of_int 14, nat_of_int 3));

    assert_equal "ex5-nat9" (nat_of_int 15) (natmul (nat_of_int 3, nat_of_int 5));
    assert_equal "ex5-nat10" (nat_of_int 12) (natmul (nat_of_int 3, nat_of_int 4));
    assert_equal "ex5-nat11" (nat_of_int 0) (natmul (nat_of_int 0, nat_of_int 3));
    assert_equal "ex5-nat12" (nat_of_int 0) (natmul (nat_of_int 4, nat_of_int 0));
    assert_equal "ex5-nat13" (nat_of_int 0) (natmul (nat_of_int 0, nat_of_int 0));
    assert_equal "ex5-nat14" (nat_of_int 3) (natmul (nat_of_int 1, nat_of_int 3));
    assert_equal "ex5-nat15" (nat_of_int 36) (natmul (nat_of_int 4, nat_of_int 9));
    assert_equal "ex5-nat16" (nat_of_int 0) (natmul (nat_of_int 0, nat_of_int 0));
  end;
end

let _ = main ()

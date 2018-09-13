include Ex1
include Ex2
include Ex3
include Ex4
include Ex5

let assert_equal name expect actual = begin
  print_endline ("Test case: " ^ name);
  try begin
    assert(expect = actual);
    print_endline "Correct";
  end
  with e -> begin
    print_endline "Incorrect";
    raise e;
  end
end

let main () = begin
  assert_equal "ex1"
    55
    (sigma (1, 10, fun x -> x));
  let matrix (i, j) = ((float_of_int i) *. 10.) +. (float_of_int j) in begin
    assert_equal "ex2"
      4573689120.
      (sumprod (matrix, 2, 7));
    assert_equal "ex2-non-positive-n"
      0.
      (sumprod (matrix, 0, 7));
    assert_equal "ex2-non-positive-k"
      1.
      (sumprod (matrix, 1, 0));
  end;
  assert_equal "ex3"
    "(Norway ((Cameroon Poland) Sweden))"
    (parenize (NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))));
  assert_equal "ex3-singleton"
    "Korea"
    (parenize (LEAF Korea));
  assert_equal "ex4"
    false
    (eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8))));
  assert_equal "ex5-natadd"
    (SUCC (SUCC ZERO))
    (natadd (ZERO, (SUCC (SUCC ZERO))));
  assert_equal "ex5-natmul"
    (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))))))))
    (natmul (SUCC (SUCC (SUCC (SUCC ZERO))), SUCC (SUCC (SUCC ZERO))))
end

let _ = main ()

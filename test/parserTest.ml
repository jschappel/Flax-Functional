open OUnit2
open Lib


let addition _ = assert_equal
  ~printer:expr_to_string
  (parse_expression @@ lexProgram "1 + 3")
  (BinaryExpr(PLUS, LiteralExpr(Num(1.0)), LiteralExpr(Num(3.0))))

let subtraction _ = assert_equal
  ~printer:expr_to_string
  (parse_expression @@ lexProgram "1 - 3")
  (BinaryExpr(MINUS, LiteralExpr(Num(1.0)), LiteralExpr(Num(3.0))))

let multiplication _ = assert_equal
  ~printer:expr_to_string
  (parse_expression @@ lexProgram "1 * 3")
  (BinaryExpr(STAR, LiteralExpr(Num(1.0)), LiteralExpr(Num(3.0))))

let division _ = assert_equal
  ~printer:expr_to_string
  (parse_expression @@ lexProgram "1 / 3")
  (BinaryExpr(SLASH, LiteralExpr(Num(1.0)), LiteralExpr(Num(3.0))))

let mixed_math_operators _ = assert_equal
  ~printer:expr_to_string
  (parse_expression @@ lexProgram "1 / 3 * 4 + 5 - 1")  
  (BinaryExpr(MINUS, 
    (BinaryExpr(PLUS,
      (BinaryExpr(STAR,
        (BinaryExpr(SLASH, LiteralExpr(Num(1.0)), LiteralExpr(Num(3.0)))),
      LiteralExpr(Num(4.0)))),
    LiteralExpr(Num(5.0)))),
  LiteralExpr(Num(1.0))))
  
let logical_operators _ = assert_equal
~printer:expr_to_string
  (parse_expression @@ lexProgram "true and false or true")
  (BinaryExpr(OR,
    (BinaryExpr(AND, LiteralExpr(Bool(true)), LiteralExpr(Bool(false)))),
    LiteralExpr(Bool(true))))

let unary_expr _ = assert_equal
~printer:expr_to_string
  (parse_expression @@ lexProgram "not true and not false")
  (BinaryExpr(AND,
    (UnaryExpr(NOT, LiteralExpr(Bool(true)))),
    UnaryExpr(NOT, LiteralExpr(Bool(false)))))

let suite =
  "AST" >:::
    ["Binary Add" >:: addition;
      "Binary Sub" >:: subtraction;
      "Binary Mult" >:: multiplication;
      "Binary Div" >:: division;
      "Binary Math Ops" >:: mixed_math_operators;
      "Logical Ops" >:: logical_operators;
      "Unary Exprs" >:: unary_expr;]
  ;;
  
  let () =
    run_test_tt_main suite
  ;;
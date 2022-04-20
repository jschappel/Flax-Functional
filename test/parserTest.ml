open OUnit2
open Lib

let addition _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 + 3")
    (BinaryExpr (PLUS, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)))

let subtraction _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 - 3")
    (BinaryExpr (MINUS, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)))

let multiplication _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 * 3")
    (BinaryExpr (STAR, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)))

let division _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 / 3")
    (BinaryExpr (SLASH, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)))

let mixed_math_operators _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 / 3 * 4 + 5 - 1")
    (BinaryExpr
       ( MINUS,
         BinaryExpr
           ( PLUS,
             BinaryExpr
               ( STAR,
                 BinaryExpr (SLASH, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)),
                 LiteralExpr (Num 4.0) ),
             LiteralExpr (Num 5.0) ),
         LiteralExpr (Num 1.0) ))

let logical_operators _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "true and false or true")
    (BinaryExpr
       ( OR,
         BinaryExpr (AND, LiteralExpr (Bool true), LiteralExpr (Bool false)),
         LiteralExpr (Bool true) ))

let unary_expr _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "not true and not false")
    (BinaryExpr
       ( AND,
         UnaryExpr (NOT, LiteralExpr (Bool true)),
         UnaryExpr (NOT, LiteralExpr (Bool false)) ))

let equality _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 == 1 and 1 != 3")
    (BinaryExpr
       ( AND,
         BinaryExpr (EQ_EQ, LiteralExpr (Num 1.0), LiteralExpr (Num 1.0)),
         BinaryExpr (NOT_EQ, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)) ))

let comparison _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "1 > 1 and 1 < 3 or 1 >= 1 and 1 <= 3")
    (BinaryExpr
       ( OR,
         BinaryExpr
           ( AND,
             BinaryExpr (GT, LiteralExpr (Num 1.0), LiteralExpr (Num 1.0)),
             BinaryExpr (LT, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)) ),
         BinaryExpr
           ( AND,
             BinaryExpr (GT_EQ, LiteralExpr (Num 1.0), LiteralExpr (Num 1.0)),
             BinaryExpr (LT_EQ, LiteralExpr (Num 1.0), LiteralExpr (Num 3.0)) )
       ))

let if_expression _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "if 5 > 1 then true else false")
    (IfExpr
       ( BinaryExpr (GT, LiteralExpr (Num 5.0), LiteralExpr (Num 1.0)),
         LiteralExpr (Bool true),
         LiteralExpr (Bool false) ))

let let_expression1 _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "let x = 10 in x")
    (LetExpr ([ ("x", LiteralExpr (Num 10.0)) ], LiteralExpr (Ident "x")))

let let_expression2 _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "let x = 10; y = 20 in x + y")
    (LetExpr
       ( [ ("x", LiteralExpr (Num 10.0)); ("y", LiteralExpr (Num 20.0)) ],
         BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y")) ))

let let_expression3 _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "let x = 10; in x")
    (LetExpr ([ ("x", LiteralExpr (Num 10.0)) ], LiteralExpr (Ident "x")))

let let_expression4 _ =
  assert_equal ~printer:show_expression
    (parse_expression
    @@ lexProgram "let x = let xx = 10 in xx + 1; y = 20 in x + y")
    (LetExpr
       ( [
           ( "x",
             LetExpr
               ( [ ("xx", LiteralExpr (Num 10.0)) ],
                 BinaryExpr
                   (PLUS, LiteralExpr (Ident "xx"), LiteralExpr (Num 1.0)) ) );
           ("y", LiteralExpr (Num 20.0));
         ],
         BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y")) ))

let letrec_expression _ =
  assert_equal ~printer:show_expression
    (LetRecExpr
       ( [
           ( "add",
             [ "x"; "y" ],
             BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y"))
           );
           ( "sub",
             [ "xx"; "yy" ],
             BinaryExpr
               (MINUS, LiteralExpr (Ident "xx"), LiteralExpr (Ident "yy")) );
         ],
         CallExpr ("add", [ LiteralExpr (Num 4.0); LiteralExpr (Num 5.0) ]) ))
    (parse_expression
    @@ lexProgram
         "letrec\n\
         \           add(x, y) => x + y;\n\
         \           sub(xx, yy) => xx - yy\n\
         \         in\n\
         \           add(4,5)")

let lambda_single _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "fn x => x + 10")
    (FuncExpr
       ( [ "x" ],
         BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Num 10.0)) ))

let lambda_mult _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "fn x, y => x + y")
    (FuncExpr
       ( [ "x"; "y" ],
         BinaryExpr (PLUS, LiteralExpr (Ident "x"), LiteralExpr (Ident "y")) ))

let empty_arg_call _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "x()")
    (CallExpr ("x", []))

let single_arg_call _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "x(10)")
    (CallExpr ("x", [ LiteralExpr (Num 10.0) ]))

let mult_arg_call _ =
  assert_equal ~printer:show_expression
    (parse_expression @@ lexProgram "add(10, y + 20)")
    (CallExpr
       ( "add",
         [
           LiteralExpr (Num 10.0);
           BinaryExpr (PLUS, LiteralExpr (Ident "y"), LiteralExpr (Num 20.0));
         ] ))

let suite =
  "AST"
  >::: [
         "Binary Add" >:: addition;
         "Binary Sub" >:: subtraction;
         "Binary Mult" >:: multiplication;
         "Binary Div" >:: division;
         "Binary Math Ops" >:: mixed_math_operators;
         "Logical Ops" >:: logical_operators;
         "Unary Exprs" >:: unary_expr;
         "Equality Ops" >:: equality;
         "Comparison Ops" >:: comparison;
         "Basic_If Expr" >:: if_expression;
         "Single Let Expr" >:: let_expression1;
         "Multipule Let Expr" >:: let_expression2;
         "Trailing ',' Let Expr" >:: let_expression3;
         "Nested Let Expr" >:: let_expression4;
         "Letrec Expr" >:: letrec_expression;
         "Single Arg lambda" >:: lambda_single;
         "Multi Arg lambda" >:: lambda_mult;
         "Call Single Arg" >:: single_arg_call;
         "Call Mult Args" >:: mult_arg_call;
         "Call Empty Args" >:: empty_arg_call;
       ]

let () = run_test_tt_main suite

open OUnit2

let dummy_test _ = assert_equal true true

let suite = "Lambda Lifter tests" >::: [ "Dummy Test" >:: dummy_test ]

let _ = run_test_tt_main suite

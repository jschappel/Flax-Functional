open OUnit2

let assert_true (v : 'a) = assert_equal ~printer:Bool.to_string true v
let assert_false (v : 'a) = assert_equal ~printer:Bool.to_string false v

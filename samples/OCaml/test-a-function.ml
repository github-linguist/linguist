open OUnit
open Palindrome

let test_palindrome_1 _ =
  assert_equal true (is_palindrome "aba")

let test_palindrome_2 _ =
  assert_equal true (is_palindrome "abba")

let test_palindrome_3 _ =
  assert_equal true (is_palindrome "abacidAdicaba")

let test_palindrome_4 _ =
  assert_equal false (is_palindrome "xREty5kgPMO")

let test_palindrome_5 _ =
  assert_equal true (is_palindrome(rem_space "in girum imus nocte et consumimur igni"))


let suite = "Test Palindrome" >::: ["test_palindrome_1" >:: test_palindrome_1;
                                    "test_palindrome_2" >:: test_palindrome_2;
                                    "test_palindrome_3" >:: test_palindrome_3;
                                    "test_palindrome_4" >:: test_palindrome_4;
                                    "test_palindrome_5" >:: test_palindrome_5]
let _ =
  run_test_tt_main suite

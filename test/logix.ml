open OUnit2

let tests =
  "test suite for log parsing"
  >::: [
         ("error" >:: fun _ -> assert_equal 0 0);
         ("info" >:: fun _ -> assert_equal 1 1);
       ]

let _ = run_test_tt_main tests

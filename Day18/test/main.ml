
open Alcotest

let one_expected = 62

let one_test () =
  let actual = Day18.one @@ Lib.Util.test_one_as_str "Day18" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 0

let two_test () =
  let actual = Day18.two @@ Lib.Util.test_one_as_str "Day18" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "Day18"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%d" two_expected) `Quick two_test ] );
    ]
      

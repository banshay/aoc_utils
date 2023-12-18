
open Alcotest

let one_expected = 405

let one_test () =
  let actual = Day13.one @@ Lib.Util.test_one_as_str "Day13" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 0

let two_test () =
  let actual = Day13.two @@ Lib.Util.test_one_as_str "Day13" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "Day13"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%d" two_expected) `Quick two_test ] );
    ]
      

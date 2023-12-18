
open Alcotest

let one_expected = 21 

let one_test () =
  let actual = Day17.one @@ Lib.Util.test_one_as_str "Day17" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 525152

let two_test () =
  let actual = Day17.two @@ Lib.Util.test_one_as_str "Day17" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "Day17"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%d" two_expected) `Quick two_test ] );
    ]
      

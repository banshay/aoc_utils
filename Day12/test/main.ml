
open Alcotest

let one_expected = 21

let one_test () =
  let actual = Day12.one @@ Lib.Util.test_one_as_str "Day12" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 0

let two_test () =
  let actual = Day12.two @@ Lib.Util.test_one_as_str "Day12" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "Day12"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%d" two_expected) `Quick two_test ] );
    ]
      

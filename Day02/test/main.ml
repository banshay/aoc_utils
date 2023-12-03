
open Alcotest

let one_expected = 8 

let one_test () =
  let actual = Day02.one @@ Lib.Util.test_one_as_str "Day02" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 2286

let two_test () =
  let actual = Day02.two @@ Lib.Util.test_two_as_str "Day02" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "Day02"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%d" two_expected) `Quick two_test ] );
    ]
      

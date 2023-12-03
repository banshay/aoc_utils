open Alcotest

let one_test () =
  let expected = 142 in
  let actual = Impl.one @@ Lib.Util.test_one_as_str "Day01" in
  Alcotest.(check int) "same int" expected actual

let two_test () =
  let expected = 61 in
  let actual = Impl.two @@ Lib.Util.test_two_as_str "Day01" in
  Alcotest.(check int) "same int" expected actual

let () =
  run "Day01"
    [
      ( "Tests",
        [ test_case "One" `Quick one_test; test_case "Two" `Quick two_test ] );
    ]

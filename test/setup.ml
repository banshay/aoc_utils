let test_range () =
  Alcotest.(check (list int)) "same lists" [ 1; 2; 3; 4; 5 ] (Lib.Setup.range 1 5)
;;

let test_range2 () =
  Alcotest.(check (list int)) "same lists" [ 3; 4; 5; 6 ] (Lib.Setup.range 3 6)
;;

let () =
  let open Alcotest in
  run
    "Setup"
    [ ( "range"
      , [ test_case "Range test" `Quick test_range
        ; test_case "Range test 2" `Quick test_range2
        ] )
    ]
;;

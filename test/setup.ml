let test_range () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3; 4; 5 ] (Lib.Setup.range 1 5)

let test_range2 () =
  Alcotest.(check (list int)) "same lists" [ 3; 4; 5; 6 ] (Lib.Setup.range 3 6)

let test_list_of_list () =
  Alcotest.(check (list (list string)))
    "same lists"
    [ [ "hello"; "world" ]; [ "advent"; "of"; "code" ]; [ "2023" ] ]
    (Lib.Util.list_of_list ""
       [ "hello"; "world"; ""; "advent"; "of"; "code"; ""; "2023" ])

let () =
  let open Alcotest in
  run "Setup"
    [
      ( "range",
        [
          test_case "Range test" `Quick test_range;
          test_case "Range test 2" `Quick test_range2;
        ] );
      ( "list_of_list",
        [ test_case "List of List test" `Quick test_list_of_list ] );
    ]

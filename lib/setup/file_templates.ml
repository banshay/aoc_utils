open Paths

type file = { dir : string; name : string; content : string }

let file_templates day =
  [
    {
      dir = create_path [ day; "lib" ];
      name = Printf.sprintf "%s.ml" day;
      content =
        {|




let one _input =
  (* solve one*)
  0

let two _input =
  (* solve one*)
  0
  |};
    };
    {
      dir = create_path [ day; "bin" ];
      name = "main.ml";
      content =
        Printf.sprintf
          {|
let () =
  print_endline @@ Printf.sprintf "Result one: %%d" @@ %s.one  @@ Lib.Util.input_as_str ();
  print_endline "-------------------";
  print_endline @@ Printf.sprintf "Result two: %%d" @@ %s.two @@ Lib.Util.input_as_str ()
  |}
          day day;
    };
    { dir = create_path [ day ]; name = "input"; content = "" };
    { dir = create_path [ day ]; name = "test_one"; content = "" };
    { dir = create_path [ day ]; name = "test_two"; content = "" };
    {
      dir = create_path [ day; "lib" ];
      name = "dune";
      content = Printf.sprintf {|
(library
 (name %s)
 (libraries lib re))
  |} day;
    };
    {
      dir = create_path [ day; "test" ];
      name = "dune";
      content =
        Printf.sprintf {|
(test
 (name main)
 (libraries lib %s alcotest))
  |}
          day;
    };
    {
      dir = create_path [ day; "bin" ];
      name = "dune";
      content =
        Printf.sprintf
          {|
(executable
 (public_name %s)
 (name main)
 (libraries lib %s))
  |}
          day day;
    };
    {
      dir = create_path [ day; "test" ];
      name = "main.ml";
      content =
        Printf.sprintf
          {|
open Alcotest

let one_expected = 0 

let one_test () =
  let actual = %s.one @@ Lib.Util.test_one_as_str "%s" in
  Alcotest.(check int) "same int" one_expected actual

let two_expected = 0

let two_test () =
  let actual = %s.two @@ Lib.Util.test_one_as_str "%s" in
  Alcotest.(check int) "same int" two_expected actual

let () =
  run "%s"
    [
      ( "Tests",
        [ test_case (Printf.sprintf "%%d" one_expected) `Quick one_test; test_case (Printf.sprintf "%%d" two_expected) `Quick two_test ] );
    ]
      |}
          day day day day day;
    };
  ]

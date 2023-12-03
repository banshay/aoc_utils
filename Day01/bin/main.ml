let () =
  print_endline @@ Printf.sprintf "Result one: %d" @@ Impl.one  @@ Lib.Util.input_as_str ();
  print_endline "-------------------";
  print_endline @@ Printf.sprintf "Result two: %d" @@ Impl.two @@ Lib.Util.input_as_str ()

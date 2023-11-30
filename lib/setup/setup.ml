let rec range lower upper =
  if lower < upper then lower :: range (lower + 1) upper else [ upper ]

let day_string number = Printf.sprintf "Day%02d" number

let create_path dir name =
  let trailing_slash = Str.regexp "/$" in
  Printf.sprintf "%s/%s" (Str.global_replace trailing_slash "" dir) name

let cleanup dir lower upper =
  let delete_dir dir name =
    [ create_path dir name ] |> FileUtil.rm ~recurse:true ~force:Force
  in
  List.map day_string @@ range lower upper |> List.iter (delete_dir dir)

type file = { name : string; content : string }

let rec create_files files =
  match files with
  | [] -> ()
  | file :: rest ->
      let out = open_out file.name in
      output_string out file.content;
      close_out out;
      create_files rest

let file_templates day =
  [
    {
      name = create_path day "main.ml";
      content = {|



let one =
  (* solve one*)
  0

let two =
  (* solve one*)
  0

let () =
  print_endline @@ Printf.sprintf "%d" one;
  print_endline @@ Printf.sprintf "%d" two
  |};
    };
    { name = create_path day "input"; content = "" };
    {
      name = create_path day "dune";
      content =
        Printf.sprintf
          {|
(executable
 (public_name %s)
 (name main)
 (libraries lib))
  |}
          day;
    };
  ]

let create dir lower upper =
  let create_dir dir name =
    FileUtil.mkdir ~parent:true (create_path dir name)
  in
  List.map day_string @@ range lower upper
  |> List.iter (fun day ->
         create_dir dir day;
         create_files @@ file_templates day)

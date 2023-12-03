open File_templates
open Paths

let rec range lower upper =
  if lower < upper then lower :: range (lower + 1) upper else [ upper ]

let day_string number = Printf.sprintf "Day%02d" number

let cleanup dir lower upper =
  let delete_dir dir name =
    [ create_path [ dir; name ] ] |> FileUtil.rm ~recurse:true ~force:Force
  in
  List.map day_string @@ range lower upper |> List.iter (delete_dir dir)

let create_dir dir name =
  FileUtil.mkdir ~parent:true (create_path [ dir; name ])

let rec create_files files =
  match files with
  | [] -> ()
  | file :: rest ->
      create_dir file.dir "";
      let path = create_path [ file.dir; file.name ] in
      (* let () = Printf.printf "Path for file: %s\n" path in *)
      let out = open_out path in
      output_string out file.content;
      close_out out;
      create_files rest

let create dir lower upper =
  List.map day_string @@ range lower upper
  |> List.iter (fun day ->
         create_dir dir day;
         create_files @@ file_templates day)

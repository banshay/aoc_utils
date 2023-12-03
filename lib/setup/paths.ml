let rec create_path ?(path = "") dirs =
  match dirs with
  | [] -> path
  | hd :: tl -> (
      let trailing_slash = Str.regexp "/$" in
      match path with
      | "" -> create_path tl ~path:hd
      | _ ->
          create_path tl
            ~path:
              (Printf.sprintf "%s/%s" path
                 (Str.global_replace trailing_slash "" hd)))

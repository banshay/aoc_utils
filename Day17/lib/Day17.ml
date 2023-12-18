module T = Domainslib.Task

let split str = String.to_seq str |> Seq.map (String.make 1) |> List.of_seq

let pp_list (list : string list option list) =
  List.filter_map (fun x -> match x with None -> x | Some y -> Some y) list
  |> List.map (String.concat "")
  |> String.concat "\n"

let rec solve ?(grp = 0) acc (to_solve : string list) possible_groups :
    string list option list =
  let handle_dot dot rest =
    if grp > 0 then
      match possible_groups with
      | hd :: r_grp ->
          if int_of_string hd = grp then solve (dot :: acc) rest r_grp ~grp:0
          else [ None ]
      | [] -> [ None ]
    else solve (dot :: acc) rest possible_groups
  in

  let handle_question_mark rest =
    (* let _ = Printf.printf "?%s spawning:\n" (String.concat "" rest) in *)
    let with_hash = solve acc ("#" :: rest) possible_groups ~grp in
    let with_dot = solve acc ("." :: rest) possible_groups ~grp in
    (* let _ = Printf.printf "%s\n%s\n" (pp_list with_hash) (pp_list with_dot) in *)
    with_hash @ with_dot
  in

  let handle_hash fst rest =
    match possible_groups with
    | p_g :: _ ->
        if grp >= int_of_string p_g then [ None ]
        else solve (fst :: acc) rest possible_groups ~grp:(grp + 1)
    | _ -> [ None ]
  in

  (* let _ = Printf.printf "Solving %s\n" (String.concat "" to_solve) in *)
  match to_solve with
  | [] -> (
      match possible_groups with
      | hd :: [] when int_of_string hd = grp -> [ Some (List.rev acc) ]
      | [] -> [ Some (List.rev acc) ]
      | _ -> [ None ])
  (* check if the lookahead is even needed... *)
  | fst :: rest ->
      let res =
        match fst with
        | "#" -> handle_hash fst rest
        | "." -> handle_dot fst rest
        | "?" -> handle_question_mark rest
        | x ->
            Printf.printf "Unknown character %s\n" x;
            assert false
      in
      (* let _ = *)
      (*   Printf.printf "to_solve (%s) produced:\n%s\n" *)
      (*     (String.concat "" to_solve) *)
      (*     (pp_list res) *)
      (* in *)
      res

let filter_empty list = List.filter Option.is_some list

let parse_input line =
  match String.split_on_char ' ' line with
  | [ line; groups ] -> (split line, String.split_on_char ',' groups)
  | _ ->
      Printf.eprintf "Cannot parse line %s\n" line;
      assert false

let exploded_input line =
  match String.split_on_char ' ' line with
  | [ line; groups ] ->
      let l = List.init 5 (fun _ -> line) |> String.concat "?" in
      let g = List.init 5 (fun _ -> groups) |> String.concat "," in
      (split l, String.split_on_char ',' g)
  | _ ->
      Printf.eprintf "Cannot parse line %s\n" line;
      assert false

let one _input =
  (* solve one*)
  List.map parse_input _input
  |> List.map (fun (line, groups) -> solve [] line groups)
  |> List.map (fun x -> filter_empty x |> List.length)
  |> List.fold_left ( + ) 0
(* let pool = T.setup_pool ~num_domains:16 () in *)
(* List.map parse_input _input *)
(* |> List.map (fun (line, groups) -> *)
(*        T.async pool (fun _ -> (line, solve [] line groups))) *)
(* |> List.map (fun t -> *)
(*        let _line, res = T.await pool t in *)
(*        res) *)
(* |> List.map (fun x -> filter_empty x |> List.length) *)
(* |> List.fold_left ( + ) 0 *)

let two _input =
  (* solve one*)
  let pool = T.setup_pool ~num_domains:16 () in
  T.run pool (fun _ -> 
  List.map exploded_input _input
  |> List.map (fun (line, groups) -> T.async pool (fun _ -> solve [] line groups |> filter_empty |> List.length) )
  |> List.map (T.await pool)
  |> List.fold_left ( + ) 0
  )

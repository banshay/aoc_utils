type part = { id : string; nr : int; start : int; stop : int }

let symbol_regex =
  let open Re in
  compile
    (alt
       [
         str "$";
         str "*";
         str "#";
         str "+";
         str "%";
         str "-";
         str "/";
         str "&";
         str "@";
         str "=";
       ])

let parts_regex =
  let open Re in
  compile (group @@ rep1 digit)

let symbol_locations str =
  Re.all symbol_regex str
  |> List.map (fun grp ->
         let s = Re.Group.start grp 0 in
         (* let m = Re.Group.get grp 0 in *)
         (* if s = 10 then Printf.printf "Matched \"%s\" at %d line %s\n" m s str; *)
         s)
  |> List.map (fun i -> [ i - 1; i; i + 1 ])
  |> List.flatten

let extract_parts str =
  Re.all parts_regex str
  |> List.map (fun grp ->
         {
           id = "";
           nr = int_of_string @@ Re.Group.get grp 0;
           start = Re.Group.start grp 0;
           stop = Re.Group.stop grp 0;
         })

let does_overlap (low, high) symbol_loc =
  if low <= symbol_loc && symbol_loc <= high then true else false

let get_relevant_parts line_nr parts symbols =
  List.mapi
    (fun i part ->
      match
        List.find_opt
          (fun symbol_loc ->
            let ov = does_overlap (part.start, part.stop) symbol_loc in
            (* let () = *)
            (*   Printf.printf "Nr. %d, start: %d stop: %d, symbol %d ov %b\n" *)
            (*     part.nr part.start part.stop symbol_loc ov *)
            (* in *)
            ov)
          symbols
      with
      | Some _ ->
          ( { part with id =  Printf.sprintf "%d_%d" line_nr i },
            true )
      | None -> (part, false))
    parts
(* |> List.filter (fun (part, overlaps) -> overlaps) *)
(* |> List.map (fun (part, _) -> part) *)

let rec gather_parts parts cnt = function
  | [] | _ :: [] -> parts
  | l1 :: l2 :: rest ->
      let l1_symbols = symbol_locations l1 in
      let l2_symbols = symbol_locations l2 in
      (* if cnt = 4 then List.iter (fun x -> Printf.printf "%d\n" x) l2_symbols; *)
      let l1_parts = extract_parts l1 in
      let l2_parts = extract_parts l2 in
      let relevant_parts =
        get_relevant_parts cnt l1_parts (l1_symbols @ l2_symbols)
        @ get_relevant_parts (cnt + 1) l2_parts (l1_symbols @ l2_symbols)
      in
      let () =
        Printf.printf "%s -> %s\n" l1
          (String.concat ", "
             (List.map
                (fun (part, overlaps) ->
                  match overlaps with
                  | true -> Printf.sprintf "%d" part.nr 
                  (* | true -> Printf.sprintf "(#%d,%s)" part.nr part.id *)
                  | false -> Printf.sprintf "#%d#" part.nr)
                relevant_parts))
      in
      let filtered_rel_parts =
        List.filter (fun (_, overlaps) -> overlaps) relevant_parts
        |> List.map (fun (part, _) -> part)
      in
      gather_parts (filtered_rel_parts @ parts) (cnt + 1) (l2 :: rest)

let one _input =
  (* solve one*)
  let table =
    gather_parts [] 1 (List.rev _input)
    |> List.fold_left
         (fun table elem ->
           match Hashtbl.find_opt table elem.id with
           | Some _ -> table
           | None ->
               Hashtbl.add table elem.id elem;
               table)
         (Hashtbl.create 1000)
  in
  let () = print_endline "" in
  let () =
    Hashtbl.iter
      (fun id part -> Printf.printf "Entry: (%s, #%d)\n" id part.nr)
      table
  in
  Hashtbl.to_seq_values table
  (* |> Hashtbl.to_seq_values *)
  |> Seq.fold_left (fun acc part -> acc + part.nr) 0

let two _input =
  (* solve one*)
  0

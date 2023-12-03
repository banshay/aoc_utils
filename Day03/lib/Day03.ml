type part = { id : int; nr : int; start : int; stop : int }

let symbol_regex =
  let open Re in
  compile (alt [ str "$"; str "*"; str "#"; str "+" ])

let parts_regex =
  let open Re in
  compile (group @@ rep1 digit)

let symbol_locations str =
  Re.all symbol_regex str
  |> List.map (fun grp -> Re.Group.start grp 0)
  |> List.map (fun i -> [ i - 1; i; i + 1 ])
  |> List.flatten

let extract_parts str =
  Re.all parts_regex str
  |> List.map (fun grp ->
         {
           id = 0;
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
          (fun symbol_loc -> does_overlap (part.start, part.stop) symbol_loc)
          symbols
      with
      | Some _ ->
          ( { part with id = int_of_string @@ Printf.sprintf "%d%d" line_nr i },
            true )
      | None -> (part, false))
    parts
  |> List.filter (fun (_, overlaps) -> overlaps)
  |> List.map (fun (part, _) -> part)

let rec gather_parts parts cnt = function
  | [] | _ :: [] -> parts
  | l1 :: l2 :: rest ->
      let l1_symbols = symbol_locations l1 in
      let l2_symbols = symbol_locations l2 in
      let l1_parts = extract_parts l1 in
      let l2_parts = extract_parts l2 in
      let relevant_parts =
        get_relevant_parts cnt l1_parts (l1_symbols @ l2_symbols)
        @ get_relevant_parts (cnt + 1) l2_parts (l1_symbols @ l2_symbols)
      in
      gather_parts (relevant_parts @ parts) (cnt + 1) (l2 :: rest)

let one _input =
  (* solve one*)
  gather_parts [] 1 _input
  |> List.fold_left
       (fun table elem ->
         match Hashtbl.find_opt table elem.id with
         | Some _ -> table
         | None -> Hashtbl.add table elem.id elem;
         table)
       (Hashtbl.create 1000)
  |> Hashtbl.to_seq_values
  |> Seq.fold_left
       (fun acc part ->
         acc + part.nr)
       0

let two _input =
  (* solve one*)
  0

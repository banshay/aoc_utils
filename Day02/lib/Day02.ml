type set = { green : int; red : int; blue : int }
type game = { nr : int; sets : set list }
type gem = Red | Green | Blue

let max a b = if a > b then a else b

let rec max_of_sets ?(red = 0) ?(green = 0) ?(blue = 0) = function
  | [] -> (red, green, blue)
  | set :: rest ->
      max_of_sets ~red:(max red set.red) ~green:(max green set.green) ~blue:(max blue set.blue) rest

let parse_sets str =
  let re =
    let open Re in
    (* let number = rep1 digit in *)
    (* let color = alt [str "red";str "green";str "blue"] in *)
    (* compile (seq [group number; space; color]) *)
    let red = group (seq [ group @@ rep1 digit; space; str "red" ]) in
    let green = group (seq [ group @@ rep1 digit; space; str "green" ]) in
    let blue = group (seq [ group @@ rep1 digit; space; str "blue" ]) in
    compile (alt [ red; green; blue ])
  in
  List.map (fun x ->
      let matches = Re.all re x in

      let update_set group set =
        try { set with red = int_of_string (Re.Group.get group 2) }
        with Not_found -> (
          try { set with green = int_of_string (Re.Group.get group 4) }
          with Not_found -> (
            try { set with blue = int_of_string (Re.Group.get group 6) }
            with Not_found -> assert false))
      in

      List.fold_left
        (fun set hit -> update_set hit set)
        { red = 0; green = 0; blue = 0 }
        matches)
  @@ String.split_on_char ';' str

let parse_line line =
  match String.split_on_char ':' line with
  | [] ->
      Printf.eprintf "line has no : (%s)" line;
      assert false
  | x :: [] ->
      Printf.eprintf "Somehow no setstring for %s..." x;
      assert false
  | game_str :: set_str :: _ ->
      let game_nr_re = Str.regexp {|Game \([0-9]+\)|} in
      let _ = Str.string_match game_nr_re game_str 0 in
      let game_nr =
        int_of_string
        @@ try Str.matched_group 1 game_str with Not_found -> "0"
      in
      let sets = parse_sets set_str in
      { nr = game_nr; sets }

let one _input =
  (* solve one*)
  List.filter (fun game ->
      List.for_all
        (fun set -> set.red <= 12 && set.green <= 13 && set.blue <= 14)
        game.sets)
  @@ List.map parse_line _input
  |> List.fold_left (fun acc x -> acc + x.nr) 0

let two _input =
  (* solve one*)
  List.map parse_line _input
  |> List.map (fun game -> max_of_sets game.sets)
  |> List.map (fun (r, g, b) -> r * g * b)
  |> List.fold_left (fun acc a -> acc + a) 0

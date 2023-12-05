module Card = struct
  type card = { nr : int; numbers : int list; winning_numbers : int list }

  let matching_winners card =
    List.filter
      (fun number -> List.exists (fun win -> number = win) card.winning_numbers)
      card.numbers

  let score card =
    matching_winners card
    |> List.fold_left (fun acc _ -> if acc = 0 then 1 else acc * 2) 0

  let alt_score cards card =
    let rec get_next_n_cards start = function
      | n when n = start -> []
      | n -> (
          match Hashtbl.find_opt cards start with
          | None ->
              let () =
                Printf.eprintf "Somehow did not find card with id %d" n
              in
              assert false
          | Some card -> card :: get_next_n_cards ( start +1 ) n)
    in
    let rec compound_score id card =
      let score = List.length @@ matching_winners card in
      let start = card.nr +1 in
      (* let () = Printf.printf "%d: Card %d scored %d\n" id card.nr score in *)
      match score with
      | 0 -> [card]
      | s ->
          card :: (get_next_n_cards start (start + s)
      (* |> List.map (fun x -> Printf.printf "%d: Got next card %d\n" id x.nr; x) *)
      |> List.map (compound_score id) |> List.flatten)
    in
    List.length @@ compound_score card.nr card

  let card_regex =
    let open Re in
    compile (group @@ rep1 digit)

  let as_table cards =
    let table = Hashtbl.create @@ List.length cards in
    let () = List.iter (fun card -> Hashtbl.add table card.nr card) cards in
    table

  let parse_cards input =
    let parse_line line =
      let id_str, numbers =
        match String.split_on_char ':' line with
        | id_str :: numbers :: _ -> (id_str, numbers)
        | _ ->
            Printf.eprintf "Splitting by : produced error on %s\n" line;
            assert false
      in
      let got_nr, win_nr =
        match String.split_on_char '|' numbers with
        | got_nr :: win_nr :: _ -> (got_nr, win_nr)
        | _ ->
            Printf.eprintf "Splitting on | produced error on %s" numbers;
            assert false
      in
      let got_nr_p =
        List.map
          (fun grp -> int_of_string @@ Re.Group.get grp 1)
          (Re.all card_regex got_nr)
      in
      let win_nr_p =
        List.map
          (fun grp -> int_of_string @@ Re.Group.get grp 1)
          (Re.all card_regex win_nr)
      in
      {
        nr = int_of_string @@ Re.Group.get (Re.exec card_regex id_str) 1;
        numbers = got_nr_p;
        winning_numbers = win_nr_p;
      }
    in

    List.map parse_line input
end

let one _input =
  (* solve one*)
  Card.parse_cards _input
  |> List.fold_left (fun acc card -> acc + Card.score card) 0

let two _input =
  (* solve one*)
  let cards = Card.parse_cards _input in
  let table = Card.as_table cards in

  List.map (Card.alt_score table) cards
  |> List.fold_left (fun acc x -> acc + x) 0

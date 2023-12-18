module Race = struct
  type race = { time : int; distance : int }

  let will_win_race race hold_time remaining_time =
    match hold_time * remaining_time with
    | x when x > race.distance -> Some hold_time
    | _ -> None

  let parse_input2 input =
    let digit_regex =
      let open Re in
      compile (group (rep1 digit))
    in
    match input with
    | [ times; distances ] ->
        let time =
          List.map (fun grp -> Re.Group.get grp 1) @@ Re.all digit_regex times
          |> List.fold_left (fun acc x -> acc ^ x) ""
          |> int_of_string
        in
        let distance =
          List.map (fun grp -> Re.Group.get grp 1)
          @@ Re.all digit_regex distances
          |> List.fold_left (fun acc x -> acc ^ x) ""
          |> int_of_string
        in
        { time; distance }
    | _ ->
        Printf.eprintf "input incorrect shape!";
        assert false

  let parse_input input =
    let digit_regex =
      let open Re in
      compile (group (rep1 digit))
    in
    match input with
    | [ times; distances ] ->
        let times =
          List.map (fun grp -> int_of_string @@ Re.Group.get grp 1)
          @@ Re.all digit_regex times
        in
        let distances =
          List.map (fun grp -> int_of_string @@ Re.Group.get grp 1)
          @@ Re.all digit_regex distances
        in
        List.map2 (fun time distance -> { time; distance }) times distances
    | _ ->
        Printf.eprintf "input incorrect shape!";
        assert false
end

let one _input =
  (* solve one*)
  Lib.Util.time
    (fun input ->
      let races = Race.parse_input @@ List.rev input in
      List.map
        (fun race ->
          let count =
            List.map2 (Race.will_win_race race)
              (Lib.Util.range 0 ~stop:race.time)
              (Lib.Util.range ~direction:DOWN race.time)
            |> List.filter_map (fun x -> x)
            |> (fun winners ->
                 Printf.printf "Winners -> %s\n"
                 @@ String.concat ", "
                 @@ List.map (Printf.sprintf "%d") winners;
                 winners)
            |> List.length
          in
          Printf.printf
            "For race (time %d, distance %d) this many can win -> %d\n"
            race.time race.distance count;
          count)
        races
      |> List.fold_left ( * ) 1)
    _input

let two _input =
  (* solve one*)
  let main =
    Lib.Util.time
      (fun input ->
        let race = Race.parse_input2 @@ List.rev input in
        Printf.printf "Race time: %d + distance %d\n" race.time race.distance;
        let range_chunks = Lib.Util.n_ranges 20 0 ~stop:race.time in

        let task range =
          Seq.map2 (Race.will_win_race race) (List.to_seq range)
            (List.rev range |> List.to_seq)
          |> Seq.filter_map (fun x -> x)
          |> Seq.length |> Lwt.return
        in

        let%lwt results = List.map task range_chunks |> Lwt.all in
        Lwt.return @@ List.fold_left (+) 0 results)
      _input
  in
  Lwt_main.run main

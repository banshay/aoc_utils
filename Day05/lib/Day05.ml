module Garden = struct
  type entry = { destination : int; source : int; range : int }

  type garden = {
    seeds : int list;
    alt_seeds : (int * int) list;
    seed_to_soil : entry list;
    soil_to_fert : entry list;
    fert_to_water : entry list;
    water_to_light : entry list;
    light_to_temp : entry list;
    temp_to_humid : entry list;
    humid_to_location : entry list;
  }

<<<<<<< Updated upstream
  (* let create input = *)
  (*   Lib.Util.list_of_list "" input *)
  (*   |>  *)
  (**)
=======
  let two_range_overlap r1 r2 =
    let low1, high1 = r1 in
    let low2, high2 = r2 in
    let enc1 = low1 <= low2 && high2 <= high1 in
    let enc2 = low2 <= low1 && high1 <= high2 in
    let ov1 = low1 <= low2 && low2 <= high1 && high1 <= high2 in
    let ov2 = low2 <= low1 && low1 <= high2 && high2 <= high1 in
    enc1 || enc2 || ov1 || ov2

  let get_overlap r1 r2 =
    let r1_l, r1_h = r1 in
    let r2_l, r2_h = r2 in
    (Int.max r1_l r2_l, Int.min r1_h r2_h)
>>>>>>> Stashed changes

  let ranges_from_entries entries seed_range =
    let number, range = seed_range in
    List.filter_map
      (fun entry ->
        match
          get_overlap
            (entry.source, entry.source + entry.range)
            (number, number + range)
        with
        | lower, higher when higher <= lower ->
            (* Printf.printf "lower %d > higher %d mean no overlap\n" lower higher; *)
            None
        | lower, higher ->
            (* overlap from seed space and source space*)
            let pass_on_offset = lower - number in
            let offset = lower - entry.source in
            let distance = higher - lower in
            let dest_low = entry.destination + offset in
            let dest_range = (dest_low, distance) in
            Printf.printf
            "overlap (%d, %d) seed space (%d, %d) and source space (%d, %d) \
                  dest space (%d, %d) offset %d distance %d -> (%d, %d)\n"
              lower higher number (number + range) entry.source
              (entry.source + entry.range)
              entry.destination
              (entry.destination + entry.range)
              offset distance dest_low distance;
            if pass_on_offset <= 0 then Some [ dest_range ]
            else Some [ (number, pass_on_offset); dest_range ])
      entries
    |> List.flatten

  let alt_follow_map entries ranges =
    List.map
      (fun range ->
        let entry_dest_ranges = ranges_from_entries entries range in
        match entry_dest_ranges with
        | [] ->
            let low, r = range in
            Printf.printf "Passing on self -> (%d, %d)\n" low r;
            [ range ]
        | x ->
            Printf.printf "Passing on %s\n"
            @@ String.concat ", "
            @@ List.map (fun (a, b) -> Printf.sprintf "(%d, %d)" a b) x;
            x)
      ranges
    |>
    (Printf.printf "---\n";
     List.flatten)

  let follow_map entries number =
    match
      List.filter
        (fun entry ->
          entry.source <= number && number < entry.source + entry.range)
        entries
    with
    | [] -> number
    | entry :: [] ->
        let delta = number - entry.source in
        (* let () = Printf.printf "number %d -> %d\n" number (entry.destination + delta) in *)
        entry.destination + delta
    | _ :: _ ->
        Printf.eprintf
          "Got multiple entries matching the number to map. We fucked\n";
        assert false

  let alt_to_locations garden =
    let () =
      Printf.printf "----------\nSeed %s\n"
        (String.concat ", "
        @@ List.map
             (fun (a, b) -> Printf.sprintf "(%d, %d)" a b)
             garden.alt_seeds)
    in
    Printf.printf "Seed to Soil\n";
    alt_follow_map garden.seed_to_soil garden.alt_seeds
    |> (Printf.printf "Soil to Fert\n";
        alt_follow_map garden.soil_to_fert)
    |> (Printf.printf "Fert to Water\n";
        alt_follow_map garden.fert_to_water)
    |> (Printf.printf "Water to Light\n";
        alt_follow_map garden.water_to_light)
    |> (Printf.printf "Light to Temp\n";
        alt_follow_map garden.light_to_temp)
    |> (Printf.printf "Temp to Humid\n";
        alt_follow_map garden.temp_to_humid)
    |>
    (Printf.printf "Humid to Location\n";
     alt_follow_map garden.humid_to_location)

  let to_locations garden =
    List.map
      (fun seed ->
        (* let () = Printf.printf "----------\nSeed %d\n" seed in *)
        follow_map garden.seed_to_soil seed
        |> follow_map garden.soil_to_fert
        |> follow_map garden.fert_to_water
        |> follow_map garden.water_to_light
        |> follow_map garden.light_to_temp
        |> follow_map garden.temp_to_humid
        |> follow_map garden.humid_to_location)
      garden.seeds

  let create input =
    let extract_digits line =
      let digit_re =
        let open Re in
        compile (group (rep1 digit))
      in
      Re.all digit_re line
      |> List.map (fun grp -> int_of_string @@ Re.Group.get grp 1)
    in

    let seeds list = List.map extract_digits list |> List.flatten in

    let alt_seeds list =
      let rec pair = function
        | [] -> []
        | _ :: [] ->
            Printf.eprintf "Boy we fucked. Uneven number of seeds";
            assert false
        | first :: second :: rest -> (first, second) :: pair rest
      in
      List.map extract_digits list |> List.map pair |> List.flatten
    in

    let rec extract_entries = function
      | [] -> []
      | line :: rest -> (
          match extract_digits line with
          | destination :: source :: range :: _ ->
              { destination; source; range } :: extract_entries rest
          | _ -> extract_entries rest)
    in
    (* List.iter (fun x -> Printf.printf "%s\n" @@ String.concat "\n" x) *)
    (* @@ Lib.Util.list_of_list "" input; *)
    match Lib.Util.list_of_list "" input with
    | seed_in :: seed_to_soil_in :: soil_to_fert_in :: fert_to_water_in
      :: water_to_light_in :: light_to_temp_in :: temp_to_humid_in
      :: humid_to_location_in :: _ ->
        {
          seeds = seeds seed_in;
          alt_seeds = alt_seeds seed_in;
          seed_to_soil = extract_entries seed_to_soil_in;
          soil_to_fert = extract_entries soil_to_fert_in;
          fert_to_water = extract_entries fert_to_water_in;
          water_to_light = extract_entries water_to_light_in;
          light_to_temp = extract_entries light_to_temp_in;
          temp_to_humid = extract_entries temp_to_humid_in;
          humid_to_location = extract_entries humid_to_location_in;
        }
    | _ ->
        Printf.printf "List of list not in the shape expected\n";
        assert false
end

let rec min m = function
  | [] -> m
  | hd :: tl -> if hd < m then min hd tl else min m tl

let one _input =
  (* solve one*)
  let garden = Garden.create _input in
  match Garden.to_locations garden with
  | [] ->
      Printf.eprintf "We fucked, garden is empty";
      assert false
  | hd :: rest -> min hd rest

let two _input =
  (* solve one*)
  let garden = Garden.create _input in
  match Garden.alt_to_locations garden with
  | [] ->
      Printf.eprintf "We fucked, garden is empty";
      assert false
  | (hd, _) :: rest -> min hd (List.map (fun (low, _) -> low) rest)

type direction = UP | DOWN

let rec range ?(direction = UP) ?(stop = 0) start =
  if (stop <= start && direction = UP) || (start <= stop && direction = DOWN)
  then [ start ]
  else
    match direction with
    | UP -> start :: range (start + 1) ~stop ~direction
    | DOWN -> start :: range (start - 1) ~stop ~direction

let n_ranges ?(direction = UP) ?(stop = 0) n start =
  let _, higher =
    match direction with UP -> (start, stop) | DOWN -> (stop, start)
  in
  let chunk_size =
    Float.to_int @@ Float.ceil @@ (Int.to_float higher /. Int.to_float n)
  in
  List.init n (fun cnt ->
      match direction with
      | UP ->
          range ~direction (cnt * chunk_size)
            ~stop:((cnt * chunk_size) + chunk_size)
      | DOWN ->
          range ~direction
            ~stop:((cnt * chunk_size) - chunk_size)
            (cnt * chunk_size))

let file_as_str file =
  let chan = open_in file in
  let rec read_line read_lines channel =
    try read_line (input_line channel :: read_lines) channel
    with End_of_file -> read_lines
  in
  let lines = read_line [] chan in
  close_in chan;
  lines

let input_as_str () = file_as_str "input"

let test_as_str day filename =
  let path = Printf.sprintf "../../../../%s/%s" day filename in
  file_as_str path

let test_one_as_str day =
  let path = Printf.sprintf "../../../../%s/test_one" day in
  file_as_str path

let test_two_as_str day =
  let path = Printf.sprintf "../../../../%s/test_two" day in
  file_as_str path

let input_as_int () = List.map int_of_string @@ input_as_str ()

let list_of_list delimiter list =
  let rec group current outer = function
    | [] -> List.rev @@ (current :: outer)
    | line :: rest when line = delimiter ->
        group [] (List.rev current :: outer) rest
    | line :: rest -> group (line :: current) outer rest
  in
  group [] [] list

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Time: %fs\n" (Sys.time () -. t);
  fx

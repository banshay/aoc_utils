let input_as_str ()  =
  let chan = open_in "input" in
  let rec read_line read_lines channel =
    try read_line (input_line channel :: read_lines) channel
    with End_of_file -> read_lines
  in
  let lines = read_line [] chan in
  close_in chan;
  lines

let input_as_int () = List.map int_of_string @@ input_as_str ()

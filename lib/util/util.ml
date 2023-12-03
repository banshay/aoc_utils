let file_as_str file =
  let chan = open_in file in
  let rec read_line read_lines channel =
    try read_line (input_line channel :: read_lines) channel
    with End_of_file -> read_lines
  in
  let lines = read_line [] chan in
  close_in chan;
  lines

let input_as_str () =
  file_as_str "input"

let test_one_as_str day =
  let path = Printf.sprintf "../../../../%s/test_one" day in
  file_as_str path

let test_two_as_str day = 
  let path = Printf.sprintf "../../../../%s/test_two" day in 
  file_as_str path

let input_as_int () = List.map int_of_string @@ input_as_str ()

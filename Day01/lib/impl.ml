let number_replace = function
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | _ -> assert false

let duplicate_char = function
  | "nei" -> "neei"
  | "eni" -> "enni"
  | "htw" -> "httw"
  | "hth" -> "htth"
  | "reei" -> "reeei"
  | "vei" -> "veei"
  | "won" -> "woon"
  | _ -> assert false

let cleanup_line line =
  let r = Str.regexp {|\(nei\|eni\|htw\|hth\|reei\|vei\|won\)|} in
  let exp_line =
    Str.global_substitute r
      (fun x -> duplicate_char (Str.matched_string x))
      line
  in
  let regex =
    Str.regexp {|\(one\|two\|three\|four\|five\|six\|seven\|eight\|nine\)|}
  in
  Str.global_substitute regex
    (fun x -> number_replace (Str.matched_string x))
    exp_line

let to_list str =
  List.map (String.make 1) @@ String.fold_right (fun a acc -> a :: acc) str []

let rec extract_numbers line =
  match line with
  | [] ->
      Printf.eprintf "Exhausted the line";
      assert false
  | hd :: tl -> ( try int_of_string hd with Failure _ -> extract_numbers tl)

let parse_line line =
  let concat_nr =
    (string_of_int @@ extract_numbers line)
    ^ string_of_int @@ extract_numbers @@ List.rev line
  in
  try int_of_string concat_nr
  with Failure _ ->
    Printf.eprintf "Failed to parse concat number %s" concat_nr;
    assert false

let one input =
  (* solve one*)
  List.fold_left (fun acc a -> acc + a) 0
  @@ List.map (fun l -> parse_line @@ to_list l) input

let two input =
  (* solve one*)
  List.fold_left (fun acc a -> acc + a) 0
  @@ List.map
       (fun l ->
         let cl = cleanup_line l in
         let parsed = parse_line @@ to_list cl in
         (* let () = Printf.printf "%s -> %s -> %d\n" l cl parsed in *)
         parsed) (* @@ List.map cleanup_line input *)
       input

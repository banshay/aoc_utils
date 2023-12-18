module Trench = Map.Make (Int)

module Digsite = struct
  include Trench
  module Holes = Map.Make (Int)

  let replace (x,color) = function
    | None -> Some Holes.(empty |> add x color)
    | Some existing -> Some (Holes.update x (fun _ -> Some color) existing)

  let pp_digsite map =
    Trench.to_list map |> List.iter (fun (key, value) -> Printf.printf "")
end

module Digger = struct
  type direction = UP | RIGHT | DOWN | LEFT
  type instruction = { direction : direction; steps : int; color : string }

  let direction_of_string = function
    | "UP" -> UP
    | "DOWN" -> DOWN
    | "LEFT" -> LEFT
    | "RIGHT" -> RIGHT
    | x ->
        Printf.eprintf "Cannot parse direction: %s" x;
        assert false

  let create_instructions input =
    let parse input =
      match String.split_on_char ' ' input with
      | [ d; s; c ] ->
          {
            direction = direction_of_string d;
            steps = int_of_string s;
            color = c;
          }
      | _ ->
          Printf.eprintf "Input has wrong shape %s\n" input;
          assert false
    in

    List.map parse input

  let dig ?(digsite = Digsite.empty) current_location instruction =
    let x, y = current_location in
    let direction, steps, color = instruction in
    match direction with
    | UP ->
        Lib.Util.range ~direction:DOWN y ~stop:(y - steps)
        |> List.fold_left
             (fun digsite id ->
               Digsite.update id (Digsite.replace (x, color)) digsite)
             digsite
    | DOWN ->
        Lib.Util.range ~direction:UP y ~stop:(y + steps)
        |> List.fold_left
             (fun digsite id ->
               Digsite.update id (Digsite.replace (x, color)) digsite)
             digsite
    | LEFT ->
        Lib.Util.range ~direction:DOWN x ~stop:(x - steps)
        |> List.fold_left
             (fun digsite id ->
               Digsite.update y (Digsite.replace (id, color)) digsite)
             digsite
    | RIGHT ->
        Lib.Util.range ~direction:UP x ~stop:(x + steps)
        |> List.fold_left
             (fun digsite id ->
               Digsite.update y (Digsite.replace (id, color)) digsite)
             digsite
end

let one _input =
  (* solve one*)
  0

let two _input =
  (* solve one*)
  0

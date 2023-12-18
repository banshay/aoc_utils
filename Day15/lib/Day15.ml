open Easy_logging

module IntKey = struct
  type t = int

  let compare = compare
end

module Boxes = Map.Make (IntKey)

let table = ref Boxes.empty
let log = Logging.make_logger "Day15" Trace [ Cli Debug ]

let hash str =
  let value = ref 0 in
  let cmp_val current_val ascii =
    let i = current_val + ascii in
    let m = i * 17 in
    m mod 256
  in
  let rec _hash = function
    | [] -> !value
    | c :: rest ->
        value := cmp_val !value (int_of_char c);
        _hash rest
  in
  let h = _hash str in
  log#ltrace
    (lazy
      (Printf.sprintf "%s -> %d"
         (String.concat "" (List.map (String.make 1) str))
         h));
  h

let alt_hash str =
  let value = ref 0 in
  let cmp_val current_val ascii =
    let i = current_val + ascii in
    let m = i * 17 in
    m mod 256
  in
  let rec _hash = function
    | [] -> !value
    | '-' :: _ | '=' :: _ -> !value
    | c :: rest ->
        value := cmp_val !value (int_of_char c);
        _hash rest
  in
  let h = _hash str in
  log#ltrace
    (lazy
      (Printf.sprintf "%s -> %d"
         (String.concat "" (List.map (String.make 1) str))
         h));
  h

let box_id str = alt_hash str

let rec label ?(acc = "") = function
  | [] -> acc
  | x :: _ when x = '-' || x = '=' -> acc
  | x :: rest -> label ~acc:(Printf.sprintf "%s%c" acc x) rest

type operation = REMOVE | ADD of int [@@deriving show]

let process_box str =
  let str2 = String.concat "" (List.map (String.make 1) str) in
  log#ltrace (lazy "--------");
  log#ldebug (lazy (Printf.sprintf "Input: %s" str2));

  let add id label nr =
    let entry = Boxes.find_opt id !table in
    (match entry with
    | None -> ()
    | Some ls ->
        log#ltrace
          (lazy
            (Printf.sprintf "Got [%s]"
               (Lib.Util.pp_list ", "
                  (fun (l, f) -> Printf.sprintf "%s: %d" l f)
                  ls))));
    match entry with
    | None -> table := Boxes.add id [ (label, nr) ] !table
    | Some lst -> table := Boxes.add id (lst @ [ (label, nr) ]) !table
  in

  let rem id label =
    let rec _rem_lst label = function
      | [] -> []
      | (x, _) :: rest when x = label -> rest
      | x :: rest -> x :: (_rem_lst label rest)
    in

    match Boxes.find_opt id !table with
    | None -> ()
    | Some lst ->
        let new_list = _rem_lst label lst in
        log#ltrace
          (lazy
            (Printf.sprintf "New list [%s]"
               (Lib.Util.pp_list ", "
                  (fun (l, f) -> Printf.sprintf "%s: %d" l f)
                  new_list)));
        table := Boxes.add id new_list !table
  in

  let rec find_op = function
    | x :: _ when x = '-' -> REMOVE
    | x :: nr :: _ when x = '=' -> ADD (int_of_string (String.make 1 nr))
    | [] ->
        log#lerror
          (lazy
            (Printf.sprintf "String (%s) has no operation!"
               (String.concat "" (List.map (String.make 1) str))));
        assert false
    | _ :: rest -> find_op rest
  in

  let op = find_op str in
  log#ltrace
    (lazy (Printf.sprintf "Op: %s l: %s" (show_operation op) (label str)));

  match op with
  | REMOVE -> rem (box_id str) (label str)
  | ADD nr -> add (box_id str) (label str) nr

let lense_power box_id slot_id focal =
  let p = (1 + box_id) * (1 + slot_id) * focal in
  log#ltrace (lazy (Printf.sprintf "Box %d: Lens %d: %d" box_id slot_id p));
  p

let box_power id lst =
  List.mapi (fun slot_id (_, focal) -> lense_power id slot_id focal) lst
  |> List.fold_left ( + ) 0

let rec one_line = function
  | [] -> ""
  | hd :: tl -> Printf.sprintf "%s%s" hd (one_line tl)

let split_comma line =
  String.split_on_char ',' line
  |> List.map (fun str -> List.init (String.length str) (String.get str))

let one _input =
  (* solve one*)
  let input = List.rev _input |> one_line |> split_comma in
  List.map hash input |> List.fold_left ( + ) 0

let two _input =
  (* solve one*)
  let input = List.rev _input |> one_line |> split_comma in
  List.iter process_box input;
  Boxes.to_seq !table
  |> Seq.map (fun (box_id, lst) ->
         log#ltrace
           (lazy
             (Printf.sprintf "Box %d: %s" box_id
                (String.concat " "
                   (List.map (fun (l, f) -> Printf.sprintf "[%s %d]" l f) lst))));
         box_power box_id lst)
  |> Seq.fold_left ( + ) 0

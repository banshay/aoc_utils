module CC = struct
  type card = A | K | Q | J | T | NR of int [@@deriving eq, ord, show]

  type combination =
    | Five_of_kind
    | Four_of_kind
    | Full_house
    | Three_of_kind
    | Two_pair
    | Pair
    | High_card
  [@@deriving show]

  type hand = { cards : card list; combination : combination; number : int }

  let str_to_card = function
    | "A" -> A
    | "K" -> K
    | "Q" -> Q
    | "J" -> J
    | "T" -> T
    | x -> NR (int_of_string x)

  let hand_to_combination cards =
    let rec count ?(acc = 0) card = function
      | [] -> acc
      | x :: rest when x = card -> count card ~acc:(acc + 1) rest
      | _ :: rest -> count card ~acc rest
    in
    match Lib.Util.unique cards with
    | [ _ ] -> Five_of_kind
    | [ x1; _ ] -> (
        match count x1 cards with 1 | 4 -> Four_of_kind | _ -> Full_house)
    | [ x1; x2; _ ] -> (
        match count x1 cards with
        | 3 -> Three_of_kind
        | 2 -> Two_pair
        | _ -> ( match count x2 cards with 3 -> Three_of_kind | _ -> Two_pair))
    | [ _; _; _; _ ] -> Pair
    | [ _; _; _; _; _ ] -> High_card
    | _ ->
        Printf.eprintf "Hand has more or less than 5 cards\n";
        assert false

  let compare_hands hand1 hand2 =
    match compare hand1.combination hand2.combination with
    | 0 ->
        List.map2 compare hand1.cards hand2.cards
        |> List.fold_left (fun ret x -> if ret = 0 then x else ret) 0
    | x -> x

  let parse input =
    List.map
      (fun line ->
        let c, w =
          match String.split_on_char ' ' line with
          | [ c; w ] -> (c, w)
          | _ ->
              Printf.eprintf "Line has wrong shape\n";
              assert false
        in
        let cards =
          String.to_seq c
          |> Seq.map (String.make 1)
          |> Seq.map str_to_card
          |> Seq.fold_left (fun l x -> x :: l) []
          |> List.rev
        in
        let combination = hand_to_combination cards in
        (* Printf.printf "%s -> %s -> %s\n" *)
        (*   (String.concat ", " (List.map show_card cards)) *)
        (*   (show_combination combination) *)
        (*   w; *)
        { cards; combination; number = int_of_string w })
      input
end

let one _input =
  (* solve one*)
  CC.parse _input |> List.sort CC.compare_hands |> List.rev
  (* |> (fun x -> *)
  (*      Printf.printf "-------\n"; *)
  (*      List.iteri *)
  (*        (fun i hand -> *)
  (*          Printf.printf "%s -> %s -> %d -> rank %d winnings %d\n" *)
  (*            (String.concat ", " (List.map CC.show_card hand.CC.cards)) *)
  (*            (CC.show_combination hand.CC.combination) *)
  (*            hand.CC.number (i + 1) *)
  (*            ((i + 1) * hand.CC.number)) *)
  (*        x; *)
  (*      x) *)
  |> List.mapi (fun i hand ->
         let w = hand.CC.number in
         (i + 1) * w)
  |> List.fold_left ( + ) 0

let two _input =
  (* solve one*)
  0

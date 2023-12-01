let command = ref []
let dir = ref "./"

let command_spec_list =
  [ ("-dir", Arg.Set_string dir, "Directory where to run the utils.") ]

let anon_fun c =
  command := !command @ [c]

let () =
  Arg.parse command_spec_list anon_fun "COMMAND is required";
  match !command with
  | "clear" :: rest -> (
      match rest with
      | lower :: upper :: _ ->
          Lib.Setup.cleanup !dir (int_of_string lower)
            (int_of_string upper)
      | lower :: _ -> Lib.Setup.cleanup !dir (int_of_string lower) 25
      | [] -> Lib.Setup.cleanup !dir 1 25)
  | "create" :: rest -> (
      match rest with
      | lower :: upper :: _ ->
          Lib.Setup.create !dir (int_of_string lower)
            (int_of_string upper)
      | lower :: _ -> Lib.Setup.create !dir (int_of_string lower) 25
      | [] -> Lib.Setup.create !dir 1 25)
  | _ -> print_endline "Command not found!"

module Garden = struct
  type garden = {
    seeds: int list;
    seed_to_soil: (int, int) Hashtbl.t;
    soil_to_fert: (int, int) Hashtbl.t;
    fert_to_water: (int, int) Hashtbl.t;
    water_to_light: (int, int) Hashtbl.t;
    light_to_temp: (int, int) Hashtbl.t;
    temp_to_humid: (int, int) Hashtbl.t;
    humid_to_location: (int, int) Hashtbl.t;
  }

  let create input =
    Lib.Util.list_of_list "" input
    |> 


end




let one _input =
  (* solve one*)
  0

let two _input =
  (* solve one*)
  0
  

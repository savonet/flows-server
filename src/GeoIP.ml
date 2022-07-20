let lookup_string ip =
  let cmd = Filename.quote_command "geoiplookup" ["-f"; "GeoIPCity.dat"; ip] in
  let ic = Unix.open_process_in cmd in
  let s = input_line ic in
  ignore (Unix.close_process_in ic);
  s

type t = {
  country_code : string;
  region : string;
  city : string;
  latitude : float;
  longitude : float;
}

let lookup_opt ip =
  let s = try lookup_string ip with _ -> "" in
  match String.split_on_char ':' s with
    | [_; l] -> (
        match String.split_on_char ',' l with
          | [country_code; _; region; city; latitude; longitude; _; _] ->
              let latitude = float_of_string latitude in
              let longitude = float_of_string longitude in
              Some { country_code; region; city; latitude; longitude }
          | _ -> None)
    | _ -> None

(* let () = Printf.printf "lookup: %s\n%!" (lookup_string "80.60.233.195") *)

let lookup ip =
  let cmd = Filename.quote_command "geoiplookup" ["-f";"GeoIPCity.dat";ip] in
  let ic = Unix.open_process_in cmd in
  input_line ic

let () =
  Printf.printf "lookip: %s\n" (lookup "80.60.233.195")

type t = {
  website : string;
  description : string;
  genre : string
}

let to_string k r = Printf.sprintf "%S,%S,%S,%S" k r.website r.description r.genre

let of_string r = Scanf.sscanf r "%S,%S,%S,%S" (fun radio website description genre -> radio, {website; description; genre})

let radios = DB.create ~to_string ~of_string "radios"

let register ~website ~description ~genre radio =
  DB.add radios radio { website; description; genre }

type t = {
  name : string;
  user : string;
  website : string;
  description : string;
  genre : string
}

let to_string id r = Printf.sprintf "%S,%S,%S,%S,%S,%S" id r.name r.user r.website r.description r.genre

let of_string r = Scanf.sscanf r "%S,%S,%S,%S,%S,%S" (fun id name user website description genre -> id, {name; user; website; description; genre})

let db = DB.create ~to_string ~of_string "radios"

let id r = r.user ^ "/" ^ r.name

let register ~name ~user ~website ~description ~genre =
  let r = { name; user; website; description; genre } in
  DB.add db (id r) r

(* let json_all () = *)
  (* List.map *)

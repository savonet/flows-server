type stream =
  {
    format : string;
    url : string;
  }

type t = {
  name : string;
  user : string;
  website : string;
  description : string;
  genre : string;
  artist : string;
  title : string;
  streams : stream list;
  last : float; (** last update *)
}

let to_string id r = Printf.sprintf "%S,%S,%S,%S,%S,%S,%f" id r.name r.user r.website r.description r.genre r.last

let of_string r = Scanf.sscanf r "%S,%S,%S,%S,%S,%S,%f" (fun id name user website description genre last -> id, {name; user; website; description; genre; artist = "?"; title = "?"; streams = []; last })

let db = DB.create ~to_string ~of_string "radios"

let id ~radio ~user = user ^ "/" ^ radio

let register ~name ~user ~website ~description ~genre =
  let last = Unix.time () in
  let r = { name; user; website; description; genre; artist = "?"; title = "?"; streams = []; last } in
  DB.add db (id ~radio:name ~user) r

let find_opt ~user ~radio =
  DB.find_opt db (id ~user ~radio)

let set r =
  DB.add db (id ~user:r.user ~radio:r.name) r

let clear_streams r =
  set {r with streams = []}

let add_stream r ~format ~url =
  set {r with streams = {format;url}::r.streams}

(* let all_to_json () = *)
  (* List.map ) db; *)
  (* Yojson.Basic.to_string json *)

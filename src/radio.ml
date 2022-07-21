(** Operations on radios. *)

(** A stream. *)
type stream = { format : string; url : string } [@@deriving yojson]

(* Public radio payload *)
module Public = struct
  type t = {
    id : string;
    name : string;
    website : string;
    description : string;
    genre : string;
    longitude : float;
    latitude : float;
    artist : string;
    title : string;
    streams : stream list;
  }
  [@@deriving yojson]
end

(** A radio. *)
type t = {
  name : string;
  user : string option;
  website : string;
  description : string;
  genre : string;
  longitude : float;
  latitude : float;
  artist : string;
  title : string;
  streams : stream list;
  last : float;  (** last update *)
}
[@@deriving
  yojson, stable_record ~version:Public.t ~remove:[user; last] ~add:[id]]

let to_json = yojson_of_t
let of_json = t_of_yojson
let db = DB.create ~every:60. ~to_json ~of_json "radios"
let id ~radio ~user =
  match user with
  | Some user -> radio ^ "@" ^ user
  | None -> radio

(** Register a radio. *)
let register ~name ~user ~website ~description ~genre ~longitude ~latitude ~streams =
  let last = Unix.time () in
  let r =
    {
      name;
      user;
      website;
      description;
      genre;
      longitude;
      latitude;
      artist = "?";
      title = "?";
      streams;
      last;
    }
  in
  DB.add db (id ~radio:name ~user) r

(** Find radio with given user and radio name. *)
let find_opt ~user ~radio = DB.find_opt db (id ~user ~radio)

(** Update values for radio. *)
let set r =
  let radio_id r = id ~radio:r.name ~user:r.user in
  DB.add db (radio_id r) r

let ping r =
  let r = { r with last = Unix.time () } in
  set r

(** Set metadata of the currently playing title. *)
let set_metadata r ~artist ~title = set { r with artist; title }

let to_seq () =
  let now = Unix.time () in
  DB.to_seq db
  (* Forget radios not updated for more than 1h. *)
  |> Seq.filter (fun (_, r) -> now -. r.last <= 3600.)

let to_list () = to_seq () |> List.of_seq

(** Export all radios to JSON. *)
let all_to_json () =
  to_seq ()
  |> Seq.map (fun (id, r) -> [%yojson_of: Public.t] (to_Public_t ~id r))
  |> List.of_seq
  |> fun l -> `List l |> JSON.to_string ~pretty:true

let iter = DB.iter db

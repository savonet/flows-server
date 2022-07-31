(** Operations on radios. *)

open Db_setup

(** A stream. *)
type stream = { format : string; url : string } [@@deriving yojson]

(* Public radio payload *)
module Public = struct
  type t = {
    id : int;
    name : string;
    website : string option;
    description : string option;
    genre : string option;
    longitude : float option;
    latitude : float option;
    artist : string option;
    title : string option;
    streams : stream list;
  }
  [@@deriving yojson]
end

(** A radio. *)
type t = {
  id : int;
  name : string;
  user : string;
  website : string option;
  description : string option;
  genre : string option;
  logo : string option;  (** url of the logo *)
  longitude : float option;
  latitude : float option;
  artist : string option;
  title : string option;
  streams : stream list;
  created_at : float;
  updated_at : float;
}
[@@deriving
  yojson,
    stable_record ~version:Public.t ~remove:[user; logo; created_at; updated_at]]

let from_sql id =
  let id = Int32.of_int id in
  let radio =
    List.hd
      [%pgsql.object
        db "load_custom_from=src/db_config.sexp" "show=pp"
          "SELECT\n\
          \       \"radio\".\"id\" AS id,\n\
          \       \"radio\".\"name\" AS name,\n\
          \       \"flows_user\".\"name\" AS user,\n\
          \       website,\n\
          \       description,\n\
          \       genre,\n\
          \       logo,\n\
          \       longitude,\n\
          \       latitude,\n\
          \       artist,\n\
          \       title,\n\
          \       \"radio\".\"updated_at\" AS updated_at,\n\
          \       \"radio\".\"created_at\" AS created_at\n\
          \     FROM radio\n\
          \     LEFT JOIN flows_user ON \"flows_user\".\"id\" = \
           \"radio\".\"user_id\"\n\
          \     WHERE \"radio\".\"id\" = $id"]
  in
  let streams =
    List.map
      (fun (format, url) -> { format; url })
      [%pgsql db "SELECT format, url FROM stream WHERE radio_id = $id"]
  in
  {
    id = radio#id;
    user = radio#user;
    website = radio#website;
    name = radio#name;
    logo = radio#logo;
    description = radio#description;
    genre = radio#genre;
    longitude = radio#longitude;
    latitude = radio#latitude;
    artist = radio#artist;
    title = radio#title;
    streams;
    updated_at = radio#updated_at;
    created_at = radio#created_at;
  }

let to_json = yojson_of_t
let of_json = t_of_yojson
let db = DB.create ~every:60. ~to_json ~of_json "radios"
let id ~radio ~user = radio ^ "@" ^ user

(** Register a radio. *)
let register ~name ~user ~website ~description ~genre ~logo ~longitude ~latitude
    ~streams () =
  let updated_at = Unix.time () in
  let r =
    {
      id = 123;
      name;
      user;
      website;
      description;
      genre;
      logo;
      longitude;
      latitude;
      artist = None;
      title = None;
      streams;
      created_at = updated_at;
      updated_at;
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
  let r = { r with updated_at = Unix.time () } in
  set r

(** Set metadata of the currently playing title. *)
let set_metadata r ~artist ~title = set { r with artist; title }

let to_seq () =
  let now = Unix.time () in
  DB.to_seq db
  (* Forget radios not updated for more than 1h. *)
  |> Seq.filter (fun (_, r) -> now -. r.updated_at <= 3600.)

let to_list () = to_seq () |> List.of_seq

(** Export all radios to JSON. *)
let all_to_json () =
  to_seq ()
  |> Seq.map (fun (_, r) -> [%yojson_of: Public.t] (to_Public_t r))
  |> List.of_seq
  |> fun l -> `List l |> JSON.to_string ~pretty:true

let iter = DB.iter db
